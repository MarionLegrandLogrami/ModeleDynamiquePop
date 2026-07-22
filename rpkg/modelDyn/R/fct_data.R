######################################################################################
# Script de création des fonctions servant à faire des traitements sur les données ###
# @uteur : Marion LEGRAND - LOGRAMI                                                ###
# date : 2022_04_08                                                                ###
#                                                                                  ###
# Documentation : chaque fonction a un bloc #' (roxygen2) juste au-dessus. Pour   ###
# que ?nom_fonction fonctionne dans R, lancer script/fonctions/build_docs_package.R ##
# (à refaire après toute modification de ce fichier) - voir rpkg/README.md.       ###
######################################################################################

#' Lit un paramètre CODA (chain + index) depuis un dossier de sorties OpenBUGS
#'
#' Enveloppe \code{\link[coda]{read.coda}} pour construire les noms de fichiers
#' \code{<file_prefix>CODAchain1.txt} / \code{<file_prefix>CODAindex.txt} à
#' partir du dossier de données du modèle.
#'
#' @param name Nom du paramètre tel qu'utilisé dans les noms de fichiers CODA (ex. "Rmax").
#' @param datawd Chemin du dossier contenant les fichiers CODA du modèle (avec "/" final).
#' @param subdir Sous-dossier optionnel (ex. "simulation"), pour les paramètres que le modèle range à part.
#' @param file_prefix Préfixe des fichiers CODA si différent de \code{name}. Par défaut \code{name}.
#' @return Un objet \code{mcmc} (package coda) : matrice itérations x paramètre(s).
#' @export
read_param_coda <- function(name, datawd, subdir = NULL, file_prefix = name) {
  base <- if (is.null(subdir)) datawd else str_c(datawd, subdir, "/")
  read.coda(str_c(base, file_prefix, "CODAchain1.txt"),
            str_c(base, file_prefix, "CODAindex.txt"))
}

#' Quantiles et moyenne d'un paramètre CODA
#'
#' Calcule les quantiles (par défaut 5/15/.../95\%) et la moyenne d'un paramètre
#' CODA, dans le format attendu par les graphiques boxplot par année
#' (\code{\link{custom_plot}} / \code{\link{boxplot_years_png}}).
#'
#' @param param Un objet \code{mcmc} issu de \code{\link[coda]{read.coda}}.
#' @param probs Vecteur de probabilités pour les quantiles. Défaut : \code{seq(0, 1, 0.1)}.
#' @return Une liste avec \code{summary} (sortie de \code{summary.mcmc}), \code{quantiles} et \code{mean}.
#' @export
param_quantiles <- function(param, probs = seq(0, 1, 0.1)) {
  sum <- summary(param, probs = probs)
  mean_col <- if (is.matrix(sum$statistics)) {
    cbind(sum$statistics[, "Mean"])
  } else {
    cbind(sum$statistics["Mean"])
  }
  list(summary = sum, quantiles = sum$quantiles, mean = mean_col)
}

#' Échappe les underscores pour LaTeX
#'
#' @param x Chaîne de caractères (ex. nom de paramètre "beta_Q_VB").
#' @return La chaîne avec les "_" échappés en "\\_".
#' @examples
#' latex_escape("beta_Q_VB")
#' @export
latex_escape <- function(x) gsub("_", "\\_", x, fixed = TRUE)

#' Réaligne un paramètre CODA partiellement échantillonné sur une grille 1:T
#'
#' Certains paramètres du modèle d'interaction réciproque (p_Rmax_V/A/L/P) ne
#' sont échantillonnés que pour certaines années, avec des noms de colonnes du
#' type "p_Rmax\[7,1\]", "p_Rmax\[15,1\]", etc. Cette fonction reconstruit une
#' série complète sur 1:T, en remplissant de NA les années absentes.
#'
#' @param param Objet \code{mcmc} dont les noms de colonnes encodent l'année (ex. "p_Rmax[7,1]").
#' @param T Nombre total d'années du modèle.
#' @param probs Probabilités pour les quantiles. Défaut : \code{seq(0, 1, 0.1)}.
#' @return Une liste avec \code{quantiles} (matrice T x 5, NA pour les années absentes) et \code{mean} (vecteur de longueur T).
#' @export
align_sparse_year_param <- function(param, T, probs = seq(0, 1, 0.1)) {
  sum <- summary(param, probs = probs)
  years_present <- as.numeric(gsub("[^0-9]", "", gsub(",[0-9]", "", rownames(sum$quantiles))))

  quantiles_full <- matrix(NA, nrow = T, ncol = 5)
  quantiles_full[years_present, ] <- sum$quantiles

  mean_full <- rep(NA, T)
  mean_full[years_present] <- sum$statistics[, "Mean"]

  list(quantiles = quantiles_full, mean = mean_full)
}

#' Masque de NA les années "tirage à blanc" (sans déversement réel)
#'
#' Pour un paramètre CODA aligné positionnellement sur une plage d'années
#' contiguë (ex. d_juv_moy_V, lignes = années 2:T dans l'ordre), remplace par
#' NA les années où \code{indicator} vaut 0 (pas de déversement réel cette
#' année-là, juste un tirage à blanc dans le modèle).
#'
#' @param param Objet \code{mcmc} aligné positionnellement sur une plage d'années.
#' @param indicator Vecteur indicateur (ex. \code{I_juv_moy[<plage>, col]}) : 1 = déversement réel, 0 = tirage à blanc à masquer.
#' @return Une liste avec \code{quantiles} et \code{mean}, les années à \code{indicator == 0} mises à NA.
#' @export
mask_no_deversement <- function(param, indicator) {
  sum <- summary(param, probs = seq(0, 1, 0.1))
  mean_col <- if (is.matrix(sum$statistics)) sum$statistics[, "Mean"] else sum$statistics["Mean"]
  quantiles <- sum$quantiles
  quantiles[indicator == 0, ] <- NA
  mean_col[indicator == 0] <- NA
  list(quantiles = quantiles, mean = mean_col)
}

#' Standardise des résidus par un écart-type tiré par itération MCMC
#'
#' Calcule \code{std[i, t] = res[i, t] / sigma[i]}. Motif récurrent pour les
#' résidus standardisés du modèle (res_vichy_std, res_p_*_std,
#' res_wild_moy_*_std, res_juv_moy_*_std, ...).
#'
#' @param res Objet coda/matrice des résidus (itérations x années).
#' @param sigma Vecteur (1 valeur par itération MCMC) de l'écart-type associé.
#' @return Un objet \code{mcmc} des résidus standardisés, mêmes noms de colonnes que \code{res}.
#' @export
standardize_residuals <- function(res, sigma) {
  std <- sweep(as.matrix(res), 1, as.vector(sigma), "/")
  colnames(std) <- colnames(res)
  as.mcmc(std)
}

#' Table récapitulative .tex pour un paramètre CODA multivarié
#'
#' Écrit une table récapitulative multi-lignes (une ligne par sous-paramètre,
#' ex. nu_d_V/A/L/P), avec des noms de lignes personnalisés, au format .tex via
#' \code{xtable}. Variante "plusieurs lignes" de \code{\link{write_param_table}}.
#'
#' @param param Objet coda multivarié (plusieurs colonnes).
#' @param tabwd Dossier de destination du fichier .tex (avec "/" final).
#' @param filename Nom du fichier .tex (sans extension).
#' @param row_labels Vecteur des noms de lignes (un par sous-paramètre).
#' @param label Label LaTeX (suffixé "_tab" en interne pour éviter les collisions
#'   avec le \code{\\label\{\}} de la figure associée). Défaut : \code{filename}.
#' @param caption Légende de la table. Si NULL, générée automatiquement ("Statistiques de <label>").
#' @param digits Nombre de décimales passé à \code{xtable} (NULL = défaut de xtable).
#' @return (Invisible) le résumé \code{summary.mcmc} du paramètre ; écrit aussi le fichier .tex en effet de bord.
#' @export
write_multiparam_table <- function(param, tabwd, filename, row_labels, label = filename,
                                    caption = NULL, digits = NULL) {
  if (is.null(caption)) caption <- str_c("Statistiques de ", latex_escape(label))

  sum <- summary(param, probs = seq(0, 1, 0.1))
  tabl <- cbind(sum$quantiles, Mean = sum$statistics[, "Mean"], SD = sum$statistics[, "SD"])
  rownames(tabl) <- row_labels

  # label suffixé "_tab" pour ne pas entrer en collision avec le \label{} du
  # \begin{figure} associé au même nom (cf. "Label multiply defined")
  tab <- if (is.null(digits)) {
    xtable(x = tabl, label = str_c(label, "_tab"), caption = caption)
  } else {
    xtable(x = tabl, label = str_c(label, "_tab"), caption = caption, digits = digits)
  }

  print(tab, file = str_c(tabwd, filename, ".tex"),
        table.placement = "htbp",
        caption.placement = "top",
        NA.string = "",
        include.rownames = TRUE)

  invisible(sum)
}

#' Table récapitulative .tex pour un paramètre CODA scalaire
#'
#' Écrit une table récapitulative (quantiles + Mean + SD) d'un paramètre
#' scalaire, au format .tex via \code{xtable}. C'est la 2e moitié du motif le
#' plus fréquent du rapport SortieOpenbugs_parameters.Rnw (voir aussi
#' \code{\link{report_param}} / \code{\link{png_trace_density}} dans
#' fct_graph.R, qui l'associent au graphique trace/densité).
#'
#' @param param Objet coda scalaire (1 colonne).
#' @param tabwd Dossier de destination du fichier .tex (avec "/" final).
#' @param filename Nom du fichier .tex (sans extension).
#' @param label Label LaTeX (suffixé "_tab" en interne). Défaut : \code{filename}.
#' @param caption Légende de la table. Si NULL, générée automatiquement ("Statistiques de <label>").
#' @param digits Nombre de décimales passé à \code{xtable} (NULL = défaut de xtable).
#' @return (Invisible) le résumé \code{summary.mcmc} du paramètre ; écrit aussi le fichier .tex en effet de bord.
#' @export
write_param_table <- function(param, tabwd, filename, label = filename,
                               caption = NULL, digits = NULL) {
  if (is.null(caption)) caption <- str_c("Statistiques de ", latex_escape(label))

  sum <- summary(param, probs = seq(0, 1, 0.1))
  tabl <- sum$quantiles
  tabl["Mean"] <- sum$statistics["Mean"]
  tabl["SD"] <- sum$statistics["SD"]
  tabl <- rbind(tabl) #pour en faire une table

  # label suffixé "_tab" pour ne pas entrer en collision avec le \label{} du
  # \begin{figure} associé au même nom (cf. "Label multiply defined")
  tab <- if (is.null(digits)) {
    xtable(x = tabl, label = str_c(label, "_tab"), caption = caption)
  } else {
    xtable(x = tabl, label = str_c(label, "_tab"), caption = caption, digits = digits)
  }

  print(tab, file = str_c(tabwd, filename, ".tex"),
        table.placement = "htbp",
        caption.placement = "top",
        NA.string = "",
        include.rownames = FALSE)

  invisible(sum)
}

#' Surfaces productives pour les juvéniles (S_juv_JP), complétées pour les projections
#'
#' Régénère puis charge le fichier \code{data.R} du modèle (dump JAGS de
#' \code{data.txt} via \code{bugs2jags}), en extrait la matrice
#' \code{S_juv_JP} (surfaces par secteur x année), la transpose en 4 x T et la
#' complète de 40 colonnes supplémentaires (répétition de la dernière année)
#' pour couvrir les projections à 20 ans.
#'
#' @details
#' Si l'objet \code{datawd} n'existe pas dans l'environnement appelant, la
#' fonction demande interactivement (via \code{readline()}) de saisir le
#' chemin du dossier CODA. Il est donc recommandé de définir \code{datawd}
#' avant d'appeler cette fonction dans un script ou un document Rmd. Attention :
#' cette fonction \code{source()} le \code{data.R} régénéré dans l'environnement
#' global, ce qui écrase au passage toute variable de même nom qui y serait
#' définie (ex. \code{S_juv_JP}, \code{T}) - à appeler en dernier si d'autres
#' étapes du script sourcent elles-mêmes un fichier data.R équivalent.
#'
#' @param surf_2021 Si TRUE (défaut), utilise le format de data.txt du modèle
#'   depuis la version 2021 (surfaces incluses dans data.txt). Si FALSE, le
#'   chemin demandé interactivement pointe directement vers un fichier data.R.
#' @return Une matrice 4 x (T+40) : 4 secteurs (V/A/L/P) en lignes, années en colonnes.
#' @export
keep_good_surf<-function(surf_2021=TRUE) {
  if(surf_2021==TRUE) {
    if(!exists("datawd")) {
      print(message(iconv("Entrez dans la console R le chemin du dossier où sont enregistrées les CODA du modèle","UTF8")))
      #print(message("Entrez dans la console R le chemin"))
      datawd <- readline()
    } else {}

  } else {
    print(message("Entrez dans la console R le chemin vers le fichier data.R du modèle"))
    datawd <- readline()
  }
  bugs2jags(str_c(datawd,"data.txt"),str_c(datawd,"data.R"))
  source(str_c(datawd,"data.R"))
  surf <- t(S_juv_JP)
  surf <- cbind(surf,matrix(rep(surf[,ncol(surf)],40),nrow=4))
  return(surf)
}

# Fonction qui calcul sur un jeu de données la moyenne des 5 dernières années
# average_5_yr<-function(data,period=last){
#     ##debug
#     data=niv
#     if(last=)
# }

#' Exporte un taux de renouvellement vers un classeur Excel
#'
#' Calcule la moyenne mobile 5 ans (ou la moyenne brute par année) d'une
#' matrice de taux de renouvellement (échelle log), et l'écrit dans un onglet
#' d'un fichier .xlsx (un onglet par scénario, pour compilation ultérieure via
#' \code{\link{concat_export_tab}}).
#'
#' @details
#' Si l'objet \code{tabwd} n'existe pas dans l'environnement appelant, la
#' fonction demande interactivement (via \code{readline()}) de saisir le
#' chemin du dossier de destination.
#'
#' @param tx_renouv Matrice du taux de renouvellement en échelle log (itérations x années).
#' @param Tyear Nombre d'années à considérer. Défaut : la variable globale \code{T}.
#' @param simulation Si TRUE, ajoute 20 ans de simulation à \code{Tyear} avant le calcul.
#' @param type_export \code{"running_mean"} (moyenne mobile 5 ans) ou \code{"mean_data"} (moyenne brute par année).
#' @param filename Nom du fichier .xlsx (sans extension).
#' @param add_existing_table Si TRUE, ajoute l'onglet à un classeur existant plutôt que de l'écraser.
#' @param scenario Nom du scénario, utilisé comme nom de colonne et de feuille.
#' @return Rien (effet de bord : écrit le fichier .xlsx).
#' @export
export_Tx_ren<-function(tx_renouv,Tyear=T,simulation=TRUE,type_export="running_mean",filename,add_existing_table=TRUE,scenario){
  ## debug
  # tx_renouv <- renew_rate_w_coef
  # scenario <- "scénario9"
  # filename <-"test_export_data"
                  require("xlsx")
                  if(simulation==TRUE){Tyear_def<-Tyear+20}else{Tyear_def<-Tyear}
                  data <- stack(as.data.frame(tx_renouv[,7:(Tyear_def-6)]))
                  mean_data <- tapply(exp(data[,1]),data[,2],mean)
                  if (type_export=="running_mean"){
                    table<-as.data.frame(running.mean(mean_data,5)[!is.na(running.mean(mean_data,5))])
                    if(exists("scenario")){colnames(table)<-scenario}else{}
                    rownames(table)<-seq(1985,(1974+Tyear_def-6),1)

                  }
                  if (type_export=="mean_data"){
                    table<-as.data.frame(mean_data)
                    if(exists("scenario")){colnames(table)<-scenario}else{}
                    rownames(table)<-seq((1974+7),(1974+Tyear_def-6),1)
                  }
                  if(!(type_export %in% c("running_mean","mean_data"))){
                    print(message(iconv("Les deux seules modalités définies sont 'running_mean' et 'mean_data'","UTF8")))
                  }
                  if(!exists("tabwd")) {
                    print(message(iconv("Entrez dans la console R le chemin du dossier où enregistrer le tableau","UTF8")))
                    tabwd <- readline()
                  }else{}

                  dataFilename<-str_c(tabwd,filename,".xlsx")
                  write.xlsx(x=table,file=dataFilename,sheetName =str_c(type_export,"_",scenario),append=add_existing_table)

}

#' Concatène les onglets d'un classeur exporté par export_Tx_ren
#'
#' Relit tous les onglets d'un fichier .xlsx produit par
#' \code{\link{export_Tx_ren}} (un onglet par scénario) et les empile dans un
#' unique data.frame, écrit dans un nouveau fichier "<tab>_concat.xlsx".
#'
#' @details
#' Si l'objet \code{tabwd} n'existe pas dans l'environnement appelant, la
#' fonction demande interactivement (via \code{readline()}) de saisir le
#' chemin du dossier où se trouve le classeur.
#'
#' @param tab Nom du fichier .xlsx à concaténer (sans extension).
#' @return Rien (effet de bord : écrit "<tab>_concat.xlsx").
#' @export
concat_export_tab<-function(tab){
  ###--- debug
  #tab <- "scenarii_dev_mean_data"
  require(readxl)
  require(xlsx)
  if(!exists("tabwd")){
    print(message(iconv("Entrez dans la console R le chemin du dossier où aller chercher le tableau","UTF8")))
    tabwd <- readline()
  }else{}
  dataFilename<-str_c(tabwd,tab,".xlsx")
  nbsheet<-length(excel_sheets(path=dataFilename))
  namesheet<-excel_sheets(path=dataFilename)
  for (i in 1:nbsheet){
    temp<-read_excel(path=dataFilename,sheet=i,col_name=TRUE)
    typedata<-as.character(str_extract_all(namesheet[i], "scenario.*"))
    temp$scenar<-rep(typedata,nrow(temp))
    if(!(exists("dat"))){
      coltype<-gsub(x=namesheet[i],"_scenario.*","")
      colnames(temp)<-c(iconv("année","UTF8"),coltype,"scenar")
      dat<-temp
    }else{
      coltype<-gsub(x=namesheet[i],"_scenario.*","")
      colnames(temp)<-c(iconv("année","UTF8"),coltype,"scenar")
      dat<-rbind(dat,temp)
      }
  }
  write.xlsx(x=dat,file=str_c(gsub(x=dataFilename,".xlsx",""),"_concat.xlsx"))
}

#===================================================================================
# Fonctions pour les 4 indicateurs PLAGEPOMI (script/analyse_retro/Indicateurs
# Plagepomi.Rmd). Portées depuis Indicateurs PLAGEPOMI.Rnw, en generalisant les
# blocs V/A/L/P/total répétés et en les adaptant au modèle 2026_07 (survie
# juv->adulte annualisée s_juv2ad[t] au lieu de la constante + scénario
# level_s/I_surv de l'ancien modèle).
#===================================================================================

#' Juvéniles atteignables pour une fraction de Rmax, par secteur et au total
#'
#' Généralise le motif Juv_Rmax_25/50/75/100 de l'indicateur "niveau de
#' population" : calcule, pour chaque itération MCMC et chaque année, la
#' quantité de juvéniles que produirait une fraction \code{pct} de la
#' capacité d'accueil (Rmax), secteur par secteur puis sommée. Avec
#' \code{pct = 1}, donne aussi le Rmax pondéré ("Rmax_ponder") utilisé par
#' \code{\link{compute_dc_wild_ratio}} (diagnostic de conservation).
#'
#' @param pct Fraction de Rmax visée (ex. 0.5 pour 50\% de Rmax).
#' @param Rmax Objet coda (1 colonne) : capacité d'accueil du modèle.
#' @param nu_d Objet coda (4 colonnes V/A/L/P) : différentiel de productivité par secteur.
#' @param S_juv_JP Matrice des surfaces productives (4 secteurs x années), voir \code{\link{keep_good_surf}}.
#' @param T Nombre d'années à calculer.
#' @return Une liste de 5 matrices (itérations x T) : \code{V}, \code{A}, \code{L}, \code{P} (par secteur) et \code{total}.
#' @export
compute_juv_rmax <- function(pct, Rmax, nu_d, S_juv_JP, T) {
  Rmax <- as.vector(Rmax)
  zones <- lapply(1:4, function(z) outer(pct * Rmax * exp(nu_d[, z]), S_juv_JP[z, 1:T]))
  names(zones) <- c("V", "A", "L", "P")
  zones$total <- Reduce(`+`, zones)
  zones
}

#' Reconstitue la chronique d'adultes observés/estimés à Vichy
#'
#' Prend les comptages réels (\code{data_vichy}) pour les années où la station
#' de comptage de Vichy fonctionnait, et la moyenne postérieure du modèle
#' (\code{bugs_N_vichy_real}) sinon. Le découpage (années 1:22 estimées, 23:29
#' réelles, 30 estimée, 31:T réelles) reflète l'historique réel de la station
#' (arrêt ponctuel une année) et reste valable quel que soit T car ancré sur
#' des années calendaires fixes.
#'
#' @param bugs_N_vichy_real Objet coda (23 colonnes) : estimation du modèle pour les années sans comptage réel.
#' @param data_vichy Vecteur des comptages réels à Vichy (NA pour les années sans comptage), longueur T.
#' @param T Nombre total d'années.
#' @return Un vecteur de longueur T : adultes observés (années avec comptage) ou estimés (sinon) à Vichy.
#' @export
reconstitute_data_vichy <- function(bugs_N_vichy_real, data_vichy, T) {
  mean_bugs <- round(apply(bugs_N_vichy_real, MARGIN = 2, FUN = mean), 0)
  c(as.vector(mean_bugs[1:22]), data_vichy[23:29], as.vector(mean_bugs[23]), data_vichy[31:T])
}

#' Juvéniles sauvages par secteur et par année
#'
#' Calcule, pour chaque itération MCMC et chaque année, la quantité de
#' juvéniles sauvages par secteur (densité sauvage x surface), à partir des
#' paramètres \code{dmoywild_V/A/L/P}. Le secteur Poutes n'est colonisé/suivi
#' qu'à partir de l'année 13, d'où l'indice \code{t - 11} appliqué à
#' \code{d_wild_moy_P} (dont la 1ère colonne correspond à l'année 13).
#'
#' @param d_wild_moy_V,d_wild_moy_A,d_wild_moy_L,d_wild_moy_P Objets coda (itérations x années) : densité moyenne de juvéniles sauvages par secteur.
#' @param S_juv_JP Matrice des surfaces productives (4 secteurs x années), voir \code{\link{keep_good_surf}}.
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @return Une liste de 4 matrices (itérations x (T+1)) : \code{V}, \code{A}, \code{L}, \code{P}.
#' @export
compute_juv_wild_by_zone <- function(d_wild_moy_V, d_wild_moy_A, d_wild_moy_L, d_wild_moy_P,
                                      S_juv_JP, T, n_iter) {
  juv_V <- matrix(0, n_iter, T + 1)
  juv_A <- matrix(0, n_iter, T + 1)
  juv_L <- matrix(0, n_iter, T + 1)
  juv_P <- matrix(0, n_iter, T + 1)
  for (t in 1:T) {
    juv_V[, t + 1] <- d_wild_moy_V[, t] * S_juv_JP[1, t + 1]
    juv_A[, t + 1] <- d_wild_moy_A[, t] * S_juv_JP[2, t + 1]
    juv_L[, t + 1] <- d_wild_moy_L[, t] * S_juv_JP[3, t + 1]
  }
  for (t in 12:T) {
    juv_P[, t + 1] <- d_wild_moy_P[, t - 11] * S_juv_JP[4, t + 1]
  }
  list(V = juv_V, A = juv_A, L = juv_L, P = juv_P)
}

#' Pool de juvéniles sauvages à l'origine des retours d'adultes
#'
#' Calcule "juv_wild_tot_system" : la moyenne des cohortes de juvéniles
#' sauvages produites 3, 4 et 5 ans plus tôt (délai smolt + mer avant retour),
#' sommée sur les secteurs. Le secteur Poutes n'est intégré au système qu'à
#' partir de l'année 16, une fois le recul de cohorte disponible.
#'
#' @param juv_wild Liste des juvéniles sauvages par secteur, telle que retournée par \code{\link{compute_juv_wild_by_zone}}.
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @return Une matrice (itérations x (T+1)) : pool de juvéniles sauvages du système, par année.
#' @export
compute_juv_wild_tot_system <- function(juv_wild, T, n_iter) {
  tot_V <- matrix(0, n_iter, T + 1)
  tot_A <- matrix(0, n_iter, T + 1)
  tot_L <- matrix(0, n_iter, T + 1)
  tot_P <- matrix(0, n_iter, T + 1)
  for (t in 7:T) {
    tot_V[, t] <- (1 / 3) * juv_wild$V[, t - 3] + (1 / 3) * juv_wild$V[, t - 4] + (1 / 3) * juv_wild$V[, t - 5]
    tot_A[, t] <- (1 / 3) * juv_wild$A[, t - 3] + (1 / 3) * juv_wild$A[, t - 4] + (1 / 3) * juv_wild$A[, t - 5]
    tot_L[, t] <- (1 / 3) * juv_wild$L[, t - 3] + (1 / 3) * juv_wild$L[, t - 4] + (1 / 3) * juv_wild$L[, t - 5]
  }
  tot_P[, 16] <- (1 / 3) * juv_wild$P[, 16 - 3]
  tot_P[, 17] <- (1 / 3) * juv_wild$P[, 17 - 3] + (1 / 3) * juv_wild$P[, 17 - 4]
  for (t in 18:T) {
    tot_P[, t] <- (1 / 3) * juv_wild$P[, t - 3] + (1 / 3) * juv_wild$P[, t - 4] + (1 / 3) * juv_wild$P[, t - 5]
  }
  system <- matrix(0, n_iter, T + 1)
  for (t in 7:15) system[, t] <- tot_V[, t] + tot_A[, t] + tot_L[, t]
  for (t in 16:T) system[, t] <- tot_V[, t] + tot_A[, t] + tot_L[, t] + tot_P[, t]
  system
}

#' Coefficients de pondération des cohortes de juvéniles voisines
#'
#' Calcule les coefficients de pondération des 3 cohortes de juvéniles
#' (t-4/t-5/t-6, t-3/t-4/t-5, t-2/t-3/t-4) utilisés pour répartir un retour
#' d'adultes observé à l'année t entre les 3 années de production juvénile
#' voisines (délai mer variable selon les individus). Basé sur les juvéniles
#' sauvages bruts par secteur (pas la cohorte déjà agrégée).
#'
#' @param juv_wild Liste des juvéniles sauvages par secteur, telle que retournée par \code{\link{compute_juv_wild_by_zone}}.
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @return Une liste de 3 matrices (itérations x T) : \code{coef1}, \code{coef2}, \code{coef3}.
#' @export
compute_juv_cohort_weights <- function(juv_wild, T, n_iter) {
  coef1 <- matrix(0, n_iter, T)
  coef2 <- matrix(0, n_iter, T)
  coef3 <- matrix(0, n_iter, T)
  sum_zones <- function(t) juv_wild$V[, t] + juv_wild$A[, t] + juv_wild$L[, t] + juv_wild$P[, t]
  for (t in 7:T) {
    s2 <- sum_zones(t - 2); s3 <- sum_zones(t - 3); s4 <- sum_zones(t - 4)
    s5 <- sum_zones(t - 5); s6 <- sum_zones(t - 6)
    coef1[, t] <- s4 / (s4 + s5 + s6)
    coef2[, t] <- s4 / (s3 + s4 + s5)
    coef3[, t] <- s4 / (s2 + s3 + s4)
  }
  list(coef1 = coef1, coef2 = coef2, coef3 = coef3)
}

#' Adultes d'origine sauvage revenant à Vichy
#'
#' Calcule N_wild_vichy à partir du pool de juvéniles sauvages, de la survie
#' juv->adulte annuelle du modèle (\code{s_juv2ad[t]}, qui remplace la
#' constante + ajustement de scénario level_s/I_surv de l'ancien modèle) et du
#' résidu du modèle sur les retours à Vichy (\code{res_vichy}, appliqué de la
#' même façon à la population totale et à la population sauvage, comme dans
#' l'ancien code).
#'
#' @param juv_wild_tot_system Pool de juvéniles sauvages du système, tel que retourné par \code{\link{compute_juv_wild_tot_system}}.
#' @param s_juv2ad Objet coda de la survie juv->adulte annuelle, indexé à partir de l'année 7 (colonne 1 = année 7).
#' @param res_vichy Objet coda du résidu du modèle sur les retours à Vichy, indexé à partir de l'année 7 (colonne 1 = année 7).
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @return Une matrice (itérations x T) : adultes d'origine sauvage à Vichy, par année (0 pour les années < 7, non calculables).
#' @export
compute_N_wild_vichy <- function(juv_wild_tot_system, s_juv2ad, res_vichy, T, n_iter) {
  N_wild_vichy <- matrix(0, n_iter, T)
  for (t in 7:T) {
    N_wild_vichy[, t] <- exp(log(juv_wild_tot_system[, t]) + log(s_juv2ad[, t - 6]) + res_vichy[, t - 6])
  }
  N_wild_vichy
}

#' Taux de renouvellement de la population sauvage
#'
#' Calcule renew_rate_w_coef (échelle log) : le ratio entre le retour
#' d'adultes sauvages pondéré sur 3 cohortes voisines et le nombre d'adultes
#' revenus à Vichy 5 ans plus tôt (génération parentale). Le dénominateur
#' alterne entre l'estimation bayésienne (\code{N_vichy_bugs}, pour les années
#' sans comptage réel) et le comptage réel (\code{data_vichy}) : ce découpage
#' reflète l'historique réel de la station de comptage de Vichy (cf.
#' \code{\link{reconstitute_data_vichy}}) et reste valable quel que soit T.
#'
#' @param N_wild_vichy Adultes d'origine sauvage à Vichy, tel que retourné par \code{\link{compute_N_wild_vichy}}.
#' @param coef Coefficients de pondération des cohortes, tels que retournés par \code{\link{compute_juv_cohort_weights}}.
#' @param N_vichy_bugs Objet coda (23 colonnes) : estimation bayésienne du nombre d'adultes à Vichy pour les années sans comptage réel.
#' @param data_vichy Vecteur des comptages réels à Vichy (NA pour les années sans comptage), longueur T.
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @return Une matrice (itérations x T) : taux de renouvellement en échelle log, NA hors de la plage calculable.
#' @export
compute_renew_rate_wild <- function(N_wild_vichy, coef, N_vichy_bugs, data_vichy, T, n_iter) {
  renew <- matrix(NA, n_iter, T)
  weighted <- function(t) {
    coef$coef1[, t] * N_wild_vichy[, t - 1] +
      coef$coef2[, t] * N_wild_vichy[, t] +
      coef$coef3[, t] * N_wild_vichy[, t + 1]
  }
  for (t in 7:27) renew[, t - 5] <- log(weighted(t) / N_vichy_bugs[, t - 5])
  for (t in 28:34) renew[, t - 5] <- log(weighted(t) / data_vichy[t - 5])
  for (t in 35) renew[, t - 5] <- log(weighted(t) / N_vichy_bugs[, t - 12])
  for (t in 36:(T - 1)) renew[, t - 5] <- log(weighted(t) / data_vichy[t - 5])
  renew
}

#' Pourcentage de Rmax atteint par la production de juvéniles sauvages
#'
#' Calcule DC_tot_wild_tot : la production de juvéniles sauvages, secteur par
#' secteur, rapportée en pourcentage à la capacité d'accueil pondérée
#' (Rmax x nu_d x surface, cf. \code{\link{compute_juv_rmax}} avec
#' \code{pct = 1}). Le secteur Poutes n'entre dans le calcul qu'à partir de
#' l'année 13.
#'
#' @param d_wild_moy_V,d_wild_moy_A,d_wild_moy_L,d_wild_moy_P Objets coda (itérations x années) : densité moyenne de juvéniles sauvages par secteur.
#' @param Rmax Objet coda (1 colonne) : capacité d'accueil du modèle.
#' @param nu_d Objet coda (4 colonnes V/A/L/P) : différentiel de productivité par secteur.
#' @param S_juv_JP Matrice des surfaces productives (4 secteurs x années), voir \code{\link{keep_good_surf}}.
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @return Une matrice (itérations x T) : pourcentage de Rmax atteint, NA pour l'année 1 (non calculable).
#' @export
compute_dc_wild_ratio <- function(d_wild_moy_V, d_wild_moy_A, d_wild_moy_L, d_wild_moy_P,
                                   Rmax, nu_d, S_juv_JP, T, n_iter) {
  rmax_ponder <- compute_juv_rmax(1, Rmax, nu_d, S_juv_JP, T)$total
  dc <- matrix(NA, n_iter, T)
  for (t in 2:12) {
    dc[, t] <- (d_wild_moy_V[, t - 1] * S_juv_JP[1, t] +
      d_wild_moy_A[, t - 1] * S_juv_JP[2, t] +
      d_wild_moy_L[, t - 1] * S_juv_JP[3, t]) * 100 / rmax_ponder[, t]
  }
  for (t in 13:T) {
    dc[, t] <- (d_wild_moy_V[, t - 1] * S_juv_JP[1, t] +
      d_wild_moy_A[, t - 1] * S_juv_JP[2, t] +
      d_wild_moy_L[, t - 1] * S_juv_JP[3, t] +
      d_wild_moy_P[, t - 12] * S_juv_JP[4, t]) * 100 / rmax_ponder[, t]
  }
  dc
}

#' Risque de ne pas atteindre un seuil de diagnostic de conservation
#'
#' Calcule, sur une fenêtre glissante de \code{window} ans (10 par défaut), la
#' probabilité d'assurer la conservation pour 3 niveaux de risque toléré (au
#' plus 1, 2 ou 3 années en dessous du seuil sur la fenêtre). La valeur à
#' l'année t est centrée sur la fenêtre \code{[t - window + 1, t]}.
#'
#' @param dc_ratio Pourcentage de Rmax atteint, tel que retourné par \code{\link{compute_dc_wild_ratio}}.
#' @param threshold_pct Seuil de diagnostic, en pourcentage de Rmax (ex. 25 pour 25\% de Rmax).
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @param window Taille de la fenêtre glissante, en années. Défaut : 10.
#' @return Une liste de 3 vecteurs de longueur T : \code{mean_risk_10}, \code{mean_risk_20}, \code{mean_risk_30} (probabilité de conservation pour un risque toléré de 10/20/30\%), NA pour les \code{window} premières années.
#' @export
compute_diagnostic_risk <- function(dc_ratio, threshold_pct, T, n_iter, window = 10) {
  above <- dc_ratio > threshold_pct
  risk10 <- matrix(NA, n_iter, T)
  risk20 <- matrix(NA, n_iter, T)
  risk30 <- matrix(NA, n_iter, T)
  for (t in 2:(T - (window - 1))) {
    below_count <- rowSums(!above[, t:(t + window - 1)])
    idx <- t + window - 1
    risk10[, idx] <- as.numeric(below_count <= 1)
    risk20[, idx] <- as.numeric(below_count <= 2)
    risk30[, idx] <- as.numeric(below_count <= 3)
  }
  list(
    mean_risk_10 = colMeans(risk10),
    mean_risk_20 = colMeans(risk20),
    mean_risk_30 = colMeans(risk30)
  )
}

#' Part des juvéniles sauvages dans l'ensemble des juvéniles
#'
#' Calcule le ratio juvéniles sauvages / juvéniles totaux (sauvages + issus de
#' déversement), secteur par secteur puis rapporté au total. Le secteur
#' Poutes n'entre dans le calcul qu'à partir de l'année 13.
#'
#' @param dmoy_tot_V,dmoy_tot_A,dmoy_tot_L,dmoy_tot_P Objets coda (itérations x années) : densité moyenne de juvéniles totaux (sauvages + élevage) par secteur.
#' @param dmoy_wild_V,dmoy_wild_A,dmoy_wild_L,dmoy_wild_P Objets coda (itérations x années) : densité moyenne de juvéniles sauvages par secteur.
#' @param S_juv_JP Matrice des surfaces productives (4 secteurs x années), voir \code{\link{keep_good_surf}}.
#' @param T Nombre d'années.
#' @param n_iter Nombre d'itérations MCMC.
#' @return Une liste avec \code{ratio} (matrice itérations x T, NA pour l'année 1) et \code{mean_ratio} (vecteur de longueur T, moyenne du ratio sur les itérations).
#' @export
compute_ratio_juv_wild <- function(dmoy_tot_V, dmoy_tot_A, dmoy_tot_L, dmoy_tot_P,
                                    dmoy_wild_V, dmoy_wild_A, dmoy_wild_L, dmoy_wild_P,
                                    S_juv_JP, T, n_iter) {
  juv_tot <- matrix(0, n_iter, T)
  juv_wild <- matrix(0, n_iter, T)
  for (t in 2:12) {
    juv_tot[, t] <- dmoy_tot_V[, t - 1] * S_juv_JP[1, t] + dmoy_tot_A[, t - 1] * S_juv_JP[2, t] +
      dmoy_tot_L[, t - 1] * S_juv_JP[3, t]
    juv_wild[, t] <- dmoy_wild_V[, t - 1] * S_juv_JP[1, t] + dmoy_wild_A[, t - 1] * S_juv_JP[2, t] +
      dmoy_wild_L[, t - 1] * S_juv_JP[3, t]
  }
  for (t in 13:T) {
    juv_tot[, t] <- dmoy_tot_V[, t - 1] * S_juv_JP[1, t] + dmoy_tot_A[, t - 1] * S_juv_JP[2, t] +
      dmoy_tot_L[, t - 1] * S_juv_JP[3, t] + dmoy_tot_P[, t - 12] * S_juv_JP[4, t]
    juv_wild[, t] <- dmoy_wild_V[, t - 1] * S_juv_JP[1, t] + dmoy_wild_A[, t - 1] * S_juv_JP[2, t] +
      dmoy_wild_L[, t - 1] * S_juv_JP[3, t] + dmoy_wild_P[, t - 12] * S_juv_JP[4, t]
  }
  ratio <- juv_wild / juv_tot
  ratio[, 1] <- NA
  list(ratio = ratio, mean_ratio = colMeans(ratio))
}

#' Résumé chiffré d'une série annuelle d'indicateur
#'
#' Pour une série annuelle (1 valeur par année, NA pour les années non
#' calculées) : renvoie la dernière année disponible, la moyenne sur les 5
#' dernières et 5 premières années disponibles, et la médiane sur toute la
#' série (plus robuste qu'une moyenne aux valeurs extrêmes).
#'
#' @param values Vecteur numérique, 1 valeur par année (NA pour les années non calculées).
#' @param label Libellé de l'indicateur, utilisé comme première colonne du résultat.
#' @param year_origin Année de référence (année 1 du modèle = year_origin + 1). Défaut : 1974.
#' @param digits Nombre de décimales pour l'arrondi. Défaut : 2.
#' @return Un data.frame d'une ligne : Indicateur, année et valeur de la dernière année disponible, moyenne 5 dernières années, moyenne 5 premières années, médiane sur toute la série.
#' @export
summarize_indicator_series <- function(values, label, year_origin = 1974, digits = 2) {
  valid <- which(!is.na(values))
  last1 <- tail(valid, 1)
  last5 <- tail(valid, 5)
  first5 <- head(valid, 5)
  data.frame(
    Indicateur = label,
    `Dernière année (année)` = year_origin + last1,
    `Dernière année` = round(values[last1], digits),
    `Moyenne 5 dernières années` = round(mean(values[last5]), digits),
    `Moyenne 5 premières années` = round(mean(values[first5]), digits),
    `Médiane (toute la série)` = round(median(values[valid]), digits),
    check.names = FALSE
  )
}
