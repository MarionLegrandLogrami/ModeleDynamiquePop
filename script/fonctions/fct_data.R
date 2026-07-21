######################################################################################
# Script de création des fonctions servant à faire des traitements sur les données ###
# @uteur : Marion LEGRAND - LOGRAMI                                                ###
# date : 2022_04_08                                                                ###
######################################################################################

#===================================================================================
# Lecture d'un paramètre CODA (chain1 + index) à partir du dossier de sorties Openbugs.
# subdir permet de lire dans un sous-dossier (ex. "simulation") comme le fait le
# modèle pour certains paramètres.
#===================================================================================
read_param_coda <- function(name, datawd, subdir = NULL, file_prefix = name) {
  base <- if (is.null(subdir)) datawd else str_c(datawd, subdir, "/")
  read.coda(str_c(base, file_prefix, "CODAchain1.txt"),
            str_c(base, file_prefix, "CODAindex.txt"))
}

#===================================================================================
# Quantiles (par défaut 5/15/25/.../95%) + moyenne d'un paramètre CODA, dans le
# format attendu par les graphiques boxplot par année (custom_plot / boxplot_years_png)
#===================================================================================
param_quantiles <- function(param, probs = seq(0, 1, 0.1)) {
  sum <- summary(param, probs = probs)
  mean_col <- if (is.matrix(sum$statistics)) {
    cbind(sum$statistics[, "Mean"])
  } else {
    cbind(sum$statistics["Mean"])
  }
  list(summary = sum, quantiles = sum$quantiles, mean = mean_col)
}

#===================================================================================
# Echappe les underscores pour LaTeX (ex. "beta_Q_VB" -> "beta\_Q\_VB")
#===================================================================================
latex_escape <- function(x) gsub("_", "\\_", x, fixed = TRUE)

#===================================================================================
# Réaligne un paramètre CODA dont seules certaines années sont échantillonnées
# (noms de type "p_Rmax[7,1]", "p_Rmax[15,1]", ...) sur une grille complète 1:T,
# en remplissant de NA les années absentes. Motif récurrent pour les paramètres
# du modèle d'interaction réciproque (p_Rmax_V/A/L/P).
#===================================================================================
align_sparse_year_param <- function(param, T, probs = seq(0, 1, 0.1)) {
  sum <- summary(param, probs = probs)
  years_present <- as.numeric(gsub("[^0-9]", "", gsub(",[0-9]", "", rownames(sum$quantiles))))

  quantiles_full <- matrix(NA, nrow = T, ncol = 5)
  quantiles_full[years_present, ] <- sum$quantiles

  mean_full <- rep(NA, T)
  mean_full[years_present] <- sum$statistics[, "Mean"]

  list(quantiles = quantiles_full, mean = mean_full)
}

#===================================================================================
# Masque de NA les années "tirage à blanc" (pas de déversement) sur un paramètre
# coda aligné positionnellement sur une plage d'années contiguë (ex. d_juv_moy_V,
# lignes = années 2:T dans l'ordre). indicator est le vecteur I_juv_moy[<plage>,col]
# (1 = déversement réel cette année-là, 0 = tirage à blanc à ignorer).
#===================================================================================
mask_no_deversement <- function(param, indicator) {
  sum <- summary(param, probs = seq(0, 1, 0.1))
  mean_col <- if (is.matrix(sum$statistics)) sum$statistics[, "Mean"] else sum$statistics["Mean"]
  quantiles <- sum$quantiles
  quantiles[indicator == 0, ] <- NA
  mean_col[indicator == 0] <- NA
  list(quantiles = quantiles, mean = mean_col)
}

#===================================================================================
# Standardise des résidus par un écart-type tiré (1 valeur par itération MCMC) :
# std[i,t] = res[i,t] / sigma[i]. Motif récurrent pour les résidus standardisés
# (res_vichy_std, res_p_*_std, res_wild_moy_*_std, res_juv_moy_*_std, ...)
#===================================================================================
standardize_residuals <- function(res, sigma) {
  std <- sweep(as.matrix(res), 1, as.vector(sigma), "/")
  colnames(std) <- colnames(res)
  as.mcmc(std)
}

#===================================================================================
# Table récapitulative multi-lignes (une ligne par sous-paramètre d'un objet coda
# multivarié, ex. nu_d_V/A/L/P), avec des noms de lignes personnalisés. Ecrit en
# .tex via xtable. Variante "plusieurs lignes" de write_param_table ci-dessous.
#===================================================================================
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

#===================================================================================
# Table récapitulative (quantiles + Mean + SD) d'un paramètre scalaire, écrite en
# .tex via xtable. C'est la 2e moitié du motif le plus fréquent du rapport
# SortieOpenbugs_parameters.Rnw (voir aussi report_param/png_trace_density dans
# fct_graph.R qui l'associent au graphique trace/densité).
#===================================================================================
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

#===================================================================================
# Quantité de juvéniles atteignable pour une fraction (pct) de Rmax, ventilée par
# secteur (V/A/L/P) puis sommée sur le système. Généralise le motif Juv_Rmax_25/
# 50/75/100 de l'indicateur "niveau de population" ; avec pct=1 donne aussi
# Rmax_ponder utilisé par le diagnostic de conservation.
#===================================================================================
compute_juv_rmax <- function(pct, Rmax, nu_d, S_juv_JP, T) {
  Rmax <- as.vector(Rmax)
  zones <- lapply(1:4, function(z) outer(pct * Rmax * exp(nu_d[, z]), S_juv_JP[z, 1:T]))
  names(zones) <- c("V", "A", "L", "P")
  zones$total <- Reduce(`+`, zones)
  zones
}

#===================================================================================
# Reconstitue la chronique d'adultes à Vichy en prenant les comptages réels
# (data_vichy) quand la station fonctionnait, et la moyenne postérieure du modèle
# (bugs_N_vichy_real) sinon. Le découpage (1:22 estimé / 23:29 réel / 30 estimé /
# 31:T réel) reflète l'historique réel de la station (arrêt ponctuel une année) et
# reste valable quel que soit T car ancré sur des années calendaires fixes.
#===================================================================================
reconstitute_data_vichy <- function(bugs_N_vichy_real, data_vichy, T) {
  mean_bugs <- round(apply(bugs_N_vichy_real, MARGIN = 2, FUN = mean), 0)
  c(as.vector(mean_bugs[1:22]), data_vichy[23:29], as.vector(mean_bugs[23]), data_vichy[31:T])
}

#===================================================================================
# Juvéniles sauvages par secteur et par année (densité sauvage x surface), à
# partir des paramètres dmoywild_V/A/L/P. Le secteur Poutes n'est colonisé/suivi
# qu'à partir de l'année 13 d'où l'indice t-11 (dmoywild_P commence à l'année 13).
#===================================================================================
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

#===================================================================================
# Pool de juvéniles sauvages à l'origine des retours d'adultes ("juv_wild_tot_
# system") : moyenne des cohortes produites 3, 4 et 5 ans plus tôt (délai
# smolt+mer avant retour), sommée sur les secteurs (Poutes intégré à partir de
# l'année 16 seulement, une fois le recul de cohorte disponible).
#===================================================================================
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

#===================================================================================
# Coefficients de pondération des 3 cohortes de juvéniles (t-4/t-5/t-6, t-3/t-4/
# t-5, t-2/t-3/t-4) utilisés pour répartir un retour d'adultes observé à l'année t
# entre les 3 années de production juvénile voisines (délai mer variable selon les
# individus). Basé sur les juvéniles sauvages bruts par secteur (pas la cohorte).
#===================================================================================
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

#===================================================================================
# Adultes d'origine sauvage revenant à Vichy (N_wild_vichy), à partir du pool de
# juvéniles sauvages, de la survie juv->adulte annuelle du modèle (s_juv2ad[t],
# qui remplace la constante + ajustement de scénario level_s/I_surv de l'ancien
# modèle) et du résidu du modèle sur les retours à Vichy (res_vichy, appliqué de
# la même façon à la population totale et à la population sauvage, comme dans
# l'ancien code). s_juv2ad et res_vichy sont indexés à partir de l'année 7
# (colonne 1 = année 7), d'où le décalage t-6.
#===================================================================================
compute_N_wild_vichy <- function(juv_wild_tot_system, s_juv2ad, res_vichy, T, n_iter) {
  N_wild_vichy <- matrix(0, n_iter, T)
  for (t in 7:T) {
    N_wild_vichy[, t] <- exp(log(juv_wild_tot_system[, t]) + log(s_juv2ad[, t - 6]) + res_vichy[, t - 6])
  }
  N_wild_vichy
}

#===================================================================================
# Taux de renouvellement de la population sauvage (renew_rate_w_coef, échelle
# log) : ratio entre le retour d'adultes sauvages pondéré sur 3 cohortes voisines
# et le nombre d'adultes revenus à Vichy 5 ans plus tôt (génération parentale).
# Le dénominateur alterne entre l'estimation bayésienne (N_vichy_bugs, pour les
# années sans comptage réel) et le comptage réel (data_vichy) : ce découpage
# reflète l'historique réel de la station de comptage de Vichy (cf.
# reconstitute_data_vichy) et reste valable quel que soit T.
#===================================================================================
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

#===================================================================================
# Pourcentage de Rmax atteint par la production de juvéniles sauvages
# (DC_tot_wild_tot), secteur par secteur puis rapporté à la capacité d'accueil
# pondérée (Rmax * nu_d * surface, cf. compute_juv_rmax(pct=1,...)). Le secteur
# Poutes n'entre dans le calcul qu'à partir de l'année 13.
#===================================================================================
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

#===================================================================================
# Risque de ne pas atteindre un seuil de diagnostic de conservation (% de Rmax)
# sur une fenêtre glissante de `window` ans (10 par défaut), pour 3 niveaux de
# risque toléré (au plus 1, 2 ou 3 années en dessous du seuil sur la fenêtre).
# La valeur à l'année t est centrée sur la fenêtre [t-window+1, t].
#===================================================================================
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

#===================================================================================
# Part des juvéniles sauvages dans l'ensemble des juvéniles (sauvages + issus de
# déversement), secteur par secteur puis rapportée au total. Le secteur Poutes
# n'entre dans le calcul qu'à partir de l'année 13.
#===================================================================================
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

#===================================================================================
# Résumé chiffré d'une série annuelle d'indicateur (valeurs = 1 par année, NA
# pour les années non calculées) : dernière année disponible, moyenne sur les 5
# dernières / 5 premières années disponibles, et médiane sur toute la série
# (plus robuste qu'une moyenne aux valeurs extrêmes, cf. demande utilisateur).
#===================================================================================
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