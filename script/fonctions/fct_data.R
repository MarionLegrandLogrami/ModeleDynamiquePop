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