######################################################################################
# Script de création des fonctions servant à faire des traitements sur les données ###
# @uteur : Marion LEGRAND - LOGRAMI                                                ###
# date : 2022_04_08                                                                ###
######################################################################################


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