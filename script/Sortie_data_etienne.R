#######################################
# Script sortie paramètres Etienne
# @uteur : Marion LEGRAND - LOGRAMI
# date : 11 mai 2023
#######################################

library(coda)
require(stringr)

datawd<-here::here("data/CODA/2023_04/")
T=48

##Tableau avec année/I_mm/médiane s_juv2ad/médiane res_vichy_std
s_juv2ad=read.coda(str_c(datawd,"s_juv2adCODAchain1.txt"),str_c(datawd,"s_juv2adCODAindex.txt"))
sum=summary(s_juv2ad, probs=seq(0,1,0.1))
s_juv2ad_med<-cbind(sum$quantiles[,3])
s_juv2ad_mean<-cbind(sum$statistics[,"Mean"])

res_vichy=read.coda(str_c(datawd,"simulation/res_vichyCODAchain1.txt"),str_c(datawd,"simulation/res_vichyCODAindex.txt"))
sigma_vichy=read.coda(str_c(datawd,"sigma_vichyCODAchain1.txt"),str_c(datawd,"sigma_vichyCODAindex.txt"))
#res_vichy_std<-as.mcmc(scale(res_vichy,center=FALSE,scale=TRUE)) #--> à revoir résidus semble bizarre sur le plot
res_vichy_std<-array(NA,c(nrow(res_vichy),(T-6)))

for (t in 1:(T-6)){
  for (i in 1:nrow(res_vichy)){
    
    res_vichy_std[i,t]<-res_vichy[i,t]/sigma_vichy[i]
  }
}
colnames(res_vichy_std)<-colnames(res_vichy)
res_vichy_std<-as.mcmc(res_vichy_std)

sum_res_vichy=summary(res_vichy_std, probs=seq(0,1,0.1))
res_vichy_med<-cbind(sum_res_vichy$quantiles[,3])
res_vichy_mean<-cbind(sum_res_vichy$statistics[,"Mean"])

I_mm=read.coda(str_c(datawd,"I_mmCODAchain1.txt"),str_c(datawd,"I_mmCODAindex.txt"))
I_mm_mean=cbind(colMeans(I_mm))

data<-cbind((1974+7):(1974+48),I_mm_mean,res_vichy_med,res_vichy_mean,s_juv2ad_med,s_juv2ad_mean)
colnames(data)<-c("year","I_mm_mean","res_vichy_std_median","res_vichy_std_mean","s_juv2ad_median","s_juv2ad_mean")
row.names(data) <- NULL
library(xlsx)
write.xlsx(data,here::here("data/data_for_etienne.xlsx"))
