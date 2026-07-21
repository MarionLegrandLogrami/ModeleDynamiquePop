# TODO: Add comment
# 
# Author: marion.legrand
###############################################################################

#Mod�le 2017.08.29_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2016
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")

#Modèle 2019_12_12
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2019_12_12/")
datawd<-("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2019_12_12/")

#Modèle 2021_09 (= structure du modèle 2019_12_12 avec MAJ données jusqu'à 2020)
setwd(here::here("data/CODA/2021_09"))
datawd<-(here::here("data/CODA/2021_09"))


library(coda)
library(boot)
library(stringr)

T=46


#====================================================
# Taux de renouvellement de la population sauvage
#====================================================

s_juv2ad=read.coda("s_juv2adCODAchain1.txt","s_juv2adCODAindex.txt")
level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt") 
I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
res_vichy=read.coda("simulation/res_vichyCODAchain1.txt","simulation/res_vichyCODAindex.txt")
d_wild_moy_vichy=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt")
d_wild_moy_alagnon=read.coda("dmoywild_ACODAchain1.txt","dmoywild_ACODAindex.txt")
d_wild_moy_langeac=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt")
d_wild_moy_poutes=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt")
N_vichy=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")


juv_wild_vichy<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_alagnon<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_langeac<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_poutes<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_tot_V<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_tot_A<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_tot_L<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_tot_P<-array(rep(0,T*5000),dim=c(5000,T+1))
juv_wild_tot_system<-array(rep(0,T*5000),dim=c(5000,T+1))
N_wild_vichy<-array(rep(0,T*5000),dim=c(5000,T))
coef_juv_1<-array(rep(0,T*5000),dim=c(5000,T))
coef_juv_2<-array(rep(0,T*5000),dim=c(5000,T))
coef_juv_3<-array(rep(0,T*5000),dim=c(5000,T))
renew_rate_w=array(0,dim=c(5000,T+1))
renew_rate_w_q=array(NA,dim=c((T),5))		
renew_rate_w_coef=array(0,dim=c(5000,T+1))
renew_rate_w_coef_q=array(NA,dim=c((T),5))
		
surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)	

#On calcule les juvéniles SAUVAGES uniquement de chaque année sur chaque secteur
for (t in 1:T){
	for (i in 1:5000){
		juv_wild_vichy[i,t+1]=d_wild_moy_vichy[i,t]*S_juv_JP[1,t+1] #d_wild_moy commence à 2 d'où on l'indice t et non t+1
		juv_wild_alagnon[i,t+1]=d_wild_moy_alagnon[i,t]*S_juv_JP[2,t+1] #d_wild_moy commence à 2 d'où on l'indice t et non t+1
		juv_wild_langeac[i,t+1]=d_wild_moy_langeac[i,t]*S_juv_JP[3,t+1] #d_wild_moy commence à 2 d'où on l'indice t et non t+1
	}
}
	
for (t in 12:T){
	for (i in 1:5000){
		juv_wild_poutes[i,t+1]=d_wild_moy_poutes[i,t-11]*S_juv_JP[4,t+1] #d_wild_moy commence � 13 d'o� on l'indice t et non t+1

	}
}

#On calcule les juvéniles SAUVAGES uniquement à l'origine des retours d'adultes
for (t in 7:T){
	for (i in 1:5000){
		juv_wild_tot_V[i,t] <- (1/3) * juv_wild_vichy[i,t-3] + (1/3) * juv_wild_vichy[i,t-4] + (1/3) * juv_wild_vichy[i,t-5]
		juv_wild_tot_A[i,t] <- (1/3) * juv_wild_alagnon[i,t-3] + (1/3) * juv_wild_alagnon[i,t-4] + (1/3) * juv_wild_alagnon[i,t-5] 
		juv_wild_tot_L[i,t] <- (1/3) * juv_wild_langeac[i,t-3] + (1/3) * juv_wild_langeac[i,t-4] + (1/3) * juv_wild_langeac[i,t-5] 
	}
}

for (t in 16:16){
	for (i in 1:5000){
		juv_wild_tot_P[i,t] <- (1/3) * juv_wild_poutes[i,t-3] 
	}
}	
for (t in 17:17){
	for (i in 1:5000){
		juv_wild_tot_P[i,t] <- (1/3) * juv_wild_poutes[i,t-3] + (1/3) * juv_wild_poutes[i,t-4]
	}
}	
for (t in 18:T){
	for (i in 1:5000){
		juv_wild_tot_P[i,t] <- (1/3) * juv_wild_poutes[i,t-3] + (1/3) * juv_wild_poutes[i,t-4] + (1/3) * juv_wild_poutes[i,t-5]
	}
}

for (t in 7:15){
	for (i in 1:5000){
		juv_wild_tot_system[i,t] <- juv_wild_tot_V[i,t]+ juv_wild_tot_A[i,t] +juv_wild_tot_L[i,t] 
	}
}
for (t in 16:T){
	for (i in 1:5000){
		juv_wild_tot_system[i,t] <- juv_wild_tot_V[i,t]+ juv_wild_tot_A[i,t] +juv_wild_tot_L[i,t]  + juv_wild_tot_P[i,t]
	}
}	


#On calcule les adultes sauvages à Vichy
for (t in 7:7){
	for (i in 1:5000){
		N_wild_vichy[i,t]<-exp(log(juv_wild_tot_system[i,t])+log(s_juv2ad[i])+ level_s[i] * 1+res_vichy[i,t-6])
				
				#exp(log(juv_wild_tot_system[i,t]*s_juv2ad[i] * exp(level_s[i] * 1)) + res_vichy[i,t-6])
	}
}


for (t in 8:T){
	for (i in 1:5000){
		N_wild_vichy[i,t]<-exp(log(juv_wild_tot_system[i,t])+log(s_juv2ad[i])+ level_s[i] * I_surv[i,t-7]+res_vichy[i,t-6])
		#N_wild_vichy[i,t]<-exp(log(juv_wild_tot_system[i,t]*s_juv2ad[i] * exp(level_s[i] * I_surv[i,t-7])) + res_vichy[i,t-6])
	}
}


#On fait les quantiles de N_wild_vichy
N_wild_vichy_q<-array(NA,dim=c((T),5))
for (t in 1:T){
	N_wild_vichy_q[t,]=quantile(N_wild_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


#on calcule le coef pondérateur des juvéniles
for (t in 7:T){
	for (i in 1:5000){
		coef_juv_1[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/((juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])+(juv_wild_vichy[i,t-5]+juv_wild_alagnon[i,t-5]+juv_wild_langeac[i,t-5]+juv_wild_poutes[i,t-5])+(juv_wild_vichy[i,t-6]+juv_wild_alagnon[i,t-6]+juv_wild_langeac[i,t-6]+juv_wild_poutes[i,t-6]))
		coef_juv_2[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/((juv_wild_vichy[i,t-3]+juv_wild_alagnon[i,t-3]+juv_wild_langeac[i,t-3]+juv_wild_poutes[i,t-3])+(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])+(juv_wild_vichy[i,t-5]+juv_wild_alagnon[i,t-5]+juv_wild_langeac[i,t-5]+juv_wild_poutes[i,t-5]))
		coef_juv_3[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/((juv_wild_vichy[i,t-2]+juv_wild_alagnon[i,t-2]+juv_wild_langeac[i,t-2]+juv_wild_poutes[i,t-2])+(juv_wild_vichy[i,t-3]+juv_wild_alagnon[i,t-3]+juv_wild_langeac[i,t-3]+juv_wild_poutes[i,t-3])+(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4]))
		
	}
}

#On calcule le taux de renouvellement. On divise par N_vichy car on veut bien regarder le taux de renouvellement de tous les saumons de retour à Vichy
#Les boucles prennent en compte que N_Vichy calculé uniquement quand pas de données à Vichy. Quand données, il faut aller directement les récupérer


source("data_2021.09.R")
data_vichy<-N[,1]

for (t in 7:27){
	for (i in 1:5000){
		renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/N_vichy[i,t-5])
		#renew_rate_w[i,t-5]<-log(((1/3)*N_wild_vichy[i,t-1]+(1/3)*N_wild_vichy[i,t]+(1/3)*N_wild_vichy[i,t+1])/N_vichy[i,t-5])
		
	}
}
for (t in 28:34){
	for (i in 1:5000){
		renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/data_vichy[t-5])
		#renew_rate_w[i,t-5]<-log(((1/3)*N_wild_vichy[i,t-1]+(1/3)*N_wild_vichy[i,t]+(1/3)*N_wild_vichy[i,t+1])/data_vichy[t-5])
		
	}
}
for (t in 35:35){
	for (i in 1:5000){
		renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/N_vichy[i,t-12])
		#renew_rate_w[i,t-5]<-log(((1/3)*N_wild_vichy[i,t-1]+(1/3)*N_wild_vichy[i,t]+(1/3)*N_wild_vichy[i,t+1])/N_vichy[i,t-12])
	}
}
for (t in 36:(T-1)){
	for (i in 1:5000){
		renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/data_vichy[t-5])
#		renew_rate_w[i,t-5]<-log(((1/3)*N_wild_vichy[i,t-1]+(1/3)*N_wild_vichy[i,t]+(1/3)*N_wild_vichy[i,t+1])/data_vichy[t-5])
		
	}
}


for (t in 7:T){
	renew_rate_w_coef_q[t,]=quantile(renew_rate_w_coef[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#	renew_rate_w_q[t,]=quantile(renew_rate_w[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


####### Calcul sur le taux de renouvellement en utilisant moyenne géométrique
library(EnvStats)
#Calcul de la moyenne géométrique sur les données qui ne sont pas en valeur logarithmique
#geoMean(exp(renew_rate_w_coef_q[7:36,3])) # revient au même que faire une moyenne arithmétique sur des données en valeur logarithmique
#Calcul de la moyenne sur les séries MCMC plutôt que sur les quantiles
exp(mean(rowMeans(renew_rate_w_coef[,7:36]))) # donne résultat quasi-identique que moyenne sur les quantiles
exp(mean(rowMeans(renew_rate_w_coef[,32:36])))
exp(mean(rowMeans(renew_rate_w_coef[,7:11])))
#exp(mean(renew_rate_w_coef_q[7:36,3])) #moyenne des m�diane sur toute la chronique
#exp(mean(renew_rate_w_coef_q[32:36,3])) #moyenne des m�diane sur 5 derni�res ann�es
#exp(mean(renew_rate_w_coef_q[7:11,3])) #moyenne des m�diane sur 5 premi�res ann�es

## en log
mean(renew_rate_w_coef_q[7:36,3]) #moyenne des m�diane sur toute la chronique : -0.97
mean(renew_rate_w_coef_q[32:36,3]) #moyenne des m�diane sur 5 derni�res ann�es : -0.75
mean(renew_rate_w_coef_q[7:11,3]) #moyenne des m�diane sur 5 premi�res ann�es : -0.55


#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage.png",width=1000,height=800)
#
#plot(1,1,type="n",axes=FALSE,xlim=c(10.5,T+0.5),xlab="Years",ylim=c(-5,5),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)
#
## trace l'axe des ordonn�es
#axis(2,at = c(-5,-4,-3,-2,-1,0,1,2,3,4,5),labels=c(-5,-4,-3,-2,-1,0,1,2,3,4,5),cex.axis = 1.2,las = 1,lwd=2,col = "black")
## trace l'axe des abscisses
#axis(1,at = c(11,16,26,36,T),
#		labels=c((1975+11),1990,2000,2010,(1975+T-1)),
#		cex.axis = 1.2,las = 1,lwd=2,col = "black")
#
#for(i in 11:T){
#	#whiskers
#	#95%
#	segments(i-0.15,renew_rate_w_q[i,5],i+0.15,renew_rate_w_q[i,5])
#	segments(i,renew_rate_w_q[i,4],i,renew_rate_w_q[i,5])
#	#5%
#	segments(i-0.15,renew_rate_w_q[i,1],i+0.15,renew_rate_w_q[i,1])
#	segments(i,renew_rate_w_q[i,2],i,renew_rate_w_q[i,1])
#	#boxplot
#	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_w_q[i,2],renew_rate_w_q[i,2],renew_rate_w_q[i,4],renew_rate_w_q[i,4]),col="light grey")
#	#median
#	segments(i-0.3,renew_rate_w_q[i,3],i+0.3,renew_rate_w_q[i,3])
#}
#abline(h=0,col="red")
#
#dev.off()



#===============================
# figure en échelle naturelle
#===============================
png(filename=here::here("img/Simulation/2021_09/2022_07_01_TauxRenouvellementPopSauvage_EchelleNat_mod2021_09.png"),width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_EchelleNat.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(6.5,T-7+0.5),xlab="Years",ylim=c(0,2),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)

# trace l'axe des ordonnées
axis(2,at = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2),labels=c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(7,16,26,(T-6)),
		labels=c((1975+6),1990,2000,(1975+T-7)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,exp(renew_rate_w_coef_q[i,5]),i+0.15,exp(renew_rate_w_coef_q[i,5]))
	segments(i,exp(renew_rate_w_coef_q[i,4]),i,exp(renew_rate_w_coef_q[i,5]))
	#5%
	segments(i-0.15,exp(renew_rate_w_coef_q[i,1]),i+0.15,exp(renew_rate_w_coef_q[i,1]))
	segments(i,exp(renew_rate_w_coef_q[i,2]),i,exp(renew_rate_w_coef_q[i,1]))
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(exp(renew_rate_w_coef_q[i,2]),exp(renew_rate_w_coef_q[i,2]),exp(renew_rate_w_coef_q[i,4]),exp(renew_rate_w_coef_q[i,4])),col="light grey")
	#median
	segments(i-0.3,exp(renew_rate_w_coef_q[i,3]),i+0.3,exp(renew_rate_w_coef_q[i,3]))
}
abline(h=1,col="red")
abline(h=exp(mean(renew_rate_w_coef_q[7:36,3])),col="black",lty=5,lwd=2)

dev.off()




#=================
# figure en log
#=================
#on enlève la dénomination coef dans l'intitulé de la figure car c'est ce qu'on conserve tout le temps (le calcul 1/3,1/3,1/3 étant faux)
png(filename=here::here("img/Simulation/2021_09/2022_07_01_TauxRenouvellementPopSauvage_EchelleLog_mod2021_09.png"),width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2019_12_IndicateursSat_model_data2018_2019_12_12/TauxRenouvellementPopSauvage_log.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(7.5,T-7+0.5),xlab="Années",ylim=c(-3,3),ylab="log(Taux de renouvellement)",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = seq(-3,3,1),labels=seq(-3,3,1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(7,16,26,(T-6)),
		labels=c((1975+6),1990,2000,(1975+T-7)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,renew_rate_w_coef_q[i,5],i+0.15,renew_rate_w_coef_q[i,5])
	segments(i,renew_rate_w_coef_q[i,4],i,renew_rate_w_coef_q[i,5])
	#5%
	segments(i-0.15,renew_rate_w_coef_q[i,1],i+0.15,renew_rate_w_coef_q[i,1])
	segments(i,renew_rate_w_coef_q[i,2],i,renew_rate_w_coef_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_w_coef_q[i,2],renew_rate_w_coef_q[i,2],renew_rate_w_coef_q[i,4],renew_rate_w_coef_q[i,4]),col="light grey")
	#median
	segments(i-0.3,renew_rate_w_coef_q[i,3],i+0.3,renew_rate_w_coef_q[i,3])
}
abline(h=0,col="red")
abline(h=mean(renew_rate_w_coef_q[7:38,3]),col="black",lty=5,lwd=2)

dev.off()

###Tx renouv 5 premières années
mean(renew_rate_w_coef_q[7:11,3]) #-0.53
exp(mean(renew_rate_w_coef_q[7:11,3])) #0.58
###Tx renouv ensemble série
mean(renew_rate_w_coef_q[7:38,3]) #-0.98
exp(mean(renew_rate_w_coef_q[7:38,3])) #0.37
###Tx renouv 5 dernières années
mean(renew_rate_w_coef_q[34:38,3]) #-0.67
exp(mean(renew_rate_w_coef_q[34:38,3])) #0.51
##Tx entre 1988 et 2006
mean(renew_rate_w_coef_q[14:32,3]) #-1.24
exp(mean(renew_rate_w_coef_q[14:32,3])) #0.29

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_82_2011.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(7.5,T-6+0.5),xlab="Years",ylim=c(-5,5),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(-5,-4,-3,-2,-1,0,1,2,3,4,5),labels=c(-5,-4,-3,-2,-1,0,1,2,3,4,5),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(7,15,25,(T-6)),
		labels=c((1975+6),1990,2000,(1975+T-7)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,renew_rate_w_coef_q[i,5],i+0.15,renew_rate_w_coef_q[i,5])
	segments(i,renew_rate_w_coef_q[i,4],i,renew_rate_w_coef_q[i,5])
	#5%
	segments(i-0.15,renew_rate_w_coef_q[i,1],i+0.15,renew_rate_w_coef_q[i,1])
	segments(i,renew_rate_w_coef_q[i,2],i,renew_rate_w_coef_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_w_coef_q[i,2],renew_rate_w_coef_q[i,2],renew_rate_w_coef_q[i,4],renew_rate_w_coef_q[i,4]),col="light grey")
	#median
	segments(i-0.3,renew_rate_w_coef_q[i,3],i+0.3,renew_rate_w_coef_q[i,3])
}
abline(h=0,col="red")

dev.off()


##### Graph Tx renouvellement moyen des m�dianes par tranche de 5 ans #####

#Calcul du taux de renouvellement en �chelle classique (1= renouvellement)
renew_rate_w_coef_q_5yr=array(0,dim=c(length(seq(7,(T-6),5)),1))
j=7

for (i in 1:length(seq(7,(T-6),5))){
	renew_rate_w_coef_q_5yr[i]<-exp(mean(renew_rate_w_coef_q[j:(j+4),3]))
	j=j+5
}
rownames(renew_rate_w_coef_q_5yr)<-c("1981-1985","1986-1990","1991-1995","1996-2000","2001-2005","2006-2010")

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_5yrs.png",width=1000,height=800)
par("mar"=c(6.1,5.1,3.1,1.1))
plot(renew_rate_w_coef_q_5yr,axes=FALSE,ylim=c(0,ifelse(max(renew_rate_w_coef_q_5yr)<1,1,max(renew_rate_w_coef_q_5yr)+0.1)),xlab="",ylab="Taux renouvellement",main=iconv("Taux de renouvellement de la population sauvage (moyenne des m�dianes sur 5 ans)","UTF8"),cex.lab=1.5)
axis(1,at = seq(1,length(seq(7,(T-6),5)),1),
		labels=rownames(renew_rate_w_coef_q_5yr),
		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
axis(2,at = seq(0,ifelse(max(renew_rate_w_coef_q_5yr)<1,1,max(renew_rate_w_coef_q_5yr)),0.2),labels=seq(0,ifelse(max(renew_rate_w_coef_q_5yr)<1,1,max(renew_rate_w_coef_q_5yr)),0.2),cex.axis = 1.2,las = 1,lwd=2,col = "black")


for (s in 1:length(seq(7,(T-6),5))){
	segments(s,renew_rate_w_coef_q_5yr[s],(s+1),renew_rate_w_coef_q_5yr[s+1])
}
abline(h=1,col="red")
dev.off()

#Calcul du taux de renouvellement en �chelle log (0=renouvellement)
L_renew_rate_w_coef_q_5yr=array(0,dim=c(length(seq(7,(T-6),5)),1))
j=7

#on cr�� une fonction pour arrondir le min(L_renew_rate_q_5yr) � la d�cimal du dessous la plus proche. ex min(L_renew_rate_q_5yr)=-0.43 et on veut arrondir � -0.5
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

for (i in 1:length(seq(7,(T-6),5))){
	L_renew_rate_w_coef_q_5yr[i]<-mean(renew_rate_w_coef_q[j:(j+4),3])
	j=j+5
}
rownames(L_renew_rate_w_coef_q_5yr)<-c("1981-1985","1986-1990","1991-1995","1996-2000","2001-2005","2006-2010")

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_5yrs_log.png",width=1000,height=800)
par("mar"=c(6.1,5.1,3.1,1.1))
plot(L_renew_rate_w_coef_q_5yr,axes=FALSE,xlab="",ylim=c(floor_dec(min(L_renew_rate_w_coef_q_5yr),1),ifelse(max(L_renew_rate_w_coef_q_5yr)<0,0.1,max(L_renew_rate_w_coef_q_5yr))),ylab=iconv("Taux renouvellement (�chelle log)","UTF8"),main=iconv("Taux de renouvellement de la population sauvage (log de la moyenne des m�dianes sur 5 ans)","UTF8"),cex.lab=1.5)
axis(1,at = seq(1,length(seq(7,(T-6),5)),1),
		labels=rownames(L_renew_rate_w_coef_q_5yr),
		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
axis(2,at = seq(floor_dec(min(L_renew_rate_w_coef_q_5yr),1),ifelse(max(L_renew_rate_w_coef_q_5yr)<0,0.1,max(L_renew_rate_w_coef_q_5yr)),0.2),labels=floor_dec(seq(round(min(L_renew_rate_w_coef_q_5yr),1),ifelse(max(L_renew_rate_w_coef_q_5yr)<0,0.1,max(L_renew_rate_w_coef_q_5yr)),0.2),1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
for (s in 1:length(seq(7,(T-6),5))){
	segments(s,L_renew_rate_w_coef_q_5yr[s],(s+1),L_renew_rate_w_coef_q_5yr[s+1])
}
abline(h=0,col="red")

dev.off()


#...............
# V�rif coef
#..............
##On sort les stats de coef_juv_1/2/3
coef_juv_1_q=array(NA,dim=c((T),5))	
coef_juv_2_q=array(NA,dim=c((T),5))	
coef_juv_3_q=array(NA,dim=c((T),5))	
coef_juv_1_min=array(NA,dim=T)	
coef_juv_2_min=array(NA,dim=T)
coef_juv_3_min=array(NA,dim=T)
coef_juv_1_max=array(NA,dim=T)
coef_juv_2_max=array(NA,dim=T)
coef_juv_3_max=array(NA,dim=T)
coef_juv_1_mean=array(NA,dim=T)
coef_juv_2_mean=array(NA,dim=T)
coef_juv_3_mean=array(NA,dim=T)
coef_juv_1_sd=array(NA,dim=T)
coef_juv_2_sd=array(NA,dim=T)
coef_juv_3_sd=array(NA,dim=T)

for (t in 1:T){
	coef_juv_1_q[t,]=quantile(coef_juv_1[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	coef_juv_2_q[t,]=quantile(coef_juv_2[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	coef_juv_3_q[t,]=quantile(coef_juv_3[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	coef_juv_1_min[t]=min(coef_juv_1[,t])
	coef_juv_2_min[t]=min(coef_juv_2[,t])
	coef_juv_3_min[t]=min(coef_juv_3[,t])
	coef_juv_1_max[t]=max(coef_juv_1[,t])
	coef_juv_2_max[t]=max(coef_juv_2[,t])
	coef_juv_3_max[t]=max(coef_juv_3[,t])
	coef_juv_1_mean[t]=mean(coef_juv_1[,t])
	coef_juv_2_mean[t]=mean(coef_juv_2[,t])
	coef_juv_3_mean[t]=mean(coef_juv_3[,t])
	coef_juv_1_sd[t]=sd(coef_juv_1[,t])
	coef_juv_2_sd[t]=sd(coef_juv_2[,t])
	coef_juv_3_sd[t]=sd(coef_juv_3[,t])

}

coef_juv_1_stat<-cbind(coef_juv_1_q,coef_juv_1_min,coef_juv_1_max,coef_juv_1_mean,coef_juv_1_sd)
colnames(coef_juv_1_stat)<-c("2.5%","25%","50%","75%","97.5%","Min","Max","Mean","SD")
coef_juv_2_stat<-cbind(coef_juv_2_q,coef_juv_2_min,coef_juv_2_max,coef_juv_2_mean,coef_juv_2_sd)
colnames(coef_juv_2_stat)<-c("2.5%","25%","50%","75%","97.5%","Min","Max","Mean","SD")
coef_juv_3_stat<-cbind(coef_juv_3_q,coef_juv_3_min,coef_juv_3_max,coef_juv_3_mean,coef_juv_3_sd)
colnames(coef_juv_3_stat)<-c("2.5%","25%","50%","75%","97.5%","Min","Max","Mean","SD")

sum_coef<-array(rep(0,T*5000),dim=c(5000,(T-1)))

for (t in 2:(T-1)){
	for (i in 1:5000){
		sum_coef[i,t]<-coef_juv_1[i,t+1]+coef_juv_2[i,t]+coef_juv_3[i,t-1]
	}
}



#==============================================================================
# Taux de renouvellement de la population quelque soit l'origine des poissons
#==============================================================================

#On récupère les paramètres pour recalculer les juvéniles issu d'openbugs car paramètres non sauvegardés
d_tot_moy_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt")
d_tot_moy_A=read.coda("dmoytot_ACODAchain1.txt","dmoytot_ACODAindex.txt")
d_tot_moy_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt")
d_tot_moy_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt")

coef_juv_tot_1<-array(rep(0,T*5000),dim=c(5000,T))
coef_juv_tot_2<-array(rep(0,T*5000),dim=c(5000,T))
coef_juv_tot_3<-array(rep(0,T*5000),dim=c(5000,T))
renew_rate=array(0,dim=c(5000,T+1))
renew_rate_q=array(NA,dim=c((T),5))		
renew_rate_coef=array(0,dim=c(5000,T+1))
renew_rate_coef_q=array(NA,dim=c((T),5))
juv_vichy<-array(rep(0,T*5000),dim=c(5000,(T+1)))
juv_alagnon<-array(rep(0,T*5000),dim=c(5000,(T+1)))
juv_langeac<-array(rep(0,T*5000),dim=c(5000,(T+1)))
juv_poutes<-array(rep(0,T*5000),dim=c(5000,(T+1)))
N_vichy=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")

##Notamment pour avoir S_juv_JP bien formaté
source("data_2021.09.R")


for (t in 1:T){
	for (i in 1:5000){
		juv_vichy[i,t+1]=d_tot_moy_V[i,t]*S_juv_JP[t+1,1] #d_tot_moy_V commence à 2 d'où on l'indice t et non t+1
		juv_alagnon[i,t+1]=d_tot_moy_A[i,t]*S_juv_JP[t+1,2] #d_tot_moy_A commence à 2 d'où on l'indice t et non t+1
		juv_langeac[i,t+1]=d_tot_moy_L[i,t]*S_juv_JP[t+1,3] #d_tot_moy_L commence à 2 d'où on l'indice t et non t+1
	}
}

for (t in 12:T){
	for (i in 1:5000){
		juv_poutes[i,t+1]=d_tot_moy_P[i,t-11]*S_juv_JP[t+1,4] #d_tot_moy_P commence à 13 d'où on l'indice t et non t+1
		
	}
}


for (t in 7:T){
	for (i in 1:5000){
		coef_juv_tot_1[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])+(juv_vichy[i,t-5]+juv_alagnon[i,t-5]+juv_langeac[i,t-5]+juv_poutes[i,t-5])+(juv_vichy[i,t-6]+juv_alagnon[i,t-6]+juv_langeac[i,t-6]+juv_poutes[i,t-6]))
		coef_juv_tot_2[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-3]+juv_alagnon[i,t-3]+juv_langeac[i,t-3]+juv_poutes[i,t-3])+(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])+(juv_vichy[i,t-5]+juv_alagnon[i,t-5]+juv_langeac[i,t-5]+juv_poutes[i,t-5]))
		coef_juv_tot_3[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-2]+juv_alagnon[i,t-2]+juv_langeac[i,t-2]+juv_poutes[i,t-2])+(juv_vichy[i,t-3]+juv_alagnon[i,t-3]+juv_langeac[i,t-3]+juv_poutes[i,t-3])+(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4]))
		
	}
}

#On calcule le taux de renouvellement.
#Les boucles prennent en compte que N_Vichy calcul� uniquement quand pas de donn�es � Vichy. Quand donn�es, il faut aller directement les r�cup�rer
#library(stringr)
#bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.07.19.R")
#source("data_4zones_Interaction_2017.07.19.R")
data_vichy<-N[,1]

for (t in 7:21){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*N_vichy[i,t-1]+coef_juv_tot_2[i,t]*N_vichy[i,t]+coef_juv_tot_3[i,t]*N_vichy[i,t+1])/N_vichy[i,t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/N_vichy[i,t-5])
		
	}
}
for (t in 22:22){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*N_vichy[i,t-1]+coef_juv_tot_2[i,t]*N_vichy[i,t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/N_vichy[i,t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/N_vichy[i,t-5])
		
	}
}
for (t in 23:23){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*N_vichy[i,t-1]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/N_vichy[i,t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/N_vichy[i,t-5])
		
	}
}
for (t in 24:27){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*data_vichy[t-1]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/N_vichy[i,t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/N_vichy[i,t-5])
		
	}
}

for (t in 28:28){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*data_vichy[t-1]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/data_vichy[t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/data_vichy[t-5])
		
	}
}
for (t in 29:29){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*data_vichy[t-1]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*N_vichy[t+1-7])/data_vichy[t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/data_vichy[t-5])
		
	}
}
for (t in 30:30){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*data_vichy[t-1]+coef_juv_tot_2[i,t]*N_vichy[t-7]+coef_juv_tot_3[i,t]*data_vichy[t+1])/data_vichy[t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/data_vichy[t-5])
		
	}
}
for (t in 31:31){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*N_vichy[t-1-7]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/data_vichy[t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/data_vichy[t-5])
		
	}
}
for (t in 32:34){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*data_vichy[t-1]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/data_vichy[t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/data_vichy[t-5])
		
	}
}
for (t in 35:35){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*data_vichy[t-1]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/N_vichy[i,t-12])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/N_vichy[i,t-12])
	}
}
for (t in 36:(T-1)){
	for (i in 1:5000){
		renew_rate_coef[i,t-5]<-log((coef_juv_tot_1[i,t]*data_vichy[t-1]+coef_juv_tot_2[i,t]*data_vichy[t]+coef_juv_tot_3[i,t]*data_vichy[t+1])/data_vichy[t-5])
		#renew_rate[i,t-5]<-log(((1/3)*N_vichy[i,t-1]+(1/3)*N_vichy[i,t]+(1/3)*N_vichy[i,t+1])/data_vichy[t-5])
		
	}
}


for (t in 7:T){
	renew_rate_coef_q[t,]=quantile(renew_rate_coef[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	#renew_rate_w_q[t,]=quantile(renew_rate[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

##ON SAUVE renew_rate_coef et renew_rate_coef_q car on en a besoin dans les indicateurs du PLAGEPOMI pour les projections (besoin d'une partie des données de
##la période rétrospective car moyenne mobile sur 5 ans
#save(renew_rate_coef,renew_rate_coef_q,file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_Tx_renouv_period_retro_2019_12_12")
save(renew_rate_coef,renew_rate_coef_q,file=here::here("2022_07_01_Tx_renouv_period_retro_2021_09.RData"))

####### Calcul sur le taux de renouvellement

#Calcul de la moyenne géométrique sur les données qui ne sont pas en valeur logarithmique
#geoMean(exp(renew_rate_coef_q[7:36,3])) # revient au même que faire une moyenne arithmétique sur des données en valeur logarithmique
#Calcul de la moyenne sur les séries MCMC plutôt que sur les quantiles
exp(mean(rowMeans(renew_rate_coef[,7:(T-6)]))) # donne résultat quasi-identique que moyenne sur les quantiles
exp(mean(rowMeans(renew_rate_coef[,(T-6-4):(T-6)])))
exp(mean(rowMeans(renew_rate_coef[,7:11])))

#exp(mean(renew_rate_coef_q[7:36,3]))
#exp(mean(renew_rate_coef_q[32:36,3]))
#exp(mean(renew_rate_coef_q[7:11,3]))

##Echelle log
mean(renew_rate_coef_q[7:36,3])
mean(renew_rate_coef_q[32:36,3])
mean(renew_rate_coef_q[7:11,3])

#======================
# Echelle naturelle
#=====================
#on enlève la dénomination coef dans l'intitulé de la figure car c'est ce qu'on conserve tout le temps (le calcul 1/3,1/3,1/3 étant faux)
png(filename=here::here("img/Simulation/2021_09/2022_07_01 Tx Renouv_PopTotale_boxplot_EchelleNat_model_2021_09.png"),width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2019_12_IndicateursSat_model_data2018_2019_12_12/TauxRenouvellementPop_EchelleNat.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPop_EchelleNat.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPop_coef.png",width=1000,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(6.5,T-7+0.5),xlab="Années",ylim=c(0,9),ylab="Taux de renouvellement",main="Taux de renouvellement de la population totale",cex.lab=1.5)

# trace l'axe des ordonnées
axis(2,at = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9),labels=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(7,16,26,(T-6)),
		labels=c((1975+6),1990,2000,(1975+T-7)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,exp(renew_rate_coef_q[i,5]),i+0.15,exp(renew_rate_coef_q[i,5]))
	segments(i,exp(renew_rate_coef_q[i,4]),i,exp(renew_rate_coef_q[i,5]))
	#5%
	segments(i-0.15,exp(renew_rate_coef_q[i,1]),i+0.15,exp(renew_rate_coef_q[i,1]))
	segments(i,exp(renew_rate_coef_q[i,2]),i,exp(renew_rate_coef_q[i,1]))
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(exp(renew_rate_coef_q[i,2]),exp(renew_rate_coef_q[i,2]),exp(renew_rate_coef_q[i,4]),exp(renew_rate_coef_q[i,4])),col="light grey")
	#median
	segments(i-0.3,exp(renew_rate_coef_q[i,3]),i+0.3,exp(renew_rate_coef_q[i,3]))
}
abline(h=1,col="red")
abline(h=exp(mean(renew_rate_coef_q[7:38,3])),col="black",lty=5,lwd=2)

dev.off()



#====================
# figure log
#====================
#on enlève la dénomination coef dans l'intitulé de la figure car c'est ce qu'on conserve tout le temps (le calcul 1/3,1/3,1/3 étant faux)
png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2019_12_IndicateursSat_model_data2018_2019_12_12/TauxRenouvellementPop_log.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPop.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPop_coef.png",width=1000,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(6.5,T+0.5-6),xlab="Années",ylim=c(-3,3),ylab="log(Taux de renouvellement)",main="Taux de renouvellement de la population totale",cex.lab=1.5)

# trace l'axe des ordonnées
axis(2,at = c(-3,-2,-1,0,1,2,3),labels=c(-3,-2,-1,0,1,2,3),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(7,16,26,(T-6)),
		labels=c((1975+6),1990,2000,(1975+T-7)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,renew_rate_coef_q[i,5],i+0.15,renew_rate_coef_q[i,5])
	segments(i,renew_rate_coef_q[i,4],i,renew_rate_coef_q[i,5])
	#5%
	segments(i-0.15,renew_rate_coef_q[i,1],i+0.15,renew_rate_coef_q[i,1])
	segments(i,renew_rate_coef_q[i,2],i,renew_rate_coef_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_coef_q[i,2],renew_rate_coef_q[i,2],renew_rate_coef_q[i,4],renew_rate_coef_q[i,4]),col="light grey")
	#median
	segments(i-0.3,renew_rate_coef_q[i,3],i+0.3,renew_rate_coef_q[i,3])
}
abline(h=0,col="red")
abline(h=mean(renew_rate_coef_q[7:38,3]),col="black",lty=5,lwd=2)

dev.off()

###Tx renouv ensemble période
mean(renew_rate_coef_q[7:38,3]) #-0.05
exp(mean(renew_rate_coef_q[7:38,3])) #0.95
###Tx renouv 5 premières années
mean(renew_rate_coef_q[7:11,3]) #-0.43
exp(mean(renew_rate_coef_q[7:11,3])) #0.65
###Tx renouv 5 dernières années
mean(renew_rate_coef_q[34:38,3]) #0.38
exp(mean(renew_rate_coef_q[34:38,3])) #1,46


png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPop_82_2011.png",width=1000,height=800)
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPop_coef.png",width=1000,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(7.5,T+0.5-6),xlab="Years",ylim=c(-5,5),ylab="Taux renouvellement",main="Taux de renouvellement de la population",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(-5,-4,-3,-2,-1,0,1,2,3,4,5),labels=c(-5,-4,-3,-2,-1,0,1,2,3,4,5),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(7,15,25,(T-6)),
		labels=c((1975+7),1990,2000,(1975+T-6)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,renew_rate_coef_q[i,5],i+0.15,renew_rate_coef_q[i,5])
	segments(i,renew_rate_coef_q[i,4],i,renew_rate_coef_q[i,5])
	#5%
	segments(i-0.15,renew_rate_coef_q[i,1],i+0.15,renew_rate_coef_q[i,1])
	segments(i,renew_rate_coef_q[i,2],i,renew_rate_coef_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_coef_q[i,2],renew_rate_coef_q[i,2],renew_rate_coef_q[i,4],renew_rate_coef_q[i,4]),col="light grey")
	#median
	segments(i-0.3,renew_rate_coef_q[i,3],i+0.3,renew_rate_coef_q[i,3])
}
abline(h=0,col="red")

dev.off()

##### Graph Tx renouvellement moyen des m�dianes par tranche de 5 ans #####

#Calcul du taux de renouvellement en �chelle classique (1= renouvellement)
renew_rate_coef_q_5yr=array(0,dim=c(length(seq(7,(T-6),5)),1))
j=7

for (i in 1:length(seq(7,(T-6),5))){
	renew_rate_coef_q_5yr[i]<-exp(mean(renew_rate_coef_q[j:(j+4),3]))
	j=j+5
}
rownames(renew_rate_coef_q_5yr)<-c("1981-1985","1986-1990","1991-1995","1996-2000","2001-2005","2006-2010")

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPop_5yrs.png",width=1000,height=800)
par("mar"=c(6.1,5.1,3.1,1.1))
plot(renew_rate_coef_q_5yr,axes=FALSE,xlab="",ylab="Taux renouvellement",main=iconv("Taux de renouvellement de la population (moyenne des m�dianes sur 5 ans)","UTF8"),cex.lab=1.5)
axis(1,at = seq(1,length(seq(7,(T-6),5)),1),
		labels=rownames(renew_rate_coef_q_5yr),
		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
axis(2,at = seq(0,max(renew_rate_coef_q_5yr),0.2),labels=seq(0,max(renew_rate_coef_q_5yr),0.2),cex.axis = 1.2,las = 1,lwd=2,col = "black")
for (s in 1:length(seq(7,(T-6),5))){
segments(s,renew_rate_coef_q_5yr[s],(s+1),renew_rate_coef_q_5yr[s+1])
}
abline(h=1,col="red")

dev.off()

#Calcul du taux de renouvellement en �chelle log (0=renouvellement)
L_renew_rate_coef_q_5yr=array(0,dim=c(length(seq(7,(T-6),5)),1))
j=7

for (i in 1:length(seq(7,(T-6),5))){
	L_renew_rate_coef_q_5yr[i]<-mean(renew_rate_coef_q[j:(j+4),3])
	j=j+5
}
rownames(L_renew_rate_coef_q_5yr)<-c("1981-1985","1986-1990","1991-1995","1996-2000","2001-2005","2006-2010")

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPop_5yrs_log.png",width=1000,height=800)
par("mar"=c(6.1,5.1,3.1,1.1))
plot(L_renew_rate_coef_q_5yr,axes=FALSE,xlab="",ylab=iconv("Taux renouvellement (�chelle log)","UTF8"),main=iconv("Taux de renouvellement de la population (log de la moyenne des m�dianes sur 5 ans)","UTF8"),cex.lab=1.5)
axis(1,at = seq(1,length(seq(7,(T-6),5)),1),
		labels=rownames(L_renew_rate_coef_q_5yr),
		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
axis(2,at = seq(min(L_renew_rate_coef_q_5yr),max(L_renew_rate_coef_q_5yr),0.2),labels=round(seq(min(L_renew_rate_coef_q_5yr),max(L_renew_rate_coef_q_5yr),0.2),1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
for (s in 1:length(seq(7,(T-6),5))){
	segments(s,L_renew_rate_coef_q_5yr[s],(s+1),L_renew_rate_coef_q_5yr[s+1])
}
abline(h=0,col="red")

dev.off()

####Old

#for (t in 7:T){
#	for (i in 1:5000){
##		coef_juv_1[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/juv_wild_tot_system[i,t-1]
##		coef_juv_2[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/juv_wild_tot_system[i,t]
##		coef_juv_3[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/juv_wild_tot_system[i,t+1]
#	
#	}
#}


