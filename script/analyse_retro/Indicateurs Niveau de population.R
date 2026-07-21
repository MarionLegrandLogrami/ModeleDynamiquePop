# Développement de l'indicateur niveau de population
# 
# Author: marion.legrand
###############################################################################
#Modèle 2017.08.29_Interraction_ss_rho_poutes_matriceVC
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")


library(coda)
library(boot)
T=42

surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)
S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Créé une 5eme ligne qui correspond � la somme par année des surfaces sur l'ensemble des secteurs

S_juv_JP_tot<-S_juv_JP[,T]


#==================================================
# Niveau population (pop totale =sauvage+élevage)
#==================================================

#On récupère Rmax et on prend plusieurs part de ce Rmax qu'on choisit à priori
Rmax=read.coda("RmaxCODAchain1.txt","RmaxCODAindex.txt")
bugs_nu_d=read.coda("nu_dCODAchain1.txt","nu_dCODAindex.txt")
#--------------------------------------------------------
# Quantité de juvénile en fonction de proportion Rmax
#--------------------------------------------------------
#..........
# 25% Rmax
#..........

Juv_Rmax_25_V=array(0,dim=c(10000,T))
Juv_Rmax_25_A=array(0,dim=c(10000,T))
Juv_Rmax_25_L=array(0,dim=c(10000,T))
Juv_Rmax_25_P=array(0,dim=c(10000,T))
Juv_Rmax_25=array(0,dim=c(10000,T))

for (t in 1:T){ #on fait tourner sur T car les quantitats d'habitat ont pu évoluer au cours du temps
	for (i in 1:10000){
		Juv_Rmax_25_V[i,t]<-0.25*Rmax[i]*exp(bugs_nu_d[i,1])*S_juv_JP[1,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_25_A[i,t]<-0.25*Rmax[i]*exp(bugs_nu_d[i,2])*S_juv_JP[2,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_25_L[i,t]<-0.25*Rmax[i]*exp(bugs_nu_d[i,3])*S_juv_JP[3,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_25_P[i,t]<-0.25*Rmax[i]*exp(bugs_nu_d[i,4])*S_juv_JP[4,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_25[i,t]<-Juv_Rmax_25_V[i,t]+Juv_Rmax_25_A[i,t]+Juv_Rmax_25_L[i,t]+Juv_Rmax_25_P[i,t] #mean=106515.9
	}
}

#..........
# 50% Rmax
#..........
Juv_Rmax_50_V=array(0,dim=c(10000,T))
Juv_Rmax_50_A=array(0,dim=c(10000,T))
Juv_Rmax_50_L=array(0,dim=c(10000,T))
Juv_Rmax_50_P=array(0,dim=c(10000,T))
Juv_Rmax_50=array(0,dim=c(10000,T))

for (t in 1:T){
	for (i in 1:10000){
		Juv_Rmax_50_V[i,t]<-0.50*Rmax[i]*exp(bugs_nu_d[i,1])*S_juv_JP[1,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_50_A[i,t]<-0.50*Rmax[i]*exp(bugs_nu_d[i,2])*S_juv_JP[2,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_50_L[i,t]<-0.50*Rmax[i]*exp(bugs_nu_d[i,3])*S_juv_JP[3,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_50_P[i,t]<-0.50*Rmax[i]*exp(bugs_nu_d[i,4])*S_juv_JP[4,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_50[i,t]<-Juv_Rmax_50_V[i,t]+Juv_Rmax_50_A[i,t]+Juv_Rmax_50_L[i,t]+Juv_Rmax_50_P[i,t] #mean=213031.8
	}
}


#..........
# 75% Rmax
#..........
Juv_Rmax_75_V=array(0,dim=c(10000,T))
Juv_Rmax_75_A=array(0,dim=c(10000,T))
Juv_Rmax_75_L=array(0,dim=c(10000,T))
Juv_Rmax_75_P=array(0,dim=c(10000,T))
Juv_Rmax_75=array(0,dim=c(10000,T))

for (t in 1:T){
	for (i in 1:10000){
		Juv_Rmax_75_V[i,t]<-0.75*Rmax[i]*exp(bugs_nu_d[i,1])*S_juv_JP[1,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_75_A[i,t]<-0.75*Rmax[i]*exp(bugs_nu_d[i,2])*S_juv_JP[2,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_75_L[i,t]<-0.75*Rmax[i]*exp(bugs_nu_d[i,3])*S_juv_JP[3,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_75_P[i,t]<-0.75*Rmax[i]*exp(bugs_nu_d[i,4])*S_juv_JP[4,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_75[i,t]<-Juv_Rmax_75_V[i,t]+Juv_Rmax_75_A[i,t]+Juv_Rmax_75_L[i,t]+Juv_Rmax_75_P[i,t] #mean=319547.8
	}
}

#..........
# 100% Rmax
#..........
Juv_Rmax_100_V=array(0,dim=c(10000,T))
Juv_Rmax_100_A=array(0,dim=c(10000,T))
Juv_Rmax_100_L=array(0,dim=c(10000,T))
Juv_Rmax_100_P=array(0,dim=c(10000,T))
Juv_Rmax_100=array(0,dim=c(10000,T))

for (t in 1:T){
	for (i in 1:10000){
		Juv_Rmax_100_V[i,t]<-Rmax[i]*exp(bugs_nu_d[i,1])*S_juv_JP[1,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_100_A[i,t]<-Rmax[i]*exp(bugs_nu_d[i,2])*S_juv_JP[2,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_100_L[i,t]<-Rmax[i]*exp(bugs_nu_d[i,3])*S_juv_JP[3,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_100_P[i,t]<-Rmax[i]*exp(bugs_nu_d[i,4])*S_juv_JP[4,t] #multiplie par nu_d pour tenir compte de la différence de production de juvéniles des différentes zones
		Juv_Rmax_100[i,t]<-Juv_Rmax_100_V[i,t]+Juv_Rmax_100_A[i,t]+Juv_Rmax_100_L[i,t]+Juv_Rmax_100_P[i,t] #mean=319547.8
	}
}

mean_Juv_Rmax_25<-mean(Juv_Rmax_25[,(T-4):T]) # 105 165 -old sans prise en compte de nu_d :90 419
mean_Juv_Rmax_50<-mean(Juv_Rmax_50[,(T-4):T]) # 210 329 -old sans prise en compte de nu_d :180 839
mean_Juv_Rmax_75<-mean(Juv_Rmax_75[,(T-4):T]) # 315 494 -old sans prise en compte de nu_d :271 258
mean_Juv_Rmax_100<-mean(Juv_Rmax_100[,(T-4):T]) # 420 659

#-----------------------------
# Survie juvénile / adulte
#-----------------------------
bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt")
bugs_s_juv2ad=read.coda("simulation/s_juv2adCODAchain1.txt","simulation/s_juv2adCODAindex.txt")
bugs_sigma_vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")


##Niveau actuelle de survie
surv<-mean(bugs_level_s)
##50 % de la survie initiale
surv_50<-mean(bugs_level_s)/2
level_s_fake_50<-log(exp(mean(bugs_level_s)/2))
##100% de la survie initiale
surv_100<-mean(bugs_level_s)
level_s_fake_100<-log(exp(mean(bugs_level_s)))

#------------------
# 25% de Rmax
#------------------

niveau_pop_25_actu<-array(0,dim=c(10000,T))
niveau_pop_25_50<-array(0,dim=c(10000,T))
niveau_pop_25_100<-array(0,dim=c(10000,T))

N_vichy_25_actu<-array(0,dim=c(10000,T))
N_vichy_25_50<-array(0,dim=c(10000,T))
N_vichy_25_100<-array(0,dim=c(10000,T))

for (t in 1:T){
	for (i in 1:10000){
		##Scénario 25% Rmax et survie actuelle
			niveau_pop_25_actu[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_25[i,t])
			N_vichy_25_actu[i,t]<-rlnorm(1,niveau_pop_25_actu[i,t],bugs_sigma_vichy[i]) #juv 25% Rmax dans condition actuelle de survie produiraient 382 SAT de retour en moyenne à Vichy
		# Scénario 25% Rmax et retour à 50% du niveau de survie historique
			niveau_pop_25_50[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_25[i,t]) + level_s_fake_50
			N_vichy_25_50[i,t]<-rlnorm(1,niveau_pop_25_50[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 50% de la survie historique produiraient 808 SAT de retour en moyenne
		# Scénario 25% Rmax et retour 100% du niveau de survie historique
			niveau_pop_25_100[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_25[i,t]) + level_s_fake_100
			N_vichy_25_100[i,t]<-rlnorm(1,niveau_pop_25_100[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 100% de la survie historique produiraient 1732 SAT de retour en moyenne
	
	}
}

mean_N_vichy_25_actu<-0
mean_N_vichy_25_50<-0
mean_N_vichy_25_100<-0

for (t in 1:T){
	mean_N_vichy_25_actu[t]<-mean(N_vichy_25_actu[,t]) # 382
	mean_N_vichy_25_50[t]<-mean(N_vichy_25_50[,t]) # 806
	mean_N_vichy_25_100[t]<-mean(N_vichy_25_100[,t]) # 1708
}
#------------------
# 50% de Rmax
#------------------

niveau_pop_50_actu<-array(0,dim=c(10000,T))
niveau_pop_50_50<-array(0,dim=c(10000,T))
niveau_pop_50_100<-array(0,dim=c(10000,T))

N_vichy_50_actu<-array(0,dim=c(10000,T))
N_vichy_50_50<-array(0,dim=c(10000,T))
N_vichy_50_100<-array(0,dim=c(10000,T))

for (t in 1:T){
	for (i in 1:10000){
		##Scénario 50% Rmax et survie actuelle
		niveau_pop_50_actu[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_50[i,t])
		N_vichy_50_actu[i,t]<-rlnorm(1,niveau_pop_50_actu[i,t],bugs_sigma_vichy[i]) #juv 25% Rmax dans condition actuelle de survie produiraient 752 SAT de retour en moyenne à Vichy
		# Scénario 50% Rmax et retour à 50% du niveau de survie historique
		niveau_pop_50_50[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_50[i,t]) + level_s_fake_50
		N_vichy_50_50[i,t]<-rlnorm(1,niveau_pop_50_50[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 50% de la survie historique produiraient 1598 SAT de retour en moyenne
		# Scénario 50% Rmax et retour 100% du niveau de survie historique
		niveau_pop_50_100[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_50[i,t]) + level_s_fake_100
		N_vichy_50_100[i,t]<-rlnorm(1,niveau_pop_50_100[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 100% de la survie historique produiraient 3440 SAT de retour en moyenne
	}
}

mean_N_vichy_50_actu<-0
mean_N_vichy_50_50<-0
mean_N_vichy_50_100<-0

for (t in 1:T){
	mean_N_vichy_50_actu[t]<-mean(N_vichy_50_actu[,t]) # 752
	mean_N_vichy_50_50[t]<-mean(N_vichy_50_50[,t]) # 1598
	mean_N_vichy_50_100[t]<-mean(N_vichy_50_100[,t]) # 3440
}

#------------------
# 75% de Rmax
#------------------
niveau_pop_75_actu<-array(0,dim=c(10000,T))
niveau_pop_75_50<-array(0,dim=c(10000,T))
niveau_pop_75_100<-array(0,dim=c(10000,T))

N_vichy_75_actu<-array(0,dim=c(10000,T))
N_vichy_75_50<-array(0,dim=c(10000,T))
N_vichy_75_100<-array(0,dim=c(10000,T))

for (t in 1:T){
	for (i in 1:10000){
		##Scénario 75% Rmax et survie actuelle
		niveau_pop_75_actu[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_75[i,t])
		N_vichy_75_actu[i,t]<-rlnorm(1,niveau_pop_75_actu[i,t],bugs_sigma_vichy[i]) #juv 25% Rmax dans condition actuelle de survie produiraient 752 SAT de retour en moyenne à Vichy
		# Scénario 75% Rmax et retour à 50% du niveau de survie historique
		niveau_pop_75_50[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_75[i,t]) + level_s_fake_50
		N_vichy_75_50[i,t]<-rlnorm(1,niveau_pop_75_50[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 50% de la survie historique produiraient 1598 SAT de retour en moyenne
		# Scénario 75% Rmax et retour 100% du niveau de survie historique
		niveau_pop_75_100[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_75[i,t]) + level_s_fake_100
		N_vichy_75_100[i,t]<-rlnorm(1,niveau_pop_75_100[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 100% de la survie historique produiraient 3440 SAT de retour en moyenne
	}
}

mean_N_vichy_75_actu<-0
mean_N_vichy_75_50<-0
mean_N_vichy_75_100<-0

for (t in 1:T){
	mean_N_vichy_75_actu[t]<-mean(N_vichy_75_actu[,t]) # 1136
	mean_N_vichy_75_50[t]<-mean(N_vichy_75_50[,t]) # 2434
	mean_N_vichy_75_100[t]<-mean(N_vichy_75_100[,t]) # 5120
}

#------------------
# 100% de Rmax
#------------------
niveau_pop_100_actu<-array(0,dim=c(10000,T))
niveau_pop_100_50<-array(0,dim=c(10000,T))
niveau_pop_100_100<-array(0,dim=c(10000,T))

N_vichy_100_actu<-array(0,dim=c(10000,T))
N_vichy_100_50<-array(0,dim=c(10000,T))
N_vichy_100_100<-array(0,dim=c(10000,T))

for (t in 1:T){
	for (i in 1:10000){
		##Scénario 100% Rmax et survie actuelle
		niveau_pop_100_actu[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_100[i,t])
		N_vichy_100_actu[i,t]<-rlnorm(1,niveau_pop_100_actu[i,t],bugs_sigma_vichy[i]) #juv 25% Rmax dans condition actuelle de survie produiraient 752 SAT de retour en moyenne à Vichy
		# Scénario 100% Rmax et retour à 50% du niveau de survie historique
		niveau_pop_100_50[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_100[i,t]) + level_s_fake_50
		N_vichy_100_50[i,t]<-rlnorm(1,niveau_pop_100_50[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 50% de la survie historique produiraient 1598 SAT de retour en moyenne
		# Scénario 100% Rmax et retour 100% du niveau de survie historique
		niveau_pop_100_100[i,t]<- log(bugs_s_juv2ad[i]*Juv_Rmax_100[i,t]) + level_s_fake_100
		N_vichy_100_100[i,t]<-rlnorm(1,niveau_pop_100_100[i,t],bugs_sigma_vichy[i]) #juv 25% de Rmax dans retour à 100% de la survie historique produiraient 3440 SAT de retour en moyenne
	}
}

mean_N_vichy_100_actu<-0
mean_N_vichy_100_50<-0
mean_N_vichy_100_100<-0

for (t in 1:T){
	mean_N_vichy_100_actu[t]<-mean(N_vichy_100_actu[,t]) # 1500
	mean_N_vichy_100_50[t]<-mean(N_vichy_100_50[,t]) # 3180
	mean_N_vichy_100_100[t]<-mean(N_vichy_100_100[,t]) # 6800
}


#======================================================================
# On regarde comment on se situe à partir de ces points de référence
#======================================================================

#----------------------------------
# Sur l'ensemble du jeu de données
#----------------------------------

#On récupère les estimations du nbr d'adulte à Vichy
bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 ann�e de suivi station (soit T+20 - 15)
#On récupère les données de passage
bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.12.12.R")
source("data_4zones_Interaction_2017.12.12.R")
data_vichy<-N[,1]

#on regarde probabilité d'être égale ou supérieur au point de référence
upper_N_vichy_25_actu=array(0,dim=c(5000,T))
upper_N_vichy_50_actu=array(0,dim=c(5000,T))
upper_N_vichy_75_actu=array(0,dim=c(5000,T))
upper_N_vichy_100_actu=array(0,dim=c(5000,T))

upper_N_vichy_25_50=array(0,dim=c(5000,T))
upper_N_vichy_50_50=array(0,dim=c(5000,T))
upper_N_vichy_75_50=array(0,dim=c(5000,T))
upper_N_vichy_100_50=array(0,dim=c(5000,T))

upper_N_vichy_25_100=array(0,dim=c(5000,T))
upper_N_vichy_50_100=array(0,dim=c(5000,T))
upper_N_vichy_75_100=array(0,dim=c(5000,T))
upper_N_vichy_100_100=array(0,dim=c(5000,T))

for (t in 1:22){
	for (i in 1:5000){
		if(bugs_N_vichy_real[i,t] >= N_vichy_25_actu[i,t]){upper_N_vichy_25_actu[i,t]=1}  
		if(bugs_N_vichy_real[i,t] >= N_vichy_50_actu[i,t]){upper_N_vichy_50_actu[i,t]=1} 
		if(bugs_N_vichy_real[i,t] >= N_vichy_75_actu[i,t]){upper_N_vichy_75_actu[i,t]=1} 
		if(bugs_N_vichy_real[i,t] >= N_vichy_100_actu[i,t]){upper_N_vichy_100_actu[i,t]=1}
		
		if(bugs_N_vichy_real[i,t] >= N_vichy_25_50[i,t]){upper_N_vichy_25_50[i,t]=1}  
		if(bugs_N_vichy_real[i,t] >= N_vichy_50_50[i,t]){upper_N_vichy_50_50[i,t]=1} 
		if(bugs_N_vichy_real[i,t] >= N_vichy_75_50[i,t]){upper_N_vichy_75_50[i,t]=1} 
		if(bugs_N_vichy_real[i,t] >= N_vichy_100_50[i,t]){upper_N_vichy_100_50[i,t]=1} 
		
		if(bugs_N_vichy_real[i,t] >= N_vichy_25_100[i,t]){upper_N_vichy_25_100[i,t]=1}  
		if(bugs_N_vichy_real[i,t] >= N_vichy_50_100[i,t]){upper_N_vichy_50_100[i,t]=1} 
		if(bugs_N_vichy_real[i,t] >= N_vichy_75_100[i,t]){upper_N_vichy_75_100[i,t]=1} 
		if(bugs_N_vichy_real[i,t] >= N_vichy_100_100[i,t]){upper_N_vichy_100_100[i,t]=1}
	}
}

for (t in 23:29){ #on commence à t=23 car 1ere année de fonctionnement de la station de Vichy
	for (i in 1:5000){
		if(data_vichy[t] >= N_vichy_25_actu[i,t]){upper_N_vichy_25_actu[i,t]=1}  
		if(data_vichy[t] >= N_vichy_50_actu[i,t]){upper_N_vichy_50_actu[i,t]=1} 
		if(data_vichy[t] >= N_vichy_75_actu[i,t]){upper_N_vichy_75_actu[i,t]=1}
		if(data_vichy[t] >= N_vichy_100_actu[i,t]){upper_N_vichy_100_actu[i,t]=1}
		
		if(data_vichy[t] >= N_vichy_25_50[i,t]){upper_N_vichy_25_50[i,t]=1}  
		if(data_vichy[t] >= N_vichy_50_50[i,t]){upper_N_vichy_50_50[i,t]=1} 
		if(data_vichy[t] >= N_vichy_75_50[i,t]){upper_N_vichy_75_50[i,t]=1} 	
		if(data_vichy[t] >= N_vichy_100_50[i,t]){upper_N_vichy_100_50[i,t]=1} 	
		
		if(data_vichy[t] >= N_vichy_25_100[i,t]){upper_N_vichy_25_100[i,t]=1}  
		if(data_vichy[t] >= N_vichy_50_100[i,t]){upper_N_vichy_50_100[i,t]=1} 
		if(data_vichy[t] >= N_vichy_75_100[i,t]){upper_N_vichy_75_100[i,t]=1} 
		if(data_vichy[t] >= N_vichy_100_100[i,t]){upper_N_vichy_100_100[i,t]=1}
	}
}

for (t in 30:30){
	for (i in 1:5000){
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_25_actu[i,t]){upper_N_vichy_25_actu[i,t]=1}  #on prend t-7 car bugs_vichy_real uniquement sur les éléments quand pas de comptage donc la colonne 23 correspond à l'année 30
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_50_actu[i,t]){upper_N_vichy_50_actu[i,t]=1} 
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_75_actu[i,t]){upper_N_vichy_75_actu[i,t]=1} 	
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_100_actu[i,t]){upper_N_vichy_100_actu[i,t]=1} 	
		
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_25_50[i,t]){upper_N_vichy_25_50[i,t]=1}  
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_50_50[i,t]){upper_N_vichy_50_50[i,t]=1} 
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_75_50[i,t]){upper_N_vichy_75_50[i,t]=1} 
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_100_50[i,t]){upper_N_vichy_100_50[i,t]=1}
		
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_25_100[i,t]){upper_N_vichy_25_100[i,t]=1}  
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_50_100[i,t]){upper_N_vichy_50_100[i,t]=1} 
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_75_100[i,t]){upper_N_vichy_75_100[i,t]=1}
		if(bugs_N_vichy_real[i,(t-7)] >= N_vichy_100_100[i,t]){upper_N_vichy_100_100[i,t]=1}
	}
}

for (t in 31:T){ #on reprend après 30 car t=30 année où comptage Vichy considéré comme minimum d'où pas de données
	for (i in 1:5000){
		if(data_vichy[t] >= N_vichy_25_actu[i,t]){upper_N_vichy_25_actu[i,t]=1}  
		if(data_vichy[t] >= N_vichy_50_actu[i,t]){upper_N_vichy_50_actu[i,t]=1} 
		if(data_vichy[t] >= N_vichy_75_actu[i,t]){upper_N_vichy_75_actu[i,t]=1} 
		if(data_vichy[t] >= N_vichy_100_actu[i,t]){upper_N_vichy_100_actu[i,t]=1} 
		
		if(data_vichy[t] >= N_vichy_25_50[i,t]){upper_N_vichy_25_50[i,t]=1}  
		if(data_vichy[t] >= N_vichy_50_50[i,t]){upper_N_vichy_50_50[i,t]=1} 
		if(data_vichy[t] >= N_vichy_75_50[i,t]){upper_N_vichy_75_50[i,t]=1} 
		if(data_vichy[t] >= N_vichy_100_50[i,t]){upper_N_vichy_100_50[i,t]=1} 
		
		if(data_vichy[t] >= N_vichy_25_100[i,t]){upper_N_vichy_25_100[i,t]=1}  
		if(data_vichy[t] >= N_vichy_50_100[i,t]){upper_N_vichy_50_100[i,t]=1} 
		if(data_vichy[t] >= N_vichy_75_100[i,t]){upper_N_vichy_75_100[i,t]=1} 	
		if(data_vichy[t] >= N_vichy_100_100[i,t]){upper_N_vichy_100_100[i,t]=1} 		
	}
}

p_upper_N_vichy_25_actu=rep(0,T)
p_upper_N_vichy_50_actu=rep(0,T)
p_upper_N_vichy_75_actu=rep(0,T)
p_upper_N_vichy_100_actu=rep(0,T)

p_upper_N_vichy_25_50=rep(0,T)
p_upper_N_vichy_50_50=rep(0,T)
p_upper_N_vichy_75_50=rep(0,T)
p_upper_N_vichy_100_50=rep(0,T)

p_upper_N_vichy_25_100=rep(0,T)
p_upper_N_vichy_50_100=rep(0,T)
p_upper_N_vichy_75_100=rep(0,T)
p_upper_N_vichy_100_100=rep(0,T)

for (t in 1:T){
	p_upper_N_vichy_25_actu[t]=mean(upper_N_vichy_25_actu[,t])
	p_upper_N_vichy_50_actu[t]=mean(upper_N_vichy_50_actu[,t])
	p_upper_N_vichy_75_actu[t]=mean(upper_N_vichy_75_actu[,t])
	p_upper_N_vichy_100_actu[t]=mean(upper_N_vichy_100_actu[,t])
	
	p_upper_N_vichy_25_50[t]=mean(upper_N_vichy_25_50[,t])
	p_upper_N_vichy_50_50[t]=mean(upper_N_vichy_50_50[,t])
	p_upper_N_vichy_75_50[t]=mean(upper_N_vichy_75_50[,t])
	p_upper_N_vichy_100_50[t]=mean(upper_N_vichy_100_50[,t])
	
	p_upper_N_vichy_25_100[t]=mean(upper_N_vichy_25_100[,t])
	p_upper_N_vichy_50_100[t]=mean(upper_N_vichy_50_100[,t])
	p_upper_N_vichy_75_100[t]=mean(upper_N_vichy_75_100[,t])
	p_upper_N_vichy_100_100[t]=mean(upper_N_vichy_100_100[,t])
}

#-----------------------------
# Sur les 5 dernières années
#-----------------------------
#on regarde probabilité d'être égale ou supérieur au point de référence
upper_N_vichy_25_actu_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_50_actu_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_75_actu_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_100_actu_5lastyr=array(0,dim=c(5000,5))

upper_N_vichy_25_50_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_50_50_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_75_50_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_100_50_5lastyr=array(0,dim=c(5000,5))

upper_N_vichy_25_100_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_50_100_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_75_100_5lastyr=array(0,dim=c(5000,5))
upper_N_vichy_100_100_5lastyr=array(0,dim=c(5000,5))

for (t in (T-4):T){
	for (i in 1:5000){
		if(data_vichy[t] >= N_vichy_25_actu[i,t]){upper_N_vichy_25_actu_5lastyr[i,(t-(T-5))]=1}  
		if(data_vichy[t] >= N_vichy_50_actu[i,t]){upper_N_vichy_50_actu_5lastyr[i,(t-(T-5))]=1} 
		if(data_vichy[t] >= N_vichy_75_actu[i,t]){upper_N_vichy_75_actu_5lastyr[i,(t-(T-5))]=1}
		if(data_vichy[t] >= N_vichy_100_actu[i,t]){upper_N_vichy_100_actu_5lastyr[i,(t-(T-5))]=1}
		
		if(data_vichy[t] >= N_vichy_25_50[i,t]){upper_N_vichy_25_50_5lastyr[i,(t-(T-5))]=1}  
		if(data_vichy[t] >= N_vichy_50_50[i,t]){upper_N_vichy_50_50_5lastyr[i,(t-(T-5))]=1} 
		if(data_vichy[t] >= N_vichy_75_50[i,t]){upper_N_vichy_75_50_5lastyr[i,(t-(T-5))]=1} 	
		if(data_vichy[t] >= N_vichy_100_50[i,t]){upper_N_vichy_100_50_5lastyr[i,(t-(T-5))]=1} 	
		
		if(data_vichy[t] >= N_vichy_25_100[i,t]){upper_N_vichy_25_100_5lastyr[i,(t-(T-5))]=1}  
		if(data_vichy[t] >= N_vichy_50_100[i,t]){upper_N_vichy_50_100_5lastyr[i,(t-(T-5))]=1} 
		if(data_vichy[t] >= N_vichy_75_100[i,t]){upper_N_vichy_75_100_5lastyr[i,(t-(T-5))]=1} 
		if(data_vichy[t] >= N_vichy_100_100[i,t]){upper_N_vichy_100_100_5lastyr[i,(t-(T-5))]=1}
	}
}


	Rmax_25_surv_actu=mean(upper_N_vichy_25_actu_5lastyr)
	Rmax_50_surv_actu=mean(upper_N_vichy_50_actu_5lastyr)
	Rmax_75_surv_actu=mean(upper_N_vichy_75_actu_5lastyr)
	Rmax_100_surv_actu=mean(upper_N_vichy_100_actu_5lastyr)
	
	Rmax_25_surv_50=mean(upper_N_vichy_25_50_5lastyr)
	Rmax_50_surv_50=mean(upper_N_vichy_50_50_5lastyr)
	Rmax_75_surv_50=mean(upper_N_vichy_75_50_5lastyr)
	Rmax_100_surv_50=mean(upper_N_vichy_100_50_5lastyr)
	
	Rmax_25_surv_100=mean(upper_N_vichy_25_100_5lastyr)
	Rmax_50_surv_100=mean(upper_N_vichy_50_100_5lastyr)
	Rmax_75_surv_100=mean(upper_N_vichy_75_100_5lastyr)
	Rmax_100_surv_100=mean(upper_N_vichy_100_100_5lastyr)

p_upper_N_vichy_5lastyr<-rbind(Rmax_25_surv_actu,
								Rmax_50_surv_actu,
								Rmax_75_surv_actu,
								Rmax_100_surv_actu,
								Rmax_25_surv_50,
								Rmax_50_surv_50,
								Rmax_75_surv_50,
								Rmax_100_surv_50,
								Rmax_25_surv_100,
								Rmax_50_surv_100,
								Rmax_75_surv_100,
								Rmax_100_surv_100
								)
p_upper_N_vichy_5lastyr<-as.data.frame(cbind(rownames(p_upper_N_vichy_5lastyr),as.vector(p_upper_N_vichy_5lastyr)),row.names=NULL)
colnames(p_upper_N_vichy_5lastyr)<-c("point_ref","proba")
p_upper_N_vichy_5lastyr<-p_upper_N_vichy_5lastyr[order(p_upper_N_vichy_5lastyr$"proba"),]
p_upper_N_vichy_5lastyr$proba<-as.numeric(as.character(p_upper_N_vichy_5lastyr$proba))

ggplot(data=p_upper_N_vichy_5lastyr, aes(x=point_ref, y=proba,fill=proba))+
		geom_bar(stat="identity")

levels(p_upper_N_vichy_5lastyr$point_ref)<-rownames(p_upper_N_vichy_5lastyr)
reorder(p_upper_N_vichy_5lastyr$point_ref,p_upper_N_vichy_5lastyr$proba)

#=======================
# GRAPH de TRAVAIL
#=======================
#------------------------
# Graph survie actuelle
#------------------------
x=seq(1,T,1)
p<-ggplot()+
	#geom_ribbon(aes(x,ymin=p_upper_N_vichy_25_actu,ymax=p_upper_N_vichy_75_actu), alpha=0.2, fill="red")+ #alpha = gère la transparence
	geom_line(aes(x,p_upper_N_vichy_25_actu,colour="1"),size=1)+
	geom_line(aes(x,p_upper_N_vichy_50_actu,colour="2"),size=1)+
	geom_line(aes(x,p_upper_N_vichy_75_actu,colour="3"),size=1)+
	geom_line(aes(x,p_upper_N_vichy_100_actu,colour="4"),size=1)+
	geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
	geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
	geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
	xlab(iconv("Année","UTF8")) + 
	ylab(iconv("Probabilité","UTF8"))+
	ggtitle("Condition de survie actuelle")+
	scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
	scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c("orange", "red", "red3","brown"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
	theme(legend.position ="top")+
	theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_actu_2019_07_04.png",width=800,height=800)
	print(p)
dev.off()

#------------------------------
# Graph 50% survie historique
#------------------------------
p<-ggplot()+
		geom_line(aes(x,p_upper_N_vichy_25_50,colour="1"),size=1)+
		geom_line(aes(x,p_upper_N_vichy_50_50,colour="2"),size=1)+
		geom_line(aes(x,p_upper_N_vichy_75_50,colour="3"),size=1)+
		geom_line(aes(x,p_upper_N_vichy_100_50,colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("50% de survie historique")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c( "chartreuse1", "palegreen","green2", "springgreen4"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_50_2019_07_04.png",width=800,height=800)
print(p)
dev.off()


#------------------------------
# Graph 100% survie historique
#------------------------------
p<-ggplot()+
		geom_line(aes(x,p_upper_N_vichy_25_100,colour="1"),size=1)+
		geom_line(aes(x,p_upper_N_vichy_50_100,colour="2"),size=1)+
		geom_line(aes(x,p_upper_N_vichy_75_100,colour="3"),size=1)+
		geom_line(aes(x,p_upper_N_vichy_100_100,colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("100% de survie historique")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c("cadetblue1", "deepskyblue", "royalblue1", "blue4"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_100_2019_07_04.png",width=800,height=800)
print(p)
dev.off()
#=======================
# GRAPH de PRESENTATION
#=======================
#-------------------------
#Moyenne mobile 5 ans
#-------------------------
library(igraph) #fonctionrunning.mean() qui calcule moyenne mobile (glissante) sur un pas de temps donné

#------------------------
# Graph survie actuelle
#------------------------
x_mob=seq(5,T,1)
p<-ggplot()+
		#geom_ribbon(aes(x,ymin=p_upper_N_vichy_25_actu,ymax=p_upper_N_vichy_75_actu), alpha=0.2, fill="red")+ #alpha = gère la transparence
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_25_actu,5),colour="1"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_50_actu,5),colour="2"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_75_actu,5),colour="3"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_100_actu,5),colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("Condition de survie actuelle")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c("orange", "red", "red3","brown"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_actu_moy_mob5_2019_07_04.png",width=800,height=800)
print(p)
dev.off()


#------------------------------
# Graph 50% survie historique
#------------------------------
p<-ggplot()+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_25_50,5),colour="1"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_50_50,5),colour="2"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_75_50,5),colour="3"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_100_50,5),colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("50% de survie historique")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c( "chartreuse1", "palegreen","green2", "springgreen4"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_50_moy_mob5_2019_07_04.png",width=800,height=800)
print(p)
dev.off()


#------------------------------
# Graph 100% survie historique
#------------------------------
p<-ggplot()+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_25_100,5),colour="1"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_50_100,5),colour="2"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_75_100,5),colour="3"),size=1)+
		geom_line(aes(x_mob,running.mean(p_upper_N_vichy_100_100,5),colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("100% de survie historique")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c("cadetblue1", "deepskyblue", "royalblue1", "blue4"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_100_moy_mob5_2019_07_04.png",width=800,height=800)
print(p)
dev.off()

#-----------------------------
# moyenne par classe de 5 ans
#-----------------------------
seq((T-floor(T/5)*5),T,5) #pour fixer le min on prend T-floor(T/5)*5) floor = arrondi au nombre le plus petit et 5 parce qu'on veut des classes de 5 ans

#On teste combien de groupe on peut faire sachant que le dernier doit avoir au moins 5 années
L<-ifelse(min(seq((T-floor(T/5)*5),T,5))<5,length(seq((T-floor(T/5)*5),T,5))-1,length(seq((T-floor(T/5)*5),T,5)))
p_upper_N_vichy_25_actu_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_50_actu_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_75_actu_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_100_actu_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_25_50_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_50_50_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_75_50_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_100_50_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_25_100_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_50_100_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_75_100_5yr=array(0,dim=c(L,1))
p_upper_N_vichy_100_100_5yr=array(0,dim=c(L,1))


j=ifelse((T-floor(T/5)*5)<5,(T-floor(T/5)*5)+1,(T-floor(T/5)*5)) #si T-floor(T/5)*5)<5 alors on commence à l'enregistrement d'après

for (i in 1:L){
	p_upper_N_vichy_25_actu_5yr[i]<-mean(p_upper_N_vichy_25_actu[j:(j+4)])
	p_upper_N_vichy_50_actu_5yr[i]<-mean(p_upper_N_vichy_50_actu[j:(j+4)])
	p_upper_N_vichy_75_actu_5yr[i]<-mean(p_upper_N_vichy_75_actu[j:(j+4)])
	p_upper_N_vichy_100_actu_5yr[i]<-mean(p_upper_N_vichy_100_actu[j:(j+4)])
	
	p_upper_N_vichy_25_50_5yr[i]<-mean(p_upper_N_vichy_25_50[j:(j+4)])
	p_upper_N_vichy_50_50_5yr[i]<-mean(p_upper_N_vichy_50_50[j:(j+4)])
	p_upper_N_vichy_75_50_5yr[i]<-mean(p_upper_N_vichy_75_50[j:(j+4)])
	p_upper_N_vichy_100_50_5yr[i]<-mean(p_upper_N_vichy_100_50[j:(j+4)])
	
	p_upper_N_vichy_25_100_5yr[i]<-mean(p_upper_N_vichy_25_100[j:(j+4)])
	p_upper_N_vichy_50_100_5yr[i]<-mean(p_upper_N_vichy_50_100[j:(j+4)])
	p_upper_N_vichy_75_100_5yr[i]<-mean(p_upper_N_vichy_75_100[j:(j+4)])
	p_upper_N_vichy_100_100_5yr[i]<-mean(p_upper_N_vichy_100_100[j:(j+4)])
	
	j=j+5
}

#------------------------
# Graph survie actuelle
#------------------------
x_5yr=seq(ifelse(min(seq((T-floor(T/5)*5),T,5))<5,seq((T-floor(T/5)*5),T,5)[2],seq((T-floor(T/5)*5),T,5)),T,5) #Si (T-floor(T/5)*5),T,5))<5 on commence au premier qui a au moins 5 années
		
p<-ggplot()+
		#geom_ribbon(aes(x,ymin=p_upper_N_vichy_25_actu,ymax=p_upper_N_vichy_75_actu), alpha=0.2, fill="red")+ #alpha = gère la transparence
		geom_line(aes(x_5yr,p_upper_N_vichy_25_actu_5yr,colour="1"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_50_actu_5yr,colour="2"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_75_actu_5yr,colour="3"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_100_actu_5yr,colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("Condition de survie actuelle")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c("orange", "red", "red3","brown"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_actu_5yr_2019_07_04.png",width=800,height=800)
print(p)
dev.off()

#------------------------------
# Graph 50% survie historique
#------------------------------
p<-ggplot()+
		geom_line(aes(x_5yr,p_upper_N_vichy_25_50_5yr,colour="1"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_50_50_5yr,colour="2"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_75_50_5yr,colour="3"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_100_50_5yr,colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("50% de survie historique")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c( "chartreuse1", "palegreen","green2", "springgreen4"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_50_5yr_2019_07_04.png",width=800,height=800)
print(p)
dev.off()


#------------------------------
# Graph 100% survie historique
#------------------------------
p<-ggplot()+
		geom_line(aes(x_5yr,p_upper_N_vichy_25_100_5yr,colour="1"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_50_100_5yr,colour="2"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_75_100_5yr,colour="3"),size=1)+
		geom_line(aes(x_5yr,p_upper_N_vichy_100_100_5yr,colour="4"),size=1)+
		geom_hline(yintercept = 0.5,linetype="dashed",size=2)+
		geom_hline(yintercept = 0.75, linetype="dashed", size=1)+
		geom_hline(yintercept = 0.25, linetype="dashed", size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité","UTF8"))+
		ggtitle("100% de survie historique")+
		scale_x_continuous(breaks=c(1,6,16,26,36,T), labels=c(1975,1980,1990,2000,2010,2016))+
		scale_colour_manual("", breaks = c("1", "2", "3", "4"),values = c("cadetblue1", "deepskyblue", "royalblue1", "blue4"),labels=c(">=25_Rmax", ">=50_Rmax", ">=75_Rmax", ">=100_Rmax")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Niveau_pop_surv_100_5yr_2019_07_04.png",width=800,height=800)
print(p)
dev.off()





length(which(p_upper_N_vichy_25_actu>=0.25))/T
length(which(p_upper_N_vichy_25_actu>=0.50))/T
length(which(p_upper_N_vichy_25_actu>=0.75))/T
length(which(p_upper_N_vichy_50_actu>=0.75))/T


#dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
#dmoy_tot_A=read.coda("dmoytot_ACODAchain1.txt","dmoytot_ACODAindex.txt",5001,10000)
#dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
#dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)
#
#juv_tot=array(rep(0,(5000*T)),dim=c(5000,T))#296000
#juv_tot_q=array(rep(0,T*5),dim=c(T,5))
#
#for (t in 2:12){
#	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1,t] + dmoy_tot_A[,t-1] * S_juv_JP[2,t] + dmoy_tot_L[,t-1] * S_juv_JP[3,t]
#}
#for (t in 13:T){
#	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1,t] + dmoy_tot_A[,t-1] * S_juv_JP[2,t] + dmoy_tot_L[,t-1] *S_juv_JP[3,t] + dmoy_tot_P[,t-12] * S_juv_JP[4,t]	
#}
#
#for (i in 1:T){
#	juv_tot_q[i,]=quantile(juv_tot[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
#}
#
##Moyenne des 5 dernières années
#mean_juv_tot<-mean(juv_tot[,(T-4):T])
#
#niveau_pop_actu<-0
#N_vichy_actu<-0
#
#for (i in 1:10000){
#	niveau_pop_actu[i]<-log(bugs_s_juv2ad[i]*mean(juv_tot[i,(T-4):T]))
#	N_vichy_actu[i]<-rlnorm(1,niveau_pop_actu[i],bugs_sigma_vichy[i])
#}
#
#	##Scénario 75% Rmax et survie actuelle
#	niveau_pop_75_actu[i]<- log(bugs_s_juv2ad[i]*Juv_Rmax_75[i])
#	N_vichy_75_actu[i]<-rlnorm(1,niveau_pop_75_actu[i],bugs_sigma_vichy[i])
