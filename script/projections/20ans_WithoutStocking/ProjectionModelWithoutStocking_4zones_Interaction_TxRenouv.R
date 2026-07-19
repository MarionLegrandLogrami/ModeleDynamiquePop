# PROJECTION MODELE WITHOUT STOCKING
# 
# Author: marion.legrand
###############################################################################

#Modèle 2017.08.29_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2016
setwd("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
#Modèle 2019.12.12_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2018
setwd("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2019_12_12/")
datawd<-("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2019_12_12/")
# surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-2))) #T-2 pour calculer sur T+27 année
# S_juv_JP<-matrix(surf,nrow=4)	

#Modèle 2021_09 - modèle 2019_12_12 avec MAJ des données jusqu'en 2020
setwd("D:/Documents/Workspace_eclipse/ModeleDynamiquePop/data/CODA/2021_09/")
datawd<-"D:/Documents/Workspace_eclipse/ModeleDynamiquePop/data/CODA/2021_09/"
imgwd<-"D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/"

#Modèle 2023_05 - modèle 2019_12_12 avec MAJ des données jusqu'en 2020 + ajout de l'influence des débits sur la survie et la répartition des adultes & modèle de mélange
setwd("D:/Documents/Workspace_eclipse/ModeleDynamiquePop/data/CODA/2023_05/2_Model_melange_homogenisation_Q_printemps/")
datawd<-"D:/Documents/Workspace_eclipse/ModeleDynamiquePop/data/CODA/2023_05/2_Model_melange_homogenisation_Q_printemps/"
imgwd<-"D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2023_05/2_Model_melange_homogenisation_Q_printemps/"

library(coda)
library(boot)
library(stringr)
T=48

source("D:/Documents/Workspace_eclipse/ModeleDynamiquePop/script/fonctions/fct_graph.R")
source("D:/Documents/Workspace_eclipse/ModeleDynamiquePop/script/fonctions/fct_data.R")

S_juv_JP<-keep_good_surf(surf_2021=TRUE)

#========================================================
# Récupération des paramètres dans le posterior joint
#========================================================
#bugs_rho_poutes=read.coda("rho_poutesCODAchain1.txt","rho_poutesCODAindex.txt")
bugs_ratio_juv_prod_L=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")
bugs_rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
bugs_adjust_p_A=read.coda("simulation/adjust_p_ACODAchain1.txt","simulation/adjust_p_ACODAindex.txt")
bugs_adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
bugs_adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")
bugs_sigma_p_alagnon=read.coda("sigma_p_alagnonCODAchain1.txt","sigma_p_alagnonCODAindex.txt")
bugs_sigma_p_langeac=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
bugs_sigma_p_poutes=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")
bugs_sigma_vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")
bugs_sigma_wild_moy=read.coda("parameters/sigma_wild_moyCODAchain1.txt","parameters/sigma_wild_moyCODAindex.txt")
#bugs_res_p_poutes=read.coda("simulation/res_p_poutesCODAchain1.txt","simulation/res_p_poutesCODAindex.txt")
#bugs_I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
#bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt")
bugs_s_juv2ad=read.coda("simulation/s_juv2adCODAchain1.txt","simulation/s_juv2adCODAindex.txt")
bugs_alpha_dd=read.coda("simulation/alpha_ddCODAchain1.txt","simulation/alpha_ddCODAindex.txt")
bugs_beta_dd=read.coda("simulation/beta_ddCODAchain1.txt","simulation/beta_ddCODAindex.txt")
bugs_nu_d=read.coda("nu_dCODAchain1.txt","nu_dCODAindex.txt")
bugs_res_wild_vichy=read.coda("simulation/res_wild_vichyCODAchain1.txt","simulation/res_wild_vichyCODAindex.txt")
bugs_res_wild_alagnon=read.coda("simulation/res_wild_alagnonCODAchain1.txt","simulation/res_wild_alagnonCODAindex.txt")
bugs_res_wild_langeac=read.coda("simulation/res_wild_langeacCODAchain1.txt","simulation/res_wild_langeacCODAindex.txt")
bugs_res_wild_poutes=read.coda("simulation/res_wild_poutesCODAchain1.txt","simulation/res_wild_poutesCODAindex.txt")
bugs_dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt")
bugs_dmoy_tot_A=read.coda("dmoytot_ACODAchain1.txt","dmoytot_ACODAindex.txt")
bugs_dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt")
bugs_dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt")
bugs_res_wild_vichy=read.coda("res_wild_VCODAchain1.txt","res_wild_VCODAindex.txt")
bugs_res_wild_alagnon=read.coda("res_wild_ACODAchain1.txt","res_wild_ACODAindex.txt")
bugs_res_wild_langeac=read.coda("res_wild_LCODAchain1.txt","res_wild_LCODAindex.txt")
bugs_res_wild_poutes=read.coda("res_wild_PCODAchain1.txt","res_wild_PCODAindex.txt")

#On choisit le nbr d'itération qu'on veut conserver
iteration=10000

#On met T+26 ou 25 pour pouvoir avoir à la fin un taux de renouvellement sur 20 ans (indicé t-5). Sur 20 ans en partant de T=42 +20 = 62 mais avec t-5 il faut finalement avoir t=67 (voire t=68 pour les t+1) pour calculer sur 20 ans
ratio_juv_prod_V=array(0,dim=c(iteration,T+26))
ratio_juv_prod_A=array(0,dim=c(iteration,T+26))
ratio_juv_prod_L=array(0,dim=c(iteration,T+26))
ratio_juv_prod_P=array(0,dim=c(iteration,T+26))
ratio_juv_A=array(0,dim=c(iteration,T+26))
ratio_juv_L=array(0,dim=c(iteration,T+26))
ratio_juv_P=array(0,dim=c(iteration,T+26))
L_ratio_juv_A=array(0,dim=c(iteration,T+26))
L_ratio_juv_L=array(0,dim=c(iteration,T+26))
L_ratio_juv_P=array(0,dim=c(iteration,T+26))
L_mu_Vichy_nm=array(0,dim=c(iteration,T+26))
L_mu_p_alagnon=array(0,dim=c(iteration,T+26))
L_mu_p_langeac=array(0,dim=c(iteration,T+26))
L_mu_p_poutes=array(0,dim=c(iteration,T+26))
L_mu_d_wild_vichy=array(0,dim=c(iteration,T+27))
L_mu_d_wild_alagnon=array(0,dim=c(iteration,T+27))
L_mu_d_wild_langeac=array(0,dim=c(iteration,T+27))
L_mu_d_wild_poutes=array(0,dim=c(iteration,T+27))
L_p_alagnon=array(0,dim=c(iteration,T+26))
L_p_langeac=array(0,dim=c(iteration,T+26))
L_p_poutes=array(0,dim=c(iteration,T+26))
L_d_moy_vichy=array(0,dim=c(iteration,T+27))
L_d_moy_alagnon=array(0,dim=c(iteration,T+27))
L_d_moy_langeac=array(0,dim=c(iteration,T+27))
L_d_moy_poutes=array(0,dim=c(iteration,T+27))
res_vichy=array(0,dim=c(iteration,T+26))
res_p_alagnon=array(0,dim=c(iteration,T+26))
res_p_langeac=array(0,dim=c(iteration,T+26))
res_p_poutes=array(0,dim=c(iteration,T+26))
p_alagnon=array(0,dim=c(iteration,T+26))
p_langeac=array(0,dim=c(iteration,T+26))
p_poutes=array(0,dim=c(iteration,T+26))
N_vichy=array(0,dim=c(iteration,T+26))
N_vichy_temp=array(0,dim=c(iteration,T+26))
N_alagnon=array(0,dim=c(iteration,T+26))
N_langeac=array(0,dim=c(iteration,T+26))
N_poutes=array(0,dim=c(iteration,T+26))
min_N_langeac=array(0,dim=c(iteration,T+26))
S_vichy=array(0,dim=c(iteration,T+26))
S_alagnon=array(0,dim=c(iteration,T+26))
S_langeac=array(0,dim=c(iteration,T+26))
S_poutes=array(0,dim=c(iteration,T+26))
d_moy_vichy=array(0,dim=c(iteration,T+27))
d_moy_alagnon=array(0,dim=c(iteration,T+27))
d_moy_langeac=array(0,dim=c(iteration,T+27))
d_moy_poutes=array(0,dim=c(iteration,T+27))
juv_vichy=array(0,dim=c(iteration,T+27))
juv_alagnon=array(0,dim=c(iteration,T+27))
juv_langeac=array(0,dim=c(iteration,T+27))
juv_poutes=array(0,dim=c(iteration,T+27))
juv_tot_vichy=array(0,dim=c(iteration,T+26))
juv_tot_alagnon=array(0,dim=c(iteration,T+26))
juv_tot_langeac=array(0,dim=c(iteration,T+26))
juv_tot_poutes=array(0,dim=c(iteration,T+26))
juv_tot_system=array(0,dim=c(iteration,T+26))
bugs_juv_vichy=array(0,dim=c(iteration,T+26))
bugs_juv_alagnon=array(0,dim=c(iteration,T+26))
bugs_juv_langeac=array(0,dim=c(iteration,T+26))
bugs_juv_poutes=array(0,dim=c(iteration,T+26))
bugs_juv_tot_vichy=array(0,dim=c(iteration,T+26))
bugs_juv_tot_alagnon=array(0,dim=c(iteration,T+26))
bugs_juv_tot_langeac=array(0,dim=c(iteration,T+26))
bugs_juv_tot_poutes=array(0,dim=c(iteration,T+26))
bugs_juv_tot_system=array(0,dim=c(iteration,T+26))
ratio_habitat=array(0,dim=c(4,T+26))

coef_juv_1<-array(rep(0,T*5000),dim=c(iteration,T+25))
coef_juv_2<-array(rep(0,T*5000),dim=c(iteration,T+25))
coef_juv_3<-array(rep(0,T*5000),dim=c(iteration,T+25))
renew_rate=array(0,dim=c(iteration,T+20))
renew_rate_q=array(0,dim=c((T+20),5))

for (t in 1:(T+26)){
	for (i in 1:4){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t])
	}
}

#Pour avoir les premiers juv_tot sans les recalculer car fout la zone dans R car en a besoin pour calculer la suite
#On commence à T-6 et non T-5 pour avoir les juv qu'il faut pour calculer les coef_juv de l'année T (il faut donc calculer de t-2 à t-6)
for(t in (T-5):T){
	for (i in 1:iteration){ 
		bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t]
		bugs_juv_alagnon[i,t]=bugs_dmoy_tot_A[i,t-1]*S_juv_JP[2,t]
		bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[3,t]
		bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[4,t]
	}
}	

#On démarre à T-1 pour pouvoir calculer les diagnostics de conservation
for (t in (T-1):(T+3)){
	for (i in 1:iteration){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_alagnon[i,t] = (1/3) * bugs_juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]
		bugs_juv_tot_system[i,t] = bugs_juv_tot_vichy[i,t] + bugs_juv_tot_alagnon[i,t] + bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]#*bugs_rho_poutes[i]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		#ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t]+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]

		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
			if (N_vichy[i,t]>15000) {N_vichy[i,t]=15000}#I(3,15000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
	
		p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
			if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_alagnon[i,t]=max(N_alagnon[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
			
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
				
		}
}


#Les juvéniles t-3 sont maintenant ceux calculés à la fin de la première boucle et non plus ceux issus directement d'openbugs
for (t in (T+4):(T+4)){
	for (i in 1:iteration){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5] 
		bugs_juv_tot_system[i,t] = bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]#*bugs_rho_poutes[i]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		#ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t]+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
			if (N_vichy[i,t]>30000) {N_vichy[i,t]=30000}#I(3,30000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
			if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
				
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_alagnon[i,t]=max(N_alagnon[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
		
	}
}

#Les juvéniles t-3 et t-4 sont maintenant ceux calculés à la fin des boucles précédentes et non plus ceux issus directement d'openbugs
for (t in (T+5):(T+5)){
	for (i in 1:iteration){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3] + (1/3) * juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5] 
		bugs_juv_tot_system[i,t] = bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]#*bugs_rho_poutes[i]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		#ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t]+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
			if (N_vichy[i,t]>30000) {N_vichy[i,t]=30000}#I(3,30000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
			if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_alagnon[i,t]=max(N_alagnon[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
		
	}
}

#Les juvéniles t-3 et t-4 t-5 sont maintenant ceux calculés à la fin des boucles précédentes et non plus ceux issus directement d'openbugs
#On va jusqu'à T+21 pour pouvoir calculer renew_rate sur 20 ans (sinon manque une année car on a besoin de faire N_vichy[T+21]
for (t in (T+6):(T+26)){
	for (i in 1:iteration){ 
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3] + (1/3) * juv_alagnon[i,t-4] + (1/3) * juv_alagnon[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5] 
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]#*bugs_rho_poutes[i]
		
		ratio_juv_prod_A[i,t] <- juv_tot_alagnon[i,t] / (juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		#ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t]+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
				
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
	
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])#*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
			if (N_vichy[i,t]>30000) {N_vichy[i,t]=30000}#I(3,30000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
			if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
				
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_alagnon[i,t]=max(N_alagnon[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
		
	}
}
#Script qui a tourné sur modèle 2021_09 (MAJ 2020)
save.image(file = "D:/Documents/Workspace_eclipse/ModeleDynamiquePop/2021_09_Projection_InteractionReciproqueMatriceVC_Maj2020_2023_08_21.RData") #avec 10 000 itérations 
#save.image(file = "D:/Documents/Workspace_eclipse/ModeleDynamiquePop/2021_09_Projection_InteractionReciproqueMatriceVC_Maj2020_2022_05_04.RData") #avec 10 000 itérations alors qu'avant on n'en faisait que 5000
#save.image(file = "D:/Documents/Workspace_eclipse/ModeleDynamiquePop/2021_09_Projection_InteractionReciproqueMatriceVC_Maj2020_2022_05_03.RData") #avec 10 000 itérations alors qu'avant on n'en faisait que 5000
#save.image(file = "D:/Documents/Workspace_eclipse/ModeleDynamiquePop/2021_09_Projection_InteractionReciproqueMatriceVC_Maj2020_2022_04_04.RData")
#save.image(file = "C:/Users/marion.legrand/workspace/ModeleDynamiquePop/2019_12_12_Projection_InteractionReciproqueMatriceVC_Maj2018_2020_01_03.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_2018_06_25.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_2017_12_12.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_07_19_ProjectionSansRepeuplementAlagnon_InteractionReciproque_Maj2016_2017_09_12.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_03_23_ProjectionSansRepeuplementAlagnon_InteractionReciproque_2017_04_25.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_12_19_ProjectionSansRepeuplementAlagnon_cor_juv_2016_12_20.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_12_19_ProjectionSansRepeuplementAlagnon_cor_juv.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_12_09_ProjectionSansRepeuplementAlagnon_cor_juv.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_12_09_ProjectionSansRepeuplementAlagnon.RData")

#Script qui a tourné sur modèle avec influence Q sur survie et répartition + Modèle mélange


#load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_12_09_ProjectionSansRepeuplementAlagnon.RData")
#load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_12_19_ProjectionSansRepeuplementAlagnon_cor_juv_2016_12_20.RData")
#load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_03_23_ProjectionSansRepeuplementAlagnon_InteractionReciproque_2017_04_25.RData")
#load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_07_19_ProjectionSansRepeuplementAlagnon_InteractionReciproque_Maj2016_2017_09_12.RData")
#load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_2017_12_12.RData")
#load(file = "C:/Users/marion.legrand/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_2018_06_25.RData")
#load(file = "C:/Users/marion.legrand/workspace/ModeleDynamiquePop/2019_12_12_Projection_InteractionReciproqueMatriceVC_Maj2018_2020_01_03.RData")
#load(file = "D:/Documents/Workspace_eclipse/ModeleDynamiquePop/2021_09_Projection_InteractionReciproqueMatriceVC_Maj2020_2022_05_03.RData")
load(file = "D:/Documents/Workspace_eclipse/ModeleDynamiquePop/2021_09_Projection_InteractionReciproqueMatriceVC_Maj2020_2022_05_04.RData") #avec 10 000 itérations alors qu'avant on n'en faisait que 5000

#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_11_03_ProjectionSansRepeuplementAlagnon.RData")

#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_01_20_ProjectionSansRepeuplement_2016.03.11.RData")
#load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015_01_24_ProjectionSansRepeuplement_2015.02.02.RData")
#
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015_12_02_PoutesCor_ProjectionSansRepeuplement_2015.12.04.RData")
#load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015_12_02_PoutesCor_ProjectionSansRepeuplement_2015.12.04.RData")
#
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015_12_04_Standard_ProjectionSansRepeuplement_2015.12.08.RData")
#load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015_12_04_Standard_ProjectionSansRepeuplement_2015.12.08.RData")


##On agglomère les 2 matrix juv_tot_ et bugs_juv_tot_ pour avoir un tableau complet sur la période
#juv_tot_vichy_tot=array(0,dim=c(5000,T+20))
#juv_tot_langeac_tot=array(0,dim=c(5000,T+20))
#juv_tot_poutes_tot=array(0,dim=c(5000,T+20))
#juv_tot_system_tot=array(0,dim=c(5000,T+20))
#
#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		juv_tot_vichy_tot[i,t]=juv_tot_vichy[i,t]+bugs_juv_tot_vichy[i,t]
#		juv_tot_langeac_tot[i,t]=juv_tot_langeac[i,t]+bugs_juv_tot_langeac[i,t]
#		juv_tot_poutes_tot[i,t]=juv_tot_poutes[i,t]+bugs_juv_tot_poutes[i,t]
#		juv_tot_system_tot[i,t]=juv_tot_system[i,t]+bugs_juv_tot_system[i,t]
#	}
#}

#=======================================
# Taux renouvellement population TOTALE
#=======================================
library(stringr)
bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.12.12.R")
source("data_4zones_Interaction_2017.12.12.R")
data_vichy<-N[,1]

##On fusionne les bugs_juv avec les juv
juv_vichy<-cbind(bugs_juv_vichy[,1:T],juv_vichy[,(T+1):(T+25)])
juv_alagnon<-cbind(bugs_juv_alagnon[,1:T],juv_alagnon[,(T+1):(T+25)])
juv_langeac<-cbind(bugs_juv_langeac[,1:T],juv_langeac[,(T+1):(T+25)])
juv_poutes<-cbind(bugs_juv_poutes[,1:T],juv_poutes[,(T+1):(T+25)])

for(t in T:(T+25)){
  for (i in 1:iteration){ 
    ###### Partie Taux Renouvellement ############
    #on calcule le coef pondérateur des juvéniles
    coef_juv_1[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])+(juv_vichy[i,t-5]+juv_alagnon[i,t-5]+juv_langeac[i,t-5]+juv_poutes[i,t-5])+(juv_vichy[i,t-6]+juv_alagnon[i,t-6]+juv_langeac[i,t-6]+juv_poutes[i,t-6]))
    coef_juv_2[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-3]+juv_alagnon[i,t-3]+juv_langeac[i,t-3]+juv_poutes[i,t-3])+(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])+(juv_vichy[i,t-5]+juv_alagnon[i,t-5]+juv_langeac[i,t-5]+juv_poutes[i,t-5]))
    coef_juv_3[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-2]+juv_alagnon[i,t-2]+juv_langeac[i,t-2]+juv_poutes[i,t-2])+(juv_vichy[i,t-3]+juv_alagnon[i,t-3]+juv_langeac[i,t-3]+juv_poutes[i,t-3])+(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4]))
  }
}


for(t in T:T){
  for (i in 1:5000){ 
    ###### Partie Taux Renouvellement ############
    #on calcule le coef pondérateur des juvéniles
    renew_rate[i,t-5]<-log((coef_juv_1[i,t]*data_vichy[t-1]+coef_juv_2[i,t]*data_vichy[t]+coef_juv_3[i,t]*N_vichy[i,t+1])/data_vichy[t-5])
  }
}
for(t in (T+1):(T+1)){
	for (i in 1:iteration){ 
		###### Partie Taux Renouvellement ############
		#on calcule le coef pondérateur des juvéniles
		renew_rate[i,t-5]<-log((coef_juv_1[i,t]*data_vichy[t-1]+coef_juv_2[i,t]*N_vichy[i,t]+coef_juv_3[i,t]*N_vichy[i,t+1])/data_vichy[t-5])
	}
}
for(t in (T+2):(T+5)){
	for (i in 1:iteration){ 
		###### Partie Taux Renouvellement ############
		#on calcule le coef pondérateur des juvéniles
		renew_rate[i,t-5]<-log((coef_juv_1[i,t]*N_vichy[i,t-1]+coef_juv_2[i,t]*N_vichy[i,t]+coef_juv_3[i,t]*N_vichy[i,t+1])/data_vichy[t-5])
	}
}
for(t in (T+6):(T+25)){
	for (i in 1:iteration){ 
		###### Partie Taux Renouvellement ############
		#on calcule le coef pondérateur des juvéniles
		renew_rate[i,t-5]<-log((coef_juv_1[i,t]*N_vichy[i,t-1]+coef_juv_2[i,t]*N_vichy[i,t]+coef_juv_3[i,t]*N_vichy[i,t+1])/N_vichy[i,t-5])
	}
}

# for (t in (T+1):(T+20)){
# 	renew_rate_q[t,]=quantile(renew_rate[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
# }

#Pour faire graph sur ensemble de la période il faut récupérer la partie rétrospective
load(file="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/renew_rate_coef_tot_2021_09.Rdata")

#On fusionne les jeux de données renew_rate et renew_rate_coef pour reconstituer toute la série
renew_rate_ws<-cbind(renew_rate_coef[,1:(T-6)],renew_rate[1:5000,(T-5):(T+20)])


####### Calcul sur le taux de renouvellement
exp(mean(rowMeans(renew_rate_ws[,(T+1):(T+20)]))) # donne résultat quasi-identique que moyenne sur les quantiles
exp(mean(rowMeans(renew_rate_ws[,(T+16):(T+20)]))) #0.74


#=======================
# Figure
#=======================
source("D:/Documents/Workspace_eclipse/ModeleDynamiquePop/script/fonctions/fct_graph.R")
draw_indic_tx_ren(tx_renouv=renew_rate_ws,simulation=TRUE,scenario="Arrêt des déversements",
                  typ_pop = "Totale",png=TRUE,pngName="TauxRenouvellementPopTotale_WS_2022_05_06")

##SANS enregistrer
draw_indic_tx_ren(tx_renouv=renew_rate_ws,simulation=TRUE,scenario="Arrêt des déversements",
                  typ_pop = "Totale",png=FALSE)
#=========================================
# Taux renouvellement population SAUVAGE
#=========================================

bugs_s_juv2ad=read.coda("s_juv2adCODAchain1.txt","s_juv2adCODAindex.txt")
bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt") 
bugs_I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
bugs_res_vichy=read.coda("simulation/res_vichyCODAchain1.txt","simulation/res_vichyCODAindex.txt",start=1,end=5000)
bugs_d_wild_moy_vichy=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt",start=1,end=5000)
bugs_d_wild_moy_alagnon=read.coda("dmoywild_ACODAchain1.txt","dmoywild_ACODAindex.txt",start=1,end=5000)
bugs_d_wild_moy_langeac=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt",start=1,end=5000)
bugs_d_wild_moy_poutes=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt",start=1,end=5000)
bugs_N_vichy=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")

juv_wild_vichy<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_alagnon<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_langeac<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_poutes<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_tot_V<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_tot_A<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_tot_L<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_tot_P<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
juv_wild_tot_system<-array(rep(0,(T+26)*5000),dim=c(5000,(T+26)))
N_wild_vichy<-array(rep(0,(T+25)*5000),dim=c(5000,(T+25)))
coef_juv_1<-array(rep(0,(T+25)*5000),dim=c(5000,(T+25)))
coef_juv_2<-array(rep(0,(T+25)*5000),dim=c(5000,(T+25)))
coef_juv_3<-array(rep(0,(T+25)*5000),dim=c(5000,(T+25)))
renew_rate_w=array(0,dim=c(5000,(T+20)))
renew_rate_w_q=array(NA,dim=c((T+20),5))		
renew_rate_w_coef=array(0,dim=c(5000,(T+20)))
renew_rate_w_coef_q=array(NA,dim=c((T+20),5))

#-----------------------------------------------------------------------------------------------------------------
# On fusionne les d_wild_moy (data jusqu'à t=T --> on met ncol()-1 pour aller jusqu'à l'avant dernière colonne) et les d_moy (data à partir de t=T+1) puisque sur le scénario
# Without Stocking on cesse tous les déversements. Du coup, pour ce scénario : d_moy=d_wild_moy à partir de T+1
# De même on fusionne les res_vichy et bugs_res_vichy
#-----------------------------------------------------------------------------------------------------------------

d_wild_moy_vichy<-cbind(bugs_d_wild_moy_vichy[,1:(ncol(bugs_d_wild_moy_vichy)-1)],d_moy_vichy[1:5000,(T+1):(T+27)])
d_wild_moy_alagnon<-cbind(bugs_d_wild_moy_alagnon[,1:(ncol(bugs_d_wild_moy_alagnon)-1)],d_moy_alagnon[1:5000,(T+1):(T+27)])
d_wild_moy_langeac<-cbind(bugs_d_wild_moy_langeac[,1:(ncol(bugs_d_wild_moy_langeac)-1)],d_moy_langeac[1:5000,(T+1):(T+27)])
d_wild_moy_poutes<-cbind(bugs_d_wild_moy_poutes[,1:(ncol(bugs_d_wild_moy_poutes)-1)],d_moy_poutes[1:5000,(T+1):(T+27)])

seq_name<-seq(2,(T+27))
colnames(d_wild_moy_vichy)<-seq_name
colnames(d_wild_moy_alagnon)<-seq_name
colnames(d_wild_moy_langeac)<-seq_name
colnames(d_wild_moy_poutes)<-seq_name[12:length(seq_name)]

res_vichy_bind<-cbind(bugs_res_vichy,res_vichy[1:5000,(T+1):(T+26)])
seq_name2<-seq(7,(T+26))
colnames(res_vichy_bind)<-seq_name2
#-------------------------------------------------------------------------
# on calcule les juvéniles sauvages de chaque année sur chaque secteur
#-------------------------------------------------------------------------

for (t in 1:(T+25)) {
  for (i in 1:5000) {
    juv_wild_vichy[i,t+1]=d_wild_moy_vichy[i,t]*S_juv_JP[1,t+1] #d_wild_moy commence à 2 d'où l'indice t et non t+1
    juv_wild_alagnon[i,t+1]=d_wild_moy_alagnon[i,t]*S_juv_JP[2,t+1] #d_wild_moy commence à 2 d'où l'indice t et non t+1
    juv_wild_langeac[i,t+1]=d_wild_moy_langeac[i,t]*S_juv_JP[3,t+1] #d_wild_moy commence à 2 d'où l'indice t et non t+1
  }
}

for (t in 12:(T+25)){
  for (i in 1:5000){
    juv_wild_poutes[i,t+1]=d_wild_moy_poutes[i,t-11]*S_juv_JP[4,t+1] #d_wild_moy commence à 13 d'où l'indice t et non t+1
    
  }
}

#-----------------------------------------------------------------------
# On calcule les juvéniles sauvages à l'origine des retours d'adultes
#-----------------------------------------------------------------------

for (t in 7:(T+25)) {
  for (i in 1:5000) {
    juv_wild_tot_V[i,t] <- (1/3) * juv_wild_vichy[i,t-3] + (1/3) * juv_wild_vichy[i,t-4] + (1/3) * juv_wild_vichy[i,t-5]
    juv_wild_tot_A[i,t] <- (1/3) * juv_wild_alagnon[i,t-3] + (1/3) * juv_wild_alagnon[i,t-4] + (1/3) * juv_wild_alagnon[i,t-5] 
    juv_wild_tot_L[i,t] <- (1/3) * juv_wild_langeac[i,t-3] + (1/3) * juv_wild_langeac[i,t-4] + (1/3) * juv_wild_langeac[i,t-5] 
  }
}

for (t in 16:16) {
  for (i in 1:5000) {
    juv_wild_tot_P[i,t] <- (1/3) * juv_wild_poutes[i,t-3] 
  }
}	
for (t in 17:17) {
  for (i in 1:5000) {
    juv_wild_tot_P[i,t] <- (1/3) * juv_wild_poutes[i,t-3] + (1/3) * juv_wild_poutes[i,t-4]
  }
}	
for (t in 18:(T+25)) {
  for (i in 1:5000) {
    juv_wild_tot_P[i,t] <- (1/3) * juv_wild_poutes[i,t-3] + (1/3) * juv_wild_poutes[i,t-4] + (1/3) * juv_wild_poutes[i,t-5]
  }
}




for (t in 7:15) {
  for (i in 1:5000) {
    juv_wild_tot_system[i,t] <- juv_wild_tot_V[i,t]+ juv_wild_tot_A[i,t] +juv_wild_tot_L[i,t] 
  }
}
for (t in 16:(T+25)) {
  for (i in 1:5000) {
    juv_wild_tot_system[i,t] <- juv_wild_tot_V[i,t]+ juv_wild_tot_A[i,t] +juv_wild_tot_L[i,t]  + juv_wild_tot_P[i,t]
  }
}	


#------------------------------------------
# On calcule les adultes sauvages à Vichy
#------------------------------------------
for (t in 7:7) {
  for (i in 1:5000) {
    N_wild_vichy[i,t]<-exp(log(juv_wild_tot_system[i,t])+log(bugs_s_juv2ad[i])+ bugs_level_s[i] * 1+res_vichy_bind[i,t-6])
  }
}

for (t in 8:T) {
  for (i in 1:5000) {
    N_wild_vichy[i,t]<-exp(log(juv_wild_tot_system[i,t])+log(bugs_s_juv2ad[i])+ bugs_level_s[i] * bugs_I_surv[i,t-7]+res_vichy_bind[i,t-6])
  }
}
#Pour les projections, on remplace bugs_I_surv par 0 puisque de toute façon depuis t=21 le paramètre est à 0
for (t in (T+1):(T+25)) {
  for (i in 1:5000) {
    N_wild_vichy[i,t]<-exp(log(juv_wild_tot_system[i,t])+log(bugs_s_juv2ad[i])+ bugs_level_s[i] * 0 +res_vichy_bind[i,t-6])
  }
}

#On fait les quantiles de N_wild_vichy
N_wild_vichy_q<-array(NA,dim=c((T+20),5))
for (t in 1:(T+20)) {
  N_wild_vichy_q[t,]=quantile(N_wild_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#-----------------------------------------------
# On calcule le coef pondérateur des juvéniles
#-----------------------------------------------

for (t in 7:(T+25)) {
  for (i in 1:5000) {
    coef_juv_1[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4]) / ((juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])+(juv_wild_vichy[i,t-5]+juv_wild_alagnon[i,t-5]+juv_wild_langeac[i,t-5]+juv_wild_poutes[i,t-5])+(juv_wild_vichy[i,t-6]+juv_wild_alagnon[i,t-6]+juv_wild_langeac[i,t-6]+juv_wild_poutes[i,t-6]))
    
    coef_juv_2[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/((juv_wild_vichy[i,t-3]+juv_wild_alagnon[i,t-3]+juv_wild_langeac[i,t-3]+juv_wild_poutes[i,t-3])+(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])+(juv_wild_vichy[i,t-5]+juv_wild_alagnon[i,t-5]+juv_wild_langeac[i,t-5]+juv_wild_poutes[i,t-5]))
    
    coef_juv_3[i,t]<-(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4])/((juv_wild_vichy[i,t-2]+juv_wild_alagnon[i,t-2]+juv_wild_langeac[i,t-2]+juv_wild_poutes[i,t-2])+(juv_wild_vichy[i,t-3]+juv_wild_alagnon[i,t-3]+juv_wild_langeac[i,t-3]+juv_wild_poutes[i,t-3])+(juv_wild_vichy[i,t-4]+juv_wild_alagnon[i,t-4]+juv_wild_langeac[i,t-4]+juv_wild_poutes[i,t-4]))
    
  }
}

#-----------------------------------------------
# On calcule le tx de renouvellement sauvage
#-----------------------------------------------

#bugs2jags(str_c(datawd,"data.txt"),"data_2021.09.R")
source("data_2021.09.R")
data_vichy<-N[,1]

for (t in 7:27){
  for (i in 1:5000){
    renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/bugs_N_vichy[i,t-5])
  }
}
for (t in 28:34){
  for (i in 1:5000){
    renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/data_vichy[t-5])
  }
}
for (t in 35:35){
  for (i in 1:5000){
    renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/bugs_N_vichy[i,t-12])
  }
}
for (t in 36:(T+5)){
  for (i in 1:5000){
    renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/data_vichy[t-5])
    
  }
}

#Ca revient bien au même d'utiliser N_wild_vichy ou N_vichy
# for (t in (T+6):(T+20)){
#   for (i in 1:5000){
#     renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_wild_vichy[i,t-1]+coef_juv_2[i,t]*N_wild_vichy[i,t]+coef_juv_3[i,t]*N_wild_vichy[i,t+1])/N_vichy[i,t-5]) #N_wild_vichy[i,t-5])  #
# 
#   }
# }

for (t in (T+6):(T+25)){
  for (i in 1:5000){
    renew_rate_w_coef[i,t-5]<-log((coef_juv_1[i,t]*N_vichy[i,t-1]+coef_juv_2[i,t]*N_vichy[i,t]+coef_juv_3[i,t]*N_vichy[i,t+1])/N_vichy[i,t-5])  #N_vichy[i,t-5])
    
  }
}



for (t in 7:(T+20)) {
  renew_rate_w_coef_q[t,]=quantile(renew_rate_w_coef[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#pour dessiner le graph on fait appel à la fonction du script "D:/Documents/Workspace_eclipse/ModeleDynamiquePopscript/fonction/fct_graph.R"
source("D:/Documents/Workspace_eclipse/ModeleDynamiquePop/script/fonctions/fct_graph.R")
draw_indic_tx_ren(tx_renouv=renew_rate_w_coef,simulation=TRUE,scenario="Arrêt des déversements",png=TRUE,pngName = "TauxRenouvellementPopSauvage_WS_2022_05_06")

###SANS enregistrer
draw_indic_tx_ren(tx_renouv=renew_rate_w_coef,simulation=TRUE,scenario="Arrêt des déversements",png=FALSE)





# #======================
# # echelle log (pop totale)
# #======================
# png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TauxRenouvellementPopSauvage_ScenarioWithoutStocking.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioWithoutStocking.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)
# par("mar"=c(7.1, 5.1, 4.1, 2.1))
# plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab="Years",ylim=c(-2,2),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)
# 
# # trace l'axe des ordonnées
# axis(2,at = seq(-2,2,0.5),labels=seq(-2,2,0.5),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# # trace l'axe des abscisses
# axis(1,at = c((T+1),(T+10),(T+20)),
# 		labels=c((1975+T),(1975+T+9),(1975+T+19)),
# 		cex.axis = 1.2,las = 1,lwd=2,col = "black")
# 
# for(i in (T+1):(T+20)){
# 	#whiskers
# 	#95%
# 	segments(i-0.15,renew_rate_q[i,5],i+0.15,renew_rate_q[i,5])
# 	segments(i,renew_rate_q[i,4],i,renew_rate_q[i,5])
# 	#5%
# 	segments(i-0.15,renew_rate_q[i,1],i+0.15,renew_rate_q[i,1])
# 	segments(i,renew_rate_q[i,2],i,renew_rate_q[i,1])
# 	#boxplot
# 	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_q[i,2],renew_rate_q[i,2],renew_rate_q[i,4],renew_rate_q[i,4]),col="light grey")
# 	#median
# 	segments(i-0.3,renew_rate_q[i,3],i+0.3,renew_rate_q[i,3])
# }
# abline(h=0,col="red")
# mtext(1,text=iconv("Scenario : Projection sans déversement à 20 ans","UTF8"),line=5)
# dev.off()
# 
# 
# #======================
# # echelle naturelle
# #======================
#---------------------------------------------------------------------------------
# Graph revu pour correction des années (en affichant toute la série de données)
# Série entière !!
#---------------------------------------------------------------------------------
 png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TauxRenouvellementPopSauvage_ScenarioWithoutStocking_EchelleNat_serieEntiere_2022_07_01.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioWithoutStocking_EchelleNat.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)
# par("mar"=c(7.1, 5.1, 4.1, 2.1))
plot(1,1,type="n",axes=FALSE,xlim=c(6.5,T+20-6),xlab="Année",ylim=c(0,2),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)
#,xlim=c(T+0.5,T+20)
# trace l'axe des ordonnées
axis(2,at = seq(0,2,0.25),labels=seq(0,2,0.25),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
#axis(1,at = c((T+1),(T+10),(T+20)),
#labels=c((1975+T-6),(1975+T-6+9),(1975+T-6+19)),
axis(1,at = c(7,16,26,(T-6),(T+4),(T+20-6)),
     labels=c((1975+6),1990,2000,(1975+T-7),(1975+T-6+9),(1975+T-6+19)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in 7:(T+20-6)){ #(T+1)
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
#mtext(40,text="Scenario : Projection sans déversement à 20 ans",line=5)
dev.off()

exp(mean(renew_rate_w_coef[,(T+20-6-4):(T+20-6)])) #0.71


png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TauxRenouvellementPopSauvage_ScenarioWithoutStocking_EchelleNat_2022_07_01.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioWithoutStocking_EchelleNat.png",width=1000,height=800)
# #png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)
# par("mar"=c(7.1, 5.1, 4.1, 2.1))
plot(1,1,type="n",axes=FALSE,xlim=c(T-6+0.5,T+20-6),xlab="Année",ylim=c(0,2),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)

# trace l'axe des ordonnées
axis(2,at = seq(0,2,0.25),labels=seq(0,2,0.25),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
#
#
axis(1,at = c((T-5),(T+10-6),(T+20-6)),
     labels=c((1975+T-6),(1975+T-6+9),(1975+T-6+19)),
     cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in (T+1-6):(T+20-6)){ #
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
#mtext(40,text="Scenario : Projection sans déversement à 20 ans",line=5)
dev.off()

exp(mean(renew_rate_w_coef[,(T+20-6-4):(T+20-6)])) #0.71

# 
# 
# ##### Graph Tx renouvellement moyen des médianes par tranche de 5 ans #####
# 
# #Calcul du taux de renouvellement en échelle classique (1= renouvellement)
# renew_rate_q_5yr=array(0,dim=c((20/5),1))
# j=T+1
# 
# for (i in 1:(20/5)){
# 	renew_rate_q_5yr[i]<-exp(mean(renew_rate_q[j:(j+4),3]))
# 	j=j+5
# }
# rownames(renew_rate_q_5yr)<-c(str_c("",1975+T,"-",1975+T+4,""),str_c("",1975+T+5,"-",1975+T+9,""),str_c("",1975+T+10,"-",1975+T+14,""),str_c("",1975+T+15,"-",1975+T+19,""))
# 
# png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioWithoutStocking_5yrs.png",width=1000,height=800)
# par("mar"=c(10.1,5.1,3.1,1.1))
# plot(renew_rate_q_5yr,axes=FALSE,ylim=c(0,ifelse(max(renew_rate_q_5yr)<1,1,max(renew_rate_q_5yr)+0.1)),xlab="",ylab="Taux renouvellement",main=iconv("Taux de renouvellement de la population sauvage (moyenne des médianes sur 5 ans)","UTF8"),cex.lab=1.5)
# axis(1,at = seq(1,(20/5),1),
# 		labels=rownames(renew_rate_q_5yr),
# 		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
# axis(2,at = seq(0,ifelse(max(renew_rate_q_5yr)<1,1,max(renew_rate_q_5yr)),0.2),labels=seq(0,ifelse(max(renew_rate_q_5yr)<1,1,max(renew_rate_q_5yr)),0.2),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# 
# 
# for (s in 1:(20/5)){
# 	segments(s,renew_rate_q_5yr[s],(s+1),renew_rate_q_5yr[s+1])
# }
# abline(h=1,col="red")
# mtext(1,text=iconv("Scenario : Projection sans déversement à 20 ans","UTF8"),line=8)
# dev.off()
# 
# #Calcul du taux de renouvellement en échelle log (0=renouvellement)
# L_renew_rate_q_5yr=array(0,dim=c((20/5),1))
# j=T+1
# 
# for (i in 1:(20/5)){
# 	L_renew_rate_q_5yr[i]<-mean(renew_rate_q[j:(j+4),3])
# 	j=j+5
# }
# rownames(L_renew_rate_q_5yr)<-c(str_c("",1975+T,"-",1975+T+4,""),str_c("",1975+T+5,"-",1975+T+9,""),str_c("",1975+T+10,"-",1975+T+14,""),str_c("",1975+T+15,"-",1975+T+19,""))
# 
# #on créée une fonction pour arrondir le min(L_renew_rate_q_5yr) à la décimal du dessous la plus proche. ex min(L_renew_rate_q_5yr)=-0.43 et on veut arrondir à -0.5
# floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
# 
# png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioWithoutStocking_5yrs_log.png",width=1000,height=800)
# par("mar"=c(10.1,5.1,3.1,1.1))
# plot(L_renew_rate_q_5yr,axes=FALSE,xlab="",ylim=c(floor_dec(min(L_renew_rate_q_5yr),1),ifelse(max(L_renew_rate_q_5yr)<0,0.1,max(L_renew_rate_q_5yr))),ylab=iconv("Taux renouvellement (échelle log)","UTF8"),main=iconv("Taux de renouvellement de la population sauvage (log de la moyenne des médianes sur 5 ans)","UTF8"),cex.lab=1.5)
# axis(1,at = seq(1,(20/5),1),
# 		labels=rownames(L_renew_rate_q_5yr),
# 		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
# axis(2,at = seq(floor_dec(min(L_renew_rate_q_5yr),1),ifelse(max(L_renew_rate_q_5yr)<0,0.1,max(L_renew_rate_q_5yr)),0.2),labels=round(seq(floor_dec(min(L_renew_rate_q_5yr),1),ifelse(max(L_renew_rate_q_5yr)<0,0.1,max(L_renew_rate_q_5yr)),0.2),1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# for (s in 1:length(seq(7,(T-6),5))){
# 	segments(s,L_renew_rate_q_5yr[s],(s+1),L_renew_rate_q_5yr[s+1])
# }
# abline(h=0,col="red")
# mtext(1,text=iconv("Scenario : Projection sans déversement à 20 ans","UTF8"),line=8)
# dev.off()




#============================
# PROJECTION SUR LES 20 ANS 
#============================
#==========================================
# CHAP : Figure : TotalReturns_proj20years
#==========================================

# Graph projection 20 years
bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 année de suivi station (soit T+20 - 15). Ne change rien car à part sur ces 23 années sinon on a tjrs un eff exhaustifs indiqué dans data_vichy

#Attention à l'année 30 estimation des passages Vichy car année jugée incomplète
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	N_vichy_real_q[(t+7),]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#N_vichy_proj_q=array(0,dim=c(43,5))
N_vichy_proj_q=array(0,dim=c(T+20,5))
for (t in (T+1):(T+20)){
	N_vichy_proj_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#------------------
# Graph
#------------------
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/TotalReturns_proj20years_2015_02_02.png",width=800,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2016_01_20_Standard_thin200/TotalReturns_Devalaison_s_juv2ad_new_proj20years_2016_03_15.png",width=800,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2016_01_20_Standard_thin200/TotalReturns_proj20years_2016_03_15.png",width=800,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2016_12_06_Alagnon/TotalReturns_proj20years_2016_12_06.png",width=800,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_03_23_4zones_Interaction/TotalReturns_proj20years_2017_04_27.png",width=800,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_07_19_4zones_Interaction_ss_rho_poutes/TotalReturns_proj20years_2017_09_12.png",width=800,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TotalReturns_proj20years_2017_12_12.png",width=800,height=800
#png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TotalReturns_proj20years_2022_04_04.png",width=800,height=800)
png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TotalReturns_proj20years_2022_05_09.png",width=800,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab=iconv("années","UTF-8","LATIN1"),ylim=c(0,9000),ylab=iconv("Retours à Vichy","UTF-8","LATIN1"),main=iconv("Projection à 20 ans sans repeuplement","UTF-8","LATIN1"),cex.lab=1.2)

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T,1974+T+10,1974+T+20)
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=lab1,
		cex.axis = 1,las = 1,lwd=2,col = "black")


for(i in 3:30){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_real_q[i,5],i+0.15,N_vichy_real_q[i,5])
	segments(i,N_vichy_real_q[i,4],i,N_vichy_real_q[i,5])
	#5%
	segments(i-0.15,N_vichy_real_q[i,1],i+0.15,N_vichy_real_q[i,1])
	segments(i,N_vichy_real_q[i,2],i,N_vichy_real_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i,2],N_vichy_real_q[i,2],N_vichy_real_q[i,4],N_vichy_real_q[i,4]),col="light grey")
	#median
	segments(i-0.3,N_vichy_real_q[i,3],i+0.3,N_vichy_real_q[i,3])
}

library(stringr)
bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.07.19.R")
source("data_4zones_Interaction_2017.07.19.R")
data_vichy<-N[,1]

points(x=seq(23,T,1),data_vichy[23:T],pch=16)

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_proj_q[i,5],i+0.15,N_vichy_proj_q[i,5])
	segments(i,N_vichy_proj_q[i,4],i,N_vichy_proj_q[i,5])
	#5%
	segments(i-0.15,N_vichy_proj_q[i,1],i+0.15,N_vichy_proj_q[i,1])
	segments(i,N_vichy_proj_q[i,2],i,N_vichy_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_proj_q[i,2],N_vichy_proj_q[i,2],N_vichy_proj_q[i,4],N_vichy_proj_q[i,4]),col="orange")
	#median
	segments(i-0.3,N_vichy_proj_q[i,3],i+0.3,N_vichy_proj_q[i,3])
}


dev.off()


#mean(N_vichy_proj_q[(T+20-4):(T+20),3]) --> ne plus faire comme ça c'est faux
mean(N_vichy[,(T+20-4):(T+20)]) #182 en moyenne sur les 5 dernières années de la période de projection


#=======================================================
# Graph projection adultes Vichy pour classement IUCN
#=======================================================
png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TotalReturns_proj20years_classementUICN_2023_01_18.png",width=800,height=600)

plot(1,1,type="n",axes=FALSE,xlim=c(22.5,T+20+0.5),xlab="Years",ylim=c(0,2000),ylab="Returns Vichy",main="Adults returns at Vichy and 20 years projection without stocking",cex.lab=1.5)

# trace l'axe des ordonnées
axis(2,at = seq(0,2000,500),labels=seq(0,2000,500),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
lab1=c(1997,2000,2010,1974+T,1974+T+10,1974+T+20)
axis(1,at = c(23,26,36,T,T+10,T+20),
     labels=lab1,
     cex.axis = 1.2,las = 1,lwd=2,col = "black")


# for(i in 3:30){
#   #whiskers
#   #95%
#   segments(i-0.15,N_vichy_real_q[i,5],i+0.15,N_vichy_real_q[i,5])
#   segments(i,N_vichy_real_q[i,4],i,N_vichy_real_q[i,5])
#   #5%
#   segments(i-0.15,N_vichy_real_q[i,1],i+0.15,N_vichy_real_q[i,1])
#   segments(i,N_vichy_real_q[i,2],i,N_vichy_real_q[i,1])
#   #boxplot
#   polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i,2],N_vichy_real_q[i,2],N_vichy_real_q[i,4],N_vichy_real_q[i,4]),col="light grey")
#   #median
#   segments(i-0.3,N_vichy_real_q[i,3],i+0.3,N_vichy_real_q[i,3])
# }

bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.07.19.R")
source("data_4zones_Interaction_2017.07.19.R")
data_vichy<-N[,1]

points(x=seq(23,T,1),data_vichy[23:T],pch=16)

for(i in (T+1):(T+20)){
  #whiskers
  #95%
  segments(i-0.15,N_vichy_proj_q[i,5],i+0.15,N_vichy_proj_q[i,5])
  segments(i,N_vichy_proj_q[i,4],i,N_vichy_proj_q[i,5])
  #5%
  segments(i-0.15,N_vichy_proj_q[i,1],i+0.15,N_vichy_proj_q[i,1])
  segments(i,N_vichy_proj_q[i,2],i,N_vichy_proj_q[i,1])
  #boxplot
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_proj_q[i,2],N_vichy_proj_q[i,2],N_vichy_proj_q[i,4],N_vichy_proj_q[i,4]),col="orange")
  #median
  segments(i-0.3,N_vichy_proj_q[i,3],i+0.3,N_vichy_proj_q[i,3])
}
moy=mean(data_vichy[(T-9):T]) #moyenne des retours observés à Vichy les 10 dernières années
lines(x=seq((T-9),T,1),y =rep(moy,10),col = "red", lty = 2,lwd=2)
rug(c((T-9),T), ticksize = 0.04, side = 1, lwd = 2, col = "red")

dev.off()



#===============================
# Graph sans projection
#===============================
png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/TotalReturns_2022_04_27.png",width=800,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Années",ylim=c(0,9000),ylab="Retours à Vichy",cex.lab=1.2)

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,T),
     labels=lab1,
     cex.axis = 1,las = 1,lwd=2,col = "black")


for(i in 3:30){
  #whiskers
  #95%
  segments(i-0.15,N_vichy_real_q[i,5],i+0.15,N_vichy_real_q[i,5])
  segments(i,N_vichy_real_q[i,4],i,N_vichy_real_q[i,5])
  #5%
  segments(i-0.15,N_vichy_real_q[i,1],i+0.15,N_vichy_real_q[i,1])
  segments(i,N_vichy_real_q[i,2],i,N_vichy_real_q[i,1])
  #boxplot
  polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i,2],N_vichy_real_q[i,2],N_vichy_real_q[i,4],N_vichy_real_q[i,4]),col="light grey")
  #median
  segments(i-0.3,N_vichy_real_q[i,3],i+0.3,N_vichy_real_q[i,3])
}

library(stringr)
bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.07.19.R")
source("data_4zones_Interaction_2017.07.19.R")
data_vichy<-N[,1]

points(x=seq(23,T,1),data_vichy[23:T],pch=16)


dev.off()

##==============================================
## Elements par rapport à critères liste rouge
##==============================================
# Dans liste rouge critère A2,A3 et A4 pour la réduction de la taille de la pop sur 3 générations=15 ans
# VU : >=30% (réduction de la taille de la pop) / EN : >=50% / CR : >=80%

load(file = "D:/Documents/Workspace_eclipse/ModeleDynamiquePop/2021_09_Projection_InteractionReciproqueMatriceVC_Maj2020_2022_05_03.RData") #avec 10 000 itérations alors qu'avant on n'en faisait que 5000

source("data_4zones_Interaction_2017.07.19.R")
data_vichy<-N[,1]

moy11_20<-round(mean(data_vichy[(T-9):T]),0) #475
#On compare avec projection 15 ans plus tard
proj_sat<-N_vichy[,(T+15)]
median(N_vichy[,(T+15)]) #144
mean(N_vichy[,(T+15)]) 
median(N_vichy[,(T+15)])/N[T,1]
ratio_vichy =array(NA,dim=c(length(proj_sat),1))
#On compare avec projection 10 ans plus tard
median(N_vichy[,(T+10)])/N[T,1]


for (i in 1:length(proj_sat)){
  ratio_vichy[i]<-1-(proj_sat[i]/moy11_20)
}

upper_30_ratio_vichy=array(NA,dim=c(length(proj_sat),1))
upper_50_ratio_vichy=array(NA,dim=c(length(proj_sat),1))
upper_80_ratio_vichy=array(NA,dim=c(length(proj_sat),1))


for (i in 1:length(proj_sat)){
  if(ratio_vichy[i] >=0.30){upper_30_ratio_vichy[i]=1}else{upper_30_ratio_vichy[i]=0} 
  if(ratio_vichy[i] >=0.50){upper_50_ratio_vichy[i]=1}else{upper_50_ratio_vichy[i]=0} 
  if(ratio_vichy[i] >=0.80){upper_80_ratio_vichy[i]=1}else{upper_80_ratio_vichy[i]=0} 
}


mean(upper_30_ratio_vichy) #85%
mean(upper_50_ratio_vichy) #72%
mean(upper_80_ratio_vichy) #28%

## Critère C1 déclin d'au moins 20% en 2 générations (=10 ans)
proj_sat_10<-N_vichy[,(T+10)]
ratio_vichy_10 =array(NA,dim=c(length(proj_sat_10),1))


for (i in 1:length(proj_sat_10)){
  ratio_vichy[i]<-1-(proj_sat_10[i]/moy11_20)
}

upper_20_ratio_vichy_10=array(NA,dim=c(length(proj_sat_10),1))

for (i in 1:length(proj_sat)){
  if(ratio_vichy[i] >=0.20){upper_20_ratio_vichy_10[i]=1}else{upper_20_ratio_vichy_10[i]=0} 
}

mean(upper_20_ratio_vichy_10) #82%


#==================================
# CHAP : Figure : Threshold
#==================================
#bugs_N_vichy=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")	

iteration=5000

under_10_vichy=array(NA,dim=c(iteration,20))
under_50_vichy=array(NA,dim=c(iteration,20))
under_100_vichy=array(NA,dim=c(iteration,20))
under_250_vichy=array(NA,dim=c(iteration,20))
under_500_vichy=array(NA,dim=c(iteration,20))


for (t in (T+1):(T+20)){
	
	for (i in 1:iteration){
		if(N_vichy[i,t] < 10){under_10_vichy[i,t-T]=1}else{under_10_vichy[i,t-T]=0} 
	  
		if(N_vichy[i,t] < 50){under_50_vichy[i,t-T]=1} else{under_50_vichy[i,t-T]=0}
	  
		if(N_vichy[i,t] < 100){under_100_vichy[i,t-T]=1}else{under_100_vichy[i,t-T]=0}
	  
		if(N_vichy[i,t] < 250){under_250_vichy[i,t-T]=1}else{under_250_vichy[i,t-T]=0}
	  
		if(N_vichy[i,t] < 500){under_500_vichy[i,t-T]=1}else{under_500_vichy[i,t-T]=0}
	}
}


p_under_10_vichy=rep(NA,20)
p_under_50_vichy=rep(NA,20)
p_under_100_vichy=rep(NA,20)
p_under_250_vichy=rep(NA,20)
p_under_500_vichy=rep(NA,20)


for (t in 1:20){
	p_under_10_vichy[t]=mean(under_10_vichy[,t])
	p_under_50_vichy[t]=mean(under_50_vichy[,t])
	p_under_100_vichy[t]=mean(under_100_vichy[,t])
	p_under_250_vichy[t]=mean(under_250_vichy[,t])
	p_under_500_vichy[t]=mean(under_500_vichy[,t])
	
}

#------------------------
# Graph
#------------------------
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/Threshold_2015_02_02.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2016_01_20_Standard_thin200/Threshold_Devalaison_s_juv2ad_new_2016_03_15.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2016_01_20_Standard_thin200/Threshold_2016_03_15.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2016_12_06_Alagnon/Threshold_2016_12_09.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_03_23_4zones_Interaction/Threshold_2017_04_27.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_07_19_4zones_Interaction_ss_rho_poutes/Threshold_2017_09_12.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/Threshold_2017_12_12.png",width=800,height=800)
png(filename="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/Simulation/2021_09/Threshold_2022_05_04.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab=iconv("années","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
lab2=c(1975+T,1975+T+9,1975+T+19)
axis(1,at = c(1,10,20),
		labels=lab2,
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,20,1)



#points(x,p_under_10_vichy,col="grey85",pch=16)
#segments(x[1:19],p_under_10_vichy[1:19],x[2:20],p_under_10_vichy[2:20],col="grey85")
#
points(x,p_under_50_vichy,col="grey75",pch=16)
segments(x[1:19],p_under_50_vichy[1:19],x[2:20],p_under_50_vichy[2:20],col="grey75")

points(x,p_under_100_vichy,col="grey65",pch=16)
segments(x[1:19],p_under_100_vichy[1:19],x[2:20],p_under_100_vichy[2:20],col="grey65")

points(x,p_under_250_vichy,col="grey55",pch=16)
segments(x[1:19],p_under_250_vichy[1:19],x[2:20],p_under_250_vichy[2:20],col="grey55")

points(x,p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],p_under_500_vichy[1:19],x[2:20],p_under_500_vichy[2:20],col="grey45")


legend("topleft",legend=c(expression(p^seuils < 500),expression(p^seuils < 250),expression(p^seuils < 100),expression(p^seuils < 50)),#,expression(p^seuils < 10)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey55","grey65","grey75"), #,"grey75","grey85"),
		bty="n")
	#	horiz = TRUE)


dev.off()


mean(p_under_500_vichy[16:20]) #0.95104
mean(p_under_250_vichy[16:20]) #0.7916
mean(p_under_100_vichy[16:20]) #0.38176
mean(p_under_50_vichy[16:20]) #0.12676

#=========================================================================
# CHAP : Figure : répartitions des juvéniles dans les différents secteurs
#=========================================================================
#Modèle 2017.03.23_4zones_Interaction - interaction réciproque juv sauvage/déversé
#setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_03_23_4zones_Interaction/")
#
#library(coda)
#library(boot)
#T=41
#
#surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
#S_juv_JP<-matrix(surf,nrow=4)	

#----------------------------------
# PARTIE SERIE TEMPORELLE

dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_A=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)

juv_tot=array(rep(0,5000*T),dim=c(5000,T))#296000
juv_tot_q=array(rep(0,T*5),dim=c(T,5))

for (t in 2:12){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[t,1] + dmoy_tot_A[,t-1] * S_juv_JP[t,2] + dmoy_tot_L[,t-1] * S_juv_JP[t,3]
}
for (t in 13:T){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[t,1] + dmoy_tot_A[,t-1] * S_juv_JP[t,2] + dmoy_tot_L[,t-1] *S_juv_JP[t,3] + dmoy_tot_P[,t-12] * S_juv_JP[t,4]	
}

for (i in 1:T){
	juv_tot_q[i,]=quantile(juv_tot[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}


##Par secteurs calcul des ratios
juv_V=array(rep(0,5000*T),dim=c(5000,T))
juv_A=array(rep(0,5000*T),dim=c(5000,T))
juv_L=array(rep(0,5000*T),dim=c(5000,T))
juv_P=array(rep(0,5000*T),dim=c(5000,T))

ratio_juv_V=array(0,c(5000,T))
ratio_juv_A=array(0,c(5000,T))
ratio_juv_L=array(0,c(5000,T))
ratio_juv_P=array(0,c(5000,T))

ratio_juv_V_q=array(rep(0,T*5),dim=c(T,5))
ratio_juv_A_q=array(rep(0,T*5),dim=c(T,5))
ratio_juv_L_q=array(rep(0,T*5),dim=c(T,5))
ratio_juv_P_q=array(rep(0,T*5),dim=c(T,5))


for (t in 2:12){
	juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[t,1]
	juv_A[,t]<-dmoy_tot_A[,t-1] * S_juv_JP[t,2]
	juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[t,3]
}

for (t in 13:T){
	juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[t,1]
	juv_A[,t]<-dmoy_tot_A[,t-1] * S_juv_JP[t,2]
	juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[t,3]
	juv_P[,t]<-dmoy_tot_P[,t-12] * S_juv_JP[t,4]
}

for (t in 2:T){
	ratio_juv_V[,t]=juv_V[,t]/juv_tot[,t]
	ratio_juv_A[,t]=juv_A[,t]/juv_tot[,t]
	ratio_juv_L[,t]=juv_L[,t]/juv_tot[,t]
	ratio_juv_P[,t]=juv_P[,t]/juv_tot[,t]
}

for (t in 1:T){
	ratio_juv_V_q[t,]=quantile(ratio_juv_V[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_A_q[t,]=quantile(ratio_juv_A[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_L_q[t,]=quantile(ratio_juv_L[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_P_q[t,]=quantile(ratio_juv_P[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

#----------------------------------
# PARTIE PROJECTION

#Récupération des caluls réalisés dans le cadre de la modélisation sans déversement sur le Modèle standard
#load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_03_23_ProjectionSansRepeuplementAlagnon_InteractionReciproque_2017_04_25.RData")

#les juv_vichy,alagnon,langeac,poutes du load ne commencent qu'à t=43 (T+2) on recréée le t=42 (T+1)
for (t in (T+1):(T+1)){
	juv_vichy[,t]<-bugs_dmoy_tot_V[5001:10000,t-1] * S_juv_JP[t,1]
	juv_alagnon[,t]<-bugs_dmoy_tot_A[5001:10000,t-1] * S_juv_JP[t,2]
	juv_langeac[,t]<-bugs_dmoy_tot_L[5001:10000,t-1] * S_juv_JP[t,3]
	juv_poutes[,t]<-bugs_dmoy_tot_P[5001:10000,t-12] * S_juv_JP[t,4]
}

#on calcul un juv_total qui somme l'ensemble
juv_total<-array(rep(0,5000*(T+20)),dim=c(5000,T+20))
juv_total_q<-array(rep(0,(T+20)*5),dim=c(T+20,5))
ratio_juv_vichy<-array(0,c(5000,T+20))
ratio_juv_alagnon<-array(0,c(5000,T+20))
ratio_juv_langeac<-array(0,c(5000,T+20))
ratio_juv_poutes<-array(0,c(5000,T+20))
ratio_juv_vichy_q<-array(rep(0,(T+20)*5),dim=c(T+20,5))
ratio_juv_alagnon_q<-array(rep(0,(T+20)*5),dim=c(T+20,5))
ratio_juv_langeac_q<-array(rep(0,(T+20)*5),dim=c(T+20,5))
ratio_juv_poutes_q<-array(rep(0,(T+20)*5),dim=c(T+20,5))


for (t in (T+1):(T+20)){
	juv_total[,t]<-juv_vichy[,t] + juv_alagnon[,t] + juv_langeac[,t] + juv_poutes[,t]
}

for (i in (T+1):(T+20)){
	juv_total_q[i,]=quantile(juv_total[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}


for (t in (T+1):(T+20)){
	ratio_juv_vichy[,t]=juv_vichy[,t]/juv_total[,t]
	ratio_juv_alagnon[,t]=juv_alagnon[,t]/juv_total[,t]
	ratio_juv_langeac[,t]=juv_langeac[,t]/juv_total[,t]
	ratio_juv_poutes[,t]=juv_poutes[,t]/juv_total[,t]
}

for (t in (T+1):(T+20)){
	ratio_juv_vichy_q[t,]=quantile(ratio_juv_vichy[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_alagnon_q[t,]=quantile(ratio_juv_alagnon[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_langeac_q[t,]=quantile(ratio_juv_langeac[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_poutes_q[t,]=quantile(ratio_juv_poutes[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}


#-------------------------------------------
# FIGURE

#png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2017_07_19_4zones_Interaction_ss_rho_poutes/RepartitionSpatialeJuv_WithoutStocking_2017_04_28.png",width=800, height=1500, units = "px",type="cairo")
#png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/RepartitionSpatialeJuv_WithoutStocking_2017_12_12.png",width=800, height=1500, units = "px",type="cairo")
png(file="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/simulation/2021_09/RepartitionSpatialeJuv_WithoutStocking_2022_04_04.png",width=800, height=1500, units = "px",type="cairo")

par(mfrow=c(5,1))

#----------------------------
#Juv totaux tous secteurs

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,400000),ylab="",main="total 0+ juvenile production (wild + stocked)")

# trace l'axe des ordonnées
axis(2,at = c(0,100000,200000,300000,400000),
		labels=c(
				expression(0),
				expression(paste(100," x ", 10^3)),
				expression(paste(200," x ", 10^3)),
				expression(paste(300," x ", 10^3)),
				expression(paste(400," x ", 10^3))),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

##Partie série temporelle
for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,juv_tot_q[i,5],i+0.15,juv_tot_q[i,5])
	segments(i,juv_tot_q[i,4],i,juv_tot_q[i,5])
	#5%
	segments(i-0.15,juv_tot_q[i,1],i+0.15,juv_tot_q[i,1])
	segments(i,juv_tot_q[i,2],i,juv_tot_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_tot_q[i,2],juv_tot_q[i,2],juv_tot_q[i,4],juv_tot_q[i,4]),col="grey85")
	#median
	segments(i-0.3,juv_tot_q[i,3],i+0.3,juv_tot_q[i,3])
}

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,juv_total_q[i,5],i+0.15,juv_total_q[i,5])
	segments(i,juv_total_q[i,4],i,juv_total_q[i,5])
	#5%
	segments(i-0.15,juv_total_q[i,1],i+0.15,juv_total_q[i,1])
	segments(i,juv_total_q[i,2],i,juv_total_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_total_q[i,2],juv_total_q[i,2],juv_total_q[i,4],juv_total_q[i,4]),col="orange")
	#median
	segments(i-0.3,juv_total_q[i,3],i+0.3,juv_total_q[i,3])
}

#-------------------------
#Vichy juv totaux ratio

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Vichy (total 0+ juvenile production)")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

##Partie série temporelle

for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_V_q[i,5],i+0.15,ratio_juv_V_q[i,5])
	segments(i,ratio_juv_V_q[i,4],i,ratio_juv_V_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_V_q[i,1],i+0.15,ratio_juv_V_q[i,1])
	segments(i,ratio_juv_V_q[i,2],i,ratio_juv_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_V_q[i,2],ratio_juv_V_q[i,2],ratio_juv_V_q[i,4],ratio_juv_V_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_juv_V_q[i,3],i+0.3,ratio_juv_V_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_vichy_q[i,5],i+0.15,ratio_juv_vichy_q[i,5])
	segments(i,ratio_juv_vichy_q[i,4],i,ratio_juv_vichy_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_vichy_q[i,1],i+0.15,ratio_juv_vichy_q[i,1])
	segments(i,ratio_juv_vichy_q[i,2],i,ratio_juv_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_vichy_q[i,2],ratio_juv_vichy_q[i,2],ratio_juv_vichy_q[i,4],ratio_juv_vichy_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_vichy_q[i,3],i+0.3,ratio_juv_vichy_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

#---------------------------
#Alagnon juv totaux ratio

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Alagnon (total 0+ juvenile production)")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

##Partie série temporelle
for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_A_q[i,5],i+0.15,ratio_juv_A_q[i,5])
	segments(i,ratio_juv_A_q[i,4],i,ratio_juv_A_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_A_q[i,1],i+0.15,ratio_juv_A_q[i,1])
	segments(i,ratio_juv_A_q[i,2],i,ratio_juv_A_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_A_q[i,2],ratio_juv_A_q[i,2],ratio_juv_A_q[i,4],ratio_juv_A_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_juv_A_q[i,3],i+0.3,ratio_juv_A_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_alagnon_q[i,5],i+0.15,ratio_juv_alagnon_q[i,5])
	segments(i,ratio_juv_alagnon_q[i,4],i,ratio_juv_alagnon_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_alagnon_q[i,1],i+0.15,ratio_juv_alagnon_q[i,1])
	segments(i,ratio_juv_alagnon_q[i,2],i,ratio_juv_alagnon_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_alagnon_q[i,2],ratio_juv_alagnon_q[i,2],ratio_juv_alagnon_q[i,4],ratio_juv_alagnon_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_alagnon_q[i,3],i+0.3,ratio_juv_alagnon_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

#------------------------------
#Langeac juv totaux ratio

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Langeac (total 0+ juvenile production)")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")


##Partie série temporelle
for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_L_q[i,5],i+0.15,ratio_juv_L_q[i,5])
	segments(i,ratio_juv_L_q[i,4],i,ratio_juv_L_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_L_q[i,1],i+0.15,ratio_juv_L_q[i,1])
	segments(i,ratio_juv_L_q[i,2],i,ratio_juv_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_L_q[i,2],ratio_juv_L_q[i,2],ratio_juv_L_q[i,4],ratio_juv_L_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_juv_L_q[i,3],i+0.3,ratio_juv_L_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_langeac_q[i,5],i+0.15,ratio_juv_langeac_q[i,5])
	segments(i,ratio_juv_langeac_q[i,4],i,ratio_juv_langeac_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_langeac_q[i,1],i+0.15,ratio_juv_langeac_q[i,1])
	segments(i,ratio_juv_langeac_q[i,2],i,ratio_juv_langeac_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_langeac_q[i,2],ratio_juv_langeac_q[i,2],ratio_juv_langeac_q[i,4],ratio_juv_langeac_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_langeac_q[i,3],i+0.3,ratio_juv_langeac_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

#-----------------------------
#Poutes juv totaux ratio

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Poutes (total 0+ juvenile production)")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

##Partie série temporelle
for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_P_q[i,5],i+0.15,ratio_juv_P_q[i,5])
	segments(i,ratio_juv_P_q[i,4],i,ratio_juv_P_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_P_q[i,1],i+0.15,ratio_juv_P_q[i,1])
	segments(i,ratio_juv_P_q[i,2],i,ratio_juv_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_P_q[i,2],ratio_juv_P_q[i,2],ratio_juv_P_q[i,4],ratio_juv_P_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_juv_P_q[i,3],i+0.3,ratio_juv_P_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_poutes_q[i,5],i+0.15,ratio_juv_poutes_q[i,5])
	segments(i,ratio_juv_poutes_q[i,4],i,ratio_juv_poutes_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_poutes_q[i,1],i+0.15,ratio_juv_poutes_q[i,1])
	segments(i,ratio_juv_poutes_q[i,2],i,ratio_juv_poutes_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_poutes_q[i,2],ratio_juv_poutes_q[i,2],ratio_juv_poutes_q[i,4],ratio_juv_poutes_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_poutes_q[i,3],i+0.3,ratio_juv_poutes_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

dev.off()

#=========================================================================
# CHAP : Figure : répartitions des Géniteurs dans les différents secteurs
#=========================================================================
#Modèle 2017.03.23_4zones_Interaction - interaction réciproque juv sauvage/déversé
#setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_03_23_4zones_Interaction/")
#
#library(coda)
#library(boot)
#T=41
#
#surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
#S_juv_JP<-matrix(surf,nrow=4)	

#----------------------------------
# PARTIE SERIE TEMPORELLE

S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_alagnon_real=read.coda("S_alagnonCODAchain1.txt","S_alagnonCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")

#On récupère les données du Modèle directement dans le fichier data
#library(coda) 
#require(stringr)
#bugs2jags(str_c("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_03_23_4zones_Interaction/","data.txt"),"data_4zones_Interaction.R")
#source("data_4zones_Interaction.R")
#head(N) #ïça marche !

S_poutes_counter=N[,4]

ratio_S_V=array(0,c(5000,T))
ratio_S_A=array(0,c(5000,T))
ratio_S_L=array(0,c(5000,T))
ratio_S_P=array(0,c(5000,T))

for (t in 1:11){
	
	for(i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t])
		ratio_S_A[i,t] = S_alagnon_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t])
		ratio_S_L[i,t] = S_langeac_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t])
	}
}

for (t in 12:T){
	for( i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_A[i,t] = S_alagnon_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_L[i,t] = S_langeac_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		
	}
}
for (t in 12:T){
	for( i in 1:5000){
		ratio_S_P[i,t] = 1 - ratio_S_V[i,t] - ratio_S_A[i,t] - ratio_S_L[i,t]
	}
}


ratio_S_V_q=array(0,c(T,5))
ratio_S_A_q=array(0,c(T,5))
ratio_S_L_q=array(0,c(T,5))
ratio_S_P_q=array(0,c(T,5))

for (t in 1:T){
	ratio_S_V_q[t,]=quantile(ratio_S_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_A_q[t,]=quantile(ratio_S_A[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_L_q[t,]=quantile(ratio_S_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_P_q[t,]=quantile(ratio_S_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


p_reach_V=read.coda("p_reach_VCODAchain1.txt","p_reach_VCODAindex.txt")
C_dwn_reach=array(0,c(5000,T))
tot_C=array(0,c(5000,T))


for (t in 1:T){
	for (i in 1:5000){
		C_dwn_reach[i,t] <- p_reach_V[i] * C_dwn[t]
		tot_C[i,t] <-round(C_dwn_reach[i,t] + C_up[t])
	}
}

S_tot=array(0,c(5000,T))
S_tot_data=array(0,T)
S_tot_q=array(0,c(T,5))

N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")

for (t in 1:22){
	for(i in 1:5000){
		S_tot[i,t]=max(N_vichy_real[i,t]-S_stocking[t]-tot_C[i,t],1)
	}
}
for (t in 23:23){
	for(i in 1:5000){
		S_tot[i,t+7]=max(N_vichy_real[i,t]-S_stocking[t]-tot_C[i,t],1)
	}
}
data_vichy=N[,1]

for (t in 24:T){
	S_tot_data[t]=max(data_vichy[t]-S_stocking[t],1)
}

for (t in 1:T){
	S_tot_q[t,]=quantile(S_tot[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


#----------------------------------
# PARTIE PROJECTION

#Récupération des caluls réalisés dans le cadre de la modélisation sans déversement sur le Modèle standard
#load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_03_23_ProjectionSansRepeuplementAlagnon_InteractionReciproque_2017_04_25.RData")

N_vichy_q<-array(0,c(T+20,5))

for (t in (T+1):(T+20)){
	N_vichy_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

ratio_S_vichy=array(0,c(5000,T+20))
ratio_S_alagnon=array(0,c(5000,T+20))
ratio_S_langeac=array(0,c(5000,T+20))
ratio_S_poutes=array(0,c(5000,T+20))

for (t in (T+1):(T+20)){
	
	for(i in 1:5000){
		ratio_S_vichy[i,t] = S_vichy[i,t] / (S_vichy[i,t] + S_alagnon[i,t] + S_langeac[i,t] + S_poutes[i,t])
		ratio_S_alagnon[i,t] = S_alagnon[i,t] / (S_vichy[i,t] + S_alagnon[i,t] + S_langeac[i,t] + S_poutes[i,t])
		ratio_S_langeac[i,t] = S_langeac[i,t] / (S_vichy[i,t] + S_alagnon[i,t] + S_langeac[i,t] + S_poutes[i,t])
		ratio_S_poutes[i,t] = S_poutes[i,t] / (S_vichy[i,t] + S_alagnon[i,t] + S_langeac[i,t] + S_poutes[i,t]) 
	}
}

ratio_S_vichy_q=array(0,c(T+20,5))
ratio_S_alagnon_q=array(0,c(T+20,5))
ratio_S_langeac_q=array(0,c(T+20,5))
ratio_S_poutes_q=array(0,c(T+20,5))

for (t in (T+1):(T+20)){
	ratio_S_vichy_q[t,]=quantile(ratio_S_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_alagnon_q[t,]=quantile(ratio_S_alagnon[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_langeac_q[t,]=quantile(ratio_S_langeac[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_poutes_q[t,]=quantile(ratio_S_poutes[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


#---------------------------------------------------
# FIGURE

#png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2017_07_19_4zones_Interaction_ss_rho_poutes/RepartitionSpatialeGen_WithoutStocking_2017_04_28.png",width=800, height=1500, units = "px",type="cairo")
#png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/RepartitionSpatialeGen_WithoutStocking_2017_12_12.png",width=800, height=1500, units = "px",type="cairo")
png(file="D:/Documents/Workspace_eclipse/ModeleDynamiquePop/img/simulation/2021_09/RepartitionSpatialeGen_WithoutStocking_2022_04_04.png",width=800, height=1500, units = "px",type="cairo")

par(mfrow=c(5,1))
#..........
# Nb total
#..........
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,4000),ylab="",main="Total number of spawners")

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000),labels=c(0,1000,2000,3000,4000),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie série temporelle
for(i in 3:22){
	#whiskers
	#95%
	segments(i-0.15,S_tot_q[i,5],i+0.15,S_tot_q[i,5])
	segments(i,S_tot_q[i,4],i,S_tot_q[i,5])
	#5%
	segments(i-0.15,S_tot_q[i,1],i+0.15,S_tot_q[i,1])
	segments(i,S_tot_q[i,2],i,S_tot_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_tot_q[i,2],S_tot_q[i,2],S_tot_q[i,4],S_tot_q[i,4]),col="grey85")
	#median
	segments(i-0.3,S_tot_q[i,3],i+0.3,S_tot_q[i,3])
}
for(i in 30:30){
	#whiskers
	#95%
	segments(i-0.15,S_tot_q[i,5],i+0.15,S_tot_q[i,5])
	segments(i,S_tot_q[i,4],i,S_tot_q[i,5])
	#5%
	segments(i-0.15,S_tot_q[i,1],i+0.15,S_tot_q[i,1])
	segments(i,S_tot_q[i,2],i,S_tot_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_tot_q[i,2],S_tot_q[i,2],S_tot_q[i,4],S_tot_q[i,4]),col="grey85")
	#median
	segments(i-0.3,S_tot_q[i,3],i+0.3,S_tot_q[i,3])
}

points(x=seq(23,T,1),S_tot_data[23:T],pch=16)

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_q[i,5],i+0.15,N_vichy_q[i,5])
	segments(i,N_vichy_q[i,4],i,N_vichy_q[i,5])
	#5%
	segments(i-0.15,N_vichy_q[i,1],i+0.15,N_vichy_q[i,1])
	segments(i,N_vichy_q[i,2],i,N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_q[i,2],N_vichy_q[i,2],N_vichy_q[i,4],N_vichy_q[i,4]),col="orange")
	#median
	segments(i-0.3,N_vichy_q[i,3],i+0.3,N_vichy_q[i,3])
}

#........
# Vichy
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Vichy-Langeac")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")


##Partie série temporelle
for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_V_q[i,5],i+0.15,ratio_S_V_q[i,5])
	segments(i,ratio_S_V_q[i,4],i,ratio_S_V_q[i,5])
	#5%
	segments(i-0.15,ratio_S_V_q[i,1],i+0.15,ratio_S_V_q[i,1])
	segments(i,ratio_S_V_q[i,2],i,ratio_S_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_V_q[i,2],ratio_S_V_q[i,2],ratio_S_V_q[i,4],ratio_S_V_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_S_V_q[i,3],i+0.3,ratio_S_V_q[i,3])
}


abline(h=1,lty=3)

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_vichy_q[i,5],i+0.15,ratio_S_vichy_q[i,5])
	segments(i,ratio_S_vichy_q[i,4],i,ratio_S_vichy_q[i,5])
	#5%
	segments(i-0.15,ratio_S_vichy_q[i,1],i+0.15,ratio_S_vichy_q[i,1])
	segments(i,ratio_S_vichy_q[i,2],i,ratio_S_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_vichy_q[i,2],ratio_S_vichy_q[i,2],ratio_S_vichy_q[i,4],ratio_S_vichy_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_vichy_q[i,3],i+0.3,ratio_S_vichy_q[i,3])
}


abline(h=1,lty=3)

#........
# Alagnon
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Alagnon")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie série temporelle
for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_A_q[i,5],i+0.15,ratio_S_A_q[i,5])
	segments(i,ratio_S_A_q[i,4],i,ratio_S_A_q[i,5])
	#5%
	segments(i-0.15,ratio_S_A_q[i,1],i+0.15,ratio_S_A_q[i,1])
	segments(i,ratio_S_A_q[i,2],i,ratio_S_A_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_A_q[i,2],ratio_S_A_q[i,2],ratio_S_A_q[i,4],ratio_S_A_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_S_A_q[i,3],i+0.3,ratio_S_A_q[i,3])
}


abline(h=1,lty=3)
##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_alagnon_q[i,5],i+0.15,ratio_S_alagnon_q[i,5])
	segments(i,ratio_S_alagnon_q[i,4],i,ratio_S_alagnon_q[i,5])
	#5%
	segments(i-0.15,ratio_S_alagnon_q[i,1],i+0.15,ratio_S_alagnon_q[i,1])
	segments(i,ratio_S_alagnon_q[i,2],i,ratio_S_alagnon_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_alagnon_q[i,2],ratio_S_alagnon_q[i,2],ratio_S_alagnon_q[i,4],ratio_S_alagnon_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_alagnon_q[i,3],i+0.3,ratio_S_alagnon_q[i,3])
}


abline(h=1,lty=3)

#........
# Langeac
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Langeac-Poutes")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie série temporelle
for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_L_q[i,5],i+0.15,ratio_S_L_q[i,5])
	segments(i,ratio_S_L_q[i,4],i,ratio_S_L_q[i,5])
	#5%
	segments(i-0.15,ratio_S_L_q[i,1],i+0.15,ratio_S_L_q[i,1])
	segments(i,ratio_S_L_q[i,2],i,ratio_S_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_q[i,2],ratio_S_L_q[i,2],ratio_S_L_q[i,4],ratio_S_L_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_S_L_q[i,3],i+0.3,ratio_S_L_q[i,3])
}


abline(h=1,lty=3)

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_langeac_q[i,5],i+0.15,ratio_S_langeac_q[i,5])
	segments(i,ratio_S_langeac_q[i,4],i,ratio_S_langeac_q[i,5])
	#5%
	segments(i-0.15,ratio_S_langeac_q[i,1],i+0.15,ratio_S_langeac_q[i,1])
	segments(i,ratio_S_langeac_q[i,2],i,ratio_S_langeac_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_langeac_q[i,2],ratio_S_langeac_q[i,2],ratio_S_langeac_q[i,4],ratio_S_langeac_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_langeac_q[i,3],i+0.3,ratio_S_langeac_q[i,3])
}


abline(h=1,lty=3)

#........
# Poutes
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Amont Poutes")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie série temporelle

for(i in 12:22){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_P_q[i,5],i+0.15,ratio_S_P_q[i,5])
	segments(i,ratio_S_P_q[i,4],i,ratio_S_P_q[i,5])
	#5%
	segments(i-0.15,ratio_S_P_q[i,1],i+0.15,ratio_S_P_q[i,1])
	segments(i,ratio_S_P_q[i,2],i,ratio_S_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_P_q[i,2],ratio_S_P_q[i,2],ratio_S_P_q[i,4],ratio_S_P_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_S_P_q[i,3],i+0.3,ratio_S_P_q[i,3])
}


abline(h=1,lty=3)

points(x=seq(23,T,1),ratio_S_P_q[23:T,3],pch=16)

##Partie projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_poutes_q[i,5],i+0.15,ratio_S_poutes_q[i,5])
	segments(i,ratio_S_poutes_q[i,4],i,ratio_S_poutes_q[i,5])
	#5%
	segments(i-0.15,ratio_S_poutes_q[i,1],i+0.15,ratio_S_poutes_q[i,1])
	segments(i,ratio_S_poutes_q[i,2],i,ratio_S_poutes_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_poutes_q[i,2],ratio_S_poutes_q[i,2],ratio_S_poutes_q[i,4],ratio_S_poutes_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_poutes_q[i,3],i+0.3,ratio_S_poutes_q[i,3])
}


abline(h=1,lty=3)


dev.off()





##########################################################################################




####################################################################################################################
# Test avec l'injection du gain obtenu dans la simulation à la dévalaison avec et sans amélioration  à la dévalaison
####################################################################################################################

load(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_01_20_s_juv2ad_new_2016_03_14.RData")

ratio_juv_prod_V=array(0,dim=c(5000,T+20))
ratio_juv_prod_L=array(0,dim=c(5000,T+20))
ratio_juv_prod_P=array(0,dim=c(5000,T+20))
ratio_juv_L=array(0,dim=c(5000,T+20))
ratio_juv_P=array(0,dim=c(5000,T+20))
L_ratio_juv_L=array(0,dim=c(5000,T+20))
L_ratio_juv_P=array(0,dim=c(5000,T+20))
L_mu_Vichy_nm=array(0,dim=c(5000,T+20))
L_mu_p_langeac=array(0,dim=c(5000,T+20))
L_mu_p_poutes=array(0,dim=c(5000,T+20))
L_mu_d_wild_vichy=array(0,dim=c(5000,T+21))
L_mu_d_wild_langeac=array(0,dim=c(5000,T+21))
L_mu_d_wild_poutes=array(0,dim=c(5000,T+21))
L_p_langeac=array(0,dim=c(5000,T+20))
L_p_poutes=array(0,dim=c(5000,T+20))
L_d_moy_vichy=array(0,dim=c(5000,T+21))
L_d_moy_langeac=array(0,dim=c(5000,T+21))
L_d_moy_poutes=array(0,dim=c(5000,T+21))
res_vichy=array(0,dim=c(5000,T+20))
res_p_langeac=array(0,dim=c(5000,T+20))
res_p_poutes=array(0,dim=c(5000,T+20))
p_langeac=array(0,dim=c(5000,T+20))
p_poutes=array(0,dim=c(5000,T+20))
N_vichy=array(0,dim=c(5000,T+20))
N_vichy_temp=array(0,dim=c(5000,T+20))
N_langeac=array(0,dim=c(5000,T+20))
N_poutes=array(0,dim=c(5000,T+20))
min_N_langeac=array(0,dim=c(5000,T+20))
S_vichy=array(0,dim=c(5000,T+20))
S_langeac=array(0,dim=c(5000,T+20))
S_poutes=array(0,dim=c(5000,T+20))
d_moy_vichy=array(0,dim=c(5000,T+21))
d_moy_langeac=array(0,dim=c(5000,T+21))
d_moy_poutes=array(0,dim=c(5000,T+21))
juv_vichy=array(0,dim=c(5000,T+21))
juv_langeac=array(0,dim=c(5000,T+21))
juv_poutes=array(0,dim=c(5000,T+21))
juv_tot_vichy=array(0,dim=c(5000,T+20))
juv_tot_langeac=array(0,dim=c(5000,T+20))
juv_tot_poutes=array(0,dim=c(5000,T+20))
juv_tot_system=array(0,dim=c(5000,T+20))
bugs_juv_vichy=array(0,dim=c(5000,T+20))
bugs_juv_langeac=array(0,dim=c(5000,T+20))
bugs_juv_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_vichy=array(0,dim=c(5000,T+20))
bugs_juv_tot_langeac=array(0,dim=c(5000,T+20))
bugs_juv_tot_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_system=array(0,dim=c(5000,T+20))
ratio_habitat=array(0,dim=c(3,T+20))


for (t in 1:(T+20)){
	for (i in 1:3){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t])
	}
}

#Pour avoir les premiers juv_tot sans les recalculer car fout la zone dans R car en a besoin pour calculer la suite
for(t in (T-4): (T+1)){
	for (i in 1:5000){ 
		bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t]
		bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[2,t]
		bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[3,t]
	}
}

for (t in (T+1):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]
		bugs_juv_tot_system[i,t] = bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]#*bugs_rho_poutes[i]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])#*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(s_juv2ad_new[i,t]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
		if (N_vichy[i,t]>30000) {N_vichy[i,t]=30000}#I(3,30000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}


#Les juvéniles t-3 sont maintenant ceux calculés à la fin de la première boucle et non plus ceux issus directement d'openbugs
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5] 
		bugs_juv_tot_system[i,t] = bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(s_juv2ad_new[i,t]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
		if (N_vichy[i,t]>30000) {N_vichy[i,t]=30000}#I(3,30000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}

#Les juvéniles t-3 et t-4 sont maintenant ceux calculés à la fin des boucles précédentes et non plus ceux issus directement d'openbugs
for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5] 
		bugs_juv_tot_system[i,t] = bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(s_juv2ad_new[i,t]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
		if (N_vichy[i,t]>30000) {N_vichy[i,t]=30000}#I(3,30000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}

#Les juvéniles t-3 et t-4 t-5 sont maintenant ceux calculés à la fin des boucles précédentes et non plus ceux issus directement d'openbugs
for (t in (T+7):(T+20)){
	for (i in 1:5000){ 
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5] 
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]*bugs_rho_poutes[i]
		
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]*bugs_rho_poutes[i]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t]*bugs_rho_poutes[i])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]*bugs_rho_poutes[i]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t]*bugs_rho_poutes[i])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(s_juv2ad_new[i,t]*juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if (N_vichy[i,t]<3) {N_vichy[i,t]=3}
		if (N_vichy[i,t]>30000) {N_vichy[i,t]=30000}#I(3,30000)
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log( (S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])#d_moy_vichy=d_wild_moy_vichy car projection sans repeuplement
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}


##############################################################
# Projection sur 20 ans du nbr de Géniteurs et juvéniles
##############################################################

#===========================================================
# Projection sur 20 ans des Géniteurs (en nombre et ratio)
#===========================================================
#--------------------------------------------------------------------------
# Récupération des données sur la première partie (1 à T) depuis OpenBugs
#--------------------------------------------------------------------------
#...................
# Nbr de Géniteurs
#...................
S_vichy_real=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_20/S_vichyCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_20/S_vichyCODAindex.txt")
S_langeac_real=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_20/S_langeacCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_20/S_langeacCODAindex.txt")

S_vichy_q=array(rep(0,T*5),dim=c(T,5))
S_langeac_q=array(rep(0,T*5),dim=c(T,5))

S_poutes_counter=c(
		0,0,
		0,0,0,0,0,
		0,0,0,0,10,
		43,110,21,4,3,
		11,9,23,6,67,
		35,31,130,112,53,
		40,154,89,74,153,
		53,39,14,26,118,59,45)

for (i in 1:T){
	S_vichy_q[i,]=quantile(S_vichy_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (i in 1:T){
	S_langeac_q[i,]=quantile(S_langeac_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#..........
# Ratio
#..........		 
ratio_S_V=array(0,c(5000,T))
ratio_S_L=array(0,c(5000,T))
ratio_S_P=array(0,c(5000,T))

for (t in 1:11){
	for(i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] + S_langeac_real[i,t])
		ratio_S_L[i,t] = 1 - ratio_S_V[i,t]
	}
}

for (t in 12:T){	
	for( i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_L[i,t] = S_langeac_real[i,t] / (S_vichy_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_P[i,t] = 1 - ratio_S_V[i,t] -ratio_S_L[i,t]
	}
}

ratio_S_V_q=array(0,c(T,5))
ratio_S_L_q=array(0,c(T,5))
ratio_S_P_q=array(0,c(T,5))

for (t in 1:T){
	ratio_S_V_q[t,]=quantile(ratio_S_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_L_q[t,]=quantile(ratio_S_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_P_q[t,]=quantile(ratio_S_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#-----------------------------------
#Recalcul des nombres de Géniteurs
#-----------------------------------
S_vichy_proj_q=array(0,c(T+20,5))
S_langeac_proj_q=array(0,c(T+20,5))
S_poutes_proj_q=array(0,c(T+20,5))

for (i in (T+1):(T+20)){
	S_vichy_proj_q[i,]=quantile(S_vichy[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (i in (T+1):(T+20)){
	S_langeac_proj_q[i,]=quantile(S_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (i in (T+1):(T+20)){
	S_poutes_proj_q[i,]=quantile(S_poutes[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#---------------------------------------------------
# Recalcul des ratios sur les 20 ans de projection
#---------------------------------------------------
ratio_S_V_proj=array(0,dim=c(5000,T+20))
ratio_S_L_proj=array(0,dim=c(5000,T+20))
ratio_S_P_proj=array(0,dim=c(5000,T+20))
ratio_S_V_proj_q=array(0,c(T+20,5))
ratio_S_L_proj_q=array(0,c(T+20,5))
ratio_S_P_proj_q=array(0,c(T+20,5))

for (t in (T+1):(T+20)){
	for (i in 1:5000){ 
		ratio_S_V_proj[i,t]=S_vichy[i,t]/(S_vichy[i,t]+S_langeac[i,t]+S_poutes[i,t])
		ratio_S_L_proj[i,t]=S_langeac[i,t]/(S_vichy[i,t]+S_langeac[i,t]+S_poutes[i,t])
		ratio_S_P_proj[i,t]=S_poutes[i,t]/(S_vichy[i,t]+S_langeac[i,t]+S_poutes[i,t])
	}
}

for (t in (T+1):(T+20)){
	ratio_S_V_proj_q[t,]=quantile(ratio_S_V_proj[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_L_proj_q[t,]=quantile(ratio_S_L_proj[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_P_proj_q[t,]=quantile(ratio_S_P_proj[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#----------------------------------------------------------------------------------------
# Conservation de la quantité d'habitat dispo identiques pour les 20 prochaines années
#----------------------------------------------------------------------------------------
surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)

#---------------------------------------------
# Graph Géniteurs avec projection sur 20 ans
#---------------------------------------------
png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2014_12_20/SpawnersRedds_GeniteursPotentiels_proj.png",width=800, height=800, units = "px",type="cairo")

par(mfrow=c(3,2),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
#........
# Vichy
#........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,6500),ylab=expression(italic(S["t,1"])),main="Vichy-Langeac",cex.main=2,cex.lab=2)

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000,6000),labels=c(0,1000,2000,3000,4000,5000,6000),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=c(1975,1980,1990,2000,2013,2023,2033),
		cex.axis = 1.5,las = 1,col = "grey55")

text(T+20,6500,labels=expression(italic("a.")),col = "grey55")

for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,S_vichy_q[i,5],i+0.15,S_vichy_q[i,5])
	segments(i,S_vichy_q[i,4],i,S_vichy_q[i,5])
	#5%
	segments(i-0.15,S_vichy_q[i,1],i+0.15,S_vichy_q[i,1])
	segments(i,S_vichy_q[i,2],i,S_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_vichy_q[i,2],S_vichy_q[i,2],S_vichy_q[i,4],S_vichy_q[i,4]),col="coral3")
	#median
	segments(i-0.3,S_vichy_q[i,3],i+0.3,S_vichy_q[i,3])
}

#20 ans de projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,S_vichy_proj_q[i,5],i+0.15,S_vichy_proj_q[i,5])
	segments(i,S_vichy_proj_q[i,4],i,S_vichy_proj_q[i,5])
	#5%
	segments(i-0.15,S_vichy_proj_q[i,1],i+0.15,S_vichy_proj_q[i,1])
	segments(i,S_vichy_proj_q[i,2],i,S_vichy_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_vichy_proj_q[i,2],S_vichy_proj_q[i,2],S_vichy_proj_q[i,4],S_vichy_proj_q[i,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,S_vichy_proj_q[i,3],i+0.3,S_vichy_proj_q[i,3])
}

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Vichy-Langeac",cex.main=2,cex.lab=2)

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=c(1975,1980,1990,2000,2013,2023,2033),
		cex.axis = 1.5,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("d.")),col = "grey55")

for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_V_q[i,5],i+0.15,ratio_S_V_q[i,5])
	segments(i,ratio_S_V_q[i,4],i,ratio_S_V_q[i,5])
	#5%
	segments(i-0.15,ratio_S_V_q[i,1],i+0.15,ratio_S_V_q[i,1])
	segments(i,ratio_S_V_q[i,2],i,ratio_S_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_V_q[i,2],ratio_S_V_q[i,2],ratio_S_V_q[i,4],ratio_S_V_q[i,4]),col="coral3")
	#median
	segments(i-0.3,ratio_S_V_q[i,3],i+0.3,ratio_S_V_q[i,3])
}
#20 ans de projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_V_proj_q[i,5],i+0.15,ratio_S_V_proj_q[i,5])
	segments(i,ratio_S_V_proj_q[i,4],i,ratio_S_V_proj_q[i,5])
	#5%
	segments(i-0.15,ratio_S_V_proj_q[i,1],i+0.15,ratio_S_V_proj_q[i,1])
	segments(i,ratio_S_V_proj_q[i,2],i,ratio_S_V_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_V_proj_q[i,2],ratio_S_V_proj_q[i,2],ratio_S_V_proj_q[i,4],ratio_S_V_proj_q[i,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,ratio_S_V_proj_q[i,3],i+0.3,ratio_S_V_proj_q[i,3])
}

segments(0,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)


abline(v=11.5,lty=3)

#..........
# Langeac
#..........
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,1200),ylab=expression(italic(S["t,2"])),main="Langeac-Poutes",cex.main=2,cex.lab=2)

# trace l'axe des ordonnées
axis(2,at = c(0,300,600,900,1200),labels=c(0,300,600,900,1200),cex.axis = 2,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=c(1975,1980,1990,2000,2013,2023,2033),
		cex.axis = 1.5,las = 1,col = "grey55")


text(T+20,1200,labels=expression(italic("b.")),col = "grey55")



for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,S_langeac_q[i,5],i+0.15,S_langeac_q[i,5])
	segments(i,S_langeac_q[i,4],i,S_langeac_q[i,5])
	#5%
	segments(i-0.15,S_langeac_q[i,1],i+0.15,S_langeac_q[i,1])
	segments(i,S_langeac_q[i,2],i,S_langeac_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_langeac_q[i,2],S_langeac_q[i,2],S_langeac_q[i,4],S_langeac_q[i,4]),col="coral3")
	#median
	segments(i-0.3,S_langeac_q[i,3],i+0.3,S_langeac_q[i,3])
}

#Proj sur 20 ans
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,S_langeac_proj_q[i,5],i+0.15,S_langeac_proj_q[i,5])
	segments(i,S_langeac_proj_q[i,4],i,S_langeac_proj_q[i,5])
	#5%
	segments(i-0.15,S_langeac_proj_q[i,1],i+0.15,S_langeac_proj_q[i,1])
	segments(i,S_langeac_proj_q[i,2],i,S_langeac_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_langeac_proj_q[i,2],S_langeac_proj_q[i,2],S_langeac_proj_q[i,4],S_langeac_proj_q[i,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,S_langeac_proj_q[i,3],i+0.3,S_langeac_proj_q[i,3])
}

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Poutes")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=c(1975,1980,1990,2000,2013,2023,2033),
		cex.axis =1.5,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("e.")),col = "grey55")

for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_L_q[i,5],i+0.15,ratio_S_L_q[i,5])
	segments(i,ratio_S_L_q[i,4],i,ratio_S_L_q[i,5])
	#5%
	segments(i-0.15,ratio_S_L_q[i,1],i+0.15,ratio_S_L_q[i,1])
	segments(i,ratio_S_L_q[i,2],i,ratio_S_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_q[i,2],ratio_S_L_q[i,2],ratio_S_L_q[i,4],ratio_S_L_q[i,4]),col="coral3")
	#median
	segments(i-0.3,ratio_S_L_q[i,3],i+0.3,ratio_S_L_q[i,3])
}

#Proj sur 20 ans
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_L_proj_q[i,5],i+0.15,ratio_S_L_proj_q[i,5])
	segments(i,ratio_S_L_proj_q[i,4],i,ratio_S_L_proj_q[i,5])
	#5%
	segments(i-0.15,ratio_S_L_proj_q[i,1],i+0.15,ratio_S_L_proj_q[i,1])
	segments(i,ratio_S_L_proj_q[i,2],i,ratio_S_L_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_proj_q[i,2],ratio_S_L_proj_q[i,2],ratio_S_L_proj_q[i,4],ratio_S_L_proj_q[i,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,ratio_S_L_proj_q[i,3],i+0.3,ratio_S_L_proj_q[i,3])
}

segments(0,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

abline(v=11.5,lty=3)

#..........
# Poutes
#..........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,400),ylab=expression(italic(S["t,3"])),main="Upstream Poutes",cex.main=2,cex.lab=2)

# trace l'axe des ordonnées
axis(2,at = c(0,100,200,300,400),labels=c(0,100,200,300,400),cex.axis = 2,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=c(1975,1980,1990,2000,2013,2023,2033),
		cex.axis = 1.5,las = 1,col = "grey55")

points(x=seq(12,T,1),y=S_poutes_counter[12:T],pch=16,col="darkolivegreen3") 
abline(v=11.5,lty=2)
text(6,100,paste( "Amont Poutes\n inaccessible"),col = "grey55")


text(T+20,400,labels=expression(italic("c.")),col = "grey55")
#Proj sur 20 ans
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,S_poutes_proj_q[i,5],i+0.15,S_poutes_proj_q[i,5])
	segments(i,S_poutes_proj_q[i,4],i,S_poutes_proj_q[i,5])
	#5%
	segments(i-0.15,S_poutes_proj_q[i,1],i+0.15,S_poutes_proj_q[i,1])
	segments(i,S_poutes_proj_q[i,2],i,S_poutes_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_poutes_proj_q[i,2],S_poutes_proj_q[i,2],S_poutes_proj_q[i,4],S_poutes_proj_q[i,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,S_poutes_proj_q[i,3],i+0.3,S_poutes_proj_q[i,3])
}


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Poutes",cex.main=2,cex.lab=2)

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis =2,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=c(1975,1980,1990,2000,2013,2023,2033),
		cex.axis =1.5,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("f.")),col = "grey55")
text(6,0.55,paste( "Amont Poutes\n inaccessible"),col = "grey55")


for(i in 12:22){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_P_q[i,5],i+0.15,ratio_S_P_q[i,5])
	segments(i,ratio_S_P_q[i,4],i,ratio_S_P_q[i,5])
	#5%
	segments(i-0.15,ratio_S_P_q[i,1],i+0.15,ratio_S_P_q[i,1])
	segments(i,ratio_S_P_q[i,2],i,ratio_S_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_P_q[i,2],ratio_S_P_q[i,2],ratio_S_P_q[i,4],ratio_S_P_q[i,4]),col="darkolivegreen3")
	#median
	segments(i-0.3,ratio_S_P_q[i,3],i+0.3,ratio_S_P_q[i,3])
}

x=seq(23,T,1)
points(x,ratio_S_P_q[23:T,3],pch=16,col="darkolivegreen3")

#Proj sur 20 ans
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_P_proj_q[i,5],i+0.15,ratio_S_P_proj_q[i,5])
	segments(i,ratio_S_P_proj_q[i,4],i,ratio_S_P_proj_q[i,5])
	#5%
	segments(i-0.15,ratio_S_P_proj_q[i,1],i+0.15,ratio_S_P_proj_q[i,1])
	segments(i,ratio_S_P_proj_q[i,2],i,ratio_S_P_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_P_proj_q[i,2],ratio_S_P_proj_q[i,2],ratio_S_P_proj_q[i,4],ratio_S_P_proj_q[i,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,ratio_S_P_proj_q[i,3],i+0.3,ratio_S_P_proj_q[i,3])
}
segments(11.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

abline(v=11.5,lty=3)

dev.off()

#===========================================================
# Projection sur 20 ans de juvéniles (en nombre et ratio)
#===========================================================

juv_vichy_proj_q=array(0,c(T+20,5))
juv_langeac_proj_q=array(0,c(T+20,5))
juv_poutes_proj_q=array(0,c(T+20,5))

for (t in (T+1):(T+20)){
	juv_vichy_proj_q[t,]=quantile(juv_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	juv_langeac_proj_q[t,]=quantile(juv_langeac[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	juv_poutes_proj_q[t,]=quantile(juv_poutes[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

ratio_S_V_proj=array(0,dim=c(5000,T+20))
ratio_S_L_proj=array(0,dim=c(5000,T+20))
ratio_S_P_proj=array(0,dim=c(5000,T+20))
ratio_S_V_proj_q=array(0,c(T+20,5))
ratio_S_L_proj_q=array(0,c(T+20,5))
ratio_S_P_proj_q=array(0,c(T+20,5))

for (t in (T+1):(T+20)){
	for (i in 1:5000){ 
		ratio_S_V_proj[i,t]=S_vichy[i,t]/(S_vichy[i,t]+S_langeac[i,t]+S_poutes[i,t])
		ratio_S_L_proj[i,t]=S_langeac[i,t]/(S_vichy[i,t]+S_langeac[i,t]+S_poutes[i,t])
		ratio_S_P_proj[i,t]=S_poutes[i,t]/(S_vichy[i,t]+S_langeac[i,t]+S_poutes[i,t])
	}
}

for (t in (T+1):(T+20)){
	ratio_S_V_proj_q[t,]=quantile(ratio_S_V_proj[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_L_proj_q[t,]=quantile(ratio_S_L_proj[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_P_proj_q[t,]=quantile(ratio_S_P_proj[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}



png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2014_12_05_Poutes50/poutes50_NbrJuveniles_RatioJuveniles.png",width=800, height=800, units = "px",type="cairo")

par(mfrow=c(3,2),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
#........
# Vichy
#........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,6500),ylab=expression(italic(S["t,1"])),main="Vichy-Langeac",cex.main=2,cex.lab=2)

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000,6000),labels=c(0,1000,2000,3000,4000,5000,6000),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=c(1975,1980,1990,2000,2013,2023,2033),
		cex.axis = 1.5,las = 1,col = "grey55")

text(T+20,6500,labels=expression(italic("a.")),col = "grey55")

for(i in 3:T){
	#whiskers
	#95%
	segments(i-0.15,juv_vichy[i,5],i+0.15,S_vichy_q[i,5])
	segments(i,S_vichy_q[i,4],i,S_vichy_q[i,5])
	#5%
	segments(i-0.15,S_vichy_q[i,1],i+0.15,S_vichy_q[i,1])
	segments(i,S_vichy_q[i,2],i,S_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_vichy_q[i,2],S_vichy_q[i,2],S_vichy_q[i,4],S_vichy_q[i,4]),col="coral3")
	#median
	segments(i-0.3,S_vichy_q[i,3],i+0.3,S_vichy_q[i,3])
}

#20 ans de projection
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,juv_vichy_proj_q[i,5],i+0.15,S_vichy_proj_q[i,5])
	segments(i,S_vichy_proj_q[i,4],i,S_vichy_proj_q[i,5])
	#5%
	segments(i-0.15,S_vichy_proj_q[i,1],i+0.15,S_vichy_proj_q[i,1])
	segments(i,S_vichy_proj_q[i,2],i,S_vichy_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_vichy_proj_q[i,2],S_vichy_proj_q[i,2],S_vichy_proj_q[i,4],S_vichy_proj_q[i,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,S_vichy_proj_q[i,3],i+0.3,S_vichy_proj_q[i,3])
}


