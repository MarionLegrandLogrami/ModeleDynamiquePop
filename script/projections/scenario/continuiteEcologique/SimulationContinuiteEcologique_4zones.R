# SIMULATION AMELIORATION CONTINUITE ECOLOGIQUE
# 
# Author: marion.legrand
###############################################################################
# TOUTES LES VARIABLES OPENBUGS UTILISEES DIRECTEMENT SONT PRECEDEES D'UN "bugs_" POUR LES DIFFERENCIER DES VARIABLES RECALCULER DANS R
#================
# CHARGEMENT
#================

#Modèle 2017.05.03_4zones_Interaction
setwd("C:/Users/utilisateur/Desktop/Marion/CODA/2017_03_23_4zones_Interaction_new/")
#Modèle 2017.05.03_4zones_Interaction_ss_rho_poutes
setwd("C:/Users/utilisateur/Desktop/Marion/CODA/2017_05_03_4zones_Interaction_ss_rho_poutes/")
#Modèle 2017.08.29_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2016
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")

library(coda)
library(boot)
T=42

surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)	


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# AMELIORATION montaison 100% + dévalaison dans les ouvrages hydroélectriques
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#=============================================================================================================================
# AMELIORATION SUR LA SURVIE DES JUVENILES : suppression des impacts à la dévalaison (mortalité dans les ouvrages hydro)
# AMELIORATION DE LA FRANCHISSABILITE : p_adjust_P, p_adjust_L et p_adjust_A
#=============================================================================================================================

#------------------
# Sans rho_poutes
#------------------

bugs_ratio_juv_prod_L=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")
bugs_rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
#bugs_adjust_p_A=read.coda("simulation/adjust_p_ACODAchain1.txt","simulation/adjust_p_ACODAindex.txt")
#bugs_adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
#bugs_adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")
bugs_sigma_p_alagnon=read.coda("sigma_p_alagnonCODAchain1.txt","sigma_p_alagnonCODAindex.txt")
bugs_sigma_p_langeac=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
bugs_sigma_p_poutes=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")
bugs_sigma_vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")
bugs_sigma_wild_moy=read.coda("parameters/sigma_wild_moyCODAchain1.txt","parameters/sigma_wild_moyCODAindex.txt")
#bugs_res_p_poutes=read.coda("simulation/res_p_poutesCODAchain1.txt","simulation/res_p_poutesCODAindex.txt")
bugs_s_juv2ad=read.coda("simulation/s_juv2adCODAchain1.txt","simulation/s_juv2adCODAindex.txt")
bugs_I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt")
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

ratio_juv_prod_V=array(0,dim=c(5000,T+20))
ratio_juv_prod_A=array(0,dim=c(5000,T+20))
ratio_juv_prod_L=array(0,dim=c(5000,T+20))
ratio_juv_prod_P=array(0,dim=c(5000,T+20))
ratio_juv_A=array(0,dim=c(5000,T+20))
ratio_juv_L=array(0,dim=c(5000,T+20))
ratio_juv_P=array(0,dim=c(5000,T+20))
L_ratio_juv_A=array(0,dim=c(5000,T+20))
L_ratio_juv_L=array(0,dim=c(5000,T+20))
L_ratio_juv_P=array(0,dim=c(5000,T+20))
L_mu_Vichy_nm=array(0,dim=c(5000,T+20))
L_mu_p_alagnon=array(0,dim=c(5000,T+20))
L_mu_p_langeac=array(0,dim=c(5000,T+20))
L_mu_p_poutes=array(0,dim=c(5000,T+20))
L_mu_d_wild_vichy=array(0,dim=c(5000,T+21))
L_mu_d_wild_alagnon=array(0,dim=c(5000,T+21))
L_mu_d_wild_langeac=array(0,dim=c(5000,T+21))
L_mu_d_wild_poutes=array(0,dim=c(5000,T+21))
L_p_alagnon=array(0,dim=c(5000,T+20))
L_p_langeac=array(0,dim=c(5000,T+20))
L_p_poutes=array(0,dim=c(5000,T+20))
L_d_moy_vichy=array(0,dim=c(5000,T+21))
L_d_moy_alagnon=array(0,dim=c(5000,T+21))
L_d_moy_langeac=array(0,dim=c(5000,T+21))
L_d_moy_poutes=array(0,dim=c(5000,T+21))
res_vichy=array(0,dim=c(5000,T+20))
res_p_alagnon=array(0,dim=c(5000,T+20))
res_p_langeac=array(0,dim=c(5000,T+20))
res_p_poutes=array(0,dim=c(5000,T+20))
p_alagnon=array(0,dim=c(5000,T+20))
p_langeac=array(0,dim=c(5000,T+20))
p_poutes=array(0,dim=c(5000,T+20))
N_vichy=array(0,dim=c(5000,T+20))
N_vichy_temp=array(0,dim=c(5000,T+20))
N_alagnon=array(0,dim=c(5000,T+20))
N_langeac=array(0,dim=c(5000,T+20))
N_poutes=array(0,dim=c(5000,T+20))
min_N_langeac=array(0,dim=c(5000,T+20))
S_vichy=array(0,dim=c(5000,T+20))
S_alagnon=array(0,dim=c(5000,T+20))
S_langeac=array(0,dim=c(5000,T+20))
S_poutes=array(0,dim=c(5000,T+20))
d_moy_vichy=array(0,dim=c(5000,T+21))
d_moy_alagnon=array(0,dim=c(5000,T+21))
d_moy_langeac=array(0,dim=c(5000,T+21))
d_moy_poutes=array(0,dim=c(5000,T+21))
juv_vichy=array(0,dim=c(5000,T+21))
juv_alagnon=array(0,dim=c(5000,T+21))
juv_langeac=array(0,dim=c(5000,T+21))
juv_poutes=array(0,dim=c(5000,T+21))
juv_tot_vichy=array(0,dim=c(5000,T+20))
juv_tot_alagnon=array(0,dim=c(5000,T+20))
juv_tot_langeac=array(0,dim=c(5000,T+20))
juv_tot_poutes=array(0,dim=c(5000,T+20))
juv_tot_system=array(0,dim=c(5000,T+20))
bugs_juv_vichy=array(0,dim=c(5000,T+20))
bugs_juv_alagnon=array(0,dim=c(5000,T+20))
bugs_juv_langeac=array(0,dim=c(5000,T+20))
bugs_juv_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_vichy=array(0,dim=c(5000,T+20))
bugs_juv_tot_alagnon=array(0,dim=c(5000,T+20))
bugs_juv_tot_langeac=array(0,dim=c(5000,T+20))
bugs_juv_tot_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_system=array(0,dim=c(5000,T+20))
ratio_habitat=array(0,dim=c(4,T+20))


for (t in 1:(T+20)){
  for (i in 1:4){
    ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t])
  }
}


## On améliore la survie d'autant que le résultat de DEVALPOMI
#15404 smolts tués sur les 11 ouvrages de notre secteurs contre 68214 smolts produits sur l'Allier, l'Alagnon et la Dore
# taux de mortalité = 0.226 soit une survie de 1-0.226=0.774

improv_surv=0.774


#On recalcule tous les juvéniles car la modification dans la distribution spatiale des géniteurs peut
#modifier les densités de juvéniles des différentes zones

#Pour avoir les premiers juv_tot

for(t in (T-4): (T+1)){
  for (i in 1:5000){ 
    bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t]
    bugs_juv_alagnon[i,t]=bugs_dmoy_tot_A[i,t-1]*S_juv_JP[2,t]
    bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[3,t]
    bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[4,t]
  }
}

#Effet sur les juvéniles au fur et à mesure
#n'impacte pas les 3 dernières années de production (car antérieures à l'année d'amélioration)

for (t in (T+1):(T+3)){
  for (i in 1:5000){ 
    bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
    bugs_juv_tot_alagnon[i,t] = (1/3) * bugs_juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
    bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
    bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5] 
    
    bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
    
    ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
    
    ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
    L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
    L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
    L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
    res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
    
    ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
    L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
    L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
    L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
    res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
    
    ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
    ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
    L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
    L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
    L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
    res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
    
    L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
    N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
    if(N_vichy[i,t]<3){N_vichy[i,t]=3}
    if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
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
    
    L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
    L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])
    juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
    
    L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
    L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
    juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
    
    L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
    L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
    juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
    
    L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
    L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
    juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
    
  }
}

#Am�lioration sur le premier tiers de production
for (t in (T+4):(T+4)){
  for (i in 1:5000){ 
    bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3]/improv_surv + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
    bugs_juv_tot_alagnon[i,t] = (1/3) * bugs_juv_alagnon[i,t-3]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
    bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3]/improv_surv + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
    bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3]/improv_surv + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5] 
    
    bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
    
    ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
    
    ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
    L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
    L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
    L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
    res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
    
    
    ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
    L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
    L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
    L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
    res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
    
    ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
    ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
    L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
    L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
    L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
    res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
    
    L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
    N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
    if(N_vichy[i,t]<3){N_vichy[i,t]=3}
    if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
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
    
    L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
    L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
    juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
    
    L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
    L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
    juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
    
    L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
    L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
    juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
    
    L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
    L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
    juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
    
  }
}


#Am�lioration sur les deux premiers tiers de production en amont de Pout�s.On utilise les donn�es de juv�niles de bugs pour t-4 et t-5
for (t in (T+5):(T+5)){
  for (i in 1:5000){ 
    bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3]/improv_surv + (1/3) * bugs_juv_vichy[i,t-4]/improv_surv + (1/3) * bugs_juv_vichy[i,t-5] 
    bugs_juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-4]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-5] 
    bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3]/improv_surv + (1/3) * bugs_juv_langeac[i,t-4]/improv_surv + (1/3) * bugs_juv_langeac[i,t-5] 
    bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3]/improv_surv + (1/3) * bugs_juv_poutes[i,t-4]/improv_surv + (1/3) * bugs_juv_poutes[i,t-5] 
    
    bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
    
    ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
    
    ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
    L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
    L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
    L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
    res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
    
    
    ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
    L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
    L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
    L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
    res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
    
    ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
    ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
    L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
    L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
    L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
    res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
    
    L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
    N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
    if(N_vichy[i,t]<3){N_vichy[i,t]=3}
    if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
    res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
    
    p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
    p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
    p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
    N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
    N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
    N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
    min_N_langeac[i,t]<-N_poutes[i,t]+1
    if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
    
    
    S_poutes[i,t]=max(N_poutes[i,t],1)
    S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
    S_alagnon[i,t]=max(N_alagnon[i,t],1)
    S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
    
    L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
    L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
    juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
    
    L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
    L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
    juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
    
    L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
    L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
    juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
    
    L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
    L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
    juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
  }
}

#Am�lioration pour l'ensemble de la production de juv�niles en amont de Pout�s. On utilise les donn�es de juv�niles de bugs pour t-5
for (t in (T+6):(T+6)){
  for (i in 1:5000){ 
    bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3]/improv_surv + (1/3) * juv_vichy[i,t-4]/improv_surv + (1/3) * bugs_juv_vichy[i,t-5]/improv_surv 
    bugs_juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3]/improv_surv + (1/3) * juv_alagnon[i,t-4]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-5]/improv_surv 
    bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3]/improv_surv + (1/3) * juv_langeac[i,t-4]/improv_surv + (1/3) * bugs_juv_langeac[i,t-5]/improv_surv 
    bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3]/improv_surv + (1/3) * juv_poutes[i,t-4]/improv_surv + (1/3) * bugs_juv_poutes[i,t-5]/improv_surv 
    
    bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
    
    ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
    ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
    
    ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
    L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
    L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
    L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
    res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
    
    
    ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
    L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
    L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
    L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
    res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
    
    ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
    ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
    L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
    L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
    L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
    res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
    
    L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
    N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
    if(N_vichy[i,t]<3){N_vichy[i,t]=3}
    if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
    res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
    
    p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
    p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
    p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
    N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
    N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
    N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
    min_N_langeac[i,t]<-N_poutes[i,t]+1
    if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
    
    
    S_poutes[i,t]=max(N_poutes[i,t],1)
    S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
    S_alagnon[i,t]=max(N_alagnon[i,t],1)
    S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
    
    L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
    L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
    juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
    
    L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
    L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
    juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
    
    L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
    L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
    juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
    
    L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
    L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
    juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
    
  }
}
#Toutes les cohortes poutes b�n�ficient de l'am�lioration. 
for (t in (T+7):(T+20)){#juste pour le test mettre +20 quand on voit que �a marche
  for (i in 1:5000){ 
    juv_tot_vichy[i,t] = (1/3) *juv_vichy[i,t-3]/improv_surv + (1/3) * juv_vichy[i,t-4]/improv_surv + (1/3) * juv_vichy[i,t-5]/improv_surv 
    juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3]/improv_surv + (1/3) * juv_alagnon[i,t-4]/improv_surv + (1/3) * juv_alagnon[i,t-5]/improv_surv 
    juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3]/improv_surv + (1/3) * juv_langeac[i,t-4]/improv_surv + (1/3) * juv_langeac[i,t-5]/improv_surv 
    juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4]/improv_surv + (1/3) * juv_poutes[i,t-5]/improv_surv 
    
    juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
    
    ratio_juv_prod_A[i,t] <- juv_tot_alagnon[i,t] / (juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
    ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
    ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
    
    ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
    L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
    L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
    L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
    res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
    
    
    ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
    L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
    L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
    L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
    res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
    
    ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])
    ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
    L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
    L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
    L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
    res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
    
    L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
    N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
    if(N_vichy[i,t]<3){N_vichy[i,t]=3}
    if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
    res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
    
    p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
    p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
    p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
    N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
    N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
    N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
    min_N_langeac[i,t]<-N_poutes[i,t]+1
    if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
    
    S_poutes[i,t]=max(N_poutes[i,t],1)
    S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
    S_alagnon[i,t]=max(N_alagnon[i,t],1)
    S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
    
    L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
    L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
    juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
    
    L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
    L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
    juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
    
    L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
    L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
    juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
    
    L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
    L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
    d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
    juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
  }
}

#save.image(file = "C:/Users/utilisateur/Desktop/Marion/2017_05_03_ss_rho_poutes_ContinuiteEcologique_2017_05_04.RData")
save.image(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_ContinuiteEcologique_2017_12_12.RData")

#load("C:/Users/utilisateur/Desktop/Marion/2017_05_03_ss_rho_poutes_ContinuiteEcologique_2017_05_04.RData")
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_ContinuiteEcologique_2017_12_12.RData")



#--------------------
# Avec rho_poutes
#--------------------

bugs_rho_poutes=read.coda("rho_poutesCODAchain1.txt","rho_poutesCODAindex.txt")
bugs_ratio_juv_prod_L=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")
bugs_rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
#bugs_adjust_p_A=read.coda("simulation/adjust_p_ACODAchain1.txt","simulation/adjust_p_ACODAindex.txt")
#bugs_adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
#bugs_adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")
bugs_sigma_p_alagnon=read.coda("sigma_p_alagnonCODAchain1.txt","sigma_p_alagnonCODAindex.txt")
bugs_sigma_p_langeac=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
bugs_sigma_p_poutes=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")
bugs_sigma_vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")
bugs_sigma_wild_moy=read.coda("parameters/sigma_wild_moyCODAchain1.txt","parameters/sigma_wild_moyCODAindex.txt")
#bugs_res_p_poutes=read.coda("simulation/res_p_poutesCODAchain1.txt","simulation/res_p_poutesCODAindex.txt")
bugs_I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt")
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

ratio_juv_prod_V=array(0,dim=c(5000,T+20))
ratio_juv_prod_A=array(0,dim=c(5000,T+20))
ratio_juv_prod_L=array(0,dim=c(5000,T+20))
ratio_juv_prod_P=array(0,dim=c(5000,T+20))
ratio_juv_A=array(0,dim=c(5000,T+20))
ratio_juv_L=array(0,dim=c(5000,T+20))
ratio_juv_P=array(0,dim=c(5000,T+20))
L_ratio_juv_A=array(0,dim=c(5000,T+20))
L_ratio_juv_L=array(0,dim=c(5000,T+20))
L_ratio_juv_P=array(0,dim=c(5000,T+20))
L_mu_Vichy_nm=array(0,dim=c(5000,T+20))
L_mu_p_alagnon=array(0,dim=c(5000,T+20))
L_mu_p_langeac=array(0,dim=c(5000,T+20))
L_mu_p_poutes=array(0,dim=c(5000,T+20))
L_mu_d_wild_vichy=array(0,dim=c(5000,T+21))
L_mu_d_wild_alagnon=array(0,dim=c(5000,T+21))
L_mu_d_wild_langeac=array(0,dim=c(5000,T+21))
L_mu_d_wild_poutes=array(0,dim=c(5000,T+21))
L_p_alagnon=array(0,dim=c(5000,T+20))
L_p_langeac=array(0,dim=c(5000,T+20))
L_p_poutes=array(0,dim=c(5000,T+20))
L_d_moy_vichy=array(0,dim=c(5000,T+21))
L_d_moy_alagnon=array(0,dim=c(5000,T+21))
L_d_moy_langeac=array(0,dim=c(5000,T+21))
L_d_moy_poutes=array(0,dim=c(5000,T+21))
res_vichy=array(0,dim=c(5000,T+20))
res_p_alagnon=array(0,dim=c(5000,T+20))
res_p_langeac=array(0,dim=c(5000,T+20))
res_p_poutes=array(0,dim=c(5000,T+20))
p_alagnon=array(0,dim=c(5000,T+20))
p_langeac=array(0,dim=c(5000,T+20))
p_poutes=array(0,dim=c(5000,T+20))
N_vichy=array(0,dim=c(5000,T+20))
N_vichy_temp=array(0,dim=c(5000,T+20))
N_alagnon=array(0,dim=c(5000,T+20))
N_langeac=array(0,dim=c(5000,T+20))
N_poutes=array(0,dim=c(5000,T+20))
min_N_langeac=array(0,dim=c(5000,T+20))
S_vichy=array(0,dim=c(5000,T+20))
S_alagnon=array(0,dim=c(5000,T+20))
S_langeac=array(0,dim=c(5000,T+20))
S_poutes=array(0,dim=c(5000,T+20))
d_moy_vichy=array(0,dim=c(5000,T+21))
d_moy_alagnon=array(0,dim=c(5000,T+21))
d_moy_langeac=array(0,dim=c(5000,T+21))
d_moy_poutes=array(0,dim=c(5000,T+21))
juv_vichy=array(0,dim=c(5000,T+21))
juv_alagnon=array(0,dim=c(5000,T+21))
juv_langeac=array(0,dim=c(5000,T+21))
juv_poutes=array(0,dim=c(5000,T+21))
juv_tot_vichy=array(0,dim=c(5000,T+20))
juv_tot_alagnon=array(0,dim=c(5000,T+20))
juv_tot_langeac=array(0,dim=c(5000,T+20))
juv_tot_poutes=array(0,dim=c(5000,T+20))
juv_tot_system=array(0,dim=c(5000,T+20))
bugs_juv_vichy=array(0,dim=c(5000,T+20))
bugs_juv_alagnon=array(0,dim=c(5000,T+20))
bugs_juv_langeac=array(0,dim=c(5000,T+20))
bugs_juv_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_vichy=array(0,dim=c(5000,T+20))
bugs_juv_tot_alagnon=array(0,dim=c(5000,T+20))
bugs_juv_tot_langeac=array(0,dim=c(5000,T+20))
bugs_juv_tot_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_system=array(0,dim=c(5000,T+20))
ratio_habitat=array(0,dim=c(4,T+20))


for (t in 1:(T+20)){
	for (i in 1:4){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t])
	}
}

## On am�liore la survie d'autant que le r�sultat de DEVALPOMI (sans prise en compte de Pout�s qui est pris en compte
# par le mod�le via rho_poutes)
#8785 smolts tu�s sur les 10 ouvrages de notre secteurs contre 45725 smolts produits sur l'Allier en aval de Poutes, l'Alagnon et la Dore
# taux de mortalit� = 0.192 soit une survie de 1-0.192=0.808

improv_surv=0.808

#On recalcule tous les juv�niles car la modification dans la distribution spatiale des g�niteurs peut
#modifier les densit�s de juv�niles des diff�rentes zones

#Pour avoir les premiers juv_tot

for(t in (T-4): (T+1)){
	for (i in 1:5000){ 
		bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t]
		bugs_juv_alagnon[i,t]=bugs_dmoy_tot_A[i,t-1]*S_juv_JP[2,t]
		bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[3,t]
		bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[4,t]
	}
}

#Effet sur les juv�niles au fur et � mesure
#n'impacte pas les 3 derni�res ann�es de production (car ant�rieures � l'ann�e d'am�lioration)

for (t in (T+1):(T+3)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_alagnon[i,t] = (1/3) * bugs_juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
				
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if(N_vichy[i,t]<3){N_vichy[i,t]=3}
			if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
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
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) + bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]=exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
			
	}
}

#Am�lioration sur le premier tiers de production en amont de Pout�s + augmentation du nbr de juv�niles pour les autres secteurs
#� auteur de 0.808 qui correspond � la suppression des impacts � la d�valaison
for (t in (T+4):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3]/improv_surv + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_alagnon[i,t] = (1/3) * bugs_juv_alagnon[i,t-3]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3]/improv_surv + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
		
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if(N_vichy[i,t]<3){N_vichy[i,t]=3}
			if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
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
				
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]

	}
}


#Am�lioration sur les deux premiers tiers de production en amont de Pout�s.On utilise les donn�es de juv�niles de bugs pour t-4 et t-5
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3]/improv_surv + (1/3) * bugs_juv_vichy[i,t-4]/improv_surv + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-4]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3]/improv_surv + (1/3) * bugs_juv_langeac[i,t-4]/improv_surv + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
		
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if(N_vichy[i,t]<3){N_vichy[i,t]=3}
			if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
		N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
			if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_alagnon[i,t]=max(N_alagnon[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
	}
}

#Am�lioration pour l'ensemble de la production de juv�niles en amont de Pout�s. On utilise les donn�es de juv�niles de bugs pour t-5
for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3]/improv_surv + (1/3) * juv_vichy[i,t-4]/improv_surv + (1/3) * bugs_juv_vichy[i,t-5]/improv_surv 
		bugs_juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3]/improv_surv + (1/3) * juv_alagnon[i,t-4]/improv_surv + (1/3) * bugs_juv_alagnon[i,t-5]/improv_surv 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3]/improv_surv + (1/3) * juv_langeac[i,t-4]/improv_surv + (1/3) * bugs_juv_langeac[i,t-5]/improv_surv 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
		
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if(N_vichy[i,t]<3){N_vichy[i,t]=3}
			if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
		N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
			if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_alagnon[i,t]=max(N_alagnon[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
		
	}
}
#Toutes les cohortes poutes b�n�ficient de l'am�lioration. 
for (t in (T+7):(T+20)){#juste pour le test mettre +20 quand on voit que �a marche
	for (i in 1:5000){ 
		juv_tot_vichy[i,t] = (1/3) *juv_vichy[i,t-3]/improv_surv + (1/3) * juv_vichy[i,t-4]/improv_surv + (1/3) * juv_vichy[i,t-5]/improv_surv 
		juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3]/improv_surv + (1/3) * juv_alagnon[i,t-4]/improv_surv + (1/3) * juv_alagnon[i,t-5]/improv_surv
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3]/improv_surv + (1/3) * juv_langeac[i,t-4]/improv_surv + (1/3) * juv_langeac[i,t-5]/improv_surv 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3]+ (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5] 
		
		juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- juv_tot_alagnon[i,t] / (juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
		ratio_juv_A[i,t]<- bugs_rho_station[i]*(S_juv_JP[2,t]/(S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_A[i,t]
		L_ratio_juv_A[i,t] <- logit(ratio_juv_A[i,t])
		L_mu_p_alagnon[i,t]<-L_ratio_juv_A[i,t] #+bugs_adjust_p_A[i]
		L_p_alagnon[i,t]<-rnorm(1,L_mu_p_alagnon[i,t],bugs_sigma_p_alagnon[i]) 
		res_p_alagnon[i,t] <- L_p_alagnon[i,t]-L_mu_p_alagnon[i,t]
		
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*((S_juv_JP[3,t]+S_juv_JP[4,t])/(S_juv_JP[1,t]+S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
			if(N_vichy[i,t]<3){N_vichy[i,t]=3}
			if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		p_alagnon[i,t]=inv.logit(L_p_alagnon[i,t])	
		N_alagnon[i,t]=rbinom(1,round(N_vichy[i,t],0),p_alagnon[i,t])
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t]-N_alagnon[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
			if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_alagnon[i,t]=max(N_alagnon[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_alagnon[i,t]-N_langeac[i,t],1)
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_alagnon[i,t+1]=log( (S_alagnon[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_alagnon[i,t]/S_juv_JP[2,t]) )) + bugs_nu_d[i,2]
		L_d_moy_alagnon[i,t+1]=rnorm(1,L_mu_d_wild_alagnon[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_alagnon[i,t+1]=exp(L_d_moy_alagnon[i,t+1])
		juv_alagnon[i,t+1]=d_moy_alagnon[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log( (S_langeac[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[3,t]) )) + bugs_nu_d[i,3]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[3,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log( (S_poutes[i,t]/S_juv_JP[4,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[4,t]) )) + bugs_nu_d[i,4]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[4,t+1]
	}
}

save.image(file = "C:/Users/utilisateur/Desktop/Marion/2017_03_23_ContinuiteEcologique_2017_05_04.RData")

load("C:/Users/utilisateur/Desktop/Marion/2017_03_23_ContinuiteEcologique_2017_05_04.RData")


#==========================================================
# SIMULATION SUR LES 20 ANS AVEC CES VARIABLES RECALCULEES
#==========================================================

#==========================================
# CHAP : Figure : TotalReturns_proj20years
#==========================================

# Graph projection 20 years
bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#4 car il y a 17 ann�e de suivi station (soit T+20 - 15)

#Attention � l'ann�e 30 estimation des passages Vichy car ann�e jug�e incompl�te
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
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_03_23_4zones_Interaction/50poutes_TotalReturns_proj20years_2017_04_28.png",width=800,height=800)
#png(filename="C:/Users/utilisateur/Desktop/Marion/img/2017_03_23_4zones_Interaction/ContinuiteEcologique_TotalReturns_proj20years_2017_05_04.png",width=800,height=800)
#png(filename="C:/Users/utilisateur/Desktop/Marion/img/2017_05_04_4zones_Interaction_ss_rho_poutes/ContinuiteEcologique_TotalReturns_proj20years_2017_05_04.png",width=800,height=800)
png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/ContinuiteEcologique_TotalReturns_proj20years_2017_12_12.png",width=800,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="20 years projection without stocking - Improvement of upstream and downstream migration",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T,1974+T+10,1974+T+20)
axis(1,at = c(1,6,16,26,T,T+10,T+20),
		labels=lab1,
		cex.axis = 1.2,las = 1,lwd=2,col = "black")


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


#data_vichy=c(
#		NA,NA,NA,NA,NA,
#		NA,NA,NA,NA,NA,
#		NA,NA,NA,NA,NA,
#		NA,NA,NA,NA,NA,
#		NA,NA,393,267,515,
#		380,400,541,1238,NA,#662,
#		510,950,572,421,491,
#		227,755,861,819,595,1177)

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

#==================================
# CHAP : Figure : Threshold
#==================================
#bugs_N_vichy=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")	

under_10_vichy=array(0,dim=c(8000,20))
under_50_vichy=array(0,dim=c(8000,20))
under_100_vichy=array(0,dim=c(8000,20))
under_250_vichy=array(0,dim=c(8000,20))
under_500_vichy=array(0,dim=c(8000,20))


for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(N_vichy[i,t] < 10){under_10_vichy[i,(t-T)]=1}  
		if(N_vichy[i,t] < 50){under_50_vichy[i,(t-T)]=1}
		if(N_vichy[i,t] < 100){under_100_vichy[i,(t-T)]=1}
		if(N_vichy[i,t] < 250){under_250_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 500){under_500_vichy[i,(t-T)]=1}
		
	}
}


p_under_10_vichy=rep(0,20)
p_under_50_vichy=rep(0,20)
p_under_100_vichy=rep(0,20)
p_under_250_vichy=rep(0,20)
p_under_500_vichy=rep(0,20)


for (t in 1:20){
	p_under_10_vichy[t]=mean(under_10_vichy[,t])
	p_under_50_vichy[t]=mean(under_50_vichy[,t])
	p_under_100_vichy[t]=mean(under_100_vichy[,t])
	p_under_250_vichy[t]=mean(under_250_vichy[,t])
	p_under_500_vichy[t]=mean(under_500_vichy[,t])
	
}

#------------------------
# Graph
#-----------------------
#png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_03_23_4zones_Interaction/50poutes_Threshold_2017_04_28.png",width=800,height=800)
#png(filename="C:/Users/utilisateur/Desktop/Marion/img/2017_03_23_4zones_Interaction/ContinuiteEcologique_Threshold_2017_05_04.png",width=800,height=800)
#png(filename="C:/Users/utilisateur/Desktop/Marion/img/2017_05_04_4zones_Interaction_ss_rho_poutes/ContinuiteEcologique_Threshold_2017_05_04.png",width=800,height=800)
png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/ContinuiteEcologique_Threshold_2017_12_12.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="Improvement of upstream and downstream migration")

# trace l'axe des ordonn�es
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


legend(15,1,legend=c(expression(p^treshold < 500),expression(p^treshold < 250),expression(p^treshold < 100),expression(p^treshold < 50)), #,expression(p^treshold < 250),expression(p^treshold < 100),expression(p^treshold < 50),expression(p^treshold < 10)),
		pch=c(16,16,16),#,16,16)
		col=c("grey45","grey55","grey65","grey75"),#"grey75","grey85")
		bty="n" )


dev.off()



#=========================================================================
# CHAP : Figure : R�partitions des Juv�niles dans les diff�rents secteurs
#=========================================================================
#Mod�le 2017.03.23_4zones_Interaction - interaction r�ciproque juv sauvage/d�vers�
setwd("C:/Users/utilisateur/Desktop/Marion/CODA/2017_03_23_4zones_Interaction_new/")

library(coda)
library(boot)
T=42

surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)	

#----------------------------------
# PARTIE SERIE TEMPORELLE

dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_A=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)

juv_tot=array(rep(0,5000*T),dim=c(5000,T))#296000
juv_tot_q=array(rep(0,T*5),dim=c(T,5))

for (t in 2:12){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1,t] + dmoy_tot_A[,t-1] * S_juv_JP[2,t] + dmoy_tot_L[,t-1] * S_juv_JP[3,t]
}
for (t in 13:T){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1,t] + dmoy_tot_A[,t-1] * S_juv_JP[2,t] + dmoy_tot_L[,t-1] *S_juv_JP[3,t] + dmoy_tot_P[,t-12] * S_juv_JP[4,t]	
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
	juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[1,t]
	juv_A[,t]<-dmoy_tot_A[,t-1] * S_juv_JP[2,t]
	juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[3,t]
}

for (t in 13:T){
	juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[1,t]
	juv_A[,t]<-dmoy_tot_A[,t-1] * S_juv_JP[2,t]
	juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[3,t]
	juv_P[,t]<-dmoy_tot_P[,t-12] * S_juv_JP[4,t]
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

#v�rif ratio =1
ratio_juv_tot_q=array(0,dim=c(5,T+20))
ratio_juv_tot_q3=array(0,dim=c(1,T+20))

for (t in 1:T){
  ratio_juv_tot_q[1,t]=ratio_juv_V_q[t,1]+ratio_juv_A_q[t,1]+ratio_juv_L_q[t,1]+ratio_juv_P_q[t,1]
  ratio_juv_tot_q[2,t]=ratio_juv_V_q[t,2]+ratio_juv_A_q[t,2]+ratio_juv_L_q[t,2]+ratio_juv_P_q[t,2]
  ratio_juv_tot_q[3,t]=ratio_juv_V_q[t,3]+ratio_juv_A_q[t,3]+ratio_juv_L_q[t,3]+ratio_juv_P_q[t,3]
  ratio_juv_tot_q[4,t]=ratio_juv_V_q[t,4]+ratio_juv_A_q[t,4]+ratio_juv_L_q[t,4]+ratio_juv_P_q[t,4]
  ratio_juv_tot_q[5,t]=ratio_juv_V_q[t,5]+ratio_juv_A_q[t,5]+ratio_juv_L_q[t,5]+ratio_juv_P_q[t,5]
}

for (t in 1:T){
  ratio_juv_tot_q3[t]=ratio_juv_V_q[t,3]+ratio_juv_A_q[t,3]+ratio_juv_L_q[t,3]+ratio_juv_P_q[t,3]
}

max(ratio_juv_tot_q3)
#----------------------------------
# PARTIE PROJECTION

#R�cup�ration des caluls r�alis�s dans le cadre de la mod�lisation ContinuiteEcologique
  #load("C:/Users/utilisateur/Desktop/Marion/2017_03_23_ContinuiteEcologique_2017_05_04.RData")
  #load("C:/Users/utilisateur/Desktop/Marion/2017_05_03_ss_rho_poutes_ContinuiteEcologique_2017_05_04.RData")



#les juv_vichy,alagnon,langeac,poutes du load ne commencent qu'� t=43 (T+2) on recr�� le t=42 (T+1) qu'on a calcul� au d�but
#du script avec bugs_juv_vichy,langeac,alagnon, poutes

#for (t in (T+1):(T+1)){
#  juv_vichy[,t]<-bugs_dmoy_tot_V[1:5000,t-1] * S_juv_JP[1,t]
#	juv_alagnon[,t]<-bugs_dmoy_tot_A[1:5000,t-1] * S_juv_JP[2,t]
#	juv_langeac[,t]<-bugs_dmoy_tot_L[1:5000,t-1] * S_juv_JP[3,t]
#	juv_poutes[,t]<-bugs_dmoy_tot_P[1:5000,t-12] * S_juv_JP[4,t]
#}

for (t in (T+1):(T+1)){
  juv_vichy[,t]<-bugs_juv_vichy[,t]
  juv_alagnon[,t]<-bugs_juv_alagnon[,t]
  juv_langeac[,t]<-bugs_juv_langeac[,t]
  juv_poutes[,t]<-bugs_juv_poutes[,t]
}


#on calcule un juv_total qui somme l'ensemble
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

#v�rif ratio =1

for (t in (T+1):(T+20)){
  ratio_juv_tot_q3[t]=ratio_juv_vichy_q[t,3]+ratio_juv_alagnon_q[t,3]+ratio_juv_langeac_q[t,3]+ratio_juv_poutes_q[t,3]
}

max(ratio_juv_tot_q3)

#-------------------------------------------
# FIGURE

#png(file="C:/Users/utilisateur/Desktop/Marion/img/2017_03_23_4zones_Interaction/RepartitionSpatialeJuv_ContinuiteEcologique_2017_05_04.png",width=800, height=1500, units = "px",type="cairo")
#png(file="C:/Users/utilisateur/Desktop/Marion/img/2017_05_04_4zones_Interaction_ss_rho_poutes/RepartitionSpatialeJuv_ContinuiteEcologique_2017_05_04.png",width=800, height=1500, units = "px",type="cairo")
png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/RepartitionSpatialeJuv_ContinuiteEcologique_2017_12_12.png",width=800, height=1500, units = "px",type="cairo")

par(mfrow=c(5,1))

#----------------------------
#Juv totaux tous secteurs

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,400000),ylab="",main="total 0+ juvenile production (wild + stocked)")

# trace l'axe des ordonn�es
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

##Partie s�rie temporelle
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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

##Partie s�rie temporelle

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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

##Partie s�rie temporelle
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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")


##Partie s�rie temporelle
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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

##Partie s�rie temporelle
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
# CHAP : Figure : R�partitions des G�niteurs dans les diff�rents secteurs
#=========================================================================
#Mod�le 2017.03.23_4zones_Interaction - interaction r�ciproque juv sauvage/d�vers�
setwd("C:/Users/utilisateur/Desktop/Marion/CODA/2017_03_23_4zones_Interaction_new/")

library(coda)
library(boot)
T=41

surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)	

#----------------------------------
# PARTIE SERIE TEMPORELLE

S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_alagnon_real=read.coda("S_alagnonCODAchain1.txt","S_alagnonCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")

#On r�cup�re les donn�es du mod�le directement dans le fichier data
#library(coda) 
#require(stringr)
#bugs2jags(str_c("C:/Users/utilisateur/Desktop/Marion/CODA/2017_03_23_4zones_Interaction_new/","data.txt"),"data_4zones_Interaction.R")
#source("data_4zones_Interaction.R")
#head(N) #�a marche !

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

#R�cup�ration des caluls r�alis�s dans le cadre de la mod�lisation ContinuiteEcologique
  #load("C:/Users/utilisateur/Desktop/Marion/2017_03_23_ContinuiteEcologique_2017_05_04.RData")
  load("C:/Users/utilisateur/Desktop/Marion/2017_05_03_ss_rho_poutes_ContinuiteEcologique_2017_05_04.RData")

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

#V�rif
max(ratio_S_V_q[,3])
max(ratio_S_vichy_q[,3])

#---------------------------------------------------
# FIGURE

#png(file="C:/Users/utilisateur/Desktop/Marion/img/2017_03_23_4zones_Interaction/RepartitionSpatialeGen_ContinuiteEcologique_2017_05_04.png",width=800, height=1500, units = "px",type="cairo")
#png(file="C:/Users/utilisateur/Desktop/Marion/img/2017_05_04_4zones_Interaction_ss_rho_poutes/RepartitionSpatialeGen_ContinuiteEcologique_2017_05_04.png",width=800, height=1500, units = "px",type="cairo")
png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/RepartitionSpatialeGen_ContinuiteEcologique_2017_12_12.png",width=800, height=1500, units = "px",type="cairo")

par(mfrow=c(5,1))
#..........
# Nb total
#..........
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,4000),ylab="",main="Total number of spawners")

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000),labels=c(0,1000,2000,3000,4000),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie s�rie temporelle
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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")


##Partie s�rie temporelle
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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie s�rie temporelle
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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie s�rie temporelle
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

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,2010,1974+T,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,6,16,26,36,T,T+5,T+10,T+15,T+20),
		labels=lab1,
		las = 1,col = "grey55")

##Partie s�rie temporelle

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




