# SIMULATION AMENAGEMENT POUTES 50%
# 
# Author: marion.legrand
###############################################################################
# TOUTES LES VARIABLES OPENBUGS UTILISEES DIRECTEMENT SONT PRECEDEES D'UN "bugs_" POUR LES DIFFERENCIER DES VARIABLES RECALCULER DANS R
#================
# CHARGEMENT
#================
#Poutès 2014_10_15
#Poutès 2014_12_05 A tourné avec MAJ données sur 2004 + correction sur indicatrice
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_05_Poutes/")

library(coda)
library(boot)
T=39


surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)

#=============================================================================================================================
# AMELIORATION DE POUTES SUR LA SURVIE DES JUVENILES : diminution par 2 de la mortalité)
# AMELIORATION DE LA FRANCHISSABILITE p_adjust_P 
# (pour l'instant on laisse sigma_p_poutes tel quel en attente de l'avis de JMB sur l'évolution des Q dans le vieil Allier)
#=============================================================================================================================
bugs_rho_poutes=read.coda("rho_poutesCODAchain1.txt","rho_poutesCODAindex.txt")
bugs_ratio_juv_prod_L=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")
bugs_rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
bugs_adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
bugs_adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")
bugs_sigma_p_langeac=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
bugs_sigma_p_poutes=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")
bugs_sigma_vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")
#bugs_res_p_poutes=read.coda("simulation/res_p_poutesCODAchain1.txt","simulation/res_p_poutesCODAindex.txt")
bugs_I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt")
bugs_s_juv2ad=read.coda("simulation/s_juv2adCODAchain1.txt","simulation/s_juv2adCODAindex.txt")
bugs_alpha_dd=read.coda("simulation/alpha_ddCODAchain1.txt","simulation/alpha_ddCODAindex.txt")
bugs_beta_dd=read.coda("simulation/beta_ddCODAchain1.txt","simulation/beta_ddCODAindex.txt")
bugs_nu_wild=read.coda("simulation/nu_wildCODAchain1.txt","simulation/nu_wildCODAindex.txt")
bugs_res_wild_vichy=read.coda("simulation/res_wild_vichyCODAchain1.txt","simulation/res_wild_vichyCODAindex.txt")
bugs_res_wild_langeac=read.coda("simulation/res_wild_langeacCODAchain1.txt","simulation/res_wild_langeacCODAindex.txt")
bugs_res_wild_poutes=read.coda("simulation/res_wild_poutesCODAchain1.txt","simulation/res_wild_poutesCODAindex.txt")
bugs_dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt")
bugs_dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt")
bugs_dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt")
bugs_res_wild_vichy=read.coda("res_wild_VCODAchain1.txt","res_wild_VCODAindex.txt")
bugs_res_wild_langeac=read.coda("res_wild_LCODAchain1.txt","res_wild_LCODAindex.txt")
bugs_res_wild_poutes=read.coda("res_wild_PCODAchain1.txt","res_wild_PCODAindex.txt")

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
L_p_langeac=array(0,dim=c(5000,T+20))
L_p_poutes=array(0,dim=c(5000,T+20))
res_vichy=array(0,dim=c(5000,T+20))
res_p_langeac=array(0,dim=c(5000,T+20))
res_p_poutes=array(0,dim=c(5000,T+20))#On le recalcule pour t>T car extrait les CODA que sur 1:39 dans script_openbugs
p_langeac=array(0,dim=c(5000,T+20))
p_poutes=array(0,dim=c(5000,T+20))
N_vichy=array(0,dim=c(5000,T+20))
N_vichy_temp=array(0,dim=c(5000,T+20))
N_langeac=array(0,dim=c(5000,T+20))
N_poutes=array(0,dim=c(5000,T+20))
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
#On recalcule tous les juvéniles car la modification dans la distribution spatiale des géniteurs peut
#modifier les densités de juvéniles des différentes zones

#Pour avoir les premiers juv_tot sans les recalculer car fout la zone dans R car en a besoin pour calculer la suite
for(t in 34:T){
	for (i in 1:5000){ 
	bugs_juv_vichy[i,t+1]=bugs_dmoy_tot_V[i,t+1]*S_juv_JP[1,t+1]
	bugs_juv_langeac[i,t+1]=bugs_dmoy_tot_L[i,t+1]*S_juv_JP[2,t+1]
	bugs_juv_poutes[i,t+1]=bugs_dmoy_tot_P[i,t+1]*S_juv_JP[3,t+1]
	}
}

#Effet sur les juvéniles au fur et à mesure
#n'impacte pas les 3 dernières années de production (car antérieures à l'année d'amélioration)

for (t in (T+1):(T+3)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		#N_vichy[i,t]<-dlnorm(L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		N_vichy_temp[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]))
		res_vichy[i,t] <- log(N_vichy_temp[i,t])-L_mu_Vichy_nm[i,t]
		N_vichy[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t])# +res_vichy[i,t-6])
				
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		#L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-dnorm(L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t])+bugs_adjust_p_L[i]+res_p_langeac[i,t] )	
		N_langeac[i,t]=max(p_langeac[i,t]*N_vichy[i,t],1)
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		#L_ratio_juv_P[i,t] <- logit(ratio_juv_P[t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-dnorm(L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		p_poutes[i,t]=inv.logit(logit(ratio_juv_P[i,t])+bugs_adjust_p_P[i]/2+res_p_poutes[i,t] )	
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces années là
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(bugs_nu_wild[i,1])) ))#+ bugs_res_wild_vichy[i,t] ))
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		#juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(bugs_nu_wild[i,2])) ))#+ bugs_res_wild_langeac[i,t] ))
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		#juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(bugs_nu_wild[i,3])) ))#+ bugs_res_wild_poutes[i,t] ))
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		#juv_tot_poutes[i,t] <- bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-5]
		#juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
	}
}

#Amélioration sur le premier tiers de production en amont de Poutès
for (t in (T+4):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = ((1+bugs_rho_poutes[i])/2)*(1/3) * bugs_juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		#N_vichy[i,t]<-dlnorm(L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		N_vichy_temp[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]))
		res_vichy[i,t] <- log(N_vichy_temp[i,t])-L_mu_Vichy_nm[i,t]
		N_vichy[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t])+res_vichy[i,t])
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		#L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-dnorm(L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t])+bugs_adjust_p_L[i]+res_p_langeac[i,t] )	
		N_langeac[i,t]=max(p_langeac[i,t]*N_vichy[i,t],1)
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		#L_ratio_juv_P[i,t] <- logit(ratio_juv_P[t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-dnorm(L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		p_poutes[i,t]=inv.logit(logit(ratio_juv_P[i,t])+bugs_adjust_p_P[i]/2+res_p_poutes[i,t] )	
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces années là
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(bugs_nu_wild[i,1])) ))#+ bugs_res_wild_vichy[i,t] ))
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		#juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(bugs_nu_wild[i,2])) ))#+ bugs_res_wild_langeac[i,t] ))
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		#juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(bugs_nu_wild[i,3])) ))#+ bugs_res_wild_poutes[i,t] ))
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		#juv_tot_poutes[i,t] <- bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-5]
		#juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
	}
}


#Amélioration sur les deux premiers tiers de production en amont de Poutès.On utilise les données de juvéniles de bugs pour t-4 et t-5
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = ((1+bugs_rho_poutes[i])/2)*(1/3) * juv_poutes[i,t-3] + ((1+bugs_rho_poutes[i])/2)*(1/3) * bugs_juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		#N_vichy[i,t]<-dlnorm(L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		N_vichy_temp[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]))
		res_vichy[i,t] <- log(N_vichy_temp[i,t])-L_mu_Vichy_nm[i,t]
		N_vichy[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t])+res_vichy[i,t])
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		#L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-dnorm(L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t])+bugs_adjust_p_L[i]+res_p_langeac[i,t] )	
		N_langeac[i,t]=max(p_langeac[i,t]*N_vichy[i,t],1)
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		#L_ratio_juv_P[i,t] <- logit(ratio_juv_P[t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-dnorm(L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		p_poutes[i,t]=inv.logit(logit(ratio_juv_P[i,t])+bugs_adjust_p_P[i]/2+res_p_poutes[i,t] )	
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces années là
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(bugs_nu_wild[i,1])) ))#+ bugs_res_wild_vichy[i,t] ))
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		#juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(bugs_nu_wild[i,2])) ))#+ bugs_res_wild_langeac[i,t] ))
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		#juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(bugs_nu_wild[i,3])) ))#+ bugs_res_wild_poutes[i,t] ))
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		#juv_tot_poutes[i,t] <- bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-5]
		#juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
	}
}

#Amélioration pour l'ensemble de la production de juvéniles en amont de Poutès. On utilise les données de juvéniles de bugs pour t-5
for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5] 
		bugs_juv_tot_poutes[i,t] = ((1+bugs_rho_poutes[i])/2)*(1/3) * juv_poutes[i,t-3] + ((1+bugs_rho_poutes[i])/2)*(1/3) * juv_poutes[i,t-4] + ((1+bugs_rho_poutes[i])/2)*(1/3) * bugs_juv_poutes[i,t-5] 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		#N_vichy[i,t]<-dlnorm(L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		N_vichy_temp[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]))
		res_vichy[i,t] <- log(N_vichy_temp[i,t])-L_mu_Vichy_nm[i,t]
		N_vichy[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t])+res_vichy[i,t])
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		#L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-dnorm(L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t])+bugs_adjust_p_L[i]+res_p_langeac[i,t] )	
		N_langeac[i,t]=max(p_langeac[i,t]*N_vichy[i,t],1)
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		#L_ratio_juv_P[i,t] <- logit(ratio_juv_P[t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-dnorm(L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		p_poutes[i,t]=inv.logit(logit(ratio_juv_P[i,t])+bugs_adjust_p_P[i]/2+res_p_poutes[i,t] )	
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces années là
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(bugs_nu_wild[i,1])) ))#+ bugs_res_wild_vichy[i,t] ))
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		#juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(bugs_nu_wild[i,2])) ))#+ bugs_res_wild_langeac[i,t] ))
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		#juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(bugs_nu_wild[i,3])) ))#+ bugs_res_wild_poutes[i,t] ))
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		#juv_tot_poutes[i,t] <- bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-5]
		#juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
	}
}
#Toutes les cohortes poutes bénéficient de l'amélioration. 
for (t in (T+7):(T+20)){#juste pour le test mettre +20 quand on voit que ça marche
	for (i in 1:5000){ 
		juv_tot_vichy[i,t] = (1/3) *juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = ((1+bugs_rho_poutes[i])/2)*(1/3) * juv_poutes[i,t-3] + ((1+bugs_rho_poutes[i])/2)*(1/3) * juv_poutes[i,t-4] + ((1+bugs_rho_poutes[i])/2)*(1/3) * juv_poutes[i,t-5] 
		
		juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
		#N_vichy[i,t]<-dlnorm(L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		N_vichy_temp[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*juv_tot_system[i,t]))
		res_vichy[i,t] <- log(N_vichy_temp[i,t])-L_mu_Vichy_nm[i,t]
		N_vichy[i,t]= exp(log(exp(bugs_level_s[i]*bugs_I_surv[i,t-7])*bugs_s_juv2ad[i]*juv_tot_system[i,t])+res_vichy[i,t])
		
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		#L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-dnorm(L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t])+bugs_adjust_p_L[i]+res_p_langeac[i,t] )	
		N_langeac[i,t]=max(p_langeac[i,t]*N_vichy[i,t],1)
		
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		#L_ratio_juv_P[i,t] <- logit(ratio_juv_P[t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i]/2 
		L_p_poutes[i,t]<-dnorm(L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		p_poutes[i,t]=inv.logit(logit(ratio_juv_P[i,t])+bugs_adjust_p_P[i]/2+res_p_poutes[i,t] )	
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces années là
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(bugs_nu_wild[i,1])) ))#+ bugs_res_wild_vichy[i,t] ))
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		#juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(bugs_nu_wild[i,2])) ))#+ bugs_res_wild_langeac[i,t] ))
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		#juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (bugs_alpha_dd[i] + bugs_beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(bugs_nu_wild[i,3])) ))#+ bugs_res_wild_poutes[i,t] ))
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		#juv_tot_poutes[i,t] <- bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + bugs_rho_poutes[i]*(1/3) * juv_poutes[i,t-5]
		#juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
	}
}

save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2014.12.05_OuverturePoutes.RData")
load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/2014.12.05_OuverturePoutes.RData")

#On agglomère les 2 matrix juv_tot_ et bugs_juv_tot_ pour avoir un tableau complet sur la période
juv_tot_vichy_tot=array(0,dim=c(5000,T+20))
juv_tot_langeac_tot=array(0,dim=c(5000,T+20))
juv_tot_poutes_tot=array(0,dim=c(5000,T+20))
juv_tot_system_tot=array(0,dim=c(5000,T+20))

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		juv_tot_vichy_tot[i,t]=juv_tot_vichy[i,t]+bugs_juv_tot_vichy[i,t]
}
}
for (t in (T+1):(T+20)){
	for (i in 1:5000){
		juv_tot_langeac_tot[i,t]=juv_tot_langeac[i,t]+bugs_juv_tot_langeac[i,t]
	}
}
for (t in (T+1):(T+20)){
	for (i in 1:5000){
		juv_tot_poutes_tot[i,t]=juv_tot_poutes[i,t]+bugs_juv_tot_poutes[i,t]
	}
}
for (t in (T+1):(T+20)){
	for (i in 1:5000){
		juv_tot_system_tot[i,t]=juv_tot_system[i,t]+bugs_juv_tot_system[i,t]
	}
}
juv_poutes_tot=array(0,dim=c(5000,T+20))
for (t in (T+1):(T+20)){
	for (i in 1:5000){
		juv_poutes_tot[i,t]=juv_poutes[i,t]+bugs_juv_poutes[i,t]
	}
}
#==========================================================
# SIMULATION SUR LES 20 ANS AVEC CES VARIABLES RECALCULEES
#==========================================================

#==========================================
# CHAP : Figure : TotalReturns_proj20years
#==========================================

# Graph projection 20 years
bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 année de suivi station (soit T+20 - 15)

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
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2014_12_05_Poutes50/poutes_TotalReturns_proj20years.png",width=800,height=800)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="20 years projection without stocking - Improvement of upstream and downstream migration at Poutes of 50%",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,46,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
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


data_vichy=c(
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,393,267,515,
		380,400,541,1238,NA,#662,
		510,950,572,421,491,
		227,755,861,819)
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
bugs_N_vichy=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")	

under_10_vichy=array(0,dim=c(8000,20))
under_50_vichy=array(0,dim=c(8000,20))
under_100_vichy=array(0,dim=c(8000,20))
under_250_vichy=array(0,dim=c(8000,20))
under_500_vichy=array(0,dim=c(8000,20))


for (t in 24:43){
	
	for (i in 1:5000){
		if(bugs_N_vichy[i,t] < 10){under_10_vichy[i,t-23]=1}  
		if(bugs_N_vichy[i,t] < 50){under_50_vichy[i,t-23]=1}
		if(bugs_N_vichy[i,t] < 100){under_100_vichy[i,t-23]=1}
		if(bugs_N_vichy[i,t] < 250){under_250_vichy[i,t-23]=1}
		if(bugs_N_vichy[i,t] < 500){under_500_vichy[i,t-23]=1}
		
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
#------------------------
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2014_12_05_Poutes50/poutes_Threshold.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="Improvement of upstream and downstream migration at Poutes of 50%")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,9,20),
		labels=c(2014,2020,2033),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,20,1)



points(x,p_under_10_vichy,col="grey85",pch=16)
segments(x[1:19],p_under_10_vichy[1:19],x[2:20],p_under_10_vichy[2:20],col="grey85")

points(x,p_under_50_vichy,col="grey75",pch=16)
segments(x[1:19],p_under_50_vichy[1:19],x[2:20],p_under_50_vichy[2:20],col="grey75")

points(x,p_under_100_vichy,col="grey65",pch=16)
segments(x[1:19],p_under_100_vichy[1:19],x[2:20],p_under_100_vichy[2:20],col="grey65")

points(x,p_under_250_vichy,col="grey55",pch=16)
segments(x[1:19],p_under_250_vichy[1:19],x[2:20],p_under_250_vichy[2:20],col="grey55")

points(x,p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],p_under_500_vichy[1:19],x[2:20],p_under_500_vichy[2:20],col="grey45")


legend(15,1,legend=c(expression(p^treshold < 500),expression(p^treshold < 250),expression(p^treshold < 100),expression(p^treshold < 50),expression(p^treshold < 10)),
		pch=c(16,16,16,16,16),col=c("grey45","grey55","grey65","grey75","grey85"),bty="n" )


dev.off()


##############################################################
# Projection sur 20 ans du nbr de géniteurs et juvéniles
##############################################################

#===========================================================
# Projection sur 20 ans des géniteurs (en nombre et ratio)
#===========================================================
	#--------------------------------------------------------------------------
	# Récupération des données sur la première partie (1 à T) depuis OpenBugs
	#--------------------------------------------------------------------------
		#...................
		# Nbr de géniteurs
		#...................
S_vichy_real=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_05_Poutes/S_vichyCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_05_Poutes/S_vichyCODAindex.txt")
S_langeac_real=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_05_Poutes/S_langeacCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_05_Poutes/S_langeacCODAindex.txt")

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
	#Recalcul des nombres de géniteurs
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
png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2014_12_05_Poutes50/poutes50_SpawnersRedds_GeniteursPotentiels.png",width=800, height=800, units = "px",type="cairo")

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

