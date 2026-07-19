###############################################################################
# SIMULATION AMELIORATION DEVALAISON SUR TOUT LE SECTEUR DU MODELE 
#
# Author: marion.legrand
###############################################################################
# TOUTES LES VARIABLES OPENBUGS UTILISEES DIRECTEMENT SONT PRECEDEES D'UN "bugs_" POUR LES DIFFERENCIER DES VARIABLES RECALCULEES DANS R
#================
# CHARGEMENT
#================


#Mod�le 2017_12_20_Devalaison_4zones_Interaction_ss_rho_poutes_MatriceVC
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_12_20_Devalaison_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/") #on laisse le fichier data du modèle Standard 2017 qui est 2017_08_29


library(coda)
library(boot)
T=42

surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-2))) #T-2 pour calculer sur T+27 ann�e
S_juv_JP<-matrix(surf,nrow=4)	



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# AMELIORATION DEVALAISON 100%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#=================================================================================================================================================
# AMELIORATION DES 11 OUVRAGES SUR LA SURVIE DES JUVENILES : diminution de 100% de la mortalit� -->on supprime le facteur de mortalit�
# ON CONSERVE L'IMPACT DES OUVRAGES A LA MONTAISON (con conserve p_adjust_L & p_adjust_P) 
# (pour l'instant on laisse sigma_p_poutes tel quel en attente de l'avis de JMB sur l'�volution des Q dans le vieil Allier)
#==================================================================================================================================================
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
bugs_I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt")
bugs_s_juv2ad=read.coda("simulation/s_juv2adCODAchain1.txt","simulation/s_juv2adCODAindex.txt")
bugs_alpha_dd=read.coda("simulation/alpha_ddCODAchain1.txt","simulation/alpha_ddCODAindex.txt")
bugs_beta_dd=read.coda("simulation/beta_ddCODAchain1.txt","simulation/beta_ddCODAindex.txt")
#bugs_nu_wild=read.coda("simulation/nu_wildCODAchain1.txt","simulation/nu_wildCODAindex.txt")
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

ratio_juv_prod_V=array(0,dim=c(5000,T+26))
ratio_juv_prod_A=array(0,dim=c(5000,T+26))
ratio_juv_prod_L=array(0,dim=c(5000,T+26))
ratio_juv_prod_P=array(0,dim=c(5000,T+26))
ratio_juv_A=array(0,dim=c(5000,T+26))
ratio_juv_L=array(0,dim=c(5000,T+26))
ratio_juv_P=array(0,dim=c(5000,T+26))
L_ratio_juv_A=array(0,dim=c(5000,T+26))
L_ratio_juv_L=array(0,dim=c(5000,T+26))
L_ratio_juv_P=array(0,dim=c(5000,T+26))
L_mu_Vichy_nm=array(0,dim=c(5000,T+26))
L_mu_p_alagnon=array(0,dim=c(5000,T+26))
L_mu_p_langeac=array(0,dim=c(5000,T+26))
L_mu_p_poutes=array(0,dim=c(5000,T+26))
L_mu_d_wild_vichy=array(0,dim=c(5000,T+27))
L_mu_d_wild_alagnon=array(0,dim=c(5000,T+27))
L_mu_d_wild_langeac=array(0,dim=c(5000,T+27))
L_mu_d_wild_poutes=array(0,dim=c(5000,T+27))
L_p_alagnon=array(0,dim=c(5000,T+26))
L_p_langeac=array(0,dim=c(5000,T+26))
L_p_poutes=array(0,dim=c(5000,T+26))
L_d_moy_vichy=array(0,dim=c(5000,T+27))
L_d_moy_alagnon=array(0,dim=c(5000,T+27))
L_d_moy_langeac=array(0,dim=c(5000,T+27))
L_d_moy_poutes=array(0,dim=c(5000,T+27))
res_vichy=array(0,dim=c(5000,T+26))
res_p_alagnon=array(0,dim=c(5000,T+26))
res_p_langeac=array(0,dim=c(5000,T+26))
res_p_poutes=array(0,dim=c(5000,T+26))
p_alagnon=array(0,dim=c(5000,T+26))
p_langeac=array(0,dim=c(5000,T+26))
p_poutes=array(0,dim=c(5000,T+26))
N_vichy=array(0,dim=c(5000,T+26))
N_vichy_temp=array(0,dim=c(5000,T+26))
N_alagnon=array(0,dim=c(5000,T+26))
N_langeac=array(0,dim=c(5000,T+26))
N_poutes=array(0,dim=c(5000,T+26))
min_N_langeac=array(0,dim=c(5000,T+26))
S_vichy=array(0,dim=c(5000,T+26))
S_alagnon=array(0,dim=c(5000,T+26))
S_langeac=array(0,dim=c(5000,T+26))
S_poutes=array(0,dim=c(5000,T+26))
d_moy_vichy=array(0,dim=c(5000,T+27))
d_moy_alagnon=array(0,dim=c(5000,T+27))
d_moy_langeac=array(0,dim=c(5000,T+27))
d_moy_poutes=array(0,dim=c(5000,T+27))
juv_vichy=array(0,dim=c(5000,T+27))
juv_alagnon=array(0,dim=c(5000,T+27))
juv_langeac=array(0,dim=c(5000,T+27))
juv_poutes=array(0,dim=c(5000,T+27))
juv_tot_vichy=array(0,dim=c(5000,T+26))
juv_tot_alagnon=array(0,dim=c(5000,T+26))
juv_tot_langeac=array(0,dim=c(5000,T+26))
juv_tot_poutes=array(0,dim=c(5000,T+26))
juv_tot_system=array(0,dim=c(5000,T+26))
bugs_juv_vichy_3=array(0,dim=c(5000,T+26))
bugs_juv_vichy_4=array(0,dim=c(5000,T+26))
bugs_juv_alagnon_5=array(0,dim=c(5000,T+26))
bugs_juv_alagnon_6=array(0,dim=c(5000,T+26))
bugs_juv_vichy_7=array(0,dim=c(5000,T+26))
bugs_juv_vichy_8=array(0,dim=c(5000,T+26))
bugs_juv_vichy_9=array(0,dim=c(5000,T+26))
bugs_juv_vichy_10=array(0,dim=c(5000,T+26))
bugs_juv_vichy_11=array(0,dim=c(5000,T+26))
bugs_juv_surv_Vall_restant=array(0,dim=c(5000,T+26))
bugs_juv_surv_ala_restant=array(0,dim=c(5000,T+26))
bugs_juv_surv_Vdor_restant=array(0,dim=c(5000,T+26))
bugs_juv_vichy=array(0,dim=c(5000,T+26))
bugs_juv_alagnon=array(0,dim=c(5000,T+26))
bugs_juv_langeac=array(0,dim=c(5000,T+26))
bugs_juv_poutes=array(0,dim=c(5000,T+26))
bugs_juv_tot_vichy=array(0,dim=c(5000,T+26))
bugs_juv_tot_alagnon=array(0,dim=c(5000,T+26))
bugs_juv_tot_langeac=array(0,dim=c(5000,T+26))
bugs_juv_tot_poutes=array(0,dim=c(5000,T+26))
bugs_juv_tot_system=array(0,dim=c(5000,T+26))
ratio_habitat=array(0,dim=c(4,T+26))

coef_juv_1<-array(rep(0,T*5000),dim=c(5000,T+25))
coef_juv_2<-array(rep(0,T*5000),dim=c(5000,T+25))
coef_juv_3<-array(rep(0,T*5000),dim=c(5000,T+25))
renew_rate=array(0,dim=c(5000,T+20))
renew_rate_q=array(0,dim=c((T+20),5))


##DANS SCENARIO SURF+PRODUCTIVITE
river=c(rep(c(0.77, 0.23),(T+26)))
ratio_river_V<-matrix(river,nrow=2)
##DANS SCENARIO SURF UNIQUEMENT
#river=c(rep(c(0.66, 0.34),(T+20)))
#ratio_river_V<-matrix(river,nrow=2)

interbar=c(rep(c(0, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),11),rep(c(1, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),18),
		rep(c(1, 1,0.0884, 0.281, 0.303, 0.174, 0.395, 0.029, 0.012, 0.06, 0.156),(T+20-23)))
ratio_interbar<-matrix(interbar,nrow=11)
rho_surv=c(0.678, 0.926, 0.916, 0.902, 0.847, 0.886, 0.838, 0.915, 0.907, 0.9, 0.853)

for (t in 1:(T+26)){
	for (i in 1:4){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t]+S_juv_JP[4,t])
	}
}
#On recalcule tous les juv�niles car la modification dans la distribution spatiale des g�niteurs peut
#modifier les densit�s de juv�niles des diff�rentes zones

#Pour avoir les premiers juv_tot sans les recalculer car fout la zone dans R car en a besoin pour calculer la suite
for(t in (T-4): (T+1)){
	for (i in 1:5000){ 
		bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t]
		bugs_juv_alagnon[i,t]=bugs_dmoy_tot_A[i,t-1]*S_juv_JP[2,t]
		bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[3,t]
		bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[4,t]
	}
}


#------------------------------------
#POUR DEVALAISON sans RHO_POUTES
#------------------------------------
#Effet sur les juv�niles au fur et à mesure
#n'impacte pas les 3 dernières ann�es de production (car ant�rieures à l'ann�e d'am�lioration)
#On commence � T-1 pour pouvoir calculer les diagnostiques de conservation d�s l'ann�e T+1
for (t in (T-1):(T+3)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_7[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]= bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_7[i,t]+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_alagnon_5[i,t]=((1/3) * bugs_juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5])*ratio_interbar[5,t]*prod(rho_surv[5:6])
		bugs_juv_alagnon_6[i,t]=((1/3) * bugs_juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5])*ratio_interbar[6,t]*prod(rho_surv[6:6])
		bugs_juv_surv_ala_restant[i,t]<-( (1/3) * bugs_juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5])*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_tot_alagnon[i,t]=bugs_juv_alagnon_5[i,t]+bugs_juv_alagnon_6[i,t]+bugs_juv_surv_ala_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = ((1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		#bugs_juv_tot_poutes[i,t] = ((1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*bugs_rho_poutes[i]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
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
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
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



#Am�lioration sur le premier tiers de production. On ne calcule une survie que pour les 2/3 restant (le premier tiers survivant entièrement)
#Attention il faut remettre sur le premier tiers le ratio_river et ratio_interbar pour le secteur Vichy. Pas la peine pour les 2 autres car ratio-interbar=1
for (t in (T+4):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[3,t] +((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[4,t] +((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_7[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[7,t]+((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[8,t]+((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[9,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[10,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[11,t]  + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]= bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_7[i,t]+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_alagnon_5[i,t]=(1/3) * bugs_juv_alagnon[i,t-3]*ratio_interbar[5,t] + ((1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5])*ratio_interbar[5,t]*prod(rho_surv[5:6])
		bugs_juv_alagnon_6[i,t]=(1/3) * bugs_juv_alagnon[i,t-3]*ratio_interbar[6,t] + ((1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5])*ratio_interbar[6,t]*prod(rho_surv[6:6])
		bugs_juv_surv_ala_restant[i,t]<-( (1/3) * bugs_juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5])*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_tot_alagnon[i,t]=bugs_juv_alagnon_5[i,t]+bugs_juv_alagnon_6[i,t]+bugs_juv_surv_ala_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + ((1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + ((1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
#		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + ((1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*bugs_rho_poutes[i]*prod(rho_surv[2:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
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
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
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


#Am�lioration sur les deux premiers tiers de production.On utilise les donn�es de juv�niles de bugs pour t-4 et t-5
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[3,t] + (1/3) * bugs_juv_vichy[i,t-5] *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[4,t] + (1/3) * bugs_juv_vichy[i,t-5] *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_7[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[7,t]+ (1/3) * bugs_juv_vichy[i,t-5] *ratio_river_V[2,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[8,t]+(1/3) * bugs_juv_vichy[i,t-5] *ratio_river_V[2,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[9,t] + (1/3) * bugs_juv_vichy[i,t-5] *ratio_river_V[2,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[10,t] + (1/3) * bugs_juv_vichy[i,t-5] *ratio_river_V[2,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[11,t]  + (1/3) * bugs_juv_vichy[i,t-5] *ratio_river_V[2,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]= bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_7[i,t]+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_alagnon_5[i,t]=((1/3) * juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4])*ratio_interbar[5,t] + (1/3) * bugs_juv_alagnon[i,t-5]*ratio_interbar[5,t]*prod(rho_surv[5:6])
		bugs_juv_alagnon_6[i,t]=((1/3) * juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4])*ratio_interbar[6,t] + (1/3) * bugs_juv_alagnon[i,t-5]*ratio_interbar[6,t]*prod(rho_surv[6:6])
		bugs_juv_surv_ala_restant[i,t]<-( (1/3) * juv_alagnon[i,t-3] + (1/3) * bugs_juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5])*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_tot_alagnon[i,t]=bugs_juv_alagnon_5[i,t]+bugs_juv_alagnon_6[i,t]+bugs_juv_surv_ala_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
#		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]*bugs_rho_poutes[i]*prod(rho_surv[2:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
	
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
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
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
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


#Am�lioration pour l'ensemble de la production de juv�niles. On utilise les donn�es de juv�niles de bugs pour t-5
for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5] 
		bugs_juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3] + (1/3) * juv_alagnon[i,t-4] + (1/3) * bugs_juv_alagnon[i,t-5] 
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- bugs_juv_tot_alagnon[i,t] / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_alagnon[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
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
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
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
#Toutes les cohortes poutes b�n�ficient de l'am�lioration. 
for (t in (T+7):(T+26)){
	for (i in 1:5000){ 
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_alagnon[i,t] = (1/3) * juv_alagnon[i,t-3] + (1/3) * juv_alagnon[i,t-4] + (1/3) * juv_alagnon[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5]
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]
		
		juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_prod_A[i,t] <- juv_tot_alagnon[i,t] / (juv_tot_vichy[i,t]+juv_tot_alagnon[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		
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
		
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[4,t]/(S_juv_JP[3,t]+S_juv_JP[4,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
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




###sans rho_poutes
save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2018_06_25.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2017_12_20.RData")

#load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2017_12_20.RData")
load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2018_06_25.RData")


#=================================
# Taux renouvellement population
#=================================
library(stringr)
bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.12.12.R")
source("data_4zones_Interaction_2017.12.12.R")
data_vichy<-N[,1]


for(t in (T+1):(T+25)){
	for (i in 1:5000){ 
		###### Partie Taux Renouvellement ############
		#on calcule le coef pond�rateur des juv�niles
		coef_juv_1[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])+(juv_vichy[i,t-5]+juv_alagnon[i,t-5]+juv_langeac[i,t-5]+juv_poutes[i,t-5])+(juv_vichy[i,t-6]+juv_alagnon[i,t-6]+juv_langeac[i,t-6]+juv_poutes[i,t-6]))
		coef_juv_2[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-3]+juv_alagnon[i,t-3]+juv_langeac[i,t-3]+juv_poutes[i,t-3])+(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])+(juv_vichy[i,t-5]+juv_alagnon[i,t-5]+juv_langeac[i,t-5]+juv_poutes[i,t-5]))
		coef_juv_3[i,t]<-(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4])/((juv_vichy[i,t-2]+juv_alagnon[i,t-2]+juv_langeac[i,t-2]+juv_poutes[i,t-2])+(juv_vichy[i,t-3]+juv_alagnon[i,t-3]+juv_langeac[i,t-3]+juv_poutes[i,t-3])+(juv_vichy[i,t-4]+juv_alagnon[i,t-4]+juv_langeac[i,t-4]+juv_poutes[i,t-4]))
	}
}


for(t in (T+1):(T+1)){
	for (i in 1:5000){ 
		###### Partie Taux Renouvellement ############
		#on calcule le coef pond�rateur des juv�niles
		renew_rate[i,t-5]<-log((coef_juv_1[i,t]*data_vichy[t-1]+coef_juv_2[i,t]*N_vichy[i,t]+coef_juv_3[i,t]*N_vichy[i,t+1])/data_vichy[t-5])
	}
}
for(t in (T+2):(T+4)){
	for (i in 1:5000){ 
		###### Partie Taux Renouvellement ############
		#on calcule le coef pond�rateur des juv�niles
		renew_rate[i,t-5]<-log((coef_juv_1[i,t]*N_vichy[i,t-1]+coef_juv_2[i,t]*N_vichy[i,t]+coef_juv_3[i,t]*N_vichy[i,t+1])/data_vichy[t-5])
	}
}
for(t in (T+5):(T+25)){
	for (i in 1:5000){ 
		###### Partie Taux Renouvellement ############
		#on calcule le coef pond�rateur des juv�niles
		renew_rate[i,t-5]<-log((coef_juv_1[i,t]*N_vichy[i,t-1]+coef_juv_2[i,t]*N_vichy[i,t]+coef_juv_3[i,t]*N_vichy[i,t+1])/N_vichy[i,t-5])
	}
}

for (t in (T+1):(T+20)){
	renew_rate_q[t,]=quantile(renew_rate[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#============================
# Echelle naturelle

png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioDevalaison_EchelleNat.png",width=1000,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)
par("mar"=c(7.1, 5.1, 4.1, 2.1))
plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab="Years",ylim=c(0,3),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = seq(0,3,0.5),labels=seq(0,3,0.5),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+10),(T+20)),
		labels=c((1975+T),(1975+T+9),(1975+T+19)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,exp(renew_rate_q[i,5]),i+0.15,exp(renew_rate_q[i,5]))
	segments(i,exp(renew_rate_q[i,4]),i,exp(renew_rate_q[i,5]))
	#5%
	segments(i-0.15,exp(renew_rate_q[i,1]),i+0.15,exp(renew_rate_q[i,1]))
	segments(i,exp(renew_rate_q[i,2]),i,exp(renew_rate_q[i,1]))
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(exp(renew_rate_q[i,2]),exp(renew_rate_q[i,2]),exp(renew_rate_q[i,4]),exp(renew_rate_q[i,4])),col="light grey")
	#median
	segments(i-0.3,exp(renew_rate_q[i,3]),i+0.3,exp(renew_rate_q[i,3]))
}
abline(h=1,col="red")
mtext(1,text=iconv("Scenario : Projection sans d�versement � 20 ans et suppression des mortalit�s � la d�valaison dans les turbines","UTF8"),line=5)
dev.off()

exp(mean(renew_rate_q[(T+16):(T+20),3]))

#============================
# Echelle log

png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioDevalaison.png",width=1000,height=800)
#png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/TauxRenouvellementPopSauvage_coef.png",width=1000,height=800)
par("mar"=c(7.1, 5.1, 4.1, 2.1))
plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab="Years",ylim=c(-2,2),ylab="Taux renouvellement",main="Taux de renouvellement de la population sauvage",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = seq(-2,2,0.5),labels=seq(-2,2,0.5),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+10),(T+20)),
		labels=c((1975+T),(1975+T+9),(1975+T+19)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,renew_rate_q[i,5],i+0.15,renew_rate_q[i,5])
	segments(i,renew_rate_q[i,4],i,renew_rate_q[i,5])
	#5%
	segments(i-0.15,renew_rate_q[i,1],i+0.15,renew_rate_q[i,1])
	segments(i,renew_rate_q[i,2],i,renew_rate_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(renew_rate_q[i,2],renew_rate_q[i,2],renew_rate_q[i,4],renew_rate_q[i,4]),col="light grey")
	#median
	segments(i-0.3,renew_rate_q[i,3],i+0.3,renew_rate_q[i,3])
}
abline(h=0,col="red")
mtext(1,text=iconv("Scenario : Projection sans d�versement � 20 ans et suppression des mortalit�s � la d�valaison dans les turbines","UTF8"),line=5)
dev.off()

##### Graph Tx renouvellement moyen des m�dianes par tranche de 5 ans #####

#Calcul du taux de renouvellement en �chelle classique (1= renouvellement)
renew_rate_q_5yr=array(0,dim=c((20/5),1))
j=T+1

for (i in 1:(20/5)){
	renew_rate_q_5yr[i]<-exp(mean(renew_rate_q[j:(j+4),3]))
	j=j+5
}
rownames(renew_rate_q_5yr)<-c(str_c("",1975+T,"-",1975+T+4,""),str_c("",1975+T+5,"-",1975+T+9,""),str_c("",1975+T+10,"-",1975+T+14,""),str_c("",1975+T+15,"-",1975+T+19,""))

png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioDevalaison_5yrs.png",width=1000,height=800)
par("mar"=c(10.1,5.1,3.1,1.1))
plot(renew_rate_q_5yr,axes=FALSE,ylim=c(0,ifelse(max(renew_rate_q_5yr)<1,1,max(renew_rate_q_5yr)+0.1)),xlab="",ylab="Taux renouvellement",main=iconv("Taux de renouvellement de la population sauvage (moyenne des m�dianes sur 5 ans)","UTF8"),cex.lab=1.5)
axis(1,at = seq(1,(20/5),1),
		labels=rownames(renew_rate_q_5yr),
		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
axis(2,at = seq(0,ifelse(max(renew_rate_q_5yr)<1,1,max(renew_rate_q_5yr)),0.2),labels=seq(0,ifelse(max(renew_rate_q_5yr)<1,1,max(renew_rate_q_5yr)),0.2),cex.axis = 1.2,las = 1,lwd=2,col = "black")


for (s in 1:(20/5)){
	segments(s,renew_rate_q_5yr[s],(s+1),renew_rate_q_5yr[s+1])
}
abline(h=1,col="red")
mtext(1,text=iconv("Scenario : Projection sans d�versement � 20 ans et suppression des mortalit�s � la d�valaison dans les turbines","UTF8"),line=8)
dev.off()

#Calcul du taux de renouvellement en �chelle log (0=renouvellement)
L_renew_rate_q_5yr=array(0,dim=c((20/5),1))
j=T+1

for (i in 1:(20/5)){
	L_renew_rate_q_5yr[i]<-mean(renew_rate_q[j:(j+4),3])
	j=j+5
}
rownames(L_renew_rate_q_5yr)<-c(str_c("",1975+T,"-",1975+T+4,""),str_c("",1975+T+5,"-",1975+T+9,""),str_c("",1975+T+10,"-",1975+T+14,""),str_c("",1975+T+15,"-",1975+T+19,""))

#on cr�� une fonction pour arrondir le min(L_renew_rate_q_5yr) � la d�cimal du dessous la plus proche. ex min(L_renew_rate_q_5yr)=-0.43 et on veut arrondir � -0.5
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/TauxRenouvellementPopSauvage_ScenarioDevalaison_5yrs_log.png",width=1000,height=800)
par("mar"=c(10.1,5.1,3.1,1.1))
plot(L_renew_rate_q_5yr,axes=FALSE,xlab="",ylim=c(floor_dec(min(L_renew_rate_q_5yr),1),ifelse(max(L_renew_rate_q_5yr)<0,0.2,max(L_renew_rate_q_5yr))),ylab=iconv("Taux renouvellement (�chelle log)","UTF8"),main=iconv("Taux de renouvellement de la population sauvage (log de la moyenne des m�dianes sur 5 ans)","UTF8"),cex.lab=1.5)
axis(1,at = seq(1,(20/5),1),
		labels=rownames(L_renew_rate_q_5yr),
		cex.axis = 1.2,las = 1,lwd=2,col = "black",las=2)
axis(2,at = seq(floor_dec(min(L_renew_rate_q_5yr),1),ifelse(max(L_renew_rate_q_5yr)<0,0.2,max(L_renew_rate_q_5yr)),0.1),labels=round(seq(floor_dec(min(L_renew_rate_q_5yr),1),ifelse(max(L_renew_rate_q_5yr)<0,0.2,max(L_renew_rate_q_5yr)),0.1),1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
for (s in 1:length(seq(7,(T-6),5))){
	segments(s,L_renew_rate_q_5yr[s],(s+1),L_renew_rate_q_5yr[s+1])
}
abline(h=0,col="red")
mtext(1,text=iconv("Scenario : Projection sans d�versement � 20 ans et suppression des mortalit�s � la d�valaison dans les turbines","UTF8"),line=8)
dev.off()



#==========================================================
# SIMULATION SUR LES 20 ANS AVEC CES VARIABLES RECALCULEES
#==========================================================

#==========================================
# CHAP : Figure : TotalReturns_proj20years
#==========================================

# Graph projection 20 years
bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 ann�e de suivi station (soit T+20 - 15)

#Attention à l'ann�e 30 estimation des passages Vichy car ann�e jug�e incomplète
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
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_12_20_Devalaison4zones_Interaction_MatriceVC/DevalMorta_TotalReturns_proj20years_2017_12_20.png",width=800,height=800)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="20 years projection without stocking - Improvement of downstream migration at 11 hydroelectric dams",cex.lab=1.5)

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
		if(N_vichy[i,t] < 10){under_10_vichy[i,t-T]=1}  
		if(N_vichy[i,t] < 50){under_50_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 100){under_100_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 250){under_250_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 500){under_500_vichy[i,t-T]=1}
		
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
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2017_12_20_Devalaison4zones_Interaction_MatriceVC/DevalaisonMorta_Threshold_2017_12_20.png",width=800,height=800)


par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="Improvement of downstream migration at 11 hydroelectric dams")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
lab2=c(1975+T,1975+T+9,1975+T+19)
axis(1,at = c(1,10,20),
		labels=lab2,
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,20,1)



points(x,p_under_10_vichy,col="grey85",pch=16)
segments(x[1:19],p_under_10_vichy[1:19],x[2:20],p_under_10_vichy[2:20],col="grey85")
#
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



#=========================================================================
# CHAP : Figure : R�partitions des Juv�niles dans les diff�rents secteurs
#=========================================================================
#Modèle 2017_12_20_Devalaison_4zones_Interaction_ss_rho_poutes_MatriceVC
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_12_20_Devalaison_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/") #on laisse le fichier data du modèle Standard 2017 qui est 2017_08_29


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

#----------------------------------
# PARTIE PROJECTION

#R�cup�ration des caluls r�alis�s dans le cadre de la mod�lisation Devalaison
load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2017_12_20.RData")


#les juv_vichy,alagnon,langeac,poutes du load ne commencent qu'à t=43 (T+2) on recr�� le t=42 (T+1)
for (t in (T+1):(T+1)){
	juv_vichy[,t]<-bugs_dmoy_tot_V[5001:10000,t-1] * S_juv_JP[1,t]
	juv_alagnon[,t]<-bugs_dmoy_tot_A[5001:10000,t-1] * S_juv_JP[2,t]
	juv_langeac[,t]<-bugs_dmoy_tot_L[5001:10000,t-1] * S_juv_JP[3,t]
	juv_poutes[,t]<-bugs_dmoy_tot_P[5001:10000,t-12] * S_juv_JP[4,t]
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

png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2017_12_20_Devalaison4zones_Interaction_MatriceVC/RepartitionSpatialeJuv_Devalaison_2017_12_20.png",width=800, height=1500, units = "px",type="cairo")
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

#----------------------------------
# PARTIE SERIE TEMPORELLE

S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_alagnon_real=read.coda("S_alagnonCODAchain1.txt","S_alagnonCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")

#On r�cupère les donn�es du modèle directement dans le fichier data
library(stringr)
bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.07.19.R")
source("data_4zones_Interaction_2017.07.19.R")


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

#R�cup�ration des caluls r�alis�s dans le cadre de la mod�lisation D�valaison
load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2017_12_20.RData")

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

png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2017_12_20_Devalaison4zones_Interaction_MatriceVC/RepartitionSpatialeGen_Devalaison_2017_12_20.png",width=800, height=1500, units = "px",type="cairo")
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
















#------------------------------------
#POUR DEVALAISON SANS RHO_POUTES
#-------------------------------------
for (t in (T+1):(T+3)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]= bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		bugs_juv_tot_langeac[i,t] = ((1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}



#Am�lioration sur le premier tiers de production. On ne calcule une survie que pour les 2/3 restant (le premier tiers survivant entièrement)
#Attention il faut remettre sur le premier tiers le ratio_river et ratio_interbar pour le secteur Vichy. Pas la peine pour les 2 autres car ratio-interbar=1
for (t in (T+4):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[3,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[4,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[5,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[6,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[3,t]*ratio_interbar[7,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[3,t]*ratio_interbar[8,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[3,t]*ratio_interbar[9,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[3,t]*ratio_interbar[10,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[3,t]*ratio_interbar[11,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + ((1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + ((1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i]
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


#Am�lioration sur les deux premiers tiers de production.On utilise les donn�es de juv�niles de bugs pour t-4 et t-5
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[3,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[4,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[5,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[6,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[3,t]*ratio_interbar[7,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[3,t]*ratio_interbar[8,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[3,t]*ratio_interbar[9,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[3,t]*ratio_interbar[10,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[3,t]*ratio_interbar[11,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

#Am�lioration pour l'ensemble de la production de juv�niles. On utilise les donn�es de juv�niles de bugs pour t-5
for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}
#Toutes les cohortes poutes b�n�ficient de l'am�lioration. 
for (t in (T+7):(T+20)){
	for (i in 1:5000){ 
		juv_tot_vichy[i,t] = (1/3) *juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5] 
		
		juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SIMULATION SANS AMELIORATION
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ratio_juv_prod_V=array(0,dim=c(5000,T+20))
ratio_juv_prod_L=array(0,dim=c(5000,T+20))
ratio_juv_prod_P=array(0,dim=c(5000,T+20))
ratio_juv_L=array(0,dim=c(5000,T+20))
ratio_juv_P=array(0,dim=c(5000,T+20))
L_ratio_juv_L=array(0,dim=c(5000,T+20))
L_ratio_juv_P=array(0,dim=c(5000,T+20))
L_mu_Vichy_nm=array(0,dim=c(5000,T+20))
L_mu_d_wild_vichy=array(0,dim=c(5000,T+21))
L_mu_d_wild_langeac=array(0,dim=c(5000,T+21))
L_mu_d_wild_poutes=array(0,dim=c(5000,T+21))
L_mu_p_langeac=array(0,dim=c(5000,T+20))
L_mu_p_poutes=array(0,dim=c(5000,T+20))
L_p_langeac=array(0,dim=c(5000,T+20))
L_p_poutes=array(0,dim=c(5000,T+20))
L_d_moy_vichy=array(0,dim=c(5000,T+21))
L_d_moy_langeac=array(0,dim=c(5000,T+21))
L_d_moy_poutes=array(0,dim=c(5000,T+21))
res_vichy=array(0,dim=c(5000,T+20))
res_p_langeac=array(0,dim=c(5000,T+20))
res_p_poutes=array(0,dim=c(5000,T+20))#On le recalcule pour t>T car extrait les CODA que sur 1:39 dans script_openbugs
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
bugs_juv_vichy_3=array(0,dim=c(5000,T+20))
bugs_juv_vichy_4=array(0,dim=c(5000,T+20))
bugs_juv_vichy_5=array(0,dim=c(5000,T+20))
bugs_juv_vichy_6=array(0,dim=c(5000,T+20))
bugs_juv_vichy_7=array(0,dim=c(5000,T+20))
bugs_juv_vichy_8=array(0,dim=c(5000,T+20))
bugs_juv_vichy_9=array(0,dim=c(5000,T+20))
bugs_juv_vichy_10=array(0,dim=c(5000,T+20))
bugs_juv_vichy_11=array(0,dim=c(5000,T+20))
bugs_juv_surv_Vall_restant=array(0,dim=c(5000,T+20))
bugs_juv_surv_Vala_restant=array(0,dim=c(5000,T+20))
bugs_juv_surv_Vdor_restant=array(0,dim=c(5000,T+20))
bugs_juv_vichy=array(0,dim=c(5000,T+20))
bugs_juv_langeac=array(0,dim=c(5000,T+20))
bugs_juv_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_vichy=array(0,dim=c(5000,T+20))
bugs_juv_tot_langeac=array(0,dim=c(5000,T+20))
bugs_juv_tot_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_system=array(0,dim=c(5000,T+20))
ratio_habitat=array(0,dim=c(3,T+20))

river=c(rep(c(0.66, 0.14, 0.2),29),rep(c(0.43, 0.44, 0.13),31))
ratio_river_V<-matrix(river,nrow=3)
interbar=c(rep(c(0, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),11),rep(c(1, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),18),
		rep(c(1, 1,0.0884, 0.281, 0.303, 0.174, 0.395, 0.029, 0.012, 0.06, 0.156),31))
ratio_interbar<-matrix(interbar,nrow=11)
rho_surv=c(0.678, 0.926, 0.916, 0.902, 0.847, 0.886, 0.838, 0.915, 0.907, 0.9, 0.853)

for (t in 1:(T+20)){
	for (i in 1:3){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t])
	}
}
#On recalcule tous les juv�niles car la modification dans la distribution spatiale des g�niteurs peut
#modifier les densit�s de juv�niles des diff�rentes zones

#Pour avoir les premiers juv_tot sans les recalculer car fout la zone dans R car en a besoin pour calculer la suite
for(t in (T-4): (T+1)){
	for (i in 1:5000){ 
		bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t-1]
		bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[2,t-1]
		bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[3,t-1]
	}
}

#Effet sur les juv�niles au fur et à mesure
#n'impacte pas les 3 dernières ann�es de production (car ant�rieures à l'ann�e d'am�lioration)

for (t in (T+1):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]
		+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		bugs_juv_tot_langeac[i,t] = ((1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]
		+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = ((1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}

for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]
		+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = ((1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}

for (t in (T+7):(T+20)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5]) *ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		bugs_juv_surv_Vdor_restant[i,t]<-( (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5])*ratio_river_V[3,t]*(1-(ratio_interbar[7,t]+ratio_interbar[8,t]+ratio_interbar[9,t]+ratio_interbar[10,t]+ratio_interbar[11,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]
		+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]+bugs_juv_surv_Vdor_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = ((1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_d[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_d[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_d[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}


#==========================================
# CHAP : Figure : TotalReturns_proj20years
#==========================================

# Graph projection 20 years
bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
simul_N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 ann�e de suivi station (soit T+20 - 15)

#Attention à l'ann�e 30 estimation des passages Vichy car ann�e jug�e incomplète
for (t in 1:22){
	simul_N_vichy_real_q[t,]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	simul_N_vichy_real_q[(t+7),]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#N_vichy_proj_q=array(0,dim=c(43,5))
simul_N_vichy_proj_q=array(0,dim=c(T+20,5))
for (t in (T+1):(T+20)){
	simul_N_vichy_proj_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#------------------
# Graph
#------------------
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2016_01_20_Devalaison_Thin200/DevalMorta_TotalReturns_proj20years_2016_03_16.png",width=800,height=800)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="20 years projection without stocking - Improvement of downstream migration at 11 hydroelectric dams",cex.lab=1.5)

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
	segments(i-0.15,simul_N_vichy_real_q[i,5],i+0.15,simul_N_vichy_real_q[i,5])
	segments(i,simul_N_vichy_real_q[i,4],i,simul_N_vichy_real_q[i,5])
	#5%
	segments(i-0.15,simul_N_vichy_real_q[i,1],i+0.15,simul_N_vichy_real_q[i,1])
	segments(i,simul_N_vichy_real_q[i,2],i,simul_N_vichy_real_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(simul_N_vichy_real_q[i,2],simul_N_vichy_real_q[i,2],simul_N_vichy_real_q[i,4],simul_N_vichy_real_q[i,4]),col="light grey")
	#median
	segments(i-0.3,simul_N_vichy_real_q[i,3],i+0.3,simul_N_vichy_real_q[i,3])
}


data_vichy=c(
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,393,267,515,
		380,400,541,1238,NA,#662,
		510,950,572,421,491,
		227,755,861,819,595)
points(x=seq(23,T,1),data_vichy[23:T],pch=16)

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,simul_N_vichy_proj_q[i,5],i+0.15,simul_N_vichy_proj_q[i,5])
	segments(i,simul_N_vichy_proj_q[i,4],i,simul_N_vichy_proj_q[i,5])
	#5%
	segments(i-0.15,simul_N_vichy_proj_q[i,1],i+0.15,simul_N_vichy_proj_q[i,1])
	segments(i,simul_N_vichy_proj_q[i,2],i,simul_N_vichy_proj_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(simul_N_vichy_proj_q[i,2],simul_N_vichy_proj_q[i,2],simul_N_vichy_proj_q[i,4],simul_N_vichy_proj_q[i,4]),col="orange")
	#median
	segments(i-0.3,simul_N_vichy_proj_q[i,3],i+0.3,simul_N_vichy_proj_q[i,3])
}


dev.off()

#==================================
# CHAP : Figure : Threshold
#==================================
#bugs_N_vichy=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")	

#under_10_vichy=array(0,dim=c(8000,20))
simul_under_50_vichy=array(0,dim=c(8000,20))
simul_under_100_vichy=array(0,dim=c(8000,20))
simul_under_250_vichy=array(0,dim=c(8000,20))
simul_under_500_vichy=array(0,dim=c(8000,20))


for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		#if(N_vichy[i,t] < 10){under_10_vichy[i,t-T]=1}  
		if(N_vichy[i,t] < 50){simul_under_50_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 100){simul_under_100_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 250){simul_under_250_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 500){simul_under_500_vichy[i,t-T]=1}
		
	}
}


#p_under_10_vichy=rep(0,20)
simul_p_under_50_vichy=rep(0,20)
simul_p_under_100_vichy=rep(0,20)
simul_p_under_250_vichy=rep(0,20)
simul_p_under_500_vichy=rep(0,20)


for (t in 1:20){
	#p_under_10_vichy[t]=mean(under_10_vichy[,t])
	simul_p_under_50_vichy[t]=mean(simul_under_50_vichy[,t])
	simul_p_under_100_vichy[t]=mean(simul_under_100_vichy[,t])
	simul_p_under_250_vichy[t]=mean(simul_under_250_vichy[,t])
	simul_p_under_500_vichy[t]=mean(simul_under_500_vichy[,t])
	
}

#------------------------
# Graph
#------------------------
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2016_01_20_Devalaison_Thin200/DevalaisonMorta_Threshold_2016_03_16.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="Improvement of downstream migration at 11 hydroelectric dams")

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
points(x,simul_p_under_50_vichy,col="grey75",pch=16)
segments(x[1:19],simul_p_under_50_vichy[1:19],x[2:20],simul_p_under_50_vichy[2:20],col="grey75")

points(x,simul_p_under_100_vichy,col="grey65",pch=16)
segments(x[1:19],simul_p_under_100_vichy[1:19],x[2:20],simul_p_under_100_vichy[2:20],col="grey65")

points(x,simul_p_under_250_vichy,col="grey55",pch=16)
segments(x[1:19],simul_p_under_250_vichy[1:19],x[2:20],simul_p_under_250_vichy[2:20],col="grey55")

points(x,simul_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],simul_p_under_500_vichy[1:19],x[2:20],simul_p_under_500_vichy[2:20],col="grey45")


legend(15,1,legend=c(expression(p^treshold < 500),expression(p^treshold < 250),expression(p^treshold < 100),expression(p^treshold < 50)),#,expression(p^treshold < 10)),
		pch=c(16,16,16,16,16),
		col=c("grey45","grey55","grey65","grey75"),#,"grey85"),
		bty="n" )


dev.off()


	#------------------------------------------------------
	# CALCUL DU GAIN Entre on ne fait rien et on am�liore
	#------------------------------------------------------

gain500<-100-(p_under_500_vichy*100/simul_p_under_500_vichy)
#on r�cupère s_juv2ad du modèle standard 2016.01.20
s_juv2ad<-read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2016_01_20_Standard_thin200/simulation/s_juv2adCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2016_01_20_Standard_thin200/simulation/s_juv2adCODAindex.txt")
#on cr�� une nouvelle variable pour recalculer le s_juv2ad am�lior� suite à l'am�lioration de la d�valaison
s_juv2ad_new=array(0,dim=c(5000,T+20))
for (t in (T+1):(T+20)){
	for (i in 1:5000){
s_juv2ad_new[i,t]<-s_juv2ad[i]+(gain500[t-40]/100*s_juv2ad[i])
}
}

save(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_01_20_s_juv2ad_new_2016_03_14.RData",list="s_juv2ad_new")

#plus on compare avec des effectifs petits plus la diff�rence entre les 2 sc�narii est grande
#gain250<-100-(p_under_250_vichy*100/simul_p_under_250_vichy)
#s_juv2ad_new250=array(0,dim=c(5000,T+20))
#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		s_juv2ad_new250[i,t]<-s_juv2ad[i]+(gain250[t-40]/100*s_juv2ad[i])
#	}
#}
#
#gain100<-100-(p_under_100_vichy*100/simul_p_under_100_vichy)
#s_juv2ad_new100=array(0,dim=c(5000,T+20))
#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		s_juv2ad_new100[i,t]<-s_juv2ad[i]+(gain100[t-40]/100*s_juv2ad[i])
#	}
#}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Simulation sans juv�niles sur la Dore
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Modèle 2016_01_18_ss_Dore
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2016_01_18_ss_Dore/")
#Modèle 2016_01_20_Devalaison_Surf_ss_Dore
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2016_01_20_Devalaison_Surf_ssDore_thin200/")

library(coda)
library(boot)
T=40


surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),32))
S_juv_JP<-matrix(surf,nrow=3)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# AMELIORATION DEVALAISON 100%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#=================================================================================================================================================
# AMELIORATION DES 11 OUVRAGES SUR LA SURVIE DES JUVENILES : diminution de 100% de la mortalit� -->on supprime le facteur de mortalit�
# ON CONSERVE L'IMPACT DES OUVRAGES A LA MONTAISON (con conserve p_adjust_L & p_adjust_P) 
# (pour l'instant on laisse sigma_p_poutes tel quel en attente de l'avis de JMB sur l'�volution des Q dans le vieil Allier)
#==================================================================================================================================================
bugs_ratio_juv_prod_L=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")
bugs_rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
bugs_adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
bugs_adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")
bugs_sigma_p_langeac=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
bugs_sigma_p_poutes=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")
bugs_sigma_vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")
bugs_sigma_wild_moy=read.coda("parameters/sigma_wild_moyCODAchain1.txt","parameters/sigma_wild_moyCODAindex.txt")
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
L_mu_d_wild_vichy=array(0,dim=c(5000,T+21))
L_mu_d_wild_langeac=array(0,dim=c(5000,T+21))
L_mu_d_wild_poutes=array(0,dim=c(5000,T+21))
L_mu_p_langeac=array(0,dim=c(5000,T+20))
L_mu_p_poutes=array(0,dim=c(5000,T+20))
L_p_langeac=array(0,dim=c(5000,T+20))
L_p_poutes=array(0,dim=c(5000,T+20))
L_d_moy_vichy=array(0,dim=c(5000,T+21))
L_d_moy_langeac=array(0,dim=c(5000,T+21))
L_d_moy_poutes=array(0,dim=c(5000,T+21))
res_vichy=array(0,dim=c(5000,T+20))
res_p_langeac=array(0,dim=c(5000,T+20))
res_p_poutes=array(0,dim=c(5000,T+20))#On le recalcule pour t>T car extrait les CODA que sur 1:39 dans script_openbugs
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
bugs_juv_vichy_3=array(0,dim=c(5000,T+20))
bugs_juv_vichy_4=array(0,dim=c(5000,T+20))
bugs_juv_vichy_5=array(0,dim=c(5000,T+20))
bugs_juv_vichy_6=array(0,dim=c(5000,T+20))
#bugs_juv_vichy_7=array(0,dim=c(5000,T+20))
#bugs_juv_vichy_8=array(0,dim=c(5000,T+20))
#bugs_juv_vichy_9=array(0,dim=c(5000,T+20))
#bugs_juv_vichy_10=array(0,dim=c(5000,T+20))
#bugs_juv_vichy_11=array(0,dim=c(5000,T+20))
bugs_juv_surv_Vall_restant=array(0,dim=c(5000,T+20))
bugs_juv_surv_Vala_restant=array(0,dim=c(5000,T+20))
#bugs_juv_surv_Vdor_restant=array(0,dim=c(5000,T+20))
bugs_juv_vichy=array(0,dim=c(5000,T+20))
bugs_juv_langeac=array(0,dim=c(5000,T+20))
bugs_juv_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_vichy=array(0,dim=c(5000,T+20))
bugs_juv_tot_langeac=array(0,dim=c(5000,T+20))
bugs_juv_tot_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_system=array(0,dim=c(5000,T+20))
ratio_habitat=array(0,dim=c(3,T+20))

#DANS SCENARIO DE REPARTITION DES JUVENILES AU PRORATA DES SURFACES + PRODUCTIVITE DES COURS D'EAU
river=c(rep(c(0.83, 0.17),29),rep(c(0.49, 0.51),31))
ratio_river_V<-matrix(river,nrow=2)
#DANS SCENARIO DE REPARTITION DES JUVENILES AU PRORATA DES SURFACES
river=c(rep(c(0.89, 0.11),29),rep(c(0.61, 0.39),31))
ratio_river_V<-matrix(river,nrow=2)

interbar=c(rep(c(0, 1, 0.0884, 0.281, 0, 0),11),rep(c(1, 1, 0.0884, 0.281, 0, 0),18),
		rep(c(1, 1,0.0884, 0.281, 0.303, 0.174),31))
ratio_interbar<-matrix(interbar,nrow=6)
rho_surv=c(0.678, 0.926, 0.916, 0.902, 0.847, 0.886)

for (t in 1:(T+20)){
	for (i in 1:3){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t])
	}
}
#On recalcule tous les juv�niles car la modification dans la distribution spatiale des g�niteurs peut
#modifier les densit�s de juv�niles des diff�rentes zones

#Pour avoir les premiers juv_tot sans les recalculer car fout la zone dans R car en a besoin pour calculer la suite
for(t in (T-4): (T+1)){
	for (i in 1:5000){ 
		bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t-1]
		bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[2,t-1]
		bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[3,t-1]
	}
}

#Effet sur les juv�niles au fur et à mesure
#n'impacte pas les 3 dernières ann�es de production (car ant�rieures à l'ann�e d'am�lioration)

for (t in (T+1):(T+3)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]) *ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-( (1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]
		bugs_juv_tot_langeac[i,t] = ((1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}



#Am�lioration sur le premier tiers de production. On ne calcule une survie que pour les 2/3 restant (le premier tiers survivant entièrement)
#Attention il faut remettre sur le premier tiers le ratio_river et ratio_interbar pour le secteur Vichy. Pas la peine pour les 2 autres car ratio-interbar=1
for (t in (T+4):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[3,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[4,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[5,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[2,t]*ratio_interbar[6,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + ((1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + ((1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i]
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


#Am�lioration sur les deux premiers tiers de production.On utilise les donn�es de juv�niles de bugs pour t-4 et t-5
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[3,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[4,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[5,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[2,t]*ratio_interbar[6,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		#On ajoute les juv�niles situ�s en aval des turbines donc non impact�s par celles-ci
		bugs_juv_surv_Vall_restant[i,t]<-((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*(1-(ratio_interbar[3,t]+ratio_interbar[4,t]))
		bugs_juv_surv_Vala_restant[i,t]<-((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*(1-(ratio_interbar[5,t]+ratio_interbar[6,t]))
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_surv_Vall_restant[i,t]+bugs_juv_surv_Vala_restant[i,t]
		
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

#Am�lioration pour l'ensemble de la production de juv�niles. On utilise les donn�es de juv�niles de bugs pour t-5
for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}
#Toutes les cohortes poutes b�n�ficient de l'am�lioration. 
for (t in (T+7):(T+20)){
	for (i in 1:5000){ 
		juv_tot_vichy[i,t] = (1/3) *juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5] 
		
		juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015.10.07_OuvertureDevalaison.RData")
#save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_01_20_OuvertureDevalaison_ssDore_2016_03_17.RData")
save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_01_20_OuvertureDevalaison_Surf_ssDore_2016_03_17.RData")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# AMELIORATION MONTAISON & DEVALAISON 100%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#=================================================================================================================================================
# AMELIORATION DES 11 OUVRAGES SUR LA SURVIE DES JUVENILES : diminution de 100% de la mortalit� -->on supprime le facteur de mortalit�
# ON SUPPRIME L'IMPACT DES OUVRAGES A LA MONTAISON (p_adjust_L & p_adjust_P) 
#==================================================================================================================================================
bugs_ratio_juv_prod_L=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")
bugs_rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
#bugs_adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
#bugs_adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")
bugs_sigma_p_langeac=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
bugs_sigma_p_poutes=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")
bugs_sigma_vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")
bugs_sigma_wild_moy=read.coda("parameters/sigma_wild_moyCODAchain1.txt","parameters/sigma_wild_moyCODAindex.txt")
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
L_mu_d_wild_vichy=array(0,dim=c(5000,T+21))
L_mu_d_wild_langeac=array(0,dim=c(5000,T+21))
L_mu_d_wild_poutes=array(0,dim=c(5000,T+21))
L_mu_p_langeac=array(0,dim=c(5000,T+20))
L_mu_p_poutes=array(0,dim=c(5000,T+20))
L_p_langeac=array(0,dim=c(5000,T+20))
L_p_poutes=array(0,dim=c(5000,T+20))
L_d_moy_vichy=array(0,dim=c(5000,T+21))
L_d_moy_langeac=array(0,dim=c(5000,T+21))
L_d_moy_poutes=array(0,dim=c(5000,T+21))
res_vichy=array(0,dim=c(5000,T+20))
res_p_langeac=array(0,dim=c(5000,T+20))
res_p_poutes=array(0,dim=c(5000,T+20))#On le recalcule pour t>T car extrait les CODA que sur 1:39 dans script_openbugs
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
bugs_juv_vichy_3=array(0,dim=c(5000,T+20))
bugs_juv_vichy_4=array(0,dim=c(5000,T+20))
bugs_juv_vichy_5=array(0,dim=c(5000,T+20))
bugs_juv_vichy_6=array(0,dim=c(5000,T+20))
bugs_juv_vichy_7=array(0,dim=c(5000,T+20))
bugs_juv_vichy_8=array(0,dim=c(5000,T+20))
bugs_juv_vichy_9=array(0,dim=c(5000,T+20))
bugs_juv_vichy_10=array(0,dim=c(5000,T+20))
bugs_juv_vichy_11=array(0,dim=c(5000,T+20))
bugs_juv_vichy=array(0,dim=c(5000,T+20))
bugs_juv_langeac=array(0,dim=c(5000,T+20))
bugs_juv_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_vichy=array(0,dim=c(5000,T+20))
bugs_juv_tot_langeac=array(0,dim=c(5000,T+20))
bugs_juv_tot_poutes=array(0,dim=c(5000,T+20))
bugs_juv_tot_system=array(0,dim=c(5000,T+20))
ratio_habitat=array(0,dim=c(3,T+20))
river=c(rep(c(0.66, 0.14, 0.2),29),rep(c(0.43, 0.44, 0.13),30))
ratio_river_V<-matrix(river,nrow=3)
interbar=c(rep(c(0, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),11),rep(c(1, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),18),
		rep(c(1, 1,0.0884, 0.281, 0.303, 0.174, 0.395, 0.029, 0.012, 0.06, 0.156),30))
ratio_interbar<-matrix(interbar,nrow=11)
rho_surv=c(0.678, 0.926, 0.916, 0.902, 0.847, 0.886, 0.838, 0.915, 0.907, 0.9, 0.853)

for (t in 1:(T+20)){
	for (i in 1:3){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t])
	}
}
#On recalcule tous les juv�niles car la modification dans la distribution spatiale des g�niteurs peut
#modifier les densit�s de juv�niles des diff�rentes zones

#Pour avoir les premiers juv_tot sans les recalculer car fout la zone dans R car en a besoin pour calculer la suite
for(t in (T-4): (T+1)){
	for (i in 1:5000){ 
		bugs_juv_vichy[i,t]=bugs_dmoy_tot_V[i,t-1]*S_juv_JP[1,t]
		bugs_juv_langeac[i,t]=bugs_dmoy_tot_L[i,t-1]*S_juv_JP[2,t]
		bugs_juv_poutes[i,t]=bugs_dmoy_tot_P[i,t-12]*S_juv_JP[3,t]
	}
}
#Effet sur les juv�niles au fur et à mesure
#n'impacte pas les 3 dernières ann�es de production (car ant�rieures à l'ann�e d'am�lioration)

for (t in (T+1):(T+3)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ((1/3) * bugs_juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]
		+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]
		bugs_juv_tot_langeac[i,t] = ((1/3) * bugs_juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = ((1/3) * bugs_juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]#+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]# +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}

#Am�lioration sur le premier tiers de production. On ne calcule une survie que pour les 2/3 restant (le premier tiers survivant entièrement)
#Attention il faut remettre sur le premier tiers le ratio_river et ratio_interbar pour le secteur Vichy. Pas la peine pour les 2 autres car ratio-interbar=1
for (t in (T+4):(T+4)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[3,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[4,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[5,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[6,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[7,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[8,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[9,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[10,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = (1/3) * bugs_juv_vichy[i,t-3]*ratio_river_V[1,t]*ratio_interbar[11,t] + ((1/3) * bugs_juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5])*ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]
		+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]
		bugs_juv_tot_langeac[i,t] = (1/3) * bugs_juv_langeac[i,t-3] + ((1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5])*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * bugs_juv_poutes[i,t-3] + ((1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5])*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]#+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]# +bugs_adjust_p_P[i]
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


#Am�lioration sur les deux premiers tiers de production.On utilise les donn�es de juv�niles de bugs pour t-4 et t-5
for (t in (T+5):(T+5)){
	for (i in 1:5000){ 
		bugs_juv_vichy_3[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[3,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[1,t]*ratio_interbar[3,t]*prod(rho_surv[3:4]) 
		bugs_juv_vichy_4[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[4,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[1,t]*ratio_interbar[4,t]*prod(rho_surv[4:4]) 
		bugs_juv_vichy_5[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[5,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[2,t]*ratio_interbar[5,t]*prod(rho_surv[5:6]) 
		bugs_juv_vichy_6[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[6,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[2,t]*ratio_interbar[6,t]*prod(rho_surv[6:6]) 
		bugs_juv_vichy_7[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[7,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[7,t]*prod(rho_surv[7:11]) 
		bugs_juv_vichy_8[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[8,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[8,t]*prod(rho_surv[8:11]) 
		bugs_juv_vichy_9[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[9,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[9,t]*prod(rho_surv[9:11]) 
		bugs_juv_vichy_10[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[10,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[10,t]*prod(rho_surv[10:11]) 
		bugs_juv_vichy_11[i,t] = ((1/3) * juv_vichy[i,t-3] + (1/3) * bugs_juv_vichy[i,t-4])*ratio_river_V[1,t]*ratio_interbar[11,t] + (1/3) * bugs_juv_vichy[i,t-5]*ratio_river_V[3,t]*ratio_interbar[11,t]*prod(rho_surv[11:11]) 
		
		bugs_juv_tot_vichy[i,t]=bugs_juv_vichy_3[i,t]+bugs_juv_vichy_4[i,t]+bugs_juv_vichy_5[i,t]+bugs_juv_vichy_6[i,t]+bugs_juv_vichy_7[i,t]
		+bugs_juv_vichy_8[i,t]+bugs_juv_vichy_9[i,t]+bugs_juv_vichy_10[i,t]+bugs_juv_vichy_11[i,t]
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * bugs_juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]*ratio_interbar[2,t]*prod(rho_surv[2:4]) 
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * bugs_juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]*ratio_interbar[1,t]*prod(rho_surv[1:4]) 
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t] #+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t] #+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

#Am�lioration pour l'ensemble de la production de juv�niles. On utilise les donn�es de juv�niles de bugs pour t-5
for (t in (T+6):(T+6)){
	for (i in 1:5000){ 
		bugs_juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * bugs_juv_vichy[i,t-5]
		bugs_juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * bugs_juv_langeac[i,t-5]
		bugs_juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * bugs_juv_poutes[i,t-5]
		
		bugs_juv_tot_system[i,t] <- bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t] +bugs_juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t]) / (bugs_juv_tot_vichy[i,t]+bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]#+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  bugs_juv_tot_poutes[i,t]/(bugs_juv_tot_langeac[i,t]+bugs_juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]# +bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*bugs_juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}
#Toutes les cohortes poutes b�n�ficient de l'am�lioration. 
for (t in (T+7):(T+20)){
	for (i in 1:5000){ 
		juv_tot_vichy[i,t] = (1/3) *juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5] 
		
		juv_tot_system[i,t] <- juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] <- (juv_tot_langeac[i,t]+juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t]+juv_tot_langeac[i,t]+juv_tot_poutes[i,t])	
		ratio_juv_prod_V[i,t] <- 1-ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]<- bugs_rho_station[i]*(ratio_habitat[2,t]+ratio_habitat[3,t])+(1-bugs_rho_station[i])*ratio_juv_prod_L[i,t]
		L_ratio_juv_L[i,t] <- logit(ratio_juv_L[i,t])
		L_mu_p_langeac[i,t]<-L_ratio_juv_L[i,t]#+bugs_adjust_p_L[i]
		L_p_langeac[i,t]<-rnorm(1,L_mu_p_langeac[i,t],bugs_sigma_p_langeac[i]) 
		res_p_langeac[i,t] <- L_p_langeac[i,t]-L_mu_p_langeac[i,t]
		
		ratio_juv_prod_P[i,t] <-  juv_tot_poutes[i,t]/(juv_tot_langeac[i,t]+juv_tot_poutes[i,t])
		ratio_juv_P[i,t]<- bugs_rho_station[i]*(S_juv_JP[3,t]/(S_juv_JP[2,t]+S_juv_JP[3,t]))+(1-bugs_rho_station[i])*ratio_juv_prod_P[i,t]
		L_ratio_juv_P[i,t] <- logit(ratio_juv_P[i,t])
		L_mu_p_poutes[i,t]<-L_ratio_juv_P[i,t]#+bugs_adjust_p_P[i] 
		L_p_poutes[i,t]<-rnorm(1,L_mu_p_poutes[i,t],bugs_sigma_p_poutes[i]) 
		res_p_poutes[i,t] <- L_p_poutes[i,t]-L_mu_p_poutes[i,t]
		
		L_mu_Vichy_nm[i,t]<- log(bugs_s_juv2ad[i]*juv_tot_system[i,t]) 
		N_vichy[i,t]<-rlnorm(1,L_mu_Vichy_nm[i,t],bugs_sigma_vichy[i])
		if(N_vichy[i,t]<3){N_vichy[i,t]=3}
		if(N_vichy[i,t]>30000){N_vichy[i,t]=30000}
		res_vichy[i,t] <- log(N_vichy[i,t])-L_mu_Vichy_nm[i,t]
		
		p_langeac[i,t]=inv.logit(L_p_langeac[i,t])	
		p_poutes[i,t]=inv.logit(L_p_poutes[i,t])	
		N_langeac[i,t]=rbinom(1,round(N_vichy[i,t],0),p_langeac[i,t])
		N_poutes[i,t]=rbinom(1,round(N_langeac[i,t],0),p_poutes[i,t])
		min_N_langeac[i,t]<-N_poutes[i,t]+1
		if(N_langeac[i,t]<min_N_langeac[i,t]){N_langeac[i,t]=min_N_langeac[i,t]}
		
		
		S_poutes[i,t]=max(N_poutes[i,t],1)
		S_langeac[i,t]=max(N_langeac[i,t]-N_poutes[i,t],1)
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t],1)#pas remis les C_up et C_dwn car plus de capture dans ces ann�es là
		
		L_mu_d_wild_vichy[i,t+1]=log((S_vichy[i,t]/S_juv_JP[1,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_vichy[i,t]/S_juv_JP[1,t])))+bugs_nu_wild[i,1]
		L_d_moy_vichy[i,t+1]=rnorm(1,L_mu_d_wild_vichy[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_vichy[i,t+1]= exp(L_d_moy_vichy[i,t+1])
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		
		L_mu_d_wild_langeac[i,t+1]=log((S_langeac[i,t]/S_juv_JP[2,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_langeac[i,t]/S_juv_JP[2,t])))+bugs_nu_wild[i,2]
		L_d_moy_langeac[i,t+1]=rnorm(1,L_mu_d_wild_langeac[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_langeac[i,t+1]=exp(L_d_moy_langeac[i,t+1])
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		
		L_mu_d_wild_poutes[i,t+1]=log((S_poutes[i,t]/S_juv_JP[3,t])/(bugs_alpha_dd[i]+bugs_beta_dd[i]*(S_poutes[i,t]/S_juv_JP[3,t])))+bugs_nu_wild[i,3]
		L_d_moy_poutes[i,t+1]=rnorm(1,L_mu_d_wild_poutes[i,t+1],bugs_sigma_wild_moy[i])
		d_moy_poutes[i,t+1]=exp(L_d_moy_poutes[i,t+1])
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015_11_26_OuvertureMontaisonDevalaison_2015_12_08.RData")
#load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015.01.24_OuverturePoutes100.RData")
load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2015_11_26_OuvertureMontaisonDevalaison_2015_12_08.RData")

#On agglomère les 2 matrix juv_tot_ et bugs_juv_tot_ pour avoir un tableau complet sur la p�riode
#juv_tot_vichy_tot=array(0,dim=c(5000,T+20))
#juv_tot_langeac_tot=array(0,dim=c(5000,T+20))
#juv_tot_poutes_tot=array(0,dim=c(5000,T+20))
#juv_tot_system_tot=array(0,dim=c(5000,T+20))

#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		juv_tot_vichy_tot[i,t]=juv_tot_vichy[i,t]+bugs_juv_tot_vichy[i,t]
#	}
#}
#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		juv_tot_langeac_tot[i,t]=juv_tot_langeac[i,t]+bugs_juv_tot_langeac[i,t]
#	}
#}
#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		juv_tot_poutes_tot[i,t]=juv_tot_poutes[i,t]+bugs_juv_tot_poutes[i,t]
#	}
#}
#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		juv_tot_system_tot[i,t]=juv_tot_system[i,t]+bugs_juv_tot_system[i,t]
#	}
#}
#juv_poutes_tot=array(0,dim=c(5000,T+20))
#for (t in (T+1):(T+20)){
#	for (i in 1:5000){
#		juv_poutes_tot[i,t]=juv_poutes[i,t]+bugs_juv_poutes[i,t]
#	}
#}
#==========================================================
# SIMULATION SUR LES 20 ANS AVEC CES VARIABLES RECALCULEES
#==========================================================

#==========================================
# CHAP : Figure : TotalReturns_proj20years
#==========================================

# Graph projection 20 years
bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 ann�e de suivi station (soit T+20 - 15)

#Attention à l'ann�e 30 estimation des passages Vichy car ann�e jug�e incomplète
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
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2015_11_26/DevalMontaMorta_TotalReturns_proj20years_2015_12_08.png",width=800,height=800)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="20 years projection without stocking - Improvement of downstream migration at 11 hydroelectric dams",cex.lab=1.5)

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


data_vichy=c(
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,393,267,515,
		380,400,541,1238,NA,#662,
		510,950,572,421,491,
		227,755,861,819,595)
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

#under_10_vichy=array(0,dim=c(8000,20))
under_50_vichy=array(0,dim=c(8000,20))
under_100_vichy=array(0,dim=c(8000,20))
under_250_vichy=array(0,dim=c(8000,20))
under_500_vichy=array(0,dim=c(8000,20))


for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		#if(N_vichy[i,t] < 10){under_10_vichy[i,t-T]=1}  
		if(N_vichy[i,t] < 50){under_50_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 100){under_100_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 250){under_250_vichy[i,t-T]=1}
		if(N_vichy[i,t] < 500){under_500_vichy[i,t-T]=1}
		
	}
}


#p_under_10_vichy=rep(0,20)
p_under_50_vichy=rep(0,20)
p_under_100_vichy=rep(0,20)
p_under_250_vichy=rep(0,20)
p_under_500_vichy=rep(0,20)


for (t in 1:20){
#	p_under_10_vichy[t]=mean(under_10_vichy[,t])
	p_under_50_vichy[t]=mean(under_50_vichy[,t])
	p_under_100_vichy[t]=mean(under_100_vichy[,t])
	p_under_250_vichy[t]=mean(under_250_vichy[,t])
	p_under_500_vichy[t]=mean(under_500_vichy[,t])
	
}

#------------------------
# Graph
#------------------------
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2015_11_26/DevalaisonMontaMorta_Threshold_2015_12_08_2.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="Improvement of downstream migration at 11 hydroelectric dams")

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


legend(15,1,legend=c(expression(p^treshold < 500),expression(p^treshold < 250),expression(p^treshold < 100),expression(p^treshold < 50)),#,expression(p^treshold < 10)),
		pch=c(16,16,16,16,16),
		col=c("grey45","grey55","grey65","grey75"),#,"grey85"),
		bty="n" )


dev.off()




##############################################################
# Projection sur 20 ans du nbr de g�niteurs et juv�niles
##############################################################

#===========================================================
# Projection sur 20 ans des g�niteurs (en nombre et ratio)
#===========================================================
	#--------------------------------------------------------------------------
	# R�cup�ration des donn�es sur la première partie (1 à T) depuis OpenBugs
	#--------------------------------------------------------------------------
		#...................
		# Nbr de g�niteurs
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
	#Recalcul des nombres de g�niteurs
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
	# Conservation de la quantit� d'habitat dispo identiques pour les 20 prochaines ann�es
	#----------------------------------------------------------------------------------------
surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)

	#---------------------------------------------
	# Graph G�niteurs avec projection sur 20 ans
	#---------------------------------------------
png(file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2014_12_05_Poutes50/poutes50_SpawnersRedds_GeniteursPotentiels.png",width=800, height=800, units = "px",type="cairo")

par(mfrow=c(3,2),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
#........
# Vichy
#........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,6500),ylab=expression(italic(S["t,1"])),main="Vichy-Langeac",cex.main=2,cex.lab=2)

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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
# Projection sur 20 ans de juv�niles (en nombre et ratio)
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

# trace l'axe des ordonn�es
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

