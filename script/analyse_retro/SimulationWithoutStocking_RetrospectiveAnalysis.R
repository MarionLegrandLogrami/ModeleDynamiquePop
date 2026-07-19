# TODO: Add comment
# 
# Author: Guillaume Dauphin & Marion Legrand
###############################################################################

#Output Allier 2014.05.30
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_05_30")
#SurfERR+SurfDev 2014.08.28
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_08_26_SufERR+SurfDev")
#SurfERR+SurfDev 2014.08.28
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_09/")
#Poutès 2014_10_15
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_10_15/Poutes/")
#Amélioration Poutes 2014.11.16
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_11_16_PoutesAmelioration50%Amenagement/")
#Poutès 2014_12_05 A tourné avec MAJ données sur 2004 + correction sur indicatrice
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_05_Poutes/")
#Modèle 2014_12_20
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_12_20/")
#Modèle 2015_01_24_thin200
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2015_01_24_thin200/")
#Modèle définitf 2015
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2016_01_20_standard_thin200")


library(coda)
library(boot)

T=40


#taking coda of the first 6 years of Returns to Vichy
N_vichy_inits=read.coda("N_VichyCODAchain1.txt","N_VichyCODAindex.txt")
#taking coda of the first 6 years of p_langeac
p_langeac_inits=read.coda("p_langeacCODAchain1.txt","p_langeacCODAindex.txt")
#others parameters required
s_juv2ad=read.coda("s_juv2adCODAchain1.txt","s_juv2adCODAindex.txt")
surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)
level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt") 
I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt")
rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
alpha_dd=read.coda("simulation/alpha_ddCODAchain1.txt","simulation/alpha_ddCODAindex.txt")
beta_dd=read.coda("simulation/beta_ddCODAchain1.txt","simulation/beta_ddCODAindex.txt")
nu_wild=read.coda("nu_wildCODAchain1.txt","nu_wildCODAindex.txt")
res_wild_vichy=read.coda("simulation/res_wild_vichyCODAchain1.txt","simulation/res_wild_vichyCODAindex.txt")
res_wild_langeac=read.coda("simulation/res_wild_langeacCODAchain1.txt","simulation/res_wild_langeacCODAindex.txt")
res_wild_poutes=read.coda("simulation/res_wild_poutesCODAchain1.txt","simulation/res_wild_poutesCODAindex.txt")

S_vichy=array(0,dim=c(5000,T))
S_langeac=array(0,dim=c(5000,T))
S_poutes=array(0,dim=c(5000,T))

N_langeac=array(0,dim=c(5000,T))
N_poutes=array(0,dim=c(5000,T))

d_moy_vichy=array(0,dim=c(5000,T+1))
d_moy_langeac=array(0,dim=c(5000,T+1))
d_moy_poutes=array(0,dim=c(5000,T+1))

juv_vichy=array(0,dim=c(5000,T+1))
juv_langeac=array(0,dim=c(5000,T+1))
juv_poutes=array(0,dim=c(5000,T+1))

juv_tot_vichy=array(0,dim=c(5000,T+1))
juv_tot_langeac=array(0,dim=c(5000,T+1))
juv_tot_poutes=array(0,dim=c(5000,T+1))
juv_tot_system=array(0,dim=c(5000,T+1))

ratio_habitat=array(0,dim=c(3,T+20))

for (t in 1:(T+20)){
	for (i in 1:3){
		ratio_habitat[i,t] <- S_juv_JP[i,t] /( S_juv_JP[1,t]+S_juv_JP[2,t]+S_juv_JP[3,t])
	}
}

ratio_juv_prod_V=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_prod_L=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_prod_P=array(rep(0,T*5000),dim=c(5000,T))

ratio_juv_V=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_L=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_P=array(rep(0,T*5000),dim=c(5000,T))

res_p_langeac=read.coda("simulation/res_p_langeacCODAchain1.txt","simulation/res_p_langeacCODAindex.txt")
res_p_poutes=read.coda("simulation/res_p_poutesCODAchain1.txt","simulation/res_p_poutesCODAindex.txt")

adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")

p_langeac=array(0,185,dim=c(5000,T))
p_poutes=array(0,185,dim=c(5000,T))

N_vichy=array(0,185,dim=c(5000,T))

res_vichy=read.coda("simulation/res_vichyCODAchain1.txt","simulation/res_vichyCODAindex.txt")
juv_tot_sys=read.coda("simulation/juv_tot_sysCODAchain1.txt","simulation/juv_tot_sysCODAindex.txt")
ratio_juv_prod_L_inits=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")
p_reach_V=read.coda("p_reach_VCODAchain1.txt","p_reach_VCODAindex.txt")

C_dwn=c(
		420,439,77,124,190,
		318,819,388,169,286,
		438,614,385,731,260,
		196,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0)


C_up=c(
		1190,700,315,220,200,
		1280,514,1163,410,314,
		807,72,91,425,140,
		88,135,110,112,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0)


dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt")
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt")
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt")

dmoy_wild_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt")
dmoy_wild_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt")
dmoy_wild_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt")

ratio_wild=array(0,dim=c(5000,T))

for (t in 7:15){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4])
	}
}

for (t in 16:16){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4] + dmoy_wild_P[i,t-15]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4]+ dmoy_tot_P[i,t-15])
	}
}

for (t in 17:17){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5]+ dmoy_wild_P[i,t-16] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4] + dmoy_wild_P[i,t-15]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5]+ dmoy_tot_P[i,t-16] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4]+ dmoy_tot_P[i,t-15])
	}
}

for (t in 18:T){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6]+ dmoy_wild_P[i,t-17] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5]+ dmoy_wild_P[i,t-16] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4] + dmoy_wild_P[i,t-15]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_P[i,t-17]+ dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5]+ dmoy_tot_P[i,t-16] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4]+ dmoy_tot_P[i,t-15])
	}
}

rho_poutes=read.coda("rho_poutesCODAchain1.txt","rho_poutesCODAindex.txt")

for (t in 1:6){
	for (i in 1:5000){ 
		
		N_langeac[i,t]=p_langeac_inits[i,t]* (N_vichy_inits[i,t]-C_up[t]-p_reach_V[i] * C_dwn[t])
		
		S_vichy[i,t]= N_vichy_inits[i,t]-N_langeac[i,t]-C_up[t]-p_reach_V[i] * C_dwn[t]
		S_langeac[i,t]= N_langeac[i,t]
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
	}
}


for (t in 7:7){
	for (i in 1:5000){
		
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]	
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * 1  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]= N_langeac[i,t]
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		
	}
}


for (t in 8:11){
	for (i in 1:5000){
		
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]	
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]= N_langeac[i,t]
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		
	}
}

for (t in 12:14){
	for (i in 1:5000){
		
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 			
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		ratio_juv_prod_P[i,t] = 0
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] ) 
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
	}
	
}

for (t in 15:15){
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 	
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		ratio_juv_prod_P[i,t] = 0
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] ) 
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
	
}

for (t in 16:16){
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] 	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


for (t in 17:17){	
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + juv_poutes[i,t-4] 	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]  = exp((log(((S_vichy[i,t]/S_juv_JP[1,t])   / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) ))   * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1] = exp((log(((S_poutes[i,t]/S_juv_JP[3,t])  / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) ))  * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}

for (t in 18:21){
	for (i in 1:5000){	
		juv_tot_vichy[i,t]  = (1/3) * juv_vichy[i,t-3]   + (1/3) * juv_vichy[i,t-4]   + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t]= (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3]  + (1/3) * juv_poutes[i,t-4]  + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}



for (t in 22:24){
	for (i in 1:5000){	
		juv_tot_vichy[i,t]  = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t]= (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


for (t in 25:27){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]  = exp((log(((S_vichy[i,t]/S_juv_JP[1,t])  / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) ))   * exp(nu_wild[i,1])) + res_wild_vichy[i,t]    ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t])/ (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2])) + res_wild_langeac[i,t]  ))
		d_moy_poutes[i,t+1] = exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) ))  * exp(nu_wild[i,3])) + res_wild_poutes[i,t-11]))
		
		juv_vichy[i,t+1]  =d_moy_vichy[i,t+1]  *S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1] =d_moy_poutes[i,t+1] *S_juv_JP[3,t+1]
	}
}

for (t in 28:31){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

for (t in 32:34){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t]
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

for (t in 35:36){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}

for (t in 37:T){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2,t]+ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  rho_poutes[i]*juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + rho_poutes[i]*juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3,t]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		
		temp=p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t])
		N_langeac[i,t]=max(temp,1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
	}
}
save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/script/lateX/data/2016_03_15_AnalyseRetro.RData")

N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")


################################################
# Retour vers le futur
################################################
#Calcul N_vichy
N_vichy_real_q=array(NA,dim=c((T+20-15),5))
N_vichy_q=array(NA,dim=c(T,5))

for (t in 1:T){
	N_vichy_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#Attention � l'ann�e 30 o� estimation des passages � Vichy car ann�e jug�e incompl�te
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	N_vichy_real_q[(t+7),]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

data_vichy=c(
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,393,267,515,
		380,400,541,1238,NA,
		510,950,572,421,491,
		227,755,861,819,595)


#Calcul diff
diff_N_vichy=array(0,dim=c(5000,T))


for (t in 7:22){
	for (i in 1:5000){
		diff_N_vichy[i,t]=N_vichy_real[i,t]-N_vichy[i,t] 
	}
} 

for (t in 23:29){
	for (i in 1:5000){
		diff_N_vichy[i,t]=data_vichy[t]-N_vichy[i,t] 
	}
} 

for (t in 30:30){
	for (i in 1:5000){
		diff_N_vichy[i,t]=N_vichy_real[i,(t-7)]-N_vichy[i,t] 
	}
} 

for (t in 31:T){
	for (i in 1:5000){
		diff_N_vichy[i,t]=data_vichy[t]-N_vichy[i,t] 
	}
} 


diff_N_vichy_q=array(0,dim=c(T,5))

for (t in 7:T){
	diff_N_vichy_q[t,]=quantile(diff_N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=TRUE,names=FALSE)
}

#Calcul pourcentage
pourcentage_N_vichy=array(0,dim=c(5000,T))


for (t in 7:22){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(N_vichy_real[i,t]-N_vichy[i,t])/N_vichy_real[i,t]
	}
} 

for (t in 23:29){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(data_vichy[t]-N_vichy[i,t])/data_vichy[t] 
	}
} 
#On estime l'année 2004 car comptage partiel. t-7 car année 30 à la suite des 22 premières année soit 23eme ligne de N_vichy_real
for (t in 30:30){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(N_vichy_real[i,(t-7)]-N_vichy[i,t])/N_vichy_real[i,(t-7)]
	}
} 

for (t in 31:T){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(data_vichy[t]-N_vichy[i,t]) /data_vichy[t] 
	}
} 


pourcentage_N_vichy_q=array(0,dim=c(T,5))

for (t in 7:T){
	pourcentage_N_vichy_q[t,]=quantile(pourcentage_N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=TRUE,names=FALSE)
}


png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/RetourVersLeFutur.png",width=800,height=800)

par(mfrow=c(2,2))


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="With stocking",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
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
		380,400,541,1238,NA,
		510,950,572,421,491,
		227,755,861,819,595)



points(x=seq(23,T,1),data_vichy[23:T],pch=16)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="Simulation without stocking",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")


for(i in 3:6){
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



for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_q[i,5],i+0.15,N_vichy_q[i,5])
	segments(i,N_vichy_q[i,4],i,N_vichy_q[i,5])
	#5%
	segments(i-0.15,N_vichy_q[i,1],i+0.15,N_vichy_q[i,1])
	segments(i,N_vichy_q[i,2],i,N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_q[i,2],N_vichy_q[i,2],N_vichy_q[i,4],N_vichy_q[i,4]),col="light grey")
	#median
	segments(i-0.3,N_vichy_q[i,3],i+0.3,N_vichy_q[i,3])
}

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-500,1500),ylab=expression(Returns^stocking-Returns^without),main="Difference of returns in Vichy with or without stocking",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(-500,0,500,1000,1500),labels=c(-500,0,500,1000,1500),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")


points(x=c(3,4,5,6),y=c(1,1,1,1),pch=16)


for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,diff_N_vichy_q[i,5],i+0.15,diff_N_vichy_q[i,5])
	segments(i,diff_N_vichy_q[i,4],i,diff_N_vichy_q[i,5])
	#5%
	segments(i-0.15,diff_N_vichy_q[i,1],i+0.15,diff_N_vichy_q[i,1])
	segments(i,diff_N_vichy_q[i,2],i,diff_N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_N_vichy_q[i,2],diff_N_vichy_q[i,2],diff_N_vichy_q[i,4],diff_N_vichy_q[i,4]),col="light grey")
	#median
	segments(i-0.3,diff_N_vichy_q[i,3],i+0.3,diff_N_vichy_q[i,3])
}

segments(1,0,38,lty=2,col="red")


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="%stocking",main="Contribution of stocking to adults returns",cex.lab = 1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

#text(T,15,labels=expression(italic("a.")))


for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,pourcentage_N_vichy_q[i,5],i+0.15,pourcentage_N_vichy_q[i,5])
	segments(i,pourcentage_N_vichy_q[i,4],i,pourcentage_N_vichy_q[i,5])
	#5%
	segments(i-0.15,pourcentage_N_vichy_q[i,1],i+0.15,pourcentage_N_vichy_q[i,1])
	segments(i,pourcentage_N_vichy_q[i,2],i,pourcentage_N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(pourcentage_N_vichy_q[i,2],pourcentage_N_vichy_q[i,2],pourcentage_N_vichy_q[i,4],pourcentage_N_vichy_q[i,4]),col="light grey")
	#median
	segments(i-0.3,pourcentage_N_vichy_q[i,3],i+0.3,pourcentage_N_vichy_q[i,3])
}


dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# OLD VERSION
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#taking coda of the first 6 years of Returns to Vichy

N_vichy_inits=read.coda("N_VichyCODAchain1.txt","N_VichyCODAindex.txt")


#taking coda of the first 6 years of p_langeac

p_langeac_inits=read.coda("p_langeacCODAchain1.txt","p_langeacCODAindex.txt")


#others parameters required

s_juv2ad=read.coda("s_juv2adCODAchain1.txt","s_juv2adCODAindex.txt")

surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)
#S_juv_JP=c(2574236,434552,655661)
#x=c(rep(916866,29),rep(1202540,10))
#y=c(rep(250441,39))
#z=c(rep(0,11),rep(301101,12),rep(383049,16))

#S_juv_JP=cbind(x,y,z)


level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt") 

I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt") 


rho_station=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")

alpha_dd=read.coda("simulation/alpha_ddCODAchain1.txt","simulation/alpha_ddCODAindex.txt")
beta_dd=read.coda("simulation/beta_ddCODAchain1.txt","simulation/beta_ddCODAindex.txt")

nu_wild=read.coda("nu_wildCODAchain1.txt","nu_wildCODAindex.txt")


res_wild_vichy=read.coda("simulation/res_wild_vichyCODAchain1.txt","simulation/res_wild_vichyCODAindex.txt")
res_wild_langeac=read.coda("simulation/res_wild_langeacCODAchain1.txt","simulation/res_wild_langeacCODAindex.txt ")
res_wild_poutes=read.coda("simulation/res_wild_poutesCODAchain1.txt","simulation/res_wild_poutesCODAindex.txt ")


S_vichy=array(0,dim=c(5000,T))
S_langeac=array(0,dim=c(5000,T))
S_poutes=array(0,dim=c(5000,T))

N_langeac=array(0,dim=c(5000,T))
N_poutes=array(0,dim=c(5000,T))

d_moy_vichy=array(0,dim=c(5000,T+1))
d_moy_langeac=array(0,dim=c(5000,T+1))
d_moy_poutes=array(0,dim=c(5000,T+1))

juv_vichy=array(0,dim=c(5000,T+1))
juv_langeac=array(0,dim=c(5000,T+1))
juv_poutes=array(0,dim=c(5000,T+1))


juv_tot_vichy=array(0,dim=c(5000,T+1))
juv_tot_langeac=array(0,dim=c(5000,T+1))
juv_tot_poutes=array(0,dim=c(5000,T+1))
juv_tot_system=array(0,dim=c(5000,T+1))


ratio_habitat=c(0,0,0)

for (i in 1:3){
	ratio_habitat[i] = S_juv_JP[i] /sum( S_juv_JP[])
}

ratio_juv_prod_V=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_prod_L=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_prod_P=array(rep(0,T*5000),dim=c(5000,T))

ratio_juv_V=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_L=array(rep(0,T*5000),dim=c(5000,T))
ratio_juv_P=array(rep(0,T*5000),dim=c(5000,T))


res_p_langeac=read.coda("simulation/res_p_langeacCODAchain1.txt","simulation/res_p_langeacCODAindex.txt")
res_p_poutes=read.coda("simulation/res_p_poutesCODAchain1.txt","simulation/res_p_poutesCODAindex.txt ")

adjust_p_L=read.coda("simulation/adjust_p_LCODAchain1.txt","simulation/adjust_p_LCODAindex.txt")
adjust_p_P=read.coda("simulation/adjust_p_PCODAchain1.txt","simulation/adjust_p_PCODAindex.txt")


p_langeac=array(0,185,dim=c(5000,T))
p_poutes=array(0,185,dim=c(5000,T))


N_vichy=array(0,185,dim=c(5000,T))




res_vichy=read.coda("simulation/res_vichyCODAchain1.txt","simulation/res_vichyCODAindex.txt")

juv_tot_sys=read.coda("simulation/juv_tot_sysCODAchain1.txt","simulation/juv_tot_sysCODAindex.txt")

#plot res Vichy vs prod juv 
plot(as.numeric(juv_tot_sys[,1:31]),as.numeric(res_vichy),pch =16,col=rgb(139,137,137,25,maxColorValue=255))
abline(h=0,lty=2,col="red")




ratio_juv_prod_L_inits=read.coda("simulation/ratio_juv_prod_LCODAchain1.txt","simulation/ratio_juv_prod_LCODAindex.txt")

p_reach_V=read.coda("p_reach_VCODAchain1.txt","p_reach_VCODAindex.txt")

C_dwn=c(
		420,439,77,124,190,
		318,819,388,169,286,
		438,614,385,731,260,
		196,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0)


C_up=c(
		1190,700,315,220,200,
		1280,514,1163,410,314,
		807,72,91,425,140,
		88,135,110,112,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0)


dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt")
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt")
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt")

dmoy_wild_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt")
dmoy_wild_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt")
dmoy_wild_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt")





ratio_wild=array(0,dim=c(5000,T))


for (t in 7:15){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4])
	}
}

for (t in 16:16){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4] + dmoy_wild_P[i,t-13]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4]+ dmoy_tot_P[i,t-13])
	}
}

for (t in 17:17){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5]+ dmoy_wild_P[i,t-14] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4] + dmoy_wild_P[i,t-13]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5]+ dmoy_tot_P[i,t-14] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4]+ dmoy_tot_P[i,t-13])
	}
}

for (t in 18:T){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6]+ dmoy_wild_P[i,t-15] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5]+ dmoy_wild_P[i,t-14] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4] + dmoy_wild_P[i,t-13]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_P[i,t-15]+ dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5]+ dmoy_tot_P[i,t-14] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4]+ dmoy_tot_P[i,t-13])
	}
}

rho_poutes=read.coda("rho_poutesCODAchain1.txt","rho_poutesCODAindex.txt")





for (t in 1:6){
	for (i in 1:5000){ 
		
		N_langeac[i,t]=p_langeac_inits[i,t]* (N_vichy_inits[i,t]-C_up[t]-p_reach_V[i] * C_dwn[t])
		
		S_vichy[i,t]= N_vichy_inits[i,t]-N_langeac[i,t]-C_up[t]-p_reach_V[i] * C_dwn[t]
		S_langeac[i,t]= N_langeac[i,t]
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
	}
}







for (t in 7:7){
	
	
	for (i in 1:5000){
		
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]	
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * 1  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]= N_langeac[i,t]
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		
	}
	
	
	
}


for (t in 8:11){
	
	
	for (i in 1:5000){
		
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]	
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]= N_langeac[i,t]
		
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		
	}
	
	
	
}

for (t in 12:14){
	for (i in 1:5000){
		
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 			
		
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]	
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		ratio_juv_prod_P[i,t] = 0
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] ) 
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]#2014.12.02 Attention S_juv_JP était en t au lieu de t+1
	}
	
}




for (t in 15:15){
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] 	
		
		ratio_juv_prod_V[i,t] =1 - ratio_juv_prod_L[i,t]	
		
		ratio_juv_prod_L[i,t] =  juv_tot_langeac[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] )
		
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] )
		
		ratio_juv_prod_P[i,t] = 0
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] ) 
		
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
	
}



for (t in 16:16){
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] 	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))		
		
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


for (t in 17:17){	
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + rho_poutes[i]*(1/3) * juv_poutes[i,t-4] 	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]  = exp((log(((S_vichy[i,t]/S_juv_JP[1,t])   / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) ))   * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1] = exp((log(((S_poutes[i,t]/S_juv_JP[3,t])  / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) ))  * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
		
	}
}

for (t in 18:21){
	for (i in 1:5000){	
		juv_tot_vichy[i,t]  = (1/3) * juv_vichy[i,t-3]   + (1/3) * juv_vichy[i,t-4]   + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t]= (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3]  + rho_poutes[i]*(1/3) * juv_poutes[i,t-4]  + rho_poutes[i]*(1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}



for (t in 22:24){
	for (i in 1:5000){	
		juv_tot_vichy[i,t]  = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t]= (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + rho_poutes[i]*(1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


for (t in 25:27){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + rho_poutes[i]*(1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]  = exp((log(((S_vichy[i,t]/S_juv_JP[1,t])  / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) ))   * exp(nu_wild[i,1])) + res_wild_vichy[i,t]    ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t])/ (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2])) + res_wild_langeac[i,t]  ))
		d_moy_poutes[i,t+1] = exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) ))  * exp(nu_wild[i,3])) + res_wild_poutes[i,t-11]))
		
		juv_vichy[i,t+1]  =d_moy_vichy[i,t+1]  *S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1] =d_moy_poutes[i,t+1] *S_juv_JP[3,t+1]
	}
}




for (t in 28:31){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + rho_poutes[i]*(1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}



for (t in 32:34){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + rho_poutes[i]*(1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}



for (t in 35:36){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + rho_poutes[i]*(1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1,t]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2,t]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3,t]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1,t+1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2,t+1]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3,t+1]
	}
}


for (t in 37:T){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = rho_poutes[i]*(1/3) * juv_poutes[i,t-3] + rho_poutes[i]*(1/3) * juv_poutes[i,t-4] + rho_poutes[i]*(1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / (juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		
		temp=p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t])
		N_langeac[i,t]=max(temp,1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
	}
}


#commande ML enregistre les objets dans RDATA pour les charger ensuite
save.image(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/script/lateX/data/2016.03.10_REtourFutur.RData")
#load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/outputAllier2014.05.30.RData")

N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")


	#====================================
	# CHAP : Figure : spawners_reels
	#====================================

	S_vichy_real=read.coda("simulation/S_vichy_realCODAchain1.txt","simulation/S_vichy_realCODAindex.txt")
	S_langeac_real=read.coda("simulation/S_langeac_realCODAchain1.txt","simulation/S_langeac_realCODAindex.txt")
	
	
	
	S_vichy_q=array(0,dim=c(T,5))
	S_langeac_q=array(0,dim=c(T,5))
	
	
	
	S_poutes_counter=c(0,0,
			0,0,0,0,0,
			0,0,0,0,10,
			43,110,21,4,3,
			11,9,23,6,67,
			35,31,130,112,53,
			40,154,89,74,153,
			53,39,14,26,118,59,45)
	
	
	
	
	
	
	for (i in 3:T){
		S_vichy_q[i,]=quantile(S_vichy_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		S_langeac_q[i,]=quantile(S_langeac_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		
	}
	
	
		#---------------------------
		#Graph with all years
		#---------------------------
		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2015_01_24_thin200/spawners_reels.png",width=800,height=800)
		par(mfrow=c(3,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,6000),ylab=expression(italic(Spawners)),main="Vichy-Langeac")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,1000,2000,3000,4000,5000,6000),labels=c(0,1000,2000,3000,4000,5000,6000),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		text(T,6000,labels=expression(italic("a.")))
		
				
		for(i in 3:T){
			#whiskers
			#95%
			segments(i-0.15,S_vichy_q[i,5],i+0.15,S_vichy_q[i,5])
			segments(i,S_vichy_q[i,4],i,S_vichy_q[i,5])
			#5%
			segments(i-0.15,S_vichy_q[i,1],i+0.15,S_vichy_q[i,1])
			segments(i,S_vichy_q[i,2],i,S_vichy_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_vichy_q[i,2],S_vichy_q[i,2],S_vichy_q[i,4],S_vichy_q[i,4]),col="light grey")
			#median
			segments(i-0.3,S_vichy_q[i,3],i+0.3,S_vichy_q[i,3])
		}
		
				
		#####################
		#####################
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1200),ylab=expression(italic(Spawners)),main="Langeac-Poutes")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,300,600,900,1200),labels=c(0,300,600,900,1200),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		
		text(T,1200,labels=expression(italic("b.")))
		
		
		
		for(i in 3:T){
			#whiskers
			#95%
			segments(i-0.15,S_langeac_q[i,5],i+0.15,S_langeac_q[i,5])
			segments(i,S_langeac_q[i,4],i,S_langeac_q[i,5])
			#5%
			segments(i-0.15,S_langeac_q[i,1],i+0.15,S_langeac_q[i,1])
			segments(i,S_langeac_q[i,2],i,S_langeac_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_langeac_q[i,2],S_langeac_q[i,2],S_langeac_q[i,4],S_langeac_q[i,4]),col="light grey")
			#median
			segments(i-0.3,S_langeac_q[i,3],i+0.3,S_langeac_q[i,3])
		}
				
		#####################
		#####################
		
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,200),ylab=expression(italic(Spawners)),main="Upstream Poutes")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,100,200),labels=c(0,100,200),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		
		text(T,200,labels=expression(italic("c.")))
		
		points(x=seq(12,T,1),y=S_poutes_counter[12:T],pch=16)
		abline(v=11.5,lty=2)
		text(4,160,paste( "upstream Poutes\n inaccessible"))
		
		
		dev.off()
		

	#====================================================
	# CHAP : Figure : spawners_DiffWith_WithoutStocking
	#====================================================
	
	diff_S_vichy=array(0,dim=c(5000,T))
	diff_S_langeac=array(0,dim=c(5000,T))
	diff_S_poutes=array(0,dim=c(5000,T))
	
	
	for (t in 1:T){
		for (i in 1:5000){
			diff_S_vichy[i,t]=S_vichy_real[i,t] -S_vichy[i,t]
			diff_S_langeac[i,t]=S_langeac_real[i,t]-S_langeac[i,t]   
			diff_S_poutes[i,t]= S_poutes_counter[t] -S_poutes[i,t]
			
		}
	} 
	
	
	diff_S_vichy_q=array(0,dim=c(T,5))
	diff_S_langeac_q=array(0,dim=c(T,5))
	diff_S_poutes_q=array(0,dim=c(T,5))
	
	
	
	
	
	
	for (i in 1:T){
		diff_S_vichy_q[i,]=quantile(diff_S_vichy[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		diff_S_langeac_q[i,]=quantile(diff_S_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		diff_S_poutes_q[i,]=quantile(diff_S_poutes[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		
		
	}

		#------------------------
		# Graph with all years
		#------------------------
		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2015_01_24_thin200/spawners_DiffWith_WithoutStocking.png",width=800,height=800)
		
		par(mfrow=c(3,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-500,1000),ylab=expression(italic(S^stocking-S^without)),main="Vichy-Langeac")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(-500,-250,0,250,500,750,1000),labels=c(-500,-250,0,250,500,750,1000),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		text(T,1000,labels=expression(italic("a.")))
		
		
		
		for(i in 3:T){
			#whiskers
			#95%
			segments(i-0.15,diff_S_vichy_q[i,5],i+0.15,diff_S_vichy_q[i,5])
			segments(i,diff_S_vichy_q[i,4],i,diff_S_vichy_q[i,5])
			#5%
			segments(i-0.15,diff_S_vichy_q[i,1],i+0.15,diff_S_vichy_q[i,1])
			segments(i,diff_S_vichy_q[i,2],i,diff_S_vichy_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_S_vichy_q[i,2],diff_S_vichy_q[i,2],diff_S_vichy_q[i,4],diff_S_vichy_q[i,4]),col="light grey")
			#median
			segments(i-0.3,diff_S_vichy_q[i,3],i+0.3,diff_S_vichy_q[i,3])
		}
		
		
		
		segments(1,0,(T+1),lty=2,col="red")
		
		#####################
		#####################
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-150,350),ylab=expression(italic(S^stocking-S^without)),main="Langeac-Poutes")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(-100,0,100,200,350),labels=c(-100,0,100,200,350),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		
		text(T,350,labels=expression(italic("b.")))
		
		
		
		for(i in 3:T){
			#whiskers
			#95%
			segments(i-0.15,diff_S_langeac_q[i,5],i+0.15,diff_S_langeac_q[i,5])
			segments(i,diff_S_langeac_q[i,4],i,diff_S_langeac_q[i,5])
			#5%
			segments(i-0.15,diff_S_langeac_q[i,1],i+0.15,diff_S_langeac_q[i,1])
			segments(i,diff_S_langeac_q[i,2],i,diff_S_langeac_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_S_langeac_q[i,2],diff_S_langeac_q[i,2],diff_S_langeac_q[i,4],diff_S_langeac_q[i,4]),col="light grey")
			#median
			segments(i-0.3,diff_S_langeac_q[i,3],i+0.3,diff_S_langeac_q[i,3])
		}
		
		segments(1,0,(T+1),lty=2,col="red")
				
		#####################
		#####################
		
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-50,155),ylab=expression(italic(S^stocking-S^without)),main="Upstream Poutes")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(-50,0,50,100,150),labels=c(-50,0,50,100,150),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		
		text(T,150,labels=expression(italic("c.")))
		
		abline(v=11.5,lty=2)
		
		for(i in 12:T){
			#whiskers
			#95%
			segments(i-0.15,diff_S_poutes_q[i,5],i+0.15,diff_S_poutes_q[i,5])
			segments(i,diff_S_poutes_q[i,4],i,diff_S_poutes_q[i,5])
			#5%
			segments(i-0.15,diff_S_poutes_q[i,1],i+0.15,diff_S_poutes_q[i,1])
			segments(i,diff_S_poutes_q[i,2],i,diff_S_poutes_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_S_poutes_q[i,2],diff_S_poutes_q[i,2],diff_S_poutes_q[i,4],diff_S_poutes_q[i,4]),col="light grey")
			#median
			segments(i-0.3,diff_S_poutes_q[i,3],i+0.3,diff_S_poutes_q[i,3])
		}
		text(4,4,paste( "upstream Poutes\n inaccessible"))
				
		segments(11.5,0,(T+1),lty=2,col="red")
		
		dev.off()


	#=======================================================
	# CHAP : Figure : ContributionStockingAllierPopulation
	#=======================================================
	#########################################################
	# Graph what did stocking bring to the Allier population
	#########################################################
	
	
	
	
	ratio_stocking_vichy=array(0,dim=c(5000,T))
	ratio_stocking_langeac=array(0,dim=c(5000,T))
	ratio_stocking_poutes=array(0,dim=c(5000,T))
	
	
	for (t in 1:T){
		for (i in 1:5000){
			ratio_stocking_vichy[i,t]= (S_vichy_real[i,t]-S_vichy[i,t])/S_vichy[i,t]  
			ratio_stocking_langeac[i,t]=(S_langeac_real[i,t]-S_langeac[i,t])/S_langeac[i,t]    
			
		}
	} 
	for (t in 12:T){
		for (i in 1:5000){
			ratio_stocking_poutes[i,t]=(S_poutes_counter[t]-S_poutes[i,t])/S_poutes[i,t]  
		}
	}
	
	
	
	ratio_stocking_vichy_q=array(0,dim=c(T,5))
	ratio_stocking_langeac_q=array(0,dim=c(T,5))
	ratio_stocking_poutes_q=array(0,dim=c(T,5))
	
	
	for (i in 1:T){
		ratio_stocking_vichy_q[i,]=quantile(ratio_stocking_vichy[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		ratio_stocking_langeac_q[i,]=quantile(ratio_stocking_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		ratio_stocking_poutes_q[i,]=quantile(ratio_stocking_poutes[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		
		
	}
	
	min(ratio_stocking_vichy_q)
	max(ratio_stocking_vichy_q)
	
	min(ratio_stocking_langeac_q)
	max(ratio_stocking_langeac_q)
	
	
	min(ratio_stocking_poutes_q)
	max(ratio_stocking_poutes_q)

		#--------------------------
		#Graph with all years
		#--------------------------
		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2015_01_24_thin200/ContributionStockingAllierPopulation.png",width=800,height=800)
		
		par(mfrow=c(3,1),mar=c(4,7.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-1,7),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Vichy-Langeac")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(-1,0,1,2,3,4,5),labels=c(-1,0,1,2,3,4,5),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		text(T,15,labels=expression(italic("a.")))
				
		for(i in 3:T){
			#whiskers
			#95%
			segments(i-0.15,ratio_stocking_vichy_q[i,5],i+0.15,ratio_stocking_vichy_q[i,5])
			segments(i,ratio_stocking_vichy_q[i,4],i,ratio_stocking_vichy_q[i,5])
			#5%
			segments(i-0.15,ratio_stocking_vichy_q[i,1],i+0.15,ratio_stocking_vichy_q[i,1])
			segments(i,ratio_stocking_vichy_q[i,2],i,ratio_stocking_vichy_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_stocking_vichy_q[i,2],ratio_stocking_vichy_q[i,2],ratio_stocking_vichy_q[i,4],ratio_stocking_vichy_q[i,4]),col="light grey")
			#median
			segments(i-0.3,ratio_stocking_vichy_q[i,3],i+0.3,ratio_stocking_vichy_q[i,3])
		}
				
		segments(1,0,(T+1),lty=2,col="red")
		
		#####################
		#####################
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-0.5),xlab="Years",ylim=c(-1,10),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Langeac-Poutes")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(-1,0,5,10),labels=c(-1,0,5,10),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		
		text(T,30,labels=expression(italic("b.")))
		
		
		
		for(i in 3:T){
			#whiskers
			#95%
			segments(i-0.15,ratio_stocking_langeac_q[i,5],i+0.15,ratio_stocking_langeac_q[i,5])
			segments(i,ratio_stocking_langeac_q[i,4],i,ratio_stocking_langeac_q[i,5])
			#5%
			segments(i-0.15,ratio_stocking_langeac_q[i,1],i+0.15,ratio_stocking_langeac_q[i,1])
			segments(i,ratio_stocking_langeac_q[i,2],i,ratio_stocking_langeac_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_stocking_langeac_q[i,2],ratio_stocking_langeac_q[i,2],ratio_stocking_langeac_q[i,4],ratio_stocking_langeac_q[i,4]),col="light grey")
			#median
			segments(i-0.3,ratio_stocking_langeac_q[i,3],i+0.3,ratio_stocking_langeac_q[i,3])
		}
				
		segments(1,0,(T+1),lty=2,col="red")
		
		#####################
		#####################
		
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-1,15),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Upstream Poutes")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(-1,0,5,10,15),labels=c(-1,0,5,10,15),cex.axis = 0.9,las = 1,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 0.9,las = 1,col = "black")
		
		
		text(T,155,labels=expression(italic("c.")))
		
		abline(v=11.5,lty=2)
		
		for(i in 12:T){
			#whiskers
			#95%
			segments(i-0.15,ratio_stocking_poutes_q[i,5],i+0.15,ratio_stocking_poutes_q[i,5])
			segments(i,ratio_stocking_poutes_q[i,4],i,ratio_stocking_poutes_q[i,5])
			#5%
			segments(i-0.15,ratio_stocking_poutes_q[i,1],i+0.15,ratio_stocking_poutes_q[i,1])
			segments(i,ratio_stocking_poutes_q[i,2],i,ratio_stocking_poutes_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_stocking_poutes_q[i,2],ratio_stocking_poutes_q[i,2],ratio_stocking_poutes_q[i,4],ratio_stocking_poutes_q[i,4]),col="light grey")
			#median
			segments(i-0.3,ratio_stocking_poutes_q[i,3],i+0.3,ratio_stocking_poutes_q[i,3])
		}
		text(4,10,paste( "upstream Poutes\n inaccessible"))
		
		segments(1,0,(T+1),lty=2,col="red")
		
		dev.off()
		
	#=======================================
	# CHAP : Figure : TotalReturns
	#=======================================
	####################################
	# Graph total returns
	####################################
	
	N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
	
	
	N_vichy_real_q=array(NA,dim=c((T+20-15),5))
	N_vichy_q=array(NA,dim=c(T,5))
	
	for (t in 1:T){
		N_vichy_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	
	#Attention � l'ann�e 30 o� estimation des passages � Vichy car ann�e jug�e incompl�te
	for (t in 1:22){
		N_vichy_real_q[t,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	for(t in 23:23){
		N_vichy_real_q[(t+7),]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}

		#-----------------------------
		# Graph
		#-----------------------------
		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2015_01_24_thin200/TotalReturns.png",width=800,height=800)
		
		par(mfrow=c(1,2))
		
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="With stocking",cex.lab=1.5)
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
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
				380,400,541,1238,NA,
				510,950,572,421,491,
				227,755,861,819)
		
		
		
		points(x=seq(23,T,1),data_vichy[23:T],pch=16)
				
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="Simulation without stocking",cex.lab=1.5)
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 1.2,las = 1,lwd=2,col = "black")
		
				
		for(i in 3:6){
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
		
		
		
		for(i in 7:T){
			#whiskers
			#95%
			segments(i-0.15,N_vichy_q[i,5],i+0.15,N_vichy_q[i,5])
			segments(i,N_vichy_q[i,4],i,N_vichy_q[i,5])
			#5%
			segments(i-0.15,N_vichy_q[i,1],i+0.15,N_vichy_q[i,1])
			segments(i,N_vichy_q[i,2],i,N_vichy_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_q[i,2],N_vichy_q[i,2],N_vichy_q[i,4],N_vichy_q[i,4]),col="light grey")
			#median
			segments(i-0.3,N_vichy_q[i,3],i+0.3,N_vichy_q[i,3])
		}
		
		
		dev.off()


	#==============================================
	# CHAP : figure : ReturnsWith&withoutStocking
	#==============================================

	diff_N_vichy=array(0,dim=c(5000,T))
	
	
	for (t in 7:22){
		for (i in 1:5000){
			diff_N_vichy[i,t]=N_vichy_real[i,t]-N_vichy[i,t] 
		}
	} 
	
	for (t in 23:29){
		for (i in 1:5000){
			diff_N_vichy[i,t]=data_vichy[t]-N_vichy[i,t] 
		}
	} 
	
	for (t in 30:30){
		for (i in 1:5000){
			diff_N_vichy[i,t]=N_vichy_real[i,(t-7)]-N_vichy[i,t] 
		}
	} 
	
	for (t in 31:T){
		for (i in 1:5000){
			diff_N_vichy[i,t]=data_vichy[t]-N_vichy[i,t] 
		}
	} 
	
	
	diff_N_vichy_q=array(0,dim=c(T,5))
	
	for (t in 7:T){
		diff_N_vichy_q[t,]=quantile(diff_N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=TRUE,names=FALSE)
	}

		#------------------------
		# Graph
		#------------------------

		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2015_01_24_thin200/ReturnsDifferenceWith&withoutStocking_.png",width=800,height=800)
		par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-500,1500),ylab=expression(Returns^stocking-Returns^without),main="Difference of returns in Vichy with or without stocking",cex.lab=1.5)
		
		# trace l'axe des ordonn�es
		axis(2,at = c(-500,0,500,1000,1500),labels=c(-500,0,500,1000,1500),cex.axis = 1.2,las = 1,lwd=2,col = "black")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,T),
				labels=c(1975,1980,1990,2000,2013),
				cex.axis = 1.2,las = 1,lwd=2,col = "black")
		
			
		points(x=c(3,4,5,6),y=c(1,1,1,1),pch=16)
			
		
		for(i in 7:T){
			#whiskers
			#95%
			segments(i-0.15,diff_N_vichy_q[i,5],i+0.15,diff_N_vichy_q[i,5])
			segments(i,diff_N_vichy_q[i,4],i,diff_N_vichy_q[i,5])
			#5%
			segments(i-0.15,diff_N_vichy_q[i,1],i+0.15,diff_N_vichy_q[i,1])
			segments(i,diff_N_vichy_q[i,2],i,diff_N_vichy_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_N_vichy_q[i,2],diff_N_vichy_q[i,2],diff_N_vichy_q[i,4],diff_N_vichy_q[i,4]),col="light grey")
			#median
			segments(i-0.3,diff_N_vichy_q[i,3],i+0.3,diff_N_vichy_q[i,3])
		}
		
		segments(1,0,38,lty=2,col="red")
		
		dev.off()

	
################################################
# Retour vers le futur
################################################

png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/RetourVersLeFutur.png",width=800,height=800)

par(mfrow=c(2,2))


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="With stocking",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
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
		380,400,541,1238,NA,
		510,950,572,421,491,
		227,755,861,819,595)



points(x=seq(23,T,1),data_vichy[23:T],pch=16)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="Simulation without stocking",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")


for(i in 3:6){
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



for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_q[i,5],i+0.15,N_vichy_q[i,5])
	segments(i,N_vichy_q[i,4],i,N_vichy_q[i,5])
	#5%
	segments(i-0.15,N_vichy_q[i,1],i+0.15,N_vichy_q[i,1])
	segments(i,N_vichy_q[i,2],i,N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_q[i,2],N_vichy_q[i,2],N_vichy_q[i,4],N_vichy_q[i,4]),col="light grey")
	#median
	segments(i-0.3,N_vichy_q[i,3],i+0.3,N_vichy_q[i,3])
}

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-500,1500),ylab=expression(Returns^stocking-Returns^without),main="Difference of returns in Vichy with or without stocking",cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(-500,0,500,1000,1500),labels=c(-500,0,500,1000,1500),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")


points(x=c(3,4,5,6),y=c(1,1,1,1),pch=16)


for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,diff_N_vichy_q[i,5],i+0.15,diff_N_vichy_q[i,5])
	segments(i,diff_N_vichy_q[i,4],i,diff_N_vichy_q[i,5])
	#5%
	segments(i-0.15,diff_N_vichy_q[i,1],i+0.15,diff_N_vichy_q[i,1])
	segments(i,diff_N_vichy_q[i,2],i,diff_N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_N_vichy_q[i,2],diff_N_vichy_q[i,2],diff_N_vichy_q[i,4],diff_N_vichy_q[i,4]),col="light grey")
	#median
	segments(i-0.3,diff_N_vichy_q[i,3],i+0.3,diff_N_vichy_q[i,3])
}

segments(1,0,38,lty=2,col="red")


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="%stocking",main="Contribution of stocking to adults returns",cex.lab = 1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

#text(T,15,labels=expression(italic("a.")))


for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,pourcentage_N_vichy_q[i,5],i+0.15,pourcentage_N_vichy_q[i,5])
	segments(i,pourcentage_N_vichy_q[i,4],i,pourcentage_N_vichy_q[i,5])
	#5%
	segments(i-0.15,pourcentage_N_vichy_q[i,1],i+0.15,pourcentage_N_vichy_q[i,1])
	segments(i,pourcentage_N_vichy_q[i,2],i,pourcentage_N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(pourcentage_N_vichy_q[i,2],pourcentage_N_vichy_q[i,2],pourcentage_N_vichy_q[i,4],pourcentage_N_vichy_q[i,4]),col="light grey")
	#median
	segments(i-0.3,pourcentage_N_vichy_q[i,3],i+0.3,pourcentage_N_vichy_q[i,3])
}


dev.off()

#================================================
# ML _ 2eme essai
# CHAP : Contribution stocking en pourcentage
#================================================

pourcentage_N_vichy=array(0,dim=c(5000,T))


for (t in 7:22){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(N_vichy_real[i,t]-N_vichy[i,t])/N_vichy_real[i,t]
	}
} 

for (t in 23:29){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(data_vichy[t]-N_vichy[i,t])/data_vichy[t] 
	}
} 
#On estime l'année 2004 car comptage partiel. t-7 car année 30 à la suite des 22 premières année soit 23eme ligne de N_vichy_real
for (t in 30:30){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(N_vichy_real[i,(t-7)]-N_vichy[i,t])/N_vichy_real[i,(t-7)]
	}
} 

for (t in 31:T){
	for (i in 1:5000){
		pourcentage_N_vichy[i,t]=(data_vichy[t]-N_vichy[i,t]) /data_vichy[t] 
	}
} 


pourcentage_N_vichy_q=array(0,dim=c(T,5))

for (t in 7:T){
	pourcentage_N_vichy_q[t,]=quantile(pourcentage_N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=TRUE,names=FALSE)
}


min(pourcentage_N_vichy_q)
max(pourcentage_N_vichy_q)

#--------------------------
#Graph with all years
#--------------------------
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/ContributionStockingAllierPopulation.png",width=800,height=800)

par(mfrow=c(1,1))

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="%stocking",main="Contribution of stocking to adults returns",cex.lab = 1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")

#text(T,15,labels=expression(italic("a.")))



for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,pourcentage_N_vichy_q[i,5],i+0.15,pourcentage_N_vichy_q[i,5])
	segments(i,pourcentage_N_vichy_q[i,4],i,pourcentage_N_vichy_q[i,5])
	#5%
	segments(i-0.15,pourcentage_N_vichy_q[i,1],i+0.15,pourcentage_N_vichy_q[i,1])
	segments(i,pourcentage_N_vichy_q[i,2],i,pourcentage_N_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(pourcentage_N_vichy_q[i,2],pourcentage_N_vichy_q[i,2],pourcentage_N_vichy_q[i,4],pourcentage_N_vichy_q[i,4]),col="light grey")
	#median
	segments(i-0.3,pourcentage_N_vichy_q[i,3],i+0.3,pourcentage_N_vichy_q[i,3])
}


dev.off()

#	#==========================================
#	# CHAP : Figure : TotalReturns_proj20years ATTENTION SCRIPT FAUX il faut projeter à partir des données N_vichy recalculée
#	#										   ET non en récupérant les projections faites sous openbugs en condition normale (= repeuplement lles 39 années précédentes
#	#==========================================
#
#	# Graph projection 20 years
#	
#	N_vichy_2=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")
#	N_vichy_2_q=array(0,dim=c(43,5))
#	
#	for (t in 1:43){
#		N_vichy_2_q[t,]=quantile(N_vichy_2[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#	}
#	
#	
#		#------------------
#		# Graph
#		#------------------
#		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2015_01_24_thin200/TotalReturns_proj20years.png",width=800,height=800)
#		
#		
#		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="20 years projection without stocking - 2015_01_24_thin200 Model",cex.lab=1.5)
#		
#		# trace l'axe des ordonn�es
#		axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
#		# trace l'axe des abscisses
#		axis(1,at = c(1,6,16,26,T,46,T+20),
#				labels=c(1975,1980,1990,2000,2013,2020,2033),
#				cex.axis = 1.2,las = 1,lwd=2,col = "black")
#				
#		for(i in 3:30){
#			#whiskers
#			#95%
#			segments(i-0.15,N_vichy_real_q[i,5],i+0.15,N_vichy_real_q[i,5])
#			segments(i,N_vichy_real_q[i,4],i,N_vichy_real_q[i,5])
#			#5%
#			segments(i-0.15,N_vichy_real_q[i,1],i+0.15,N_vichy_real_q[i,1])
#			segments(i,N_vichy_real_q[i,2],i,N_vichy_real_q[i,1])
#			#boxplot
#			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i,2],N_vichy_real_q[i,2],N_vichy_real_q[i,4],N_vichy_real_q[i,4]),col="light grey")
#			#median
#			segments(i-0.3,N_vichy_real_q[i,3],i+0.3,N_vichy_real_q[i,3])
#		}
#		
#		
#		data_vichy=c(
#				NA,NA,NA,NA,NA,
#				NA,NA,NA,NA,NA,
#				NA,NA,NA,NA,NA,
#				NA,NA,NA,NA,NA,
#				NA,NA,393,267,515,
#				380,400,541,1238,NA,#662,
#				510,950,572,421,491,
#				227,755,861,819)
#		points(x=seq(23,T,1),data_vichy[23:T],pch=16)
#		
#		for(i in (T+1):(T+20)){
#			#whiskers
#			#95%
#			segments(i-0.15,N_vichy_2_q[i-17,5],i+0.15,N_vichy_2_q[i-17,5])
#			segments(i,N_vichy_2_q[i-17,4],i,N_vichy_2_q[i-17,5])
#			#5%
#			segments(i-0.15,N_vichy_2_q[i-17,1],i+0.15,N_vichy_2_q[i-17,1])
#			segments(i,N_vichy_2_q[i-17,2],i,N_vichy_2_q[i-17,1])
#			#boxplot
#			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_2_q[i-17,2],N_vichy_2_q[i-17,2],N_vichy_2_q[i-17,4],N_vichy_2_q[i-17,4]),col="orange")
#			#median
#			segments(i-0.3,N_vichy_2_q[i-17,3],i+0.3,N_vichy_2_q[i-17,3])
#		}
#		
#		
#		dev.off()
#
#
#	#==================================
#	# CHAP : Figure : Threshold
#	#==================================
#N_vichy_real=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")	
#	
#	under_10_vichy=array(0,dim=c(8000,20))
#	under_50_vichy=array(0,dim=c(8000,20))
#	under_100_vichy=array(0,dim=c(8000,20))
#	under_250_vichy=array(0,dim=c(8000,20))
#	under_500_vichy=array(0,dim=c(8000,20))
#	
#		
#	for (t in 24:43){
#	
#		for (i in 1:5000){
#			if(N_vichy_real[i,t] < 10){under_10_vichy[i,t-23]=1}  
#			if(N_vichy_real[i,t] < 50){under_50_vichy[i,t-23]=1}
#			if(N_vichy_real[i,t] < 100){under_100_vichy[i,t-23]=1}
#			if(N_vichy_real[i,t] < 250){under_250_vichy[i,t-23]=1}
#			if(N_vichy_real[i,t] < 500){under_500_vichy[i,t-23]=1}
#			
#		}
#	}
#	
#	
#	p_under_10_vichy=rep(0,20)
#	p_under_50_vichy=rep(0,20)
#	p_under_100_vichy=rep(0,20)
#	p_under_250_vichy=rep(0,20)
#	p_under_500_vichy=rep(0,20)
#	
#	
#	for (t in 1:20){
#		p_under_10_vichy[t]=mean(under_10_vichy[,t])
#		p_under_50_vichy[t]=mean(under_50_vichy[,t])
#		p_under_100_vichy[t]=mean(under_100_vichy[,t])
#		p_under_250_vichy[t]=mean(under_250_vichy[,t])
#		p_under_500_vichy[t]=mean(under_500_vichy[,t])
#		
#	}
#	
#		#------------------------
#		# Graph
#		#------------------------
#		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014_12_20/Threshold.png",width=800,height=800)
#		
#		par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
#		
#		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="Modele 2014_12_20")
#		
#		# trace l'axe des ordonn�es
#		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
#		# trace l'axe des abscisses
#		axis(1,at = c(1,9,20),
#				labels=c(2014,2020,2033),
#				cex.axis = 0.9,las = 1,col = "black")
#		
#		x=seq(1,20,1)
#		
#		
#		
#		points(x,p_under_10_vichy,col="grey85",pch=16)
#		segments(x[1:19],p_under_10_vichy[1:19],x[2:20],p_under_10_vichy[2:20],col="grey85")
#		
#		points(x,p_under_50_vichy,col="grey75",pch=16)
#		segments(x[1:19],p_under_50_vichy[1:19],x[2:20],p_under_50_vichy[2:20],col="grey75")
#		
#		points(x,p_under_100_vichy,col="grey65",pch=16)
#		segments(x[1:19],p_under_100_vichy[1:19],x[2:20],p_under_100_vichy[2:20],col="grey65")
#		
#		points(x,p_under_250_vichy,col="grey55",pch=16)
#		segments(x[1:19],p_under_250_vichy[1:19],x[2:20],p_under_250_vichy[2:20],col="grey55")
#		
#		points(x,p_under_500_vichy,col="grey45",pch=16)
#		segments(x[1:19],p_under_500_vichy[1:19],x[2:20],p_under_500_vichy[2:20],col="grey45")
#		
#		
#		legend(15,1,legend=c(expression(p^treshold < 500),expression(p^treshold < 250),expression(p^treshold < 100),expression(p^treshold < 50),expression(p^treshold < 10)),
#				pch=c(16,16,16,16,16),col=c("grey45","grey55","grey65","grey75","grey85"),bty="n" )
#		
#		
#		dev.off()

#================================================
# ML
# CHAP : Contribution stocking en pourcentage
#================================================

#	pour_stocking_vichy=array(0,dim=c(5000,T))


#	for (t in 1:T){
#		for (i in 1:5000){
#			pour_stocking_vichy[i,t]= (S_vichy_real[i,t]-S_vichy[i,t])/S_vichy_real[i,t]  
#			}
#	} 



#	pour_stocking_vichy_q=array(0,dim=c(T,5))


#	for (i in 1:T){
#		pour_stocking_vichy_q[i,]=quantile(pour_stocking_vichy[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)

#	}

#	min(pour_stocking_vichy_q)
#	max(pour_stocking_vichy_q)

#--------------------------
#Graph with all years
#--------------------------
#		png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014_12_20/ContributionStockingAllierPopulation_.png",width=800,height=800)

#		par(mfrow=c(1,1))

#		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="%stocking",main="Contribution of stocking to adults returns",cex.lab = 1.5)

#		# trace l'axe des ordonn�es
#		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.2,las = 1,lwd=2,col = "black")
#		# trace l'axe des abscisses
#		axis(1,at = c(1,6,16,26,T),
#				labels=c(1975,1980,1990,2000,2013),
#				cex.axis = 1.2,las = 1,lwd=2,col = "black")

#		#text(T,15,labels=expression(italic("a.")))



#		for(i in 7:T){
#			#whiskers
#			#95%
#			segments(i-0.15,pour_stocking_vichy_q[i,5],i+0.15,pour_stocking_vichy_q[i,5])
#			segments(i,pour_stocking_vichy_q[i,4],i,pour_stocking_vichy_q[i,5])
#			#5%
#			segments(i-0.15,pour_stocking_vichy_q[i,1],i+0.15,pour_stocking_vichy_q[i,1])
#			segments(i,pour_stocking_vichy_q[i,2],i,pour_stocking_vichy_q[i,1])
#			#boxplot
#			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(pour_stocking_vichy_q[i,2],pour_stocking_vichy_q[i,2],pour_stocking_vichy_q[i,4],pour_stocking_vichy_q[i,4]),col="light grey")
#			#median
#			segments(i-0.3,pour_stocking_vichy_q[i,3],i+0.3,pour_stocking_vichy_q[i,3])
#		}


#	dev.off()



#################################################
# Code Guillaume contribution stocking en ratio
#################################################

#plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-0.5),xlab="Years",ylim=c(-1,10),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Langeac-Poutes")
#
## trace l'axe des ordonn�es
#axis(2,at = c(-1,0,5,10),labels=c(-1,0,5,10),cex.axis = 0.9,las = 1,col = "black")
## trace l'axe des abscisses
#axis(1,at = c(1,6,16,26,T),
#		labels=c(1975,1980,1990,2000,2013),
#		cex.axis = 0.9,las = 1,col = "black")
#
#
#text(T,30,labels=expression(italic("b.")))
#
#
#
#for(i in 3:T){
#	#whiskers
#	#95%
#	segments(i-0.15,ratio_stocking_langeac_q[i,5],i+0.15,ratio_stocking_langeac_q[i,5])
#	segments(i,ratio_stocking_langeac_q[i,4],i,ratio_stocking_langeac_q[i,5])
#	#5%
#	segments(i-0.15,ratio_stocking_langeac_q[i,1],i+0.15,ratio_stocking_langeac_q[i,1])
#	segments(i,ratio_stocking_langeac_q[i,2],i,ratio_stocking_langeac_q[i,1])
#	#boxplot
#	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_stocking_langeac_q[i,2],ratio_stocking_langeac_q[i,2],ratio_stocking_langeac_q[i,4],ratio_stocking_langeac_q[i,4]),col="light grey")
#	#median
#	segments(i-0.3,ratio_stocking_langeac_q[i,3],i+0.3,ratio_stocking_langeac_q[i,3])
#}
#
#
#
#segments(1,0,(T+1),lty=2,col="red")
#
#
#
######################
######################
#
#
#plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-1,15),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Upstream Poutes")
#
## trace l'axe des ordonn�es
#axis(2,at = c(-1,0,5,10,15),labels=c(-1,0,5,10,15),cex.axis = 0.9,las = 1,col = "black")
## trace l'axe des abscisses
#axis(1,at = c(1,6,16,26,T),
#		labels=c(1975,1980,1990,2000,2013),
#		cex.axis = 0.9,las = 1,col = "black")
#
#
#text(T,155,labels=expression(italic("c.")))
#
#abline(v=11.5,lty=2)
#
#for(i in 12:T){
#	#whiskers
#	#95%
#	segments(i-0.15,ratio_stocking_poutes_q[i,5],i+0.15,ratio_stocking_poutes_q[i,5])
#	segments(i,ratio_stocking_poutes_q[i,4],i,ratio_stocking_poutes_q[i,5])
#	#5%
#	segments(i-0.15,ratio_stocking_poutes_q[i,1],i+0.15,ratio_stocking_poutes_q[i,1])
#	segments(i,ratio_stocking_poutes_q[i,2],i,ratio_stocking_poutes_q[i,1])
#	#boxplot
#	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_stocking_poutes_q[i,2],ratio_stocking_poutes_q[i,2],ratio_stocking_poutes_q[i,4],ratio_stocking_poutes_q[i,4]),col="light grey")
#	#median
#	segments(i-0.3,ratio_stocking_poutes_q[i,3],i+0.3,ratio_stocking_poutes_q[i,3])
#}
#text(4,10,paste( "upstream Poutes\n inaccessible"))
#
#segments(1,0,(T+1),lty=2,col="red")
#
#dev.off()
#
#
#med_S_vichy_real=array(0,dim=c(T,1))
#for (t in 1:T){
#	med_S_vichy_real[t,]=median(S_vichy_real[,t])
#}
#
#
#med_S_vichy=array(0,dim=c(T,1))
#
#for (t in 1:T){
#	med_S_vichy[t,]=median(S_vichy[,t])
#}
#
#plot(med_S_vichy_real,col="orange")
#points(med_S_vichy,col="blue")
