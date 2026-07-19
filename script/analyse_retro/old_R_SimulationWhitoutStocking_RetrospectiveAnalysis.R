# TODO: Add comment
# 
# Author: Guillaume Dauphin
###############################################################################

#Output Allier 2014.05.30
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_05_30")
#SurfERR+SurfDev 2014.08.28
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2014_08_26_SufERR+SurfDev")


library(coda)
library(boot)

T=39

#taking coda of the first 6 years of Returns to Vichy

N_vichy_inits=read.coda("N_VichyCODAchain1.txt","N_VichyCODAindex.txt")


#taking coda of the first 6 years of p_langeac

p_langeac_inits=read.coda("p_langeacCODAchain1.txt","p_langeacCODAindex.txt")


#others parameters required

s_juv2ad=read.coda("s_juv2adCODAchain1.txt","s_juv2adCODAindex.txt")

S_juv_JP=c(2574236,434552,655661)
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

for (t in 18:39){
	for (i in 1:5000){	
		ratio_wild[i,t] = (dmoy_wild_V[i,t-6] + dmoy_wild_L[i,t-6]+ dmoy_wild_P[i,t-15] + dmoy_wild_V[i,t-5] + dmoy_wild_L[i,t-5]+ dmoy_wild_P[i,t-14] + dmoy_wild_V[i,t-4] + dmoy_wild_L[i,t-4] + dmoy_wild_P[i,t-13]) / (dmoy_tot_V[i,t-6] + dmoy_tot_L[i,t-6] + dmoy_tot_P[i,t-15]+ dmoy_tot_V[i,t-5] + dmoy_tot_L[i,t-5]+ dmoy_tot_P[i,t-14] +dmoy_tot_V[i,t-4] + dmoy_tot_L[i,t-4]+ dmoy_tot_P[i,t-13])
	}
}





for (t in 1:6){
	for (i in 1:5000){ 
		
		N_langeac[i,t]=p_langeac_inits[i,t]* (N_vichy_inits[i,t]-C_up[t]-p_reach_V[i] * C_dwn[t])
		
		S_vichy[i,t]= N_vichy_inits[i,t]-N_langeac[i,t]-C_up[t]-p_reach_V[i] * C_dwn[t]
		S_langeac[i,t]= N_langeac[i,t]
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
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
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		
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
		
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		
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
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
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
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
	}
	
}



for (t in 16:16){
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] 	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))		
		
		
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
	}
}


for (t in 17:17){	
	for (i in 1:5000){
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] 	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]  = exp((log(((S_vichy[i,t]/S_juv_JP[1])   / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) ))   * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1] = exp((log(((S_poutes[i,t]/S_juv_JP[3])  / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) ))  * exp(nu_wild[i,3]))+ res_wild_poutes[i,t] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
		
	}
}

for (t in 18:21){
	for (i in 1:5000){	
		juv_tot_vichy[i,t]  = (1/3) * juv_vichy[i,t-3]   + (1/3) * juv_vichy[i,t-4]   + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t]= (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3]  + (1/3) * juv_poutes[i,t-4]  + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
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
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
	}
}



for (t in 22:24){
	for (i in 1:5000){	
		juv_tot_vichy[i,t]  = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t]= (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
	}
}


for (t in 25:27){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=p_langeac[i,t]* max( N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=p_poutes[i,t]*N_langeac[i,t]	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]  = exp((log(((S_vichy[i,t]/S_juv_JP[1])  / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) ))   * exp(nu_wild[i,1])) + res_wild_vichy[i,t]    ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2])/ (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2])) + res_wild_langeac[i,t]  ))
		d_moy_poutes[i,t+1] = exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) ))  * exp(nu_wild[i,3])) + res_wild_poutes[i,t-11]))
		
		juv_vichy[i,t+1]  =d_moy_vichy[i,t+1]  *S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1] =d_moy_poutes[i,t+1] *S_juv_JP[3]
	}
}




for (t in 28:31){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
	}
}



for (t in 32:34){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)	
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
	}
}



for (t in 35:36){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_P[i,t]= rho_station[i] * (ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_P[i,t]
		
		p_poutes[i,t]=inv.logit( logit(ratio_juv_P[i,t]) + adjust_p_P[i] + res_p_poutes[i,t-11] )	
		
		
		N_vichy[i,t]=exp( log( exp(level_s[i] * I_surv[i,t-7]  ) *s_juv2ad[i] * juv_tot_system[i,t]) +res_vichy[i,t-6] )
		
		N_langeac[i,t]=max(p_langeac[i,t]*  N_vichy[i,t] - ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		
		N_poutes[i,t]=max(p_poutes[i,t]*N_langeac[i,t],1)
		
		S_vichy[i,t]= max(N_vichy[i,t]-N_langeac[i,t]- ratio_wild[i,t] *(C_up[t]+p_reach_V[i] * C_dwn[t]),1)
		S_langeac[i,t]=max( N_langeac[i,t]-N_poutes[i,t],1)
		S_poutes[i,t]=max(N_poutes[i,t],1)
		
		
		
		d_moy_vichy[i,t+1]= exp((log(((S_vichy[i,t]/S_juv_JP[1]) / (alpha_dd[i] + beta_dd[i] * (S_vichy[i,t]/S_juv_JP[1]) )) * exp(nu_wild[i,1]))+ res_wild_vichy[i,t] ))
		d_moy_langeac[i,t+1]= exp((log(((S_langeac[i,t]/S_juv_JP[2]) / (alpha_dd[i] + beta_dd[i] * (S_langeac[i,t]/S_juv_JP[2]) )) * exp(nu_wild[i,2]))+ res_wild_langeac[i,t] ))
		d_moy_poutes[i,t+1]= exp((log(((S_poutes[i,t]/S_juv_JP[3]) / (alpha_dd[i] + beta_dd[i] * (S_poutes[i,t]/S_juv_JP[3]) )) * exp(nu_wild[i,3]))+ res_wild_poutes[i,t-11] ))
		
		juv_vichy[i,t+1]=d_moy_vichy[i,t+1]*S_juv_JP[1]
		juv_langeac[i,t+1]=d_moy_langeac[i,t+1]*S_juv_JP[2]
		juv_poutes[i,t+1]=d_moy_poutes[i,t+1]*S_juv_JP[3]
	}
}


for (t in 37:T){
	for (i in 1:5000){	
		juv_tot_vichy[i,t] = (1/3) * juv_vichy[i,t-3] + (1/3) * juv_vichy[i,t-4] + (1/3) * juv_vichy[i,t-5] 
		juv_tot_langeac[i,t] = (1/3) * juv_langeac[i,t-3] + (1/3) * juv_langeac[i,t-4] + (1/3) * juv_langeac[i,t-5] 
		juv_tot_poutes[i,t] = (1/3) * juv_poutes[i,t-3] + (1/3) * juv_poutes[i,t-4] + (1/3) * juv_poutes[i,t-5]	
		
		juv_tot_system[i,t] = juv_tot_vichy[i,t]+juv_tot_langeac[i,t] +juv_tot_poutes[i,t]
		
		ratio_juv_V[i,t] =1 - ratio_juv_prod_L[i,t] 
		
		ratio_juv_prod_L[i,t] =  (juv_tot_langeac[i,t] + juv_tot_poutes[i,t]) / (juv_tot_vichy[i,t] +juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
		ratio_juv_L[i,t]= rho_station[i] * (ratio_habitat[2]+ratio_habitat[3]) + (1 -  rho_station[i]) * ratio_juv_prod_L[i,t]
		
		p_langeac[i,t]=inv.logit( logit(ratio_juv_L[i,t]) + adjust_p_L[i] + res_p_langeac[i,t] ) 
		
		
		ratio_juv_prod_P[i,t] =  juv_tot_poutes[i,t] / ( juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t] )
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
save(juv_tot_vichy,juv_tot_langeac,juv_tot_poutes,juv_tot_system,ratio_juv_V,
		ratio_juv_prod_L,ratio_juv_L,p_langeac,ratio_juv_prod_P,ratio_juv_P,
		p_poutes,N_vichy,temp,N_langeac,N_poutes,S_vichy,S_langeac,S_poutes ,file="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/outputAllier2014.05.30.RData")

load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/outputAllier2014.05.30.RData")

N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")

###############################
#
# ne marche surement pas
#
#


par(mfrow=c(6,5))

x=seq(0,2,0.01)

for (t in 6:(T-1)){
	plot(d_moy_vichy[1:5000,t],dmoy_tot_V[1:5000,t])
	points(x,x,col="red",type="l")
}



par(mfrow=c(6,5))

x=seq(0,5000,1)



for (t in 7:(T-1)){
	plot(S_vichy[1:5000,t],S_vichy_real[1:5000,t])
	points(x,x,col="red",type="l")
}




par(mfrow=c(6,5))

x=seq(0,5000,1)



for (t in 7:(T-1)){
	plot(N_vichy[1:5000,t],N_vichy_real[1:5000,t])
	points(x,x,col="red",type="l")
}





for (t in 1:(T-1)){ print(quantile(d_moy_vichy[,t]))}




for (t in 1:(T-1)){
	#print(quantile(d_moy_vichy[,t]))
	#print(quantile(d_moy_langeac[,t]))
	print(quantile(d_moy_poutes[,t]))
	
}


###############################
#
# Ca remarche
#
#




#######################################
# Figure Spawners
#######################################

S_vichy_q=array(0,dim=c(T,5))
S_langeac_q=array(0,dim=c(T,5))
S_poutes_q=array(0,dim=c(T,5))






for (i in 1:T){
	S_vichy_q[i,]=quantile(S_vichy[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	S_langeac_q[i,]=quantile(S_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	S_poutes_q[i,]=quantile(S_poutes[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
	
}



#Graph with all years
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014.05.30/outputAllier_spawners_WithoutSpawners.png",width=800,height=800)
par(mfrow=c(3,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,5000),ylab=expression(italic(Spawners)),main="Vichy-Langeac")

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000),labels=c(0,1000,2000,3000,4000,5000),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")

text(37,2500,labels=expression(italic("a.")))



for(i in 7:T){
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

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,800),ylab=expression(italic(Spawners)),main="Langeac-Poutès")

# trace l'axe des ordonnées
axis(2,at = c(0,200,400,600,800),labels=c(0,200,400,600,800),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(37,800,labels=expression(italic("b.")))



for(i in 7:T){
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


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,300),ylab=expression(italic(Spawners)),main="Upstream Poutès")

# trace l'axe des ordonnées
axis(2,at = c(0,100,200,300),labels=c(0,100,200,300),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(37,300,labels=expression(italic("c.")))

abline(v=11.5,lty=2)

for(i in 12:T){
	#whiskers
	#95%
	segments(i-0.15,S_poutes_q[i,5],i+0.15,S_poutes_q[i,5])
	segments(i,S_poutes_q[i,4],i,S_poutes_q[i,5])
	#5%
	segments(i-0.15,S_poutes_q[i,1],i+0.15,S_poutes_q[i,1])
	segments(i,S_poutes_q[i,2],i,S_poutes_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_poutes_q[i,2],S_poutes_q[i,2],S_poutes_q[i,4],S_poutes_q[i,4]),col="light grey")
	#median
	segments(i-0.3,S_poutes_q[i,3],i+0.3,S_poutes_q[i,3])
}
text(4,160,paste( "upstream Poutès\n inaccessible"))

dev.off()



#######################################
# Figure Spawners (data
#######################################


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






for (i in 1:T){
	S_vichy_q[i,]=quantile(S_vichy_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	S_langeac_q[i,]=quantile(S_langeac_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}



#Graph with all years
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014.05.30/outputAllier_spawners_reels.png",width=800,height=800)
par(mfrow=c(3,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,5000),ylab=expression(italic(Spawners)),main="Vichy-Langeac")

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000),labels=c(0,1000,2000,3000,4000,5000),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")

text(T,5000,labels=expression(italic("a.")))



for(i in 7:T){
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

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,800),ylab=expression(italic(Spawners)),main="Langeac-Poutès")

# trace l'axe des ordonnées
axis(2,at = c(0,200,400,600,800),labels=c(0,200,400,600,800),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(T,800,labels=expression(italic("b.")))



for(i in 7:T){
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


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,300),ylab=expression(italic(Spawners)),main="Upstream Poutès")

# trace l'axe des ordonnées
axis(2,at = c(0,100,200,300),labels=c(0,100,200,300),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(37,300,labels=expression(italic("c.")))

points(x=seq(12,T,1),y=S_poutes_counter[12:T],pch=16)
abline(v=11.5,lty=2)
text(4,160,paste( "upstream Poutès\n inaccessible"))


dev.off()

#######################################
#differences repeuplement - sans repeuplement
#######################################


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




#Graph with all years
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014.05.30/outputAllier_spawners_DiffWith_WithoutStocking.png",width=800,height=800)

par(mfrow=c(3,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-500,1000),ylab=expression(italic(S^stocking-S^without)),main="Vichy-Langeac")

# trace l'axe des ordonnées
axis(2,at = c(-500,-250,0,250,500,750,1000),labels=c(-500,-250,0,250,500,750,1000),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")

text(37,1800,labels=expression(italic("a.")))



for(i in 7:T){
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



segments(1,0,38,lty=2,col="red")



#####################
#####################

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-150,350),ylab=expression(italic(S^stocking-S^without)),main="Langeac-Poutès")

# trace l'axe des ordonnées
axis(2,at = c(-100,0,100,200,350),labels=c(-100,0,100,200,350),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(37,450,labels=expression(italic("b.")))



for(i in 7:T){
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




segments(1,0,38,lty=2,col="red")



#####################
#####################


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-50,155),ylab=expression(italic(S^stocking-S^without)),main="Upstream Poutès")

# trace l'axe des ordonnées
axis(2,at = c(-50,0,50,100,150),labels=c(-50,0,50,100,150),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(37,155,labels=expression(italic("c.")))

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
text(4,4,paste( "upstream Poutès\n inaccessible"))



segments(11.5,0,38,lty=2,col="red")

dev.off()



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







#Graph with all years
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014.05.30/outputAllier_ContributionStockingAllierPopulation.png",width=800,height=800)

par(mfrow=c(3,1),mar=c(4,7.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-1,7),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Vichy-Langeac")

# trace l'axe des ordonnées
axis(2,at = c(-1,0,1,2,3,4,5),labels=c(-1,0,1,2,3,4,5),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")

text(37,15,labels=expression(italic("a.")))



for(i in 7:T){
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



segments(1,0,38,lty=2,col="red")



#####################
#####################

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-0.5),xlab="Years",ylim=c(-1,10),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Langeac-Poutès")

# trace l'axe des ordonnées
axis(2,at = c(-1,0,5,10),labels=c(-1,0,5,10),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(37,30,labels=expression(italic("b.")))



for(i in 7:T){
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



segments(1,0,38,lty=2,col="red")



#####################
#####################


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-1,15),ylab=expression(italic(frac((S^stocking-S^without),S^without))),main="Upstream Poutès")

# trace l'axe des ordonnées
axis(2,at = c(-1,0,5,10,15),labels=c(-1,0,5,10,15),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")


text(37,155,labels=expression(italic("c.")))

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
text(4,10,paste( "upstream Poutès\n inaccessible"))

segments(1,0,38,lty=2,col="red")

dev.off()
####################################
# Graph total returns
####################################

N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")


N_vichy_real_q=array(0,dim=c(T+5,5))

for (t in 1:T+5){
	N_vichy_real_q[t,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014.05.30/outputAllier_TotalReturns.png",width=800,height=800)

par(mfrow=c(1,2))


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,7000),ylab="Returns Vichy",main="With stocking")

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000),labels=c(0,1000,2000,3000,4000,5000,6000,7000),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")




for(i in 3:22){
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
		380,400,541,1238,662,
		510,950,572,421,491,
		227,755,861,819)



points(x=seq(23,T,1),data_vichy[23:T],pch=16)









N_vichy_q=array(0,dim=c(T,5))

for (t in 1:T){
	N_vichy_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}




plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,7000),ylab="Returns Vichy",main="Simulation without stocking")

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000),labels=c(0,1000,2000,3000,4000,5000,6000,7000),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")



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



# for t in 23:37 data 


par(mfrow=c(5,5))

for (t in 7:22){
	plot(N_vichy[1:5000,t],N_vichy_real[1:5000,t]) 
	points(x,x,col="red",type="l")
}







diff_N_vichy=array(0,dim=c(5000,T))


for (t in 7:22){
	for (i in 1:5000){
		diff_N_vichy[i,t]=N_vichy_real[i,t]-N_vichy[i,t] 
	}
} 

for (t in 23:T){
	for (i in 1:5000){
		diff_N_vichy[i,t]=data_vichy[t]-N_vichy[i,t] 
	}
} 




diff_N_vichy_q=array(0,dim=c(T,5))

for (t in 7:T){
	diff_N_vichy_q[t,]=quantile(diff_N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}




par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(-500,1000),ylab=expression(Returns^stocking-Returns^without),main="Difference of returns in Vichy with or without stocking")

# trace l'axe des ordonnées
axis(2,at = c(-500,0,500,1000),labels=c(-500,0,500,1000),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")



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




# Graph projection 20 years



png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014.05.30/outputAllier_TotalReturns_proj20years.png",width=800,height=800)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,7000),ylab="Returns Vichy",main="20 years projection without stocking")

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000),labels=c(0,1000,2000,3000,4000,5000,6000,7000),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,37,46,56),
		labels=c(1975,1980,1990,2000,2011,2020,2030),
		cex.axis = 0.9,las = 1,col = "black")




for(i in 3:22){
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
		380,400,541,1238,657,
		510,950,572,421,491,
		227,755,861,819)



points(x=seq(23,T,1),data_vichy[23:T],pch=16)



for(i in T+1:T+20){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_real_q[i-15,5],i+0.15,N_vichy_real_q[i-15,5])
	segments(i,N_vichy_real_q[i-15,4],i,N_vichy_real_q[i-15,5])
	#5%
	segments(i-0.15,N_vichy_real_q[i-15,1],i+0.15,N_vichy_real_q[i-15,1])
	segments(i,N_vichy_real_q[i-15,2],i,N_vichy_real_q[i-15,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i-15,2],N_vichy_real_q[i-15,2],N_vichy_real_q[i-15,4],N_vichy_real_q[i-15,4]),col="orange")
	#median
	segments(i-0.3,N_vichy_real_q[i-15,3],i+0.3,N_vichy_real_q[i-15,3])
}


dev.off()

under_10_vichy=array(0,dim=c(8000,20))
under_50_vichy=array(0,dim=c(8000,20))
under_100_vichy=array(0,dim=c(8000,20))
under_250_vichy=array(0,dim=c(8000,20))
under_500_vichy=array(0,dim=c(8000,20))


for (t in 23:42){
	
	for (i in 1:5000){
		if(N_vichy_real[i,t] < 10){under_10_vichy[i,t-22]=1} 
		if(N_vichy_real[i,t] < 50){under_50_vichy[i,t-22]=1}
		if(N_vichy_real[i,t] < 100){under_100_vichy[i,t-22]=1}
		if(N_vichy_real[i,t] < 250){under_250_vichy[i,t-22]=1}
		if(N_vichy_real[i,t] < 500){under_500_vichy[i,t-22]=1}
		
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
png(filename="C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/img/simulation/2014.05.30/outputAllier_Threshold.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,9,19),
		labels=c(2012,2020,2030),
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




