# TODO: Add comment
# 
# Author: marion.legrand
###############################################################################


#Modèle 2017.08.29_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2016
setwd("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")


library(coda)
library(boot)
library(stringr)

T=42

surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)
S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Créé une 5eme ligne qui correspond � la somme par année des surfaces sur l'ensemble des secteurs

S_juv_JP_tot<-S_juv_JP[,T]


#=================================================================================================
# Diagnostique en saturant le milieu en géniteurs
# l'idée est de voir la variabilité du recrutement même quand saturation du milieu en géniteurs
#=================================================================================================
#alpha_dd=read.coda("simulation/alpha_ddCODAchain1.txt","simulation/alpha_ddCODAindex.txt")
#beta_dd=read.coda("simulation/beta_ddCODAchain1.txt","simulation/beta_ddCODAindex.txt")
#nu_d=read.coda("nu_dCODAchain1.txt","nu_dCODAindex.txt")
#res_wild_vichy=read.coda("simulation/res_wild_vichyCODAchain1.txt","simulation/res_wild_vichyCODAindex.txt")
#res_wild_alagnon=read.coda("simulation/res_wild_alagnonCODAchain1.txt","simulation/res_wild_alagnonCODAindex.txt")
#res_wild_langeac=read.coda("simulation/res_wild_langeacCODAchain1.txt","simulation/res_wild_langeacCODAindex.txt")
#res_wild_poutes=read.coda("simulation/res_wild_poutesCODAchain1.txt","simulation/res_wild_poutesCODAindex.txt")
#Rmax=read.coda("RmaxCODAchain1.txt","RmaxCODAindex.txt")
#
##charge données (pour avoir I_juv_moy et stock_juv directement des data utilisés pour faire tourner le modèle openBugs
#library(stringr)
#bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2016_2017_08_29.R")
#source("data_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2016_2017_08_29.R")
#
#surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
#S_juv_JP<-matrix(surf,nrow=4)
#S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Créé une 5eme ligne qui correspond � la somme par année des surfaces sur l'ensemble des secteurs
#
#S_juv_JP_tot<-S_juv_JP[,T]
#
#d_moy_vichy=array(0,dim=c(5000,T+1))
#d_moy_alagnon=array(0,dim=c(5000,T+1))
#d_moy_langeac=array(0,dim=c(5000,T+1))
#d_moy_poutes=array(0,dim=c(5000,T+1))
#
##On reconstruit les densités de juvéniles en saturant en géniteur (10 000 dans chaque zone)
#S_vichy=50000
#S_alagnon=50000
#S_langeac=50000
#S_poutes=50000
#
#for (t in 1:12){
#	for (i in 1:5000){		
#d_moy_vichy[i,t+1]= exp((log(((S_vichy/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy/S_juv_JP[1,t]) )) * exp(nu_d[i,1]))+ res_wild_vichy[i,t] ))
#d_moy_alagnon[i,t+1]= exp((log(((S_alagnon/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_alagnon/S_juv_JP[2,t]) )) * exp(nu_d[i,2]))+ res_wild_alagnon[i,t] ))
#d_moy_langeac[i,t+1]= exp((log(((S_langeac/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac/S_juv_JP[3,t]) )) * exp(nu_d[i,3]))+ res_wild_langeac[i,t] ))
#	}
#}
#for (t in 13:(T-1)){	
#	for (i in 1:5000){
#d_moy_vichy[i,t+1]= exp((log(((S_vichy/S_juv_JP[1,t]) / (alpha_dd[i] + beta_dd[i] * (S_vichy/S_juv_JP[1,t]) )) * exp(nu_d[i,1]))+ res_wild_vichy[i,t] ))
#d_moy_alagnon[i,t+1]= exp((log(((S_alagnon/S_juv_JP[2,t]) / (alpha_dd[i] + beta_dd[i] * (S_alagnon/S_juv_JP[2,t]) )) * exp(nu_d[i,2]))+ res_wild_alagnon[i,t] ))
#d_moy_langeac[i,t+1]= exp((log(((S_langeac/S_juv_JP[3,t]) / (alpha_dd[i] + beta_dd[i] * (S_langeac/S_juv_JP[3,t]) )) * exp(nu_d[i,3]))+ res_wild_langeac[i,t] ))
#d_moy_poutes[i,t+1]= exp((log(((S_poutes/S_juv_JP[4,t]) / (alpha_dd[i] + beta_dd[i] * (S_poutes/S_juv_JP[4,t]) )) * exp(nu_d[i,4]))+ res_wild_poutes[i,t-11] ))
#	}
#}
#
#DC_tot_tot=array(rep(0,T*5000),dim=c(5000,T)) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
#DC_tot=array(rep(0,T*5000),dim=c(5000,T)) # en prenant en compte uniquement les surfaces accessibles
#
##-----------------------------
## Pour DC_tot_tot
##-----------------------------
#
##Attention dans le calcul de la part de juv produite sur la part de la capacité d'accueil, il faut prendre en compte
##pour Rmax que les secteurs ne produisent pas à équivalence
#Rmax_ponder_V<-array(0,dim=c(10000,T))
#Rmax_ponder_A<-array(0,dim=c(10000,T))
#Rmax_ponder_L<-array(0,dim=c(10000,T))
#Rmax_ponder_P<-array(0,dim=c(10000,T))
#Rmax_ponder<-array(0,dim=c(10000,T))
#for (t in 1:T){
#	for (i in 1:10000){
#		Rmax_ponder_V[i,t]<-Rmax[i]*exp(nu_d[i,1])*S_juv_JP[1,t]
#		Rmax_ponder_A[i,t]<-Rmax[i]*exp(nu_d[i,2])*S_juv_JP[2,t]
#		Rmax_ponder_L[i,t]<-Rmax[i]*exp(nu_d[i,3])*S_juv_JP[3,t]
#		Rmax_ponder_P[i,t]<-Rmax[i]*exp(nu_d[i,4])*S_juv_JP[4,t]
#		Rmax_ponder[i,t]<-Rmax_ponder_V[i,t]+Rmax_ponder_A[i,t]+Rmax_ponder_L[i,t]+Rmax_ponder_P[i,t]
#	}
#}
#		
#for (t in 2:T){
#	for (i in 1:5000){
#		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
#		DC_tot_tot[i,t]<-((d_moy_vichy[i,t]*S_juv_JP[1,t]+d_moy_alagnon[i,t]*S_juv_JP[2,t]+d_moy_langeac[i,t]*S_juv_JP[3,t]+d_moy_poutes[i,t]*S_juv_JP[4,t])*100)/Rmax_ponder[i,t]	
#	}
#}
#
#for (t in 2:T){
#	for (i in 1:5000){
#		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
#		DC_tot[i,t]<-((d_moy_vichy[i,t]*S_juv_JP[1,t]+d_moy_alagnon[i,t]*S_juv_JP[2,t]+d_moy_langeac[i,t]*S_juv_JP[3,t]+d_moy_poutes[i,t]*S_juv_JP[4,t])*100)/Rmax_ponder[i,t]		
#	}
#}
#
#
#upper_tot_25_Rmax=array(0,dim=c(5000,T))
#upper_tot_50_Rmax=array(0,dim=c(5000,T))
#upper_tot_75_Rmax=array(0,dim=c(5000,T))
#
#
#
#for (t in 2:T){
#	
#	for (i in 1:5000){
#		if(DC_tot_tot[i,t] > 25){upper_tot_25_Rmax[i,t]=1}  
#		if(DC_tot_tot[i,t] > 50){upper_tot_50_Rmax[i,t]=1}
#		if(DC_tot_tot[i,t] > 75){upper_tot_75_Rmax[i,t]=1}
#	}
#}
#
#
#p_upper_tot_25_Rmax=rep(0,T)
#p_upper_tot_50_Rmax=rep(0,T)
#p_upper_tot_75_Rmax=rep(0,T)
#
#
#
#for (t in 2:T){
#	p_upper_tot_25_Rmax[t]=mean(upper_tot_25_Rmax[,t])
#	p_upper_tot_50_Rmax[t]=mean(upper_tot_50_Rmax[,t])
#	p_upper_tot_75_Rmax[t]=mean(upper_tot_75_Rmax[,t])
#	
#}
#
##-----------------------------
## Pour DC_tot
##-----------------------------
#
#upper_25_Rmax=array(0,dim=c(5000,T))
#upper_50_Rmax=array(0,dim=c(5000,T))
#upper_75_Rmax=array(0,dim=c(5000,T))
#
#
#
#for (t in 2:T){
#	
#	for (i in 1:5000){
#		if(DC_tot[i,t] > 25){upper_25_Rmax[i,t]=1}  
#		if(DC_tot[i,t] > 50){upper_50_Rmax[i,t]=1}
#		if(DC_tot[i,t] > 75){upper_75_Rmax[i,t]=1}
#	}
#}
#
#p_upper_25_Rmax=rep(0,T)
#p_upper_50_Rmax=rep(0,T)
#p_upper_75_Rmax=rep(0,T)
#
#
#
#for (t in 2:T){
#	p_upper_25_Rmax[t]=mean(upper_25_Rmax[,t])
#	p_upper_50_Rmax[t]=mean(upper_50_Rmax[,t])
#	p_upper_75_Rmax[t]=mean(upper_75_Rmax[,t])
#	
#}
#
#
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_SaturationGenTheorique_2018_10_10.png",width=800,height=800)
#
#plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Années","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5,cex.main=1,main=iconv("Diagnostique Conservation - Saturation théorique des habitats en géniteurs","UTF8"),sub=iconv("Surfaces totales même si non accessibles en début de période","UTF8"))
## trace l'axe des ordonn�es
#axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
## trace l'axe des abscisses
#axis(1,at = c(2,11,16,26,36,T),
#		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
#		cex.axis = 0.9,las = 1,col = "black")
#
#x=seq(1,T,1)
#
#points(x[2:T],p_upper_tot_25_Rmax[2:T],col="grey75",pch=16)
#segments(x[2:(T-1)],p_upper_tot_25_Rmax[2:(T-1)],x[3:T],p_upper_tot_25_Rmax[3:T],col="grey75")
#
#points(x[2:T],p_upper_tot_50_Rmax[2:T],col="grey65",pch=16)
#segments(x[2:(T-1)],p_upper_tot_50_Rmax[2:(T-1)],x[3:T],p_upper_tot_50_Rmax[3:T],col="grey55")
#
#points(x[2:T],p_upper_tot_75_Rmax[2:T],col="grey55",pch=16)
#segments(x[2:(T-1)],p_upper_tot_75_Rmax[2:(T-1)],x[3:T],p_upper_tot_75_Rmax[3:T],col="grey15")
#
#
#legend(35,0.95,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
#		pch=c(16,16,16,16),#16),
#		col=c("grey45","grey65","grey75"), 
#		bty="n" )
#
#abline(h=0.75,col="red",lty=2)
#abline(h=0.5,col="red")
#abline(h=0.25,col="red",lty=2)
#
#
#dev.off()
#
#
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_SaturationGenTheorique_2018_10_10.png",width=800,height=800)
#
#plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Années","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5,cex.main=1,main=iconv("Diagnostique Conservation - Saturation théorique des habitats en géniteurs","UTF8"),sub=iconv("Surfaces réellement accessibles - évolutif au cours du temps","UTF8"))
## trace l'axe des ordonn�es
#axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
## trace l'axe des abscisses
#axis(1,at = c(2,11,16,26,36,T),
#		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
#		cex.axis = 0.9,las = 1,col = "black")
#
#x=seq(1,T,1)
#
#points(x[2:T],p_upper_25_Rmax[2:T],col="grey75",pch=16)
#segments(x[2:(T-1)],p_upper_25_Rmax[2:(T-1)],x[3:T],p_upper_25_Rmax[3:T],col="grey75")
#
#points(x[2:T],p_upper_50_Rmax[2:T],col="grey65",pch=16)
#segments(x[2:(T-1)],p_upper_50_Rmax[2:(T-1)],x[3:T],p_upper_50_Rmax[3:T],col="grey55")
#
#points(x[2:T],p_upper_75_Rmax[2:T],col="grey55",pch=16)
#segments(x[2:(T-1)],p_upper_75_Rmax[2:(T-1)],x[3:T],p_upper_75_Rmax[3:T],col="grey15")
#
#
#legend(35,0.95,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
#		pch=c(16,16,16,16),#16),
#		col=c("grey45","grey65","grey75"), 
#		bty="n" )
#
#abline(h=0.75,col="red",lty=2)
#abline(h=0.5,col="red")
#abline(h=0.25,col="red",lty=2)
#
#
#dev.off()
#

#=================================================================
# Diagnostique sur tous les juvéniles quelque soit leur origine
#=================================================================

Rmax=read.coda("RmaxCODAchain1.txt","RmaxCODAindex.txt")
nu_d=read.coda("nu_dCODAchain1.txt","nu_dCODAindex.txt")
d_tot_moy_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt")
d_tot_moy_A=read.coda("dmoytot_ACODAchain1.txt","dmoytot_ACODAindex.txt")
d_tot_moy_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt")
d_tot_moy_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt")

DC_tot_tot=array(rep(0,T*5000),dim=c(5000,T)) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
DC_tot=array(rep(0,T*5000),dim=c(5000,T)) # en prenant en compte uniquement les surfaces accessibles

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

#Attention dans le calcul de la part de juv produite sur la part de la capacité d'accueil, il faut prendre en compte
#pour Rmax que les secteurs ne produisent pas à équivalence
Rmax_ponder_V<-array(0,dim=c(10000,T))
Rmax_ponder_A<-array(0,dim=c(10000,T))
Rmax_ponder_L<-array(0,dim=c(10000,T))
Rmax_ponder_P<-array(0,dim=c(10000,T))
Rmax_ponder<-array(0,dim=c(10000,T))
for (t in 1:T){
	for (i in 1:10000){
		Rmax_ponder_V[i,t]<-Rmax[i]*exp(nu_d[i,1])*S_juv_JP[1,t]
		Rmax_ponder_A[i,t]<-Rmax[i]*exp(nu_d[i,2])*S_juv_JP[2,t]
		Rmax_ponder_L[i,t]<-Rmax[i]*exp(nu_d[i,3])*S_juv_JP[3,t]
		Rmax_ponder_P[i,t]<-Rmax[i]*exp(nu_d[i,4])*S_juv_JP[4,t]
		Rmax_ponder[i,t]<-Rmax_ponder_V[i,t]+Rmax_ponder_A[i,t]+Rmax_ponder_L[i,t]+Rmax_ponder_P[i,t]
	}
}

for (t in 2:12){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot[i,t]<-((d_tot_moy_V[i,t-1]*S_juv_JP[1,t]+d_tot_moy_A[i,t-1]*S_juv_JP[2,t]+d_tot_moy_L[i,t-1]*S_juv_JP[3,t])*100)/Rmax_ponder[i,t] #(Rmax[i]*S_juv_JP_tot[5])
		}
}
for (t in 13:T){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot[i,t]<-((d_tot_moy_V[i,t-1]*S_juv_JP[1,t]+d_tot_moy_A[i,t-1]*S_juv_JP[2,t]+d_tot_moy_L[i,t-1]*S_juv_JP[3,t]+d_tot_moy_P[i,t-12]*S_juv_JP[4,t])*100)/Rmax_ponder[i,t] #(Rmax[i]*S_juv_JP_tot[5])		
	}
}

#-----------------------------
# Pour DC_tot
#-----------------------------

#for (t in 2:12){
#	for (i in 1:5000){
#		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
#		DC_tot[i,t]<-((d_tot_moy_V[i,t-1]*S_juv_JP[1,t]+d_tot_moy_A[i,t-1]*S_juv_JP[2,t]+d_tot_moy_L[i,t-1]*S_juv_JP[3,t])*100)/(Rmax[i]*S_juv_JP[5,t])
#	}
#}
#for (t in 13:T){
#	for (i in 1:5000){
#		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
#		DC_tot[i,t]<-((d_tot_moy_V[i,t-1]*S_juv_JP[1,t]+d_tot_moy_A[i,t-1]*S_juv_JP[2,t]+d_tot_moy_L[i,t-1]*S_juv_JP[3,t]+d_tot_moy_P[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])	
#	}
#}

#................................................
# on regarde le diagnostique ann�e par ann�e
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax=array(0,dim=c(5000,T))
upper_tot_50_Rmax=array(0,dim=c(5000,T))
upper_tot_75_Rmax=array(0,dim=c(5000,T))



for (t in 2:T){
	
	for (i in 1:5000){
		if(DC_tot_tot[i,t] > 25){upper_tot_25_Rmax[i,t]=1}  
		if(DC_tot_tot[i,t] > 50){upper_tot_50_Rmax[i,t]=1}
		if(DC_tot_tot[i,t] > 75){upper_tot_75_Rmax[i,t]=1}
	}
}


p_upper_tot_25_Rmax=rep(0,T)
p_upper_tot_50_Rmax=rep(0,T)
p_upper_tot_75_Rmax=rep(0,T)



for (t in 2:T){
	p_upper_tot_25_Rmax[t]=mean(upper_tot_25_Rmax[,t])
	p_upper_tot_50_Rmax[t]=mean(upper_tot_50_Rmax[,t])
	p_upper_tot_75_Rmax[t]=mean(upper_tot_75_Rmax[,t])
	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

#upper_25_Rmax=array(0,dim=c(5000,T))
#upper_50_Rmax=array(0,dim=c(5000,T))
#upper_75_Rmax=array(0,dim=c(5000,T))
#
#
#
#for (t in 2:T){
#	
#	for (i in 1:5000){
#		if(DC_tot[i,t] > 25){upper_25_Rmax[i,t]=1}  
#		if(DC_tot[i,t] > 50){upper_50_Rmax[i,t]=1}
#		if(DC_tot[i,t] > 75){upper_75_Rmax[i,t]=1}
#	}
#}
#
#p_upper_25_Rmax=rep(0,T)
#p_upper_50_Rmax=rep(0,T)
#p_upper_75_Rmax=rep(0,T)
#
#
#
#for (t in 2:T){
#	p_upper_25_Rmax[t]=mean(upper_25_Rmax[,t])
#	p_upper_50_Rmax[t]=mean(upper_50_Rmax[,t])
#	p_upper_75_Rmax[t]=mean(upper_75_Rmax[,t])
#	
#}


#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/DiagnostiqueConservation_annuel_surfTot_2018_04_20.png",width=800,height=800)
png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_2018_06_22.png",width=800,height=800)


#par(mfrow=c(1,1),mar=c(4,6.1,4,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(2,11,16,26,36,T),
		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,T,1)

points(x[2:T],p_upper_tot_25_Rmax[2:T],col="grey75",pch=16)
segments(x[2:(T-1)],p_upper_tot_25_Rmax[2:(T-1)],x[3:T],p_upper_tot_25_Rmax[3:T],col="grey75")

points(x[2:T],p_upper_tot_50_Rmax[2:T],col="grey65",pch=16)
segments(x[2:(T-1)],p_upper_tot_50_Rmax[2:(T-1)],x[3:T],p_upper_tot_50_Rmax[3:T],col="grey55")

points(x[2:T],p_upper_tot_75_Rmax[2:T],col="grey55",pch=16)
segments(x[2:(T-1)],p_upper_tot_75_Rmax[2:(T-1)],x[3:T],p_upper_tot_75_Rmax[3:T],col="grey15")


legend(14,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)


dev.off()


#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/DiagnostiqueConservation_annuel_surfTot_2018_04_20.png",width=800,height=800)
png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_2018_06_22.png",width=800,height=800)


#par(mfrow=c(1,1),mar=c(4,6.1,4,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(2,11,16,26,36,T),
		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,T,1)

points(x[2:T],p_upper_25_Rmax[2:T],col="grey75",pch=16)
segments(x[2:(T-1)],p_upper_25_Rmax[2:(T-1)],x[3:T],p_upper_25_Rmax[3:T],col="grey75")

points(x[2:T],p_upper_50_Rmax[2:T],col="grey65",pch=16)
segments(x[2:(T-1)],p_upper_50_Rmax[2:(T-1)],x[3:T],p_upper_50_Rmax[3:T],col="grey55")

points(x[2:T],p_upper_75_Rmax[2:T],col="grey55",pch=16)
segments(x[2:(T-1)],p_upper_75_Rmax[2:(T-1)],x[3:T],p_upper_75_Rmax[3:T],col="grey15")


legend(14,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)


dev.off()

#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq(T,2,-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_tot_mean5<-array(0,dim=c(5000,(length(seq)-1))) #surface totale quelque soit l'accessibilit�
DC_tot_mean5<-array(0,dim=c(5000,(length(seq)-1))) # surface en prenant en compte les pb d'accessibilit�

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_tot_mean5[i,s]<-mean(DC_tot_tot[i,((seq[s+1]+1):(seq[s]))])
	}
}

for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_mean5[i,s]<-mean(DC_tot[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_50_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_75_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_tot_mean5[i,t] > 25){upper_tot_25_Rmax_mean5[i,t]=1}  
		if(DC_tot_tot_mean5[i,t] > 50){upper_tot_50_Rmax_mean5[i,t]=1}
		if(DC_tot_tot_mean5[i,t] > 75){upper_tot_75_Rmax_mean5[i,t]=1}
	}
}


p_upper_tot_25_Rmax_mean5=rep(0,(length(seq)-1))
p_upper_tot_50_Rmax_mean5=rep(0,(length(seq)-1))
p_upper_tot_75_Rmax_mean5=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_tot_25_Rmax_mean5[t]=mean(upper_tot_25_Rmax_mean5[,t])
	p_upper_tot_50_Rmax_mean5[t]=mean(upper_tot_50_Rmax_mean5[,t])
	p_upper_tot_75_Rmax_mean5[t]=mean(upper_tot_75_Rmax_mean5[,t])	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))
upper_50_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))
upper_75_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_mean5[i,t] > 25){upper_25_Rmax_mean5[i,t]=1}  
		if(DC_tot_mean5[i,t] > 50){upper_50_Rmax_mean5[i,t]=1}
		if(DC_tot_mean5[i,t] > 75){upper_75_Rmax_mean5[i,t]=1}
	}
}


p_upper_25_Rmax_mean5=rep(0,(length(seq)-1))
p_upper_50_Rmax_mean5=rep(0,(length(seq)-1))
p_upper_75_Rmax_mean5=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_25_Rmax_mean5[t]=mean(upper_25_Rmax_mean5[,t])
	p_upper_50_Rmax_mean5[t]=mean(upper_50_Rmax_mean5[,t])
	p_upper_75_Rmax_mean5[t]=mean(upper_75_Rmax_mean5[,t])	
}

#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!

p_upper_tot_25_Rmax_mean5<-rev(p_upper_tot_25_Rmax_mean5)
p_upper_tot_50_Rmax_mean5<-rev(p_upper_tot_50_Rmax_mean5)
p_upper_tot_75_Rmax_mean5<-rev(p_upper_tot_75_Rmax_mean5)

p_upper_25_Rmax_mean5<-rev(p_upper_25_Rmax_mean5)
p_upper_50_Rmax_mean5<-rev(p_upper_50_Rmax_mean5)
p_upper_75_Rmax_mean5<-rev(p_upper_75_Rmax_mean5)

#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/DiagnostiqueConservation_mean5_2018_04_20.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_2018_06_22.png",width=800,height=800)
png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_surfTot_2018_06_22.png",width=800,height=800)

par(mar=c(6,6.1,4,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(4.5,(T+0.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T-35),(T-30),(T-25),(T-20),(T-15),(T-10),(T-5),T),
		labels=c(str_c("1977-",(1977+4),""),str_c((1977+5),"-"(1977+9)),str_c((1977+10),"-",(1977+14)),str_c((1977+15),"-"(1977+19)),str_c((1977+20),"-",(1977+24)),str_c((1977+25),"-",(1977+29)),str_c((1977+30),"-",(1977+34)),str_c((1977+35),"-",(1975+T-1))),
		cex.axis = 0.9,las = 2,col = "black")

x=rev(seq)

points(x[2:(length(seq))],p_upper_tot_25_Rmax_mean5[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)],p_upper_tot_25_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_tot_25_Rmax_mean5[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))],p_upper_tot_50_Rmax_mean5[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)],p_upper_tot_50_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_tot_50_Rmax_mean5[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))],p_upper_tot_75_Rmax_mean5[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)],p_upper_tot_75_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_tot_75_Rmax_mean5[2:(length(seq)-1)],col="grey15")


legend(5,0.7,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)

dev.off()


#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/DiagnostiqueConservation_mean5_2018_04_20.png",width=800,height=800)
#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_2018_06_22.png",width=800,height=800)
png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_2018_06_22.png",width=800,height=800)

par(mar=c(6,6.1,4,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(4.5,(T+0.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T-35),(T-30),(T-25),(T-20),(T-15),(T-10),(T-5),T),
		labels=c(str_c("1977-",(1977+4),""),str_c((1977+5),"-"(1977+9)),str_c((1977+10),"-",(1977+14)),str_c((1977+15),"-"(1977+19)),str_c((1977+20),"-",(1977+24)),str_c((1977+25),"-",(1977+29)),str_c((1977+30),"-",(1977+34)),str_c((1977+35),"-",(1975+T-1))),
		cex.axis = 0.9,las = 2,col = "black")

x=rev(seq)

points(x[2:(length(seq))],p_upper_25_Rmax_mean5[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)],p_upper_25_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_25_Rmax_mean5[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))],p_upper_50_Rmax_mean5[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)],p_upper_50_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_50_Rmax_mean5[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))],p_upper_75_Rmax_mean5[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)],p_upper_75_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_75_Rmax_mean5[2:(length(seq)-1)],col="grey15")


legend(5,0.7,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)

dev.off()

#===========================================
# Diagnostique sur les juv�niles sauvages
#===========================================

Rmax=read.coda("RmaxCODAchain1.txt","RmaxCODAindex.txt")
nu_d=read.coda("nu_dCODAchain1.txt","nu_dCODAindex.txt")
d_wild_moy_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt")
d_wild_moy_A=read.coda("dmoywild_ACODAchain1.txt","dmoywild_ACODAindex.txt")
d_wild_moy_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt")
d_wild_moy_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt")

DC_tot_wild_tot=array(rep(0,T*5000),dim=c(5000,T)) #en consid�rant toutes les surfaces m�me si elles ne sont pas accessibles
#DC_tot_wild=array(rep(0,T*5000),dim=c(5000,T))	#en consid�rant les surfaces accessibles uniquement (change au fil des ann�es
##Pour décliner par secteur
DC_tot_wild_V=array(0,dim=c(5000,T))
DC_tot_wild_A=array(0,dim=c(5000,T))
DC_tot_wild_L=array(0,dim=c(5000,T))
DC_tot_wild_P=array(0,dim=c(5000,T))
#-----------------------------
# Sur l'ensemble du syst�me
#-----------------------------

#Attention dans le calcul de la part de juv produite sur la part de la capacité d'accueil, il faut prendre en compte
#pour Rmax que les secteurs ne produisent pas à équivalence
Rmax_ponder_V<-array(0,dim=c(10000,T))
Rmax_ponder_A<-array(0,dim=c(10000,T))
Rmax_ponder_L<-array(0,dim=c(10000,T))
Rmax_ponder_P<-array(0,dim=c(10000,T))
Rmax_ponder<-array(0,dim=c(10000,T))
for (t in 1:T){
	for (i in 1:10000){
		Rmax_ponder_V[i,t]<-Rmax[i]*exp(nu_d[i,1])*S_juv_JP[1,t]
		Rmax_ponder_A[i,t]<-Rmax[i]*exp(nu_d[i,2])*S_juv_JP[2,t]
		Rmax_ponder_L[i,t]<-Rmax[i]*exp(nu_d[i,3])*S_juv_JP[3,t]
		Rmax_ponder_P[i,t]<-Rmax[i]*exp(nu_d[i,4])*S_juv_JP[4,t]
		Rmax_ponder[i,t]<-Rmax_ponder_V[i,t]+Rmax_ponder_A[i,t]+Rmax_ponder_L[i,t]+Rmax_ponder_P[i,t]
	}
}

#En plus de la part de juv sauvage tot sur la capacité d'accueil, on calcule aussi par secteur pour pouvoir sortir un graph total et par secteur

for (t in 2:12){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_wild_tot[i,t]<-((d_wild_moy_V[i,t-1]*S_juv_JP[1,t]+d_wild_moy_A[i,t-1]*S_juv_JP[2,t]+d_wild_moy_L[i,t-1]*S_juv_JP[3,t])*100)/Rmax_ponder[i,t] #(Rmax[i]*S_juv_JP_tot[5])
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		#DC_tot[i,t]<-((d_wild_moy_V[i,t-1]*S_juv_JP[1,t]+d_wild_moy_A[i,t-1]*S_juv_JP[2,t]+d_wild_moy_L[i,t-1]*S_juv_JP[3,t])*100)/(Rmax[i]*S_juv_JP[5,t])
		
		##Par secteur
		DC_tot_wild_V[i,t]<-d_wild_moy_V[i,t-1]*S_juv_JP[1,t]*100/Rmax_ponder_V[i,t]
		DC_tot_wild_A[i,t]<-d_wild_moy_A[i,t-1]*S_juv_JP[2,t]*100/Rmax_ponder_A[i,t]
		DC_tot_wild_L[i,t]<-d_wild_moy_L[i,t-1]*S_juv_JP[3,t]*100/Rmax_ponder_L[i,t]
		}
}
for (t in 13:T){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_wild_tot[i,t]<-((d_wild_moy_V[i,t-1]*S_juv_JP[1,t]+d_wild_moy_A[i,t-1]*S_juv_JP[2,t]+d_wild_moy_L[i,t-1]*S_juv_JP[3,t]+d_wild_moy_P[i,t-12]*S_juv_JP[4,t])*100)/Rmax_ponder[i,t] #(Rmax[i]*S_juv_JP_tot[5])
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		#DC_tot[i,t]<-((d_wild_moy_V[i,t-1]*S_juv_JP[1,t]+d_wild_moy_A[i,t-1]*S_juv_JP[2,t]+d_wild_moy_L[i,t-1]*S_juv_JP[3,t]+d_wild_moy_P[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])
		
		##Par secteur
		DC_tot_wild_V[i,t]<-d_wild_moy_V[i,t-1]*S_juv_JP[1,t]*100/Rmax_ponder_V[i,t]
		DC_tot_wild_A[i,t]<-d_wild_moy_A[i,t-1]*S_juv_JP[2,t]*100/Rmax_ponder_A[i,t]
		DC_tot_wild_L[i,t]<-d_wild_moy_L[i,t-1]*S_juv_JP[3,t]*100/Rmax_ponder_L[i,t]
		DC_tot_wild_P[i,t]<-d_wild_moy_P[i,t-12]*S_juv_JP[4,t]*100/Rmax_ponder_P[i,t]
	}
}

#-----------------------
# Pour DC_tot_wild_tot
#-----------------------
### 25, 50 et 75% Rmax pris comme r�f�rence

w_upper_tot_25_Rmax=array(0,dim=c(5000,T))
w_upper_tot_50_Rmax=array(0,dim=c(5000,T))
w_upper_tot_75_Rmax=array(0,dim=c(5000,T))
##On décline par secteur 25% de Rmax seulement car déjà difficile de l'atteindre sur les sauvages
w_upper_tot_25_Rmax_V=array(0,dim=c(5000,T))
w_upper_tot_25_Rmax_A=array(0,dim=c(5000,T))
w_upper_tot_25_Rmax_L=array(0,dim=c(5000,T))
w_upper_tot_25_Rmax_P=array(0,dim=c(5000,T))

for (t in 2:T){
	
	for (i in 1:5000){
		if(DC_tot_wild_tot[i,t] > 25){w_upper_tot_25_Rmax[i,t]=1}  
		if(DC_tot_wild_tot[i,t] > 50){w_upper_tot_50_Rmax[i,t]=1}
		if(DC_tot_wild_tot[i,t] > 75){w_upper_tot_75_Rmax[i,t]=1}
		##Par secteur
		if(DC_tot_wild_V[i,t] > 25){w_upper_tot_25_Rmax_V[i,t]=1}
		if(DC_tot_wild_A[i,t] > 25){w_upper_tot_25_Rmax_A[i,t]=1}
		if(DC_tot_wild_L[i,t] > 25){w_upper_tot_25_Rmax_L[i,t]=1}
		if(DC_tot_wild_P[i,t] > 25){w_upper_tot_25_Rmax_P[i,t]=1}
	}
}


p_w_upper_tot_25_Rmax=rep(0,T)
p_w_upper_tot_50_Rmax=rep(0,T)
p_w_upper_tot_75_Rmax=rep(0,T)



for (t in 2:T){
	p_w_upper_tot_25_Rmax[t]=mean(w_upper_tot_25_Rmax[,t])
	p_w_upper_tot_50_Rmax[t]=mean(w_upper_tot_50_Rmax[,t])
	p_w_upper_tot_75_Rmax[t]=mean(w_upper_tot_75_Rmax[,t])
	
}

##------------------
## pour DC_tot_wild
##------------------
#w_upper_25_Rmax=array(0,dim=c(5000,T))
#w_upper_50_Rmax=array(0,dim=c(5000,T))
#w_upper_75_Rmax=array(0,dim=c(5000,T))
#
#
#
#for (t in 2:T){
#	
#	for (i in 1:5000){
#		if(DC_tot_wild[i,t] > 25){w_upper_25_Rmax[i,t]=1}  
#		if(DC_tot_wild[i,t] > 50){w_upper_50_Rmax[i,t]=1}
#		if(DC_tot_wild[i,t] > 75){w_upper_75_Rmax[i,t]=1}
#	}
#}
#
#
#p_w_upper_25_Rmax=rep(0,T)
#p_w_upper_50_Rmax=rep(0,T)
#p_w_upper_75_Rmax=rep(0,T)
#
#
#
#for (t in 2:T){
#	p_w_upper_25_Rmax[t]=mean(w_upper_25_Rmax[,t])
#	p_w_upper_50_Rmax[t]=mean(w_upper_50_Rmax[,t])
#	p_w_upper_75_Rmax[t]=mean(w_upper_75_Rmax[,t])
#	
#}


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservationWild_annuel_surfTot_2018_06_22.png",width=800,height=800)

#par(mfrow=c(1,1),mar=c(4,6.1,4,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(2,11,16,26,36,T),
		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,T,1)

points(x[2:T],p_w_upper_tot_25_Rmax[2:T],col="grey75",pch=16)
segments(x[2:(T-1)],p_w_upper_tot_25_Rmax[2:(T-1)],x[3:T],p_w_upper_tot_25_Rmax[3:T],col="grey75")

points(x[2:T],p_w_upper_tot_50_Rmax[2:T],col="grey65",pch=16)
segments(x[2:(T-1)],p_w_upper_tot_50_Rmax[2:(T-1)],x[3:T],p_w_upper_tot_50_Rmax[3:T],col="grey55")

points(x[2:T],p_w_upper_tot_75_Rmax[2:T],col="grey55",pch=16)
segments(x[2:(T-1)],p_w_upper_tot_75_Rmax[2:(T-1)],x[3:T],p_w_upper_tot_75_Rmax[3:T],col="grey15")


legend(15,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)


dev.off()


#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservationWild_annuel_2018_06_22.png",width=800,height=800)
#
##par(mfrow=c(1,1),mar=c(4,6.1,4,0.5),cex.lab=1.4, cex.lab=1.4)
#
#plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
## trace l'axe des ordonn�es
#axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
## trace l'axe des abscisses
#axis(1,at = c(2,11,16,26,36,T),
#		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
#		cex.axis = 0.9,las = 1,col = "black")
#
#x=seq(1,T,1)
#
#points(x[2:T],p_w_upper_25_Rmax[2:T],col="grey75",pch=16)
#segments(x[2:(T-1)],p_w_upper_25_Rmax[2:(T-1)],x[3:T],p_w_upper_25_Rmax[3:T],col="grey75")
#
#points(x[2:T],p_w_upper_50_Rmax[2:T],col="grey65",pch=16)
#segments(x[2:(T-1)],p_w_upper_50_Rmax[2:(T-1)],x[3:T],p_w_upper_50_Rmax[3:T],col="grey55")
#
#points(x[2:T],p_w_upper_75_Rmax[2:T],col="grey55",pch=16)
#segments(x[2:(T-1)],p_w_upper_75_Rmax[2:(T-1)],x[3:T],p_w_upper_75_Rmax[3:T],col="grey15")
#
#
#legend(15,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
#		pch=c(16,16,16,16),#16),
#		col=c("grey45","grey65","grey75"), 
#		bty="n" )
#
#abline(h=0.75,col="red",lty=2)
#abline(h=0.5,col="red")
#abline(h=0.25,col="red",lty=2)
#
#
#dev.off()

##########################################################################################
# On calcule le nbr de fois qu'on passe en dessous (risque de ne pas atteindre le seuil)
##########################################################################################

#On récupère les 0 et 1 en fonction des objectifs de Rmax. on prend 10 années mobiles et on regarde si on atteint nos objectifs de risque par ex 25% de risque signifie qu'on 
#ne doit pas avoir plus de 3 années sur 10 en dessous de l'objectif 25% Rmax
risk_10_Rmax_25<-array(NA,dim=c(5000,T))
risk_20_Rmax_25<-array(NA,dim=c(5000,T))
risk_30_Rmax_25<-array(NA,dim=c(5000,T))

##Par secteur
risk_10_Rmax_25_V<-array(NA,dim=c(5000,T))
risk_10_Rmax_25_A<-array(NA,dim=c(5000,T))
risk_10_Rmax_25_L<-array(NA,dim=c(5000,T))
risk_10_Rmax_25_P<-array(NA,dim=c(5000,T))

risk_20_Rmax_25_V<-array(NA,dim=c(5000,T))
risk_20_Rmax_25_A<-array(NA,dim=c(5000,T))
risk_20_Rmax_25_L<-array(NA,dim=c(5000,T))
risk_20_Rmax_25_P<-array(NA,dim=c(5000,T))

risk_30_Rmax_25_V<-array(NA,dim=c(5000,T))
risk_30_Rmax_25_A<-array(NA,dim=c(5000,T))
risk_30_Rmax_25_L<-array(NA,dim=c(5000,T))
risk_30_Rmax_25_P<-array(NA,dim=c(5000,T))

##Pour simplifier graph ensuite on aggrège dans un même jeu de donnée les w_upper_tot_25_Rmax_V
w_upper_tot_25_Rmax_secteur<-
for(t in 11:T){
	for (i in 1:5000){
		#Risque 10% : il ne faut pas plus de 1 année en dessous de l'objectif pour le remplir
		risk_10_Rmax_25[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax[i,(t-10):t]==0))<=1,1,0)
		#Risque 20% : il ne faut pas plus de 2 années en dessous de l'objectif pour le remplir
		risk_20_Rmax_25[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax[i,(t-10):t]==0))<=2,1,0)
		#Risque 30% : il ne faut pas plus de 3 années en dessous de l'objectif pour le remplir
		risk_30_Rmax_25[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax[i,(t-10):t]==0))<=3,1,0)
		
		##Par secteur
		#Risque 10%
		risk_10_Rmax_25_V[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_V[i,(t-10):t]==0))<=1,1,0)
		risk_10_Rmax_25_A[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_A[i,(t-10):t]==0))<=1,1,0)
		risk_10_Rmax_25_L[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_L[i,(t-10):t]==0))<=1,1,0)
		risk_10_Rmax_25_P[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_P[i,(t-10):t]==0))<=1,1,0)
		#Risque 20%
		risk_20_Rmax_25_V[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_V[i,(t-10):t]==0))<=2,1,0)
		risk_20_Rmax_25_A[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_A[i,(t-10):t]==0))<=2,1,0)
		risk_20_Rmax_25_L[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_L[i,(t-10):t]==0))<=2,1,0)
		risk_20_Rmax_25_P[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_P[i,(t-10):t]==0))<=2,1,0)
		#Risque 30%
		risk_30_Rmax_25_V[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_V[i,(t-10):t]==0))<=3,1,0)
		risk_30_Rmax_25_A[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_A[i,(t-10):t]==0))<=3,1,0)
		risk_30_Rmax_25_L[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_L[i,(t-10):t]==0))<=3,1,0)
		risk_30_Rmax_25_P[i,t]<-ifelse(length(which(w_upper_tot_25_Rmax_P[i,(t-10):t]==0))<=3,1,0)	
				
	}
}
mean_risk_10_Rmax_25<-0
mean_risk_20_Rmax_25<-0
mean_risk_30_Rmax_25<-0
##Par secteur
mean_risk_10_Rmax_25_V<-0
mean_risk_10_Rmax_25_A<-0
mean_risk_10_Rmax_25_L<-0
mean_risk_10_Rmax_25_P<-0

mean_risk_20_Rmax_25_V<-0
mean_risk_20_Rmax_25_A<-0
mean_risk_20_Rmax_25_L<-0
mean_risk_20_Rmax_25_P<-0

mean_risk_30_Rmax_25_V<-0
mean_risk_30_Rmax_25_A<-0
mean_risk_30_Rmax_25_L<-0
mean_risk_30_Rmax_25_P<-0

for (t in 11:T){
	mean_risk_10_Rmax_25[t]<-mean(risk_10_Rmax_25[,t])
	mean_risk_20_Rmax_25[t]<-mean(risk_20_Rmax_25[,t])
	mean_risk_30_Rmax_25[t]<-mean(risk_30_Rmax_25[,t])
	##Par secteur
	mean_risk_10_Rmax_25_V[t]<-mean(risk_10_Rmax_25_V[,t])
	mean_risk_10_Rmax_25_A[t]<-mean(risk_10_Rmax_25_A[,t])
	mean_risk_10_Rmax_25_L[t]<-mean(risk_10_Rmax_25_L[,t])
	mean_risk_10_Rmax_25_P[t]<-mean(risk_10_Rmax_25_P[,t])
	
	mean_risk_20_Rmax_25_V[t]<-mean(risk_20_Rmax_25_V[,t])
	mean_risk_20_Rmax_25_A[t]<-mean(risk_20_Rmax_25_A[,t])
	mean_risk_20_Rmax_25_L[t]<-mean(risk_20_Rmax_25_L[,t])
	mean_risk_20_Rmax_25_P[t]<-mean(risk_20_Rmax_25_P[,t])
	
	mean_risk_30_Rmax_25_V[t]<-mean(risk_30_Rmax_25_V[,t])
	mean_risk_30_Rmax_25_A[t]<-mean(risk_30_Rmax_25_A[,t])
	mean_risk_30_Rmax_25_L[t]<-mean(risk_30_Rmax_25_L[,t])
	mean_risk_30_Rmax_25_P[t]<-mean(risk_30_Rmax_25_P[,t])
}

#Pour simplifier graph on aggrège les données par secteur dans un même data.frame
mean_risk_10_secteur<-data.frame(secteur=rep(c("Vichy","Alagnon","Langeac","Poutes"),each=(T-10)),risk_10_Rmax_25=c(mean_risk_10_Rmax_25_V[11:T],mean_risk_10_Rmax_25_A[11:T],mean_risk_10_Rmax_25_L[11:T],mean_risk_10_Rmax_25_P[11:T]))
mean_risk_20_secteur<-data.frame(secteur=rep(c("Vichy","Alagnon","Langeac","Poutes"),each=(T-10)),risk_20_Rmax_25=c(mean_risk_20_Rmax_25_V[11:T],mean_risk_20_Rmax_25_A[11:T],mean_risk_20_Rmax_25_L[11:T],mean_risk_20_Rmax_25_P[11:T]))
mean_risk_30_secteur<-data.frame(secteur=rep(c("Vichy","Alagnon","Langeac","Poutes"),each=(T-10)),risk_30_Rmax_25=c(mean_risk_30_Rmax_25_V[11:T],mean_risk_30_Rmax_25_A[11:T],mean_risk_30_Rmax_25_L[11:T],mean_risk_30_Rmax_25_P[11:T]))
toto<-cbind(mean_risk_10_secteur,mean_risk_20_secteur,mean_risk_30_secteur)
toto<-stack(toto)
secteur<-rep(rep(c("Vichy","Alagnon","Langeac","Poutes"),each=((T-10))),3)
mean_risk_secteur<-cbind(toto,secteur)
	
mean_risk_secteur$secteur=factor(mean_risk_secteur$secteur, levels=c("Vichy","Alagnon","Langeac","Poutes"))
colnames(mean_risk_secteur)[colnames(mean_risk_secteur)=="ind"]<-"risk"
#mean_risk_10_secteur$secteur = factor(mean_risk_10_secteur$secteur, levels=c("Vichy","Alagnon","Langeac","Poutes"))
#mean_risk_20_secteur$secteur = factor(mean_risk_20_secteur$secteur, levels=c("Vichy","Alagnon","Langeac","Poutes"))
#mean_risk_30_secteur$secteur = factor(mean_risk_30_secteur$secteur, levels=c("Vichy","Alagnon","Langeac","Poutes"))

x=seq(11,T,1)

p<-ggplot()+
		geom_line(aes(x,mean_risk_10_Rmax_25[11:T],colour="1"),size=1)+
		geom_line(aes(x,mean_risk_20_Rmax_25[11:T],colour="2"),size=1)+
		geom_line(aes(x,mean_risk_30_Rmax_25[11:T],colour="3"),size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité d'assurer la conservation selon le diagnostic choisi","UTF8"))+
		scale_x_continuous(breaks=c(11,21,31,T), labels=c(1985,1995,2005,2016))+
		scale_y_continuous(breaks=seq(0,1,0.2), labels=seq(0,1,0.2))+
		scale_colour_manual("", breaks = c("1", "2", "3"),values = c("blue", "green", "orange"),labels=c("Risque 10%", "Risque 20%", "Risque 30%")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/marion.legrand/workspace/ModeleDynamiquePop/img/Simulation/2019_07_IndicateursSat_model2017_08_29/DiagnosticConservation_Wild_Moy_mob10_25_Rmax_2019_07_04.png",width=800,height=800)
print(p)
dev.off()

###Par secteur
x=rep(seq(11,T,1),4*3)

p<-ggplot(mean_risk_secteur)+
		geom_line(aes(x,values,group=risk,colour=risk),size=1)+
		xlab(iconv("Année","UTF8")) + 
		ylab(iconv("Probabilité d'assurer la conservation selon le diagnostic choisi","UTF8"))+
		scale_x_continuous(breaks=c(11,21,31,T), labels=c(1985,1995,2005,2016))+
		scale_y_continuous(breaks=seq(0,1,0.2), labels=seq(0,1,0.2))+
		facet_wrap(~ secteur, ncol=2)+
		scale_colour_manual(values=c("blue", "green", "orange"))+
		#scale_colour_manual("", breaks = c("1", "2", "3"),values = c("blue", "green", "orange"),labels=c("Risque 10%", "Risque 20%", "Risque 30%")) +
		theme(legend.position ="top")+
		theme_bw()

png(filename="C:/Users/marion.legrand/workspace/ModeleDynamiquePop/img/Simulation/2019_07_IndicateursSat_model2017_08_29/DiagnosticConservation_Wild_Moy_mob10_25_Rmax_secteur_2019_07_04.png",width=800,height=800)
print(p)
dev.off()

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
df <- ToothGrowth
bp <- ggplot(df, aes(x=dose, y=len)) + 
		geom_boxplot(aes(fill=dose))+
		facet_wrap(~ dose, ncol=2)

#.......................................
#On décline ce diagnostic par secteur
#.......................................



#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
#write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq(T,2,-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_wild_mean5<-array(0,dim=c(5000,(length(seq)-1))) 

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_wild_mean5[i,s]<-mean(DC_tot_wild[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

w_upper_25_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))
w_upper_50_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))
w_upper_75_Rmax_mean5=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_wild_mean5[i,t] > 25){w_upper_25_Rmax_mean5[i,t]=1}  
		if(DC_tot_wild_mean5[i,t] > 50){w_upper_50_Rmax_mean5[i,t]=1}
		if(DC_tot_wild_mean5[i,t] > 75){w_upper_75_Rmax_mean5[i,t]=1}
	}
}


p_w_upper_25_Rmax_mean5=rep(0,(length(seq)-1))
p_w_upper_50_Rmax_mean5=rep(0,(length(seq)-1))
p_w_upper_75_Rmax_mean5=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_w_upper_25_Rmax_mean5[t]=mean(w_upper_25_Rmax_mean5[,t])
	p_w_upper_50_Rmax_mean5[t]=mean(w_upper_50_Rmax_mean5[,t])
	p_w_upper_75_Rmax_mean5[t]=mean(w_upper_75_Rmax_mean5[,t])	
}


#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!
p_w_upper_25_Rmax_mean5<-rev(p_w_upper_25_Rmax_mean5)
p_w_upper_50_Rmax_mean5<-rev(p_w_upper_50_Rmax_mean5)
p_w_upper_75_Rmax_mean5<-rev(p_w_upper_75_Rmax_mean5)

#png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/DiagnostiqueConservation_mean5_2018_06_22.png",width=800,height=800)
png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservationWild_mean5_surfTot_2018_06_22.png",width=800,height=800)

par(mar=c(6,6.1,4,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(4.5,(T+0.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T-35),(T-30),(T-25),(T-20),(T-15),(T-10),(T-5),T),
		labels=c(str_c("1977-",(1977+4),""),str_c((1977+5),"-"(1977+9)),str_c((1977+10),"-",(1977+14)),str_c((1977+15),"-"(1977+19)),str_c((1977+20),"-",(1977+24)),str_c((1977+25),"-",(1977+29)),str_c((1977+30),"-",(1977+34)),str_c((1977+35),"-",(1975+T-1))),
		cex.axis = 0.9,las = 2,col = "black")

x=rev(seq)

points(x[2:(length(seq))],p_w_upper_25_Rmax_mean5[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)],p_w_upper_25_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_w_upper_25_Rmax_mean5[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))],p_w_upper_50_Rmax_mean5[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)],p_w_upper_50_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_w_upper_50_Rmax_mean5[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))],p_w_upper_75_Rmax_mean5[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)],p_w_upper_75_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_w_upper_75_Rmax_mean5[2:(length(seq)-1)],col="grey15")


legend(5,0.7,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)

dev.off()



###############################################################
## Diagnostique de conservation pour les sc�narii de gestion ##
###############################################################

#=========================
# Retour vers le futur
#=========================

#On charge les donn�es du sc�nario Retour vers le futur
retourVersFutur<-load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_AnalyseRetro_InteractionReciproqueMatriceVC_TxRenouv_2018_06.RData")
#On charge Rmax avec les lignes de code du d�but du script

DC_tot_tot_RVF=array(rep(0,T*5000),dim=c(5000,T)) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
DC_tot_RVF=array(rep(0,T*5000),dim=c(5000,T)) # en prenant en compte uniquement les surfaces accessibles

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

for (t in 2:12){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot_RVF[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t])*100)/(Rmax[i]*S_juv_JP_tot[5])
	}
}
for (t in 13:T){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot_RVF[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP_tot[5])		
	}
}

#-----------------------------
# Pour DC_tot
#-----------------------------

for (t in 2:12){
	for (i in 1:5000){
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		DC_tot_RVF[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t])*100)/(Rmax[i]*S_juv_JP[5,t])
	}
}
for (t in 13:T){
	for (i in 1:5000){
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		DC_tot_RVF[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])	
	}
}

#................................................
# on regarde le diagnostique ann�e par ann�e
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_RVF=array(0,dim=c(5000,T))
upper_tot_50_Rmax_RVF=array(0,dim=c(5000,T))
upper_tot_75_Rmax_RVF=array(0,dim=c(5000,T))



for (t in 2:T){
	
	for (i in 1:5000){
		if(DC_tot_tot_RVF[i,t] > 25){upper_tot_25_Rmax_RVF[i,t]=1}  
		if(DC_tot_tot_RVF[i,t] > 50){upper_tot_50_Rmax_RVF[i,t]=1}
		if(DC_tot_tot_RVF[i,t] > 75){upper_tot_75_Rmax_RVF[i,t]=1}
	}
}


p_upper_tot_25_Rmax_RVF=rep(0,T)
p_upper_tot_50_Rmax_RVF=rep(0,T)
p_upper_tot_75_Rmax_RVF=rep(0,T)



for (t in 2:T){
	p_upper_tot_25_Rmax_RVF[t]=mean(upper_tot_25_Rmax_RVF[,t])
	p_upper_tot_50_Rmax_RVF[t]=mean(upper_tot_50_Rmax_RVF[,t])
	p_upper_tot_75_Rmax_RVF[t]=mean(upper_tot_75_Rmax_RVF[,t])
	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_RVF=array(0,dim=c(5000,T))
upper_50_Rmax_RVF=array(0,dim=c(5000,T))
upper_75_Rmax_RVF=array(0,dim=c(5000,T))



for (t in 2:T){
	
	for (i in 1:5000){
		if(DC_tot_RVF[i,t] > 25){upper_25_Rmax_RVF[i,t]=1}  
		if(DC_tot_RVF[i,t] > 50){upper_50_Rmax_RVF[i,t]=1}
		if(DC_tot_RVF[i,t] > 75){upper_75_Rmax_RVF[i,t]=1}
	}
}

p_upper_25_Rmax_RVF=rep(0,T)
p_upper_50_Rmax_RVF=rep(0,T)
p_upper_75_Rmax_RVF=rep(0,T)



for (t in 2:T){
	p_upper_25_Rmax_RVF[t]=mean(upper_25_Rmax_RVF[,t])
	p_upper_50_Rmax_RVF[t]=mean(upper_50_Rmax_RVF[,t])
	p_upper_75_Rmax_RVF[t]=mean(upper_75_Rmax_RVF[,t])
	
}


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_RetourVersLeFutur_2018_06_25.png",width=800,height=800)

par(mar=c(8,6,2,0.5))
plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(2,11,16,26,36,T),
		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,T,1)

points(x[2:T],p_upper_tot_25_Rmax_RVF[2:T],col="grey75",pch=16)
segments(x[2:(T-1)],p_upper_tot_25_Rmax_RVF[2:(T-1)],x[3:T],p_upper_tot_25_Rmax_RVF[3:T],col="grey75")

points(x[2:T],p_upper_tot_50_Rmax_RVF[2:T],col="grey65",pch=16)
segments(x[2:(T-1)],p_upper_tot_50_Rmax_RVF[2:(T-1)],x[3:T],p_upper_tot_50_Rmax_RVF[3:T],col="grey55")

points(x[2:T],p_upper_tot_75_Rmax_RVF[2:T],col="grey55",pch=16)
segments(x[2:(T-1)],p_upper_tot_75_Rmax_RVF[2:(T-1)],x[3:T],p_upper_tot_75_Rmax_RVF[3:T],col="grey15")


legend(14,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text="Scenario : Retour vers le Futur",line=6)

dev.off()


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_RetourVersLeFutur_2018_06_25.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(8,6,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(2,11,16,26,36,T),
		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,T,1)

points(x[2:T],p_upper_25_Rmax_RVF[2:T],col="grey75",pch=16)
segments(x[2:(T-1)],p_upper_25_Rmax_RVF[2:(T-1)],x[3:T],p_upper_25_Rmax_RVF[3:T],col="grey75")

points(x[2:T],p_upper_50_Rmax_RVF[2:T],col="grey65",pch=16)
segments(x[2:(T-1)],p_upper_50_Rmax_RVF[2:(T-1)],x[3:T],p_upper_50_Rmax_RVF[3:T],col="grey55")

points(x[2:T],p_upper_75_Rmax_RVF[2:T],col="grey55",pch=16)
segments(x[2:(T-1)],p_upper_75_Rmax_RVF[2:(T-1)],x[3:T],p_upper_75_Rmax_RVF[3:T],col="grey15")


legend(14,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text="Scenario : Retour vers le Futur",line=6)

dev.off()

#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
#write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq(T,2,-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_tot_mean5_RVF<-array(0,dim=c(5000,(length(seq)-1))) #surface totale quelque soit l'accessibilit�
DC_tot_mean5_RVF<-array(0,dim=c(5000,(length(seq)-1))) # surface en prenant en compte les pb d'accessibilit�

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_tot_mean5_RVF[i,s]<-mean(DC_tot_tot_RVF[i,((seq[s+1]+1):(seq[s]))])
	}
}

for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_mean5_RVF[i,s]<-mean(DC_tot_RVF[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_mean5_RVF=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_50_Rmax_mean5_RVF=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_75_Rmax_mean5_RVF=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_tot_mean5_RVF[i,t] > 25){upper_tot_25_Rmax_mean5_RVF[i,t]=1}  
		if(DC_tot_tot_mean5_RVF[i,t] > 50){upper_tot_50_Rmax_mean5_RVF[i,t]=1}
		if(DC_tot_tot_mean5_RVF[i,t] > 75){upper_tot_75_Rmax_mean5_RVF[i,t]=1}
	}
}


p_upper_tot_25_Rmax_mean5_RVF=rep(0,(length(seq)-1))
p_upper_tot_50_Rmax_mean5_RVF=rep(0,(length(seq)-1))
p_upper_tot_75_Rmax_mean5_RVF=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_tot_25_Rmax_mean5_RVF[t]=mean(upper_tot_25_Rmax_mean5_RVF[,t])
	p_upper_tot_50_Rmax_mean5_RVF[t]=mean(upper_tot_50_Rmax_mean5_RVF[,t])
	p_upper_tot_75_Rmax_mean5_RVF[t]=mean(upper_tot_75_Rmax_mean5_RVF[,t])	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_mean5_RVF=array(0,dim=c(5000,(length(seq)-1)))
upper_50_Rmax_mean5_RVF=array(0,dim=c(5000,(length(seq)-1)))
upper_75_Rmax_mean5_RVF=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_mean5_RVF[i,t] > 25){upper_25_Rmax_mean5_RVF[i,t]=1}  
		if(DC_tot_mean5_RVF[i,t] > 50){upper_50_Rmax_mean5_RVF[i,t]=1}
		if(DC_tot_mean5_RVF[i,t] > 75){upper_75_Rmax_mean5_RVF[i,t]=1}
	}
}


p_upper_25_Rmax_mean5_RVF=rep(0,(length(seq)-1))
p_upper_50_Rmax_mean5_RVF=rep(0,(length(seq)-1))
p_upper_75_Rmax_mean5_RVF=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_25_Rmax_mean5_RVF[t]=mean(upper_25_Rmax_mean5_RVF[,t])
	p_upper_50_Rmax_mean5_RVF[t]=mean(upper_50_Rmax_mean5_RVF[,t])
	p_upper_75_Rmax_mean5_RVF[t]=mean(upper_75_Rmax_mean5_RVF[,t])	
}

#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!

p_upper_tot_25_Rmax_mean5_RVF<-rev(p_upper_tot_25_Rmax_mean5_RVF)
p_upper_tot_50_Rmax_mean5_RVF<-rev(p_upper_tot_50_Rmax_mean5_RVF)
p_upper_tot_75_Rmax_mean5_RVF<-rev(p_upper_tot_75_Rmax_mean5_RVF)

p_upper_25_Rmax_mean5_RVF<-rev(p_upper_25_Rmax_mean5_RVF)
p_upper_50_Rmax_mean5_RVF<-rev(p_upper_50_Rmax_mean5_RVF)
p_upper_75_Rmax_mean5_RVF<-rev(p_upper_75_Rmax_mean5_RVF)

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_surfTot_RetourVersLeFutur_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c(4.5,(T+0.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T-35),(T-30),(T-25),(T-20),(T-15),(T-10),(T-5),T),
		labels=c(str_c("1977-",(1977+4),""),str_c((1977+5),"-"(1977+9)),str_c((1977+10),"-",(1977+14)),str_c((1977+15),"-"(1977+19)),str_c((1977+20),"-",(1977+24)),str_c((1977+25),"-",(1977+29)),str_c((1977+30),"-",(1977+34)),str_c((1977+35),"-",(1975+T-1))),
		cex.axis = 0.9,las = 2,col = "black")

x=rev(seq)

points(x[2:(length(seq))],p_upper_tot_25_Rmax_mean5_RVF[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)],p_upper_tot_25_Rmax_mean5_RVF[1:(length(seq)-2)],x[3:(length(seq))],p_upper_tot_25_Rmax_mean5_RVF[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))],p_upper_tot_50_Rmax_mean5_RVF[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)],p_upper_tot_50_Rmax_mean5_RVF[1:(length(seq)-2)],x[3:(length(seq))],p_upper_tot_50_Rmax_mean5_RVF[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))],p_upper_tot_75_Rmax_mean5_RVF[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)],p_upper_tot_75_Rmax_mean5_RVF[1:(length(seq)-2)],x[3:(length(seq))],p_upper_tot_75_Rmax_mean5_RVF[2:(length(seq)-1)],col="grey15")


legend(5,0.7,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text="Scenario : Retour vers le Futur",line=6)

dev.off()

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_RetourVersLeFutur_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c(4.5,(T+0.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T-35),(T-30),(T-25),(T-20),(T-15),(T-10),(T-5),T),
		labels=c(str_c("1977-",(1977+4),""),str_c((1977+5),"-"(1977+9)),str_c((1977+10),"-",(1977+14)),str_c((1977+15),"-"(1977+19)),str_c((1977+20),"-",(1977+24)),str_c((1977+25),"-",(1977+29)),str_c((1977+30),"-",(1977+34)),str_c((1977+35),"-",(1975+T-1))),
		cex.axis = 0.9,las = 2,col = "black")

x=rev(seq)

points(x[2:(length(seq))],p_upper_25_Rmax_mean5_RVF[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)],p_upper_25_Rmax_mean5_RVF[1:(length(seq)-2)],x[3:(length(seq))],p_upper_25_Rmax_mean5_RVF[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))],p_upper_50_Rmax_mean5_RVF[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)],p_upper_50_Rmax_mean5_RVF[1:(length(seq)-2)],x[3:(length(seq))],p_upper_50_Rmax_mean5_RVF[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))],p_upper_75_Rmax_mean5_RVF[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)],p_upper_75_Rmax_mean5_RVF[1:(length(seq)-2)],x[3:(length(seq))],p_upper_75_Rmax_mean5_RVF[2:(length(seq)-1)],col="grey15")


legend(5,0.7,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text="Scenario : Retour vers le Futur",line=6)

dev.off()

##On supprime les donn�es du jeu de donn�es pr�c�dent pour ne pas faire de m�lange
rm(list=c(retourVersFutur, "retourVersFutur"))
#On enl�ve tous les jeux de donn�es qui finissent par RVF
rm(list=ls(pattern="RVF$"))

#======================
# Without Stocking
#======================

#On charge les donn�es du sc�nario Retour vers le futur
withoutStocking<-load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_2018_06_25.RData")
#On charge Rmax avec les lignes de code du d�but du script

#On remet S_juv_JP car annul� avec l'import de withoutStocking
surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)
S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Cr�� une 5eme ligne qui correspond � la somme par ann�e des surfaces sur l'ensemble des secteurs
S_juv_JP_tot<-S_juv_JP[,T]

DC_tot_tot_WS=array(rep(0,T*5000),dim=c(5000,(T+20))) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
DC_tot_WS=array(rep(0,T*5000),dim=c(5000,(T+20))) # en prenant en compte uniquement les surfaces accessibles

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot_WS[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP_tot[5])		
	}
}

#-----------------------------
# Pour DC_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		DC_tot_WS[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])	
	}
}

#................................................
# on regarde le diagnostique ann�e par ann�e
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_WS=array(0,dim=c(5000,(T+20)))
upper_tot_50_Rmax_WS=array(0,dim=c(5000,(T+20)))
upper_tot_75_Rmax_WS=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_tot_WS[i,t] > 25){upper_tot_25_Rmax_WS[i,t]=1}  
		if(DC_tot_tot_WS[i,t] > 50){upper_tot_50_Rmax_WS[i,t]=1}
		if(DC_tot_tot_WS[i,t] > 75){upper_tot_75_Rmax_WS[i,t]=1}
	}
}


p_upper_tot_25_Rmax_WS=rep(0,(T+20))
p_upper_tot_50_Rmax_WS=rep(0,(T+20))
p_upper_tot_75_Rmax_WS=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_tot_25_Rmax_WS[t]=mean(upper_tot_25_Rmax_WS[,t])
	p_upper_tot_50_Rmax_WS[t]=mean(upper_tot_50_Rmax_WS[,t])
	p_upper_tot_75_Rmax_WS[t]=mean(upper_tot_75_Rmax_WS[,t])
	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_WS=array(0,dim=c(5000,(T+20)))
upper_50_Rmax_WS=array(0,dim=c(5000,(T+20)))
upper_75_Rmax_WS=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_WS[i,t] > 25){upper_25_Rmax_WS[i,t]=1}  
		if(DC_tot_WS[i,t] > 50){upper_50_Rmax_WS[i,t]=1}
		if(DC_tot_WS[i,t] > 75){upper_75_Rmax_WS[i,t]=1}
	}
}

p_upper_25_Rmax_WS=rep(0,(T+20))
p_upper_50_Rmax_WS=rep(0,(T+20))
p_upper_75_Rmax_WS=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_25_Rmax_WS[t]=mean(upper_25_Rmax_WS[,t])
	p_upper_50_Rmax_WS[t]=mean(upper_50_Rmax_WS[,t])
	p_upper_75_Rmax_WS[t]=mean(upper_75_Rmax_WS[,t])
	
}


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_WithoutStocking_2018_06_25.png",width=800,height=800)

par(mar=c(8,6,2,0.5))
plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_tot_25_Rmax_WS[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_tot_25_Rmax_WS[(T+1):(T+20-1)],x[3:21],p_upper_tot_25_Rmax_WS[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_tot_50_Rmax_WS[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_tot_50_Rmax_WS[(T+1):(T+20-1)],x[3:21],p_upper_tot_50_Rmax_WS[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_tot_75_Rmax_WS[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_tot_75_Rmax_WS[(T+1):(T+20-1)],x[3:21],p_upper_tot_75_Rmax_WS[(T+1+1):(T+20)],col="grey15")


legend(54,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text="Scenario : Retour vers le Futur",line=6)

dev.off()


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_WithoutStocking_2018_06_25.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(8,6,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+20.5)),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_25_Rmax_WS[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_25_Rmax_WS[(T+1):(T+20-1)],x[3:21],p_upper_25_Rmax_WS[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_50_Rmax_WS[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_50_Rmax_WS[(T+1):(T+20-1)],x[3:21],p_upper_50_Rmax_WS[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_75_Rmax_WS[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_75_Rmax_WS[(T+1):(T+20-1)],x[3:21],p_upper_75_Rmax_WS[(T+1+1):(T+20)],col="grey15")


legend(54,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text="Scenario : Retour vers le Futur",line=6)

dev.off()

#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
#write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq((T+20),(T),-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_tot_mean5_WS<-array(0,dim=c(5000,(length(seq)-1))) #surface totale quelque soit l'accessibilit�
DC_tot_mean5_WS<-array(0,dim=c(5000,(length(seq)-1))) # surface en prenant en compte les pb d'accessibilit�

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_tot_mean5_WS[i,s]<-mean(DC_tot_tot_WS[i,((seq[s+1]+1):(seq[s]))])
	}
}

for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_mean5_WS[i,s]<-mean(DC_tot_WS[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_mean5_WS=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_50_Rmax_mean5_WS=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_75_Rmax_mean5_WS=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_tot_mean5_WS[i,t] > 25){upper_tot_25_Rmax_mean5_WS[i,t]=1}  
		if(DC_tot_tot_mean5_WS[i,t] > 50){upper_tot_50_Rmax_mean5_WS[i,t]=1}
		if(DC_tot_tot_mean5_WS[i,t] > 75){upper_tot_75_Rmax_mean5_WS[i,t]=1}
	}
}


p_upper_tot_25_Rmax_mean5_WS=rep(0,(length(seq)-1))
p_upper_tot_50_Rmax_mean5_WS=rep(0,(length(seq)-1))
p_upper_tot_75_Rmax_mean5_WS=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_tot_25_Rmax_mean5_WS[t]=mean(upper_tot_25_Rmax_mean5_WS[,t])
	p_upper_tot_50_Rmax_mean5_WS[t]=mean(upper_tot_50_Rmax_mean5_WS[,t])
	p_upper_tot_75_Rmax_mean5_WS[t]=mean(upper_tot_75_Rmax_mean5_WS[,t])	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_mean5_WS=array(0,dim=c(5000,(length(seq)-1)))
upper_50_Rmax_mean5_WS=array(0,dim=c(5000,(length(seq)-1)))
upper_75_Rmax_mean5_WS=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_mean5_WS[i,t] > 25){upper_25_Rmax_mean5_WS[i,t]=1}  
		if(DC_tot_mean5_WS[i,t] > 50){upper_50_Rmax_mean5_WS[i,t]=1}
		if(DC_tot_mean5_WS[i,t] > 75){upper_75_Rmax_mean5_WS[i,t]=1}
	}
}


p_upper_25_Rmax_mean5_WS=rep(0,(length(seq)-1))
p_upper_50_Rmax_mean5_WS=rep(0,(length(seq)-1))
p_upper_75_Rmax_mean5_WS=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_25_Rmax_mean5_WS[t]=mean(upper_25_Rmax_mean5_WS[,t])
	p_upper_50_Rmax_mean5_WS[t]=mean(upper_50_Rmax_mean5_WS[,t])
	p_upper_75_Rmax_mean5_WS[t]=mean(upper_75_Rmax_mean5_WS[,t])	
}

#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!

p_upper_tot_25_Rmax_mean5_WS<-rev(p_upper_tot_25_Rmax_mean5_WS)
p_upper_tot_50_Rmax_mean5_WS<-rev(p_upper_tot_50_Rmax_mean5_WS)
p_upper_tot_75_Rmax_mean5_WS<-rev(p_upper_tot_75_Rmax_mean5_WS)

p_upper_25_Rmax_mean5_WS<-rev(p_upper_25_Rmax_mean5_WS)
p_upper_50_Rmax_mean5_WS<-rev(p_upper_50_Rmax_mean5_WS)
p_upper_75_Rmax_mean5_WS<-rev(p_upper_75_Rmax_mean5_WS)

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_surfTot_WithoutStocking_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)


points(x[2:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_WS[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_25_Rmax_mean5_WS[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_WS[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_WS[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_50_Rmax_mean5_WS[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_WS[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_WS[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_75_Rmax_mean5_WS[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_WS[2:(length(seq)-1)],col="grey15")


legend(50,0.7,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Projection � 20 ans sans d�versement","UTF8"),line=6)

dev.off()

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_WithoutStocking_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)

points(x[2:(length(seq))]-4,p_upper_25_Rmax_mean5_WS[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_25_Rmax_mean5_WS[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_25_Rmax_mean5_WS[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_50_Rmax_mean5_WS[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_50_Rmax_mean5_WS[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_50_Rmax_mean5_WS[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_75_Rmax_mean5_WS[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_75_Rmax_mean5_WS[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_75_Rmax_mean5_WS[2:(length(seq)-1)],col="grey15")


legend(5,0.7,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Projection � 20 ans sans d�versement","UTF8"),line=6)

dev.off()

#On enl�ve le jeu de donn�es pr�c�dent
rm(list=c(withoutStocking, "withoutStocking"))
#On enl�ve tous les jeux de donn�es qui finissent par WS
rm(list=ls(pattern="WS$"))


#======================
# D�valaison
#======================

#On charge les donn�es du sc�nario Retour vers le futur
devalaison<-load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2018_06_25.RData")
#On charge Rmax avec les lignes de code du d�but du script

#On remet S_juv_JP car annul� avec l'import de withoutStocking
surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)
S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Cr�� une 5eme ligne qui correspond � la somme par ann�e des surfaces sur l'ensemble des secteurs
S_juv_JP_tot<-S_juv_JP[,T]

DC_tot_tot_D=array(rep(0,T*5000),dim=c(5000,(T+20))) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
DC_tot_D=array(rep(0,T*5000),dim=c(5000,(T+20))) # en prenant en compte uniquement les surfaces accessibles

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot_D[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP_tot[5])		
	}
}

#-----------------------------
# Pour DC_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		DC_tot_D[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])	
	}
}

#................................................
# on regarde le diagnostique ann�e par ann�e
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_D=array(0,dim=c(5000,(T+20)))
upper_tot_50_Rmax_D=array(0,dim=c(5000,(T+20)))
upper_tot_75_Rmax_D=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_tot_D[i,t] > 25){upper_tot_25_Rmax_D[i,t]=1}  
		if(DC_tot_tot_D[i,t] > 50){upper_tot_50_Rmax_D[i,t]=1}
		if(DC_tot_tot_D[i,t] > 75){upper_tot_75_Rmax_D[i,t]=1}
	}
}


p_upper_tot_25_Rmax_D=rep(0,(T+20))
p_upper_tot_50_Rmax_D=rep(0,(T+20))
p_upper_tot_75_Rmax_D=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_tot_25_Rmax_D[t]=mean(upper_tot_25_Rmax_D[,t])
	p_upper_tot_50_Rmax_D[t]=mean(upper_tot_50_Rmax_D[,t])
	p_upper_tot_75_Rmax_D[t]=mean(upper_tot_75_Rmax_D[,t])
	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_D=array(0,dim=c(5000,(T+20)))
upper_50_Rmax_D=array(0,dim=c(5000,(T+20)))
upper_75_Rmax_D=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_D[i,t] > 25){upper_25_Rmax_D[i,t]=1}  
		if(DC_tot_D[i,t] > 50){upper_50_Rmax_D[i,t]=1}
		if(DC_tot_D[i,t] > 75){upper_75_Rmax_D[i,t]=1}
	}
}

p_upper_25_Rmax_D=rep(0,(T+20))
p_upper_50_Rmax_D=rep(0,(T+20))
p_upper_75_Rmax_D=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_25_Rmax_D[t]=mean(upper_25_Rmax_D[,t])
	p_upper_50_Rmax_D[t]=mean(upper_50_Rmax_D[,t])
	p_upper_75_Rmax_D[t]=mean(upper_75_Rmax_D[,t])
	
}


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_Devalaison_2018_06_25.png",width=800,height=800)

par(mar=c(8,6,2,0.5))
plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_tot_25_Rmax_D[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_tot_25_Rmax_D[(T+1):(T+20-1)],x[3:21],p_upper_tot_25_Rmax_D[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_tot_50_Rmax_D[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_tot_50_Rmax_D[(T+1):(T+20-1)],x[3:21],p_upper_tot_50_Rmax_D[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_tot_75_Rmax_D[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_tot_75_Rmax_D[(T+1):(T+20-1)],x[3:21],p_upper_tot_75_Rmax_D[(T+1+1):(T+20)],col="grey15")


legend(54,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Suppression des impacts � la d�valaison","UTF8"),line=6)

dev.off()


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_Devalaison_2018_06_25.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(8,6,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+20.5)),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_25_Rmax_D[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_25_Rmax_D[(T+1):(T+20-1)],x[3:21],p_upper_25_Rmax_D[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_50_Rmax_D[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_50_Rmax_D[(T+1):(T+20-1)],x[3:21],p_upper_50_Rmax_D[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_75_Rmax_D[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_75_Rmax_D[(T+1):(T+20-1)],x[3:21],p_upper_75_Rmax_D[(T+1+1):(T+20)],col="grey15")


legend(54,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Suppression des impacts � la d�valaison","UTF8"),line=6)

dev.off()

#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
#write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq((T+20),(T),-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_tot_mean5_D<-array(0,dim=c(5000,(length(seq)-1))) #surface totale quelque soit l'accessibilit�
DC_tot_mean5_D<-array(0,dim=c(5000,(length(seq)-1))) # surface en prenant en compte les pb d'accessibilit�

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_tot_mean5_D[i,s]<-mean(DC_tot_tot_D[i,((seq[s+1]+1):(seq[s]))])
	}
}

for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_mean5_D[i,s]<-mean(DC_tot_D[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_mean5_D=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_50_Rmax_mean5_D=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_75_Rmax_mean5_D=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_tot_mean5_D[i,t] > 25){upper_tot_25_Rmax_mean5_D[i,t]=1}  
		if(DC_tot_tot_mean5_D[i,t] > 50){upper_tot_50_Rmax_mean5_D[i,t]=1}
		if(DC_tot_tot_mean5_D[i,t] > 75){upper_tot_75_Rmax_mean5_D[i,t]=1}
	}
}


p_upper_tot_25_Rmax_mean5_D=rep(0,(length(seq)-1))
p_upper_tot_50_Rmax_mean5_D=rep(0,(length(seq)-1))
p_upper_tot_75_Rmax_mean5_D=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_tot_25_Rmax_mean5_D[t]=mean(upper_tot_25_Rmax_mean5_D[,t])
	p_upper_tot_50_Rmax_mean5_D[t]=mean(upper_tot_50_Rmax_mean5_D[,t])
	p_upper_tot_75_Rmax_mean5_D[t]=mean(upper_tot_75_Rmax_mean5_D[,t])	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_mean5_D=array(0,dim=c(5000,(length(seq)-1)))
upper_50_Rmax_mean5_D=array(0,dim=c(5000,(length(seq)-1)))
upper_75_Rmax_mean5_D=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_mean5_D[i,t] > 25){upper_25_Rmax_mean5_D[i,t]=1}  
		if(DC_tot_mean5_D[i,t] > 50){upper_50_Rmax_mean5_D[i,t]=1}
		if(DC_tot_mean5_D[i,t] > 75){upper_75_Rmax_mean5_D[i,t]=1}
	}
}


p_upper_25_Rmax_mean5_D=rep(0,(length(seq)-1))
p_upper_50_Rmax_mean5_D=rep(0,(length(seq)-1))
p_upper_75_Rmax_mean5_D=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_25_Rmax_mean5_D[t]=mean(upper_25_Rmax_mean5_D[,t])
	p_upper_50_Rmax_mean5_D[t]=mean(upper_50_Rmax_mean5_D[,t])
	p_upper_75_Rmax_mean5_D[t]=mean(upper_75_Rmax_mean5_D[,t])	
}

#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!

p_upper_tot_25_Rmax_mean5_D<-rev(p_upper_tot_25_Rmax_mean5_D)
p_upper_tot_50_Rmax_mean5_D<-rev(p_upper_tot_50_Rmax_mean5_D)
p_upper_tot_75_Rmax_mean5_D<-rev(p_upper_tot_75_Rmax_mean5_D)

p_upper_25_Rmax_mean5_D<-rev(p_upper_25_Rmax_mean5_D)
p_upper_50_Rmax_mean5_D<-rev(p_upper_50_Rmax_mean5_D)
p_upper_75_Rmax_mean5_D<-rev(p_upper_75_Rmax_mean5_D)

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_surfTot_Devalaison_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)


points(x[2:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_D[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_25_Rmax_mean5_D[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_D[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_D[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_50_Rmax_mean5_D[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_D[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_D[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_75_Rmax_mean5_D[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_D[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Suppression des impacts � la d�valaison","UTF8"),line=6)

dev.off()

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_Devalaison_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)

points(x[2:(length(seq))]-4,p_upper_25_Rmax_mean5_D[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_25_Rmax_mean5_D[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_25_Rmax_mean5_D[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_50_Rmax_mean5_D[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_50_Rmax_mean5_D[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_50_Rmax_mean5_D[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_75_Rmax_mean5_D[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_75_Rmax_mean5_D[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_75_Rmax_mean5_D[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Suppression des impacts � la d�valaison","UTF8"),line=6)

dev.off()

#On enl�ve le jeu de donn�es pr�c�dent
rm(list=c(devalaison, "devalaison"))
#On enl�ve tous les jeux de donn�es qui finissent par WS
rm(list=ls(pattern="D$"))

#======================
# Survie 50
#======================

#On charge les donn�es du sc�nario Retour vers le futur
surv50<-load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_AmeliorationSurvie50_2018_06_26.RData")
#On charge Rmax avec les lignes de code du d�but du script

#On remet S_juv_JP car annul� avec l'import de withoutStocking
surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)
S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Cr�� une 5eme ligne qui correspond � la somme par ann�e des surfaces sur l'ensemble des secteurs
S_juv_JP_tot<-S_juv_JP[,T]

DC_tot_tot_S50=array(rep(0,T*5000),dim=c(5000,(T+20))) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
DC_tot_S50=array(rep(0,T*5000),dim=c(5000,(T+20))) # en prenant en compte uniquement les surfaces accessibles

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot_S50[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP_tot[5])		
	}
}

#-----------------------------
# Pour DC_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		DC_tot_S50[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])	
	}
}

#................................................
# on regarde le diagnostique ann�e par ann�e
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_S50=array(0,dim=c(5000,(T+20)))
upper_tot_50_Rmax_S50=array(0,dim=c(5000,(T+20)))
upper_tot_75_Rmax_S50=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_tot_S50[i,t] > 25){upper_tot_25_Rmax_S50[i,t]=1}  
		if(DC_tot_tot_S50[i,t] > 50){upper_tot_50_Rmax_S50[i,t]=1}
		if(DC_tot_tot_S50[i,t] > 75){upper_tot_75_Rmax_S50[i,t]=1}
	}
}


p_upper_tot_25_Rmax_S50=rep(0,(T+20))
p_upper_tot_50_Rmax_S50=rep(0,(T+20))
p_upper_tot_75_Rmax_S50=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_tot_25_Rmax_S50[t]=mean(upper_tot_25_Rmax_S50[,t])
	p_upper_tot_50_Rmax_S50[t]=mean(upper_tot_50_Rmax_S50[,t])
	p_upper_tot_75_Rmax_S50[t]=mean(upper_tot_75_Rmax_S50[,t])
	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_S50=array(0,dim=c(5000,(T+20)))
upper_50_Rmax_S50=array(0,dim=c(5000,(T+20)))
upper_75_Rmax_S50=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_S50[i,t] > 25){upper_25_Rmax_S50[i,t]=1}  
		if(DC_tot_S50[i,t] > 50){upper_50_Rmax_S50[i,t]=1}
		if(DC_tot_S50[i,t] > 75){upper_75_Rmax_S50[i,t]=1}
	}
}

p_upper_25_Rmax_S50=rep(0,(T+20))
p_upper_50_Rmax_S50=rep(0,(T+20))
p_upper_75_Rmax_S50=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_25_Rmax_S50[t]=mean(upper_25_Rmax_S50[,t])
	p_upper_50_Rmax_S50[t]=mean(upper_50_Rmax_S50[,t])
	p_upper_75_Rmax_S50[t]=mean(upper_75_Rmax_S50[,t])
	
}


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_Surv50_2018_06_25.png",width=800,height=800)

par(mar=c(8,6,2,0.5))
plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_tot_25_Rmax_S50[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_tot_25_Rmax_S50[(T+1):(T+20-1)],x[3:21],p_upper_tot_25_Rmax_S50[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_tot_50_Rmax_S50[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_tot_50_Rmax_S50[(T+1):(T+20-1)],x[3:21],p_upper_tot_50_Rmax_S50[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_tot_75_Rmax_S50[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_tot_75_Rmax_S50[(T+1):(T+20-1)],x[3:21],p_upper_tot_75_Rmax_S50[(T+1+1):(T+20)],col="grey15")


legend(54,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 50%","UTF8"),line=6)

dev.off()


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_Surv50_2018_06_25.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(8,6,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+20.5)),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_25_Rmax_S50[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_25_Rmax_S50[(T+1):(T+20-1)],x[3:21],p_upper_25_Rmax_S50[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_50_Rmax_S50[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_50_Rmax_S50[(T+1):(T+20-1)],x[3:21],p_upper_50_Rmax_S50[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_75_Rmax_S50[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_75_Rmax_S50[(T+1):(T+20-1)],x[3:21],p_upper_75_Rmax_S50[(T+1+1):(T+20)],col="grey15")


legend(54,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 50%","UTF8"),line=6)

dev.off()

#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
#write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq((T+20),(T),-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_tot_mean5_S50<-array(0,dim=c(5000,(length(seq)-1))) #surface totale quelque soit l'accessibilit�
DC_tot_mean5_S50<-array(0,dim=c(5000,(length(seq)-1))) # surface en prenant en compte les pb d'accessibilit�

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_tot_mean5_S50[i,s]<-mean(DC_tot_tot_S50[i,((seq[s+1]+1):(seq[s]))])
	}
}

for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_mean5_S50[i,s]<-mean(DC_tot_S50[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_mean5_S50=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_50_Rmax_mean5_S50=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_75_Rmax_mean5_S50=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_tot_mean5_S50[i,t] > 25){upper_tot_25_Rmax_mean5_S50[i,t]=1}  
		if(DC_tot_tot_mean5_S50[i,t] > 50){upper_tot_50_Rmax_mean5_S50[i,t]=1}
		if(DC_tot_tot_mean5_S50[i,t] > 75){upper_tot_75_Rmax_mean5_S50[i,t]=1}
	}
}


p_upper_tot_25_Rmax_mean5_S50=rep(0,(length(seq)-1))
p_upper_tot_50_Rmax_mean5_S50=rep(0,(length(seq)-1))
p_upper_tot_75_Rmax_mean5_S50=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_tot_25_Rmax_mean5_S50[t]=mean(upper_tot_25_Rmax_mean5_S50[,t])
	p_upper_tot_50_Rmax_mean5_S50[t]=mean(upper_tot_50_Rmax_mean5_S50[,t])
	p_upper_tot_75_Rmax_mean5_S50[t]=mean(upper_tot_75_Rmax_mean5_S50[,t])	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_mean5_S50=array(0,dim=c(5000,(length(seq)-1)))
upper_50_Rmax_mean5_S50=array(0,dim=c(5000,(length(seq)-1)))
upper_75_Rmax_mean5_S50=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_mean5_S50[i,t] > 25){upper_25_Rmax_mean5_S50[i,t]=1}  
		if(DC_tot_mean5_S50[i,t] > 50){upper_50_Rmax_mean5_S50[i,t]=1}
		if(DC_tot_mean5_S50[i,t] > 75){upper_75_Rmax_mean5_S50[i,t]=1}
	}
}


p_upper_25_Rmax_mean5_S50=rep(0,(length(seq)-1))
p_upper_50_Rmax_mean5_S50=rep(0,(length(seq)-1))
p_upper_75_Rmax_mean5_S50=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_25_Rmax_mean5_S50[t]=mean(upper_25_Rmax_mean5_S50[,t])
	p_upper_50_Rmax_mean5_S50[t]=mean(upper_50_Rmax_mean5_S50[,t])
	p_upper_75_Rmax_mean5_S50[t]=mean(upper_75_Rmax_mean5_S50[,t])	
}

#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!

p_upper_tot_25_Rmax_mean5_S50<-rev(p_upper_tot_25_Rmax_mean5_S50)
p_upper_tot_50_Rmax_mean5_S50<-rev(p_upper_tot_50_Rmax_mean5_S50)
p_upper_tot_75_Rmax_mean5_S50<-rev(p_upper_tot_75_Rmax_mean5_S50)

p_upper_25_Rmax_mean5_S50<-rev(p_upper_25_Rmax_mean5_S50)
p_upper_50_Rmax_mean5_S50<-rev(p_upper_50_Rmax_mean5_S50)
p_upper_75_Rmax_mean5_S50<-rev(p_upper_75_Rmax_mean5_S50)

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_surfTot_Surv50_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)


points(x[2:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_S50[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_25_Rmax_mean5_S50[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_S50[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_S50[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_50_Rmax_mean5_S50[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_S50[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_S50[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_75_Rmax_mean5_S50[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_S50[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 50%","UTF8"),line=6)

dev.off()

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_Surv50_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)

points(x[2:(length(seq))]-4,p_upper_25_Rmax_mean5_S50[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_25_Rmax_mean5_S50[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_25_Rmax_mean5_S50[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_50_Rmax_mean5_S50[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_50_Rmax_mean5_S50[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_50_Rmax_mean5_S50[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_75_Rmax_mean5_S50[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_75_Rmax_mean5_S50[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_75_Rmax_mean5_S50[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 50%","UTF8"),line=6)

dev.off()

#On enl�ve le jeu de donn�es pr�c�dent
rm(list=c(surv50, "surv50"))
#On enl�ve tous les jeux de donn�es qui finissent par WS
rm(list=ls(pattern="S50$"))


#======================
# Survie 100
#======================

#On charge les donn�es du sc�nario Retour vers le futur
surv100<-load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_AmeliorationSurvie100_2018_06_26.RData")
#On charge Rmax avec les lignes de code du d�but du script

#On remet S_juv_JP car annul� avec l'import de withoutStocking
surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)
S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Cr�� une 5eme ligne qui correspond � la somme par ann�e des surfaces sur l'ensemble des secteurs
S_juv_JP_tot<-S_juv_JP[,T]

DC_tot_tot_S100=array(rep(0,T*5000),dim=c(5000,(T+20))) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
DC_tot_S100=array(rep(0,T*5000),dim=c(5000,(T+20))) # en prenant en compte uniquement les surfaces accessibles

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot_S100[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP_tot[5])		
	}
}

#-----------------------------
# Pour DC_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		DC_tot_S100[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])	
	}
}

#................................................
# on regarde le diagnostique ann�e par ann�e
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_S100=array(0,dim=c(5000,(T+20)))
upper_tot_50_Rmax_S100=array(0,dim=c(5000,(T+20)))
upper_tot_75_Rmax_S100=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_tot_S100[i,t] > 25){upper_tot_25_Rmax_S100[i,t]=1}  
		if(DC_tot_tot_S100[i,t] > 50){upper_tot_50_Rmax_S100[i,t]=1}
		if(DC_tot_tot_S100[i,t] > 75){upper_tot_75_Rmax_S100[i,t]=1}
	}
}


p_upper_tot_25_Rmax_S100=rep(0,(T+20))
p_upper_tot_50_Rmax_S100=rep(0,(T+20))
p_upper_tot_75_Rmax_S100=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_tot_25_Rmax_S100[t]=mean(upper_tot_25_Rmax_S100[,t])
	p_upper_tot_50_Rmax_S100[t]=mean(upper_tot_50_Rmax_S100[,t])
	p_upper_tot_75_Rmax_S100[t]=mean(upper_tot_75_Rmax_S100[,t])
	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_S100=array(0,dim=c(5000,(T+20)))
upper_50_Rmax_S100=array(0,dim=c(5000,(T+20)))
upper_75_Rmax_S100=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_S100[i,t] > 25){upper_25_Rmax_S100[i,t]=1}  
		if(DC_tot_S100[i,t] > 50){upper_50_Rmax_S100[i,t]=1}
		if(DC_tot_S100[i,t] > 75){upper_75_Rmax_S100[i,t]=1}
	}
}

p_upper_25_Rmax_S100=rep(0,(T+20))
p_upper_50_Rmax_S100=rep(0,(T+20))
p_upper_75_Rmax_S100=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_25_Rmax_S100[t]=mean(upper_25_Rmax_S100[,t])
	p_upper_50_Rmax_S100[t]=mean(upper_50_Rmax_S100[,t])
	p_upper_75_Rmax_S100[t]=mean(upper_75_Rmax_S100[,t])
	
}


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_Surv100_2018_06_25.png",width=800,height=800)

par(mar=c(8,6,2,0.5))
plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_tot_25_Rmax_S100[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_tot_25_Rmax_S100[(T+1):(T+20-1)],x[3:21],p_upper_tot_25_Rmax_S100[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_tot_50_Rmax_S100[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_tot_50_Rmax_S100[(T+1):(T+20-1)],x[3:21],p_upper_tot_50_Rmax_S100[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_tot_75_Rmax_S100[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_tot_75_Rmax_S100[(T+1):(T+20-1)],x[3:21],p_upper_tot_75_Rmax_S100[(T+1+1):(T+20)],col="grey15")


legend(43,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 100%","UTF8"),line=6)

dev.off()


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_Surv100_2018_06_25.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(8,6,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+20.5)),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_25_Rmax_S100[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_25_Rmax_S100[(T+1):(T+20-1)],x[3:21],p_upper_25_Rmax_S100[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_50_Rmax_S100[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_50_Rmax_S100[(T+1):(T+20-1)],x[3:21],p_upper_50_Rmax_S100[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_75_Rmax_S100[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_75_Rmax_S100[(T+1):(T+20-1)],x[3:21],p_upper_75_Rmax_S100[(T+1+1):(T+20)],col="grey15")


legend(43,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 100%","UTF8"),line=6)

dev.off()

#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
#write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq((T+20),(T),-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_tot_mean5_S100<-array(0,dim=c(5000,(length(seq)-1))) #surface totale quelque soit l'accessibilit�
DC_tot_mean5_S100<-array(0,dim=c(5000,(length(seq)-1))) # surface en prenant en compte les pb d'accessibilit�

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_tot_mean5_S100[i,s]<-mean(DC_tot_tot_S100[i,((seq[s+1]+1):(seq[s]))])
	}
}

for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_mean5_S100[i,s]<-mean(DC_tot_S100[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_mean5_S100=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_50_Rmax_mean5_S100=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_75_Rmax_mean5_S100=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_tot_mean5_S100[i,t] > 25){upper_tot_25_Rmax_mean5_S100[i,t]=1}  
		if(DC_tot_tot_mean5_S100[i,t] > 50){upper_tot_50_Rmax_mean5_S100[i,t]=1}
		if(DC_tot_tot_mean5_S100[i,t] > 75){upper_tot_75_Rmax_mean5_S100[i,t]=1}
	}
}


p_upper_tot_25_Rmax_mean5_S100=rep(0,(length(seq)-1))
p_upper_tot_50_Rmax_mean5_S100=rep(0,(length(seq)-1))
p_upper_tot_75_Rmax_mean5_S100=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_tot_25_Rmax_mean5_S100[t]=mean(upper_tot_25_Rmax_mean5_S100[,t])
	p_upper_tot_50_Rmax_mean5_S100[t]=mean(upper_tot_50_Rmax_mean5_S100[,t])
	p_upper_tot_75_Rmax_mean5_S100[t]=mean(upper_tot_75_Rmax_mean5_S100[,t])	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_mean5_S100=array(0,dim=c(5000,(length(seq)-1)))
upper_50_Rmax_mean5_S100=array(0,dim=c(5000,(length(seq)-1)))
upper_75_Rmax_mean5_S100=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_mean5_S100[i,t] > 25){upper_25_Rmax_mean5_S100[i,t]=1}  
		if(DC_tot_mean5_S100[i,t] > 50){upper_50_Rmax_mean5_S100[i,t]=1}
		if(DC_tot_mean5_S100[i,t] > 75){upper_75_Rmax_mean5_S100[i,t]=1}
	}
}


p_upper_25_Rmax_mean5_S100=rep(0,(length(seq)-1))
p_upper_50_Rmax_mean5_S100=rep(0,(length(seq)-1))
p_upper_75_Rmax_mean5_S100=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_25_Rmax_mean5_S100[t]=mean(upper_25_Rmax_mean5_S100[,t])
	p_upper_50_Rmax_mean5_S100[t]=mean(upper_50_Rmax_mean5_S100[,t])
	p_upper_75_Rmax_mean5_S100[t]=mean(upper_75_Rmax_mean5_S100[,t])	
}

#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!

p_upper_tot_25_Rmax_mean5_S100<-rev(p_upper_tot_25_Rmax_mean5_S100)
p_upper_tot_50_Rmax_mean5_S100<-rev(p_upper_tot_50_Rmax_mean5_S100)
p_upper_tot_75_Rmax_mean5_S100<-rev(p_upper_tot_75_Rmax_mean5_S100)

p_upper_25_Rmax_mean5_S100<-rev(p_upper_25_Rmax_mean5_S100)
p_upper_50_Rmax_mean5_S100<-rev(p_upper_50_Rmax_mean5_S100)
p_upper_75_Rmax_mean5_S100<-rev(p_upper_75_Rmax_mean5_S100)

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_surfTot_Surv100_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)


points(x[2:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_S100[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_25_Rmax_mean5_S100[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_S100[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_S100[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_50_Rmax_mean5_S100[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_S100[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_S100[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_75_Rmax_mean5_S100[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_S100[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 100%","UTF8"),line=6)

dev.off()

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_Surv100_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)

points(x[2:(length(seq))]-4,p_upper_25_Rmax_mean5_S100[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_25_Rmax_mean5_S100[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_25_Rmax_mean5_S100[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_50_Rmax_mean5_S100[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_50_Rmax_mean5_S100[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_50_Rmax_mean5_S100[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_75_Rmax_mean5_S100[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_75_Rmax_mean5_S100[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_75_Rmax_mean5_S100[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la survie � hauteur de 100%","UTF8"),line=6)

dev.off()

#On enl�ve le jeu de donn�es pr�c�dent
rm(list=c(surv100, "surv100"))
#On enl�ve tous les jeux de donn�es qui finissent par WS
rm(list=ls(pattern="S100$"))


#=======================
# ContinuiteEcologique
#=======================

#On charge les donn�es du sc�nario Retour vers le futur
continuiteEcologique<-load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_ContinuiteEcologique_2018_06_26.RData")
#On charge Rmax avec les lignes de code du d�but du script

#On remet S_juv_JP car annul� avec l'import de withoutStocking
surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)
S_juv_JP<-rbind(S_juv_JP,colSums(S_juv_JP)) #Cr�� une 5eme ligne qui correspond � la somme par ann�e des surfaces sur l'ensemble des secteurs
S_juv_JP_tot<-S_juv_JP[,T]

DC_tot_tot_CE=array(rep(0,T*5000),dim=c(5000,(T+20))) #en prenant l'ensemble des surfaces m�me si elles ne sont pas accessibles
DC_tot_CE=array(rep(0,T*5000),dim=c(5000,(T+20))) # en prenant en compte uniquement les surfaces accessibles

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en consid�rant l'ensemble des habitats favorables qu'ils soient accessibles ou non
		DC_tot_tot_CE[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP_tot[5])		
	}
}

#-----------------------------
# Pour DC_tot
#-----------------------------

for (t in (T+1):(T+20)){
	for (i in 1:5000){
		#On fait l'analyse en regardant ann�e apr�s ann�es les surfaces favorables effectivement accessibles
		DC_tot_CE[i,t]<-((d_moy_vichy[i,t-1]*S_juv_JP[1,t]+d_moy_alagnon[i,t-1]*S_juv_JP[2,t]+d_moy_langeac[i,t-1]*S_juv_JP[3,t]+d_moy_poutes[i,t-12]*S_juv_JP[4,t])*100)/(Rmax[i]*S_juv_JP[5,t])	
	}
}

#................................................
# on regarde le diagnostique ann�e par ann�e
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_CE=array(0,dim=c(5000,(T+20)))
upper_tot_50_Rmax_CE=array(0,dim=c(5000,(T+20)))
upper_tot_75_Rmax_CE=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_tot_CE[i,t] > 25){upper_tot_25_Rmax_CE[i,t]=1}  
		if(DC_tot_tot_CE[i,t] > 50){upper_tot_50_Rmax_CE[i,t]=1}
		if(DC_tot_tot_CE[i,t] > 75){upper_tot_75_Rmax_CE[i,t]=1}
	}
}


p_upper_tot_25_Rmax_CE=rep(0,(T+20))
p_upper_tot_50_Rmax_CE=rep(0,(T+20))
p_upper_tot_75_Rmax_CE=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_tot_25_Rmax_CE[t]=mean(upper_tot_25_Rmax_CE[,t])
	p_upper_tot_50_Rmax_CE[t]=mean(upper_tot_50_Rmax_CE[,t])
	p_upper_tot_75_Rmax_CE[t]=mean(upper_tot_75_Rmax_CE[,t])
	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_CE=array(0,dim=c(5000,(T+20)))
upper_50_Rmax_CE=array(0,dim=c(5000,(T+20)))
upper_75_Rmax_CE=array(0,dim=c(5000,(T+20)))



for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		if(DC_tot_CE[i,t] > 25){upper_25_Rmax_CE[i,t]=1}  
		if(DC_tot_CE[i,t] > 50){upper_50_Rmax_CE[i,t]=1}
		if(DC_tot_CE[i,t] > 75){upper_75_Rmax_CE[i,t]=1}
	}
}

p_upper_25_Rmax_CE=rep(0,(T+20))
p_upper_50_Rmax_CE=rep(0,(T+20))
p_upper_75_Rmax_CE=rep(0,(T+20))



for (t in (T+1):(T+20)){
	p_upper_25_Rmax_CE[t]=mean(upper_25_Rmax_CE[,t])
	p_upper_50_Rmax_CE[t]=mean(upper_50_Rmax_CE[,t])
	p_upper_75_Rmax_CE[t]=mean(upper_75_Rmax_CE[,t])
	
}


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_surfTot_ContinuiteEcologique_2018_06_25.png",width=800,height=800)

par(mar=c(8,6,2,0.5))
plot(1,1,type="n",axes=FALSE,xlim=c(T+0.5,T+20.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_tot_25_Rmax_CE[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_tot_25_Rmax_CE[(T+1):(T+20-1)],x[3:21],p_upper_tot_25_Rmax_CE[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_tot_50_Rmax_CE[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_tot_50_Rmax_CE[(T+1):(T+20-1)],x[3:21],p_upper_tot_50_Rmax_CE[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_tot_75_Rmax_CE[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_tot_75_Rmax_CE[(T+1):(T+20-1)],x[3:21],p_upper_tot_75_Rmax_CE[(T+1+1):(T+20)],col="grey15")


legend(43,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la continuit� �cologique (montaison + d�valaison)","UTF8"),line=6)

dev.off()


png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_annuel_ContinuiteEcologique_2018_06_25.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(8,6,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+20.5)),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+4),(T+9),(T+14),(T+19)),
		labels=c((1975+T),(1975+T+4),(1975+T+9),(1975+T+14),(1975+T+19)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(T,(T+20),1)

points(x[2:21],p_upper_25_Rmax_CE[(T+1):(T+20)],col="grey75",pch=16)
segments(x[2:20],p_upper_25_Rmax_CE[(T+1):(T+20-1)],x[3:21],p_upper_25_Rmax_CE[(T+1+1):(T+20)],col="grey75")

points(x[2:21],p_upper_50_Rmax_CE[(T+1):(T+20)],col="grey65",pch=16)
segments(x[2:20],p_upper_50_Rmax_CE[(T+1):(T+20-1)],x[3:21],p_upper_50_Rmax_CE[(T+1+1):(T+20)],col="grey55")

points(x[2:21],p_upper_75_Rmax_CE[(T+1):(T+20)],col="grey55",pch=16)
segments(x[2:20],p_upper_75_Rmax_CE[(T+1):(T+20-1)],x[3:21],p_upper_75_Rmax_CE[(T+1+1):(T+20)],col="grey15")


legend(43,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la continuit� �cologique (montaison + d�valaison)","UTF8"),line=6)

dev.off()

#................................................
# on regarde le diagnostique moyenn� sur 5 ans
#................................................

### 25, 50 et 75% Rmax pris comme r�f�rence

#Pour pouvoir v�rifier le calcul qu'on fait pour avoir un diagnostique moyen sur p�riode de 5 ans, on sauve l'objet DC_tot pour pouvoir comparer la moyennes des 5 derni�res ann�es par exemple de DC_tot avec
# celle de DC_tot_mean5.
#On pourrait aussi tout simplement comparer colMeans(DC_tot_mean5)[1] et mean(DC_tot[,38:42]) par exemple
#write.csv(DC_tot, file = "DC_tot.csv")

seq<-seq((T+20),(T),-5) #Cr�ation d'une s�quence pour calculer la moyenne mobile
DC_tot_tot_mean5_CE<-array(0,dim=c(5000,(length(seq)-1))) #surface totale quelque soit l'accessibilit�
DC_tot_mean5_CE<-array(0,dim=c(5000,(length(seq)-1))) # surface en prenant en compte les pb d'accessibilit�

#Calcul de la moyenne mobile. On commence � l'ann�e la plus r�cente et on laisse les derni�res ann�es si �a ne fait pas un groupe de 5 ann�es
##### ATTENTION quand s=1 DC_tot_mean5 calcul� pour la p�riode 2012-2016 ##############
for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_tot_mean5_CE[i,s]<-mean(DC_tot_tot_CE[i,((seq[s+1]+1):(seq[s]))])
	}
}

for (s in 1:(length(seq)-1)){#(s in 1:(length(seq)-1)){
	for (i in 1:5000){
		DC_tot_mean5_CE[i,s]<-mean(DC_tot_CE[i,((seq[s+1]+1):(seq[s]))])
	}
}

##V�rif que colMeans(DC_tot_mean5) est bien �gal � l'objet DC_tot sauvegard� en .csv et � la moyenne 5 ans par 5 ans.
#Le code pour calculer la moyenne par tranche de 5 ans est bon

#-----------------------------
# Pour DC_tot_tot
#-----------------------------

upper_tot_25_Rmax_mean5_CE=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_50_Rmax_mean5_CE=array(0,dim=c(5000,(length(seq)-1)))
upper_tot_75_Rmax_mean5_CE=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_tot_mean5_CE[i,t] > 25){upper_tot_25_Rmax_mean5_CE[i,t]=1}  
		if(DC_tot_tot_mean5_CE[i,t] > 50){upper_tot_50_Rmax_mean5_CE[i,t]=1}
		if(DC_tot_tot_mean5_CE[i,t] > 75){upper_tot_75_Rmax_mean5_CE[i,t]=1}
	}
}


p_upper_tot_25_Rmax_mean5_CE=rep(0,(length(seq)-1))
p_upper_tot_50_Rmax_mean5_CE=rep(0,(length(seq)-1))
p_upper_tot_75_Rmax_mean5_CE=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_tot_25_Rmax_mean5_CE[t]=mean(upper_tot_25_Rmax_mean5_CE[,t])
	p_upper_tot_50_Rmax_mean5_CE[t]=mean(upper_tot_50_Rmax_mean5_CE[,t])
	p_upper_tot_75_Rmax_mean5_CE[t]=mean(upper_tot_75_Rmax_mean5_CE[,t])	
}


#-----------------------------
# Pour DC_tot
#-----------------------------

upper_25_Rmax_mean5_CE=array(0,dim=c(5000,(length(seq)-1)))
upper_50_Rmax_mean5_CE=array(0,dim=c(5000,(length(seq)-1)))
upper_75_Rmax_mean5_CE=array(0,dim=c(5000,(length(seq)-1)))



for (t in 1:(length(seq)-1)){
	for (i in 1:5000){
		if(DC_tot_mean5_CE[i,t] > 25){upper_25_Rmax_mean5_CE[i,t]=1}  
		if(DC_tot_mean5_CE[i,t] > 50){upper_50_Rmax_mean5_CE[i,t]=1}
		if(DC_tot_mean5_CE[i,t] > 75){upper_75_Rmax_mean5_CE[i,t]=1}
	}
}


p_upper_25_Rmax_mean5_CE=rep(0,(length(seq)-1))
p_upper_50_Rmax_mean5_CE=rep(0,(length(seq)-1))
p_upper_75_Rmax_mean5_CE=rep(0,(length(seq)-1))



for (t in 1:(length(seq)-1)){
	p_upper_25_Rmax_mean5_CE[t]=mean(upper_25_Rmax_mean5_CE[,t])
	p_upper_50_Rmax_mean5_CE[t]=mean(upper_50_Rmax_mean5_CE[,t])
	p_upper_75_Rmax_mean5_CE[t]=mean(upper_75_Rmax_mean5_CE[,t])	
}

#ATTENTION p_upper_Rmax_mean5 est � l'envers. l'indice 1 correspond � l'ann�e la plus r�cente
##ON REMET DANS LE BON SENS !!

p_upper_tot_25_Rmax_mean5_CE<-rev(p_upper_tot_25_Rmax_mean5_CE)
p_upper_tot_50_Rmax_mean5_CE<-rev(p_upper_tot_50_Rmax_mean5_CE)
p_upper_tot_75_Rmax_mean5_CE<-rev(p_upper_tot_75_Rmax_mean5_CE)

p_upper_25_Rmax_mean5_CE<-rev(p_upper_25_Rmax_mean5_CE)
p_upper_50_Rmax_mean5_CE<-rev(p_upper_50_Rmax_mean5_CE)
p_upper_75_Rmax_mean5_CE<-rev(p_upper_75_Rmax_mean5_CE)

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_surfTot_ContinuiteEcologique_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)


points(x[2:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_CE[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_25_Rmax_mean5_CE[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_25_Rmax_mean5_CE[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_CE[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_50_Rmax_mean5_CE[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_50_Rmax_mean5_CE[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_CE[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_tot_75_Rmax_mean5_CE[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_tot_75_Rmax_mean5_CE[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la continuit� �cologique (montaison + d�valaison)","UTF8"),line=6)

dev.off()

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/2018_06_TxRenouv_DiagConserv_model2017_08_29/DiagnostiqueConservation_mean5_ContinuiteEcologique_2018_06_25.png",width=800,height=800)

par(mar=c(8,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c((T+0.5),(T+16.5)),xlab="",ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T+1),(T+5),(T+11),(T+16)),
		labels=c(str_c((1975+T),"-",(1975+T+4)),str_c((1975+T+5),"-",(1975+T+9)),str_c((1975+T+10),"-",(1975+T+14)),str_c((1975+T+15),"-",(1975+T+19))),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)

points(x[2:(length(seq))]-4,p_upper_25_Rmax_mean5_CE[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_25_Rmax_mean5_CE[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_25_Rmax_mean5_CE[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))]-4,p_upper_50_Rmax_mean5_CE[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_50_Rmax_mean5_CE[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_50_Rmax_mean5_CE[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))]-4,p_upper_75_Rmax_mean5_CE[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)]-4,p_upper_75_Rmax_mean5_CE[1:(length(seq)-2)],x[3:(length(seq))]-4,p_upper_75_Rmax_mean5_CE[2:(length(seq)-1)],col="grey15")


legend(50,1,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

abline(h=0.75,col="red",lty=2)
abline(h=0.5,col="red")
abline(h=0.25,col="red",lty=2)
mtext(1,text=iconv("Scenario : Am�lioration de la continuit� �cologique (montaison + d�valaison)","UTF8"),line=6)

dev.off()

#On enl�ve le jeu de donn�es pr�c�dent
rm(list=c(continuiteEcologique, "continuiteEcologique"))
#On enl�ve tous les jeux de donn�es qui finissent par WS
rm(list=ls(pattern="CE$"))


#=======================================
# Les 2 graphs c�te � c�te

par(mfrow=c(1,2),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(1.5,T+0.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(2,11,16,26,36,T),
		labels=c(1976,(1976+9),1990,2000,2010,(1975+T-1)),
		cex.axis = 0.9,las = 1,col = "black")

x=seq(1,T,1)

points(x[2:T],p_upper_25_Rmax[2:T],col="grey75",pch=16)
segments(x[2:(T-1)],p_upper_25_Rmax[2:(T-1)],x[3:T],p_upper_25_Rmax[3:T],col="grey75")

points(x[2:T],p_upper_50_Rmax[2:T],col="grey65",pch=16)
segments(x[2:(T-1)],p_upper_50_Rmax[2:(T-1)],x[3:T],p_upper_50_Rmax[3:T],col="grey55")

points(x[2:T],p_upper_75_Rmax[2:T],col="grey55",pch=16)
segments(x[2:(T-1)],p_upper_75_Rmax[2:(T-1)],x[3:T],p_upper_75_Rmax[3:T],col="grey15")


legend(14,1.05,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

plot(1,1,type="n",axes=FALSE,xlim=c(4.5,(T+0.5)),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c((T-35),(T-30),(T-25),(T-20),(T-15),(T-10),(T-5),T),
		labels=c((1977+4),(1977+9),(1977+14),(1977+19),(1977+24),(1977+29),(1977+34),(1975+T-1)),
		cex.axis = 0.9,las = 1,col = "black")

x=rev(seq)

points(x[2:(length(seq))],p_upper_25_Rmax_mean5[1:(length(seq)-1)],col="grey75",pch=16)
segments(x[2:(length(seq)-1)],p_upper_25_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_25_Rmax_mean5[2:(length(seq)-1)],col="grey75")

points(x[2:(length(seq))],p_upper_50_Rmax_mean5[1:(length(seq)-1)],col="grey65",pch=16)
segments(x[2:(length(seq)-1)],p_upper_50_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_50_Rmax_mean5[2:(length(seq)-1)],col="grey55")

points(x[2:(length(seq))],p_upper_75_Rmax_mean5[1:(length(seq)-1)],col="grey55",pch=16)
segments(x[2:(length(seq)-1)],p_upper_75_Rmax_mean5[1:(length(seq)-2)],x[3:(length(seq))],p_upper_75_Rmax_mean5[2:(length(seq)-1)],col="grey15")


legend(5,0.9,legend=c(expression(p^seuils >75),expression(p^seuils >50),expression(p^seuils >25)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey65","grey75"), 
		bty="n" )

