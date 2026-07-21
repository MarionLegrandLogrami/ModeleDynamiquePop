# GRAPH de la densité de juvénile (sauvage, élevage, totale)  et des géniteurs à l'échelle du bassin
# pendant la période de projection
# Author: marion.legrand
###############################################################################

#Modèle 2017.03.23_4zones_Interaction - interaction réciproque juv sauvage/déversé
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_03_23_4zones_Interaction/")

library(coda)
library(boot)
T=41

surf=c(rep(c(846021,70845,250441,0),11),rep(c(846021,70845,250441,301101),12),rep(c(846021,70845,250441,383049),6),rep(c(846021,356519,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=4)	


###############################################################################################################
#--------------------- Graphique répartition spatiale des juvéniles totaux RATIO------------------------------#
###############################################################################################################
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

max(juv_tot_q)#387516
max(juv_V_q)#230515
max(juv_A_q)#258893
max(juv_L_q)#179908
max(juv_P_q)#283883

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
	

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_03_23_4zones_Interaction/RepartitionSpatiale_juv_totaux_ratio.png",width=800, height=1000, units = "px",type="cairo")
par(mfrow=c(5,1))

	#Juv totaux tous secteurs
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,400000),ylab="",main="total 0+ juvenile production (wild + stocked)")
		
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
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				cex.axis = 0.9,las = 1,col = "grey55")
		
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
		
	#Vichy juv totaux ratio
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Vichy (total 0+ juvenile production)")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
				labels=c(0,0.2,0.4,0.6,0.8,1),
				cex.axis = 0.8,las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				cex.axis = 0.9,las = 1,col = "grey55")
		
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
		
		#Alagnon juv totaux ratio
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Alagnon (total 0+ juvenile production)")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
				labels=c(0,0.2,0.4,0.6,0.8,1),
				cex.axis = 0.8,las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				cex.axis = 0.9,las = 1,col = "grey55")
		
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
		
	#Langeac juv totaux ratio
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Langeac (total 0+ juvenile production)")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
				labels=c(0,0.2,0.4,0.6,0.8,1),
				cex.axis = 0.8,las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				cex.axis = 0.9,las = 1,col = "grey55")
		
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
		
	#Poutes juv totaux ratio
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Poutes (total 0+ juvenile production)")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
				labels=c(0,0.2,0.4,0.6,0.8,1),
				cex.axis = 0.8,las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				cex.axis = 0.9,las = 1,col = "grey55")
		
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
		
dev.off()

###############################################################################################
#---------------------------- Pendant la période des projections -----------------------------# 
###############################################################################################

#Récupération des caluls réalisés dans le cadre de la modélisation sans déversement sur le modèle standard
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_03_23_ProjectionSansRepeuplementAlagnon_InteractionReciproque_2017_04_25.RData")

#les juv_vichy,alagnon,langeac,poutes du load ne commencent qu'à t=43 (T+2) on recréé le t=42 (T+1)
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


png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_03_23_4zones_Interaction/RepartitionSpatiale_juv_totaux_ratio_proj20years.png",width=800, height=1500, units = "px",type="cairo")
par(mfrow=c(5,1))

#Juv totaux tous secteurs
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,400000),ylab="",main="total 0+ juvenile production (wild + stocked)")

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
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,juv_total_q[i+T,5],i+0.15,juv_total_q[i+T,5])
	segments(i,juv_total_q[i+T,4],i,juv_total_q[i+T,5])
	#5%
	segments(i-0.15,juv_total_q[i+T,1],i+0.15,juv_total_q[i+T,1])
	segments(i,juv_total_q[i+T,2],i,juv_total_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_total_q[i+T,2],juv_total_q[i+T,2],juv_total_q[i+T,4],juv_total_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,juv_total_q[i+T,3],i+0.3,juv_total_q[i+T,3])
}

#Vichy juv totaux ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Vichy (total 0+ juvenile production)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_vichy_q[i+T,5],i+0.15,ratio_juv_vichy_q[i+T,5])
	segments(i,ratio_juv_vichy_q[i+T,4],i,ratio_juv_vichy_q[i+T,5])
	#5%
	segments(i-0.15,ratio_juv_vichy_q[i+T,1],i+0.15,ratio_juv_vichy_q[i+T,1])
	segments(i,ratio_juv_vichy_q[i+T,2],i,ratio_juv_vichy_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_vichy_q[i+T,2],ratio_juv_vichy_q[i+T,2],ratio_juv_vichy_q[i+T,4],ratio_juv_vichy_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_vichy_q[i+T,3],i+0.3,ratio_juv_vichy_q[i+T,3])
}
abline(h=1,lty=3,col = "grey55")

#Alagnon juv totaux ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Alagnon (total 0+ juvenile production)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_alagnon_q[i+T,5],i+0.15,ratio_juv_alagnon_q[i+T,5])
	segments(i,ratio_juv_alagnon_q[i+T,4],i,ratio_juv_alagnon_q[i+T,5])
	#5%
	segments(i-0.15,ratio_juv_alagnon_q[i+T,1],i+0.15,ratio_juv_alagnon_q[i+T,1])
	segments(i,ratio_juv_alagnon_q[i+T,2],i,ratio_juv_alagnon_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_alagnon_q[i+T,2],ratio_juv_alagnon_q[i+T,2],ratio_juv_alagnon_q[i+T,4],ratio_juv_alagnon_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_alagnon_q[i+T,3],i+0.3,ratio_juv_alagnon_q[i+T,3])
}
abline(h=1,lty=3,col = "grey55")

#Langeac juv totaux ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Langeac (total 0+ juvenile production)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_langeac_q[i+T,5],i+0.15,ratio_juv_langeac_q[i+T,5])
	segments(i,ratio_juv_langeac_q[i+T,4],i,ratio_juv_langeac_q[i+T,5])
	#5%
	segments(i-0.15,ratio_juv_langeac_q[i+T,1],i+0.15,ratio_juv_langeac_q[i+T,1])
	segments(i,ratio_juv_langeac_q[i+T,2],i,ratio_juv_langeac_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_langeac_q[i+T,2],ratio_juv_langeac_q[i+T,2],ratio_juv_langeac_q[i+T,4],ratio_juv_langeac_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_langeac_q[i+T,3],i+0.3,ratio_juv_langeac_q[i+T,3])
}
abline(h=1,lty=3,col = "grey55")

#Poutes juv totaux ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Poutes (total 0+ juvenile production)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_poutes_q[i+T,5],i+0.15,ratio_juv_poutes_q[i+T,5])
	segments(i,ratio_juv_poutes_q[i+T,4],i,ratio_juv_poutes_q[i+T,5])
	#5%
	segments(i-0.15,ratio_juv_poutes_q[i+T,1],i+0.15,ratio_juv_poutes_q[i+T,1])
	segments(i,ratio_juv_poutes_q[i+T,2],i,ratio_juv_poutes_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_poutes_q[i+T,2],ratio_juv_poutes_q[i+T,2],ratio_juv_poutes_q[i+T,4],ratio_juv_poutes_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_poutes_q[i+T,3],i+0.3,ratio_juv_poutes_q[i+T,3])
}
abline(h=1,lty=3,col = "grey55")

dev.off()

#====================================================
# Figure avec période jusqu'à T puis de T à T+20
#====================================================

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_03_23_4zones_Interaction/RepartitionSpatiale_juv_totaux_ratio_serieTemp&proj20years.png",width=800, height=1500, units = "px",type="cairo")
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

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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

# trace l'axe des ordonn�es
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

###############################################################################################
#--------------------------- Géniteurs totaux + ratio par secteur ----------------------------#
###############################################################################################

	#---------- Sans projection --------------#
S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_alagnon_real=read.coda("S_alagnonCODAchain1.txt","S_alagnonCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")

#On récupère les données du modèle directement dans le fichier data

library(coda) 
require(stringr)
bugs2jags(str_c("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2017_03_23_4zones_Interaction/","data.txt"),"data_4zones_Interaction.R")
source("data_4zones_Interaction.R")
head(N) #ça marche !

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


png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_03_23_4zones_Interaction/RepartitionSpatialeGéniteurs_ratio.png",width=800, height=1000, units = "px",type="cairo")
par(mfrow=c(5,1))
	#..........
	# Nb total
	#..........
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,8000),ylab="",main="Total number of spawners")
		
		# trace l'axe des ordonnées
		axis(2,at = c(0,2000,4000,6000,8000),labels=c(0,2000,4000,6000,8000),las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				las = 1,col = "grey55")
		
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
		
	#........
	# Vichy
	#........
	
		plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Vichy-Langeac")
		
		# trace l'axe des ordonnées
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				las = 1,col = "grey55")
		
		#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)
		
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
		
		#........
		# Alagnon
		#........
		
		plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Alagnon")
		
		# trace l'axe des ordonnées
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				las = 1,col = "grey55")
		
		#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)
		
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

	#........
	# Langeac
	#........
		
		plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Langeac-Poutes")
		
		# trace l'axe des ordonnées
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				las = 1,col = "grey55")
		
		#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)
		
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

	#........
	# Poutes
	#........
		
		plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Amont Poutes")
		
		# trace l'axe des ordonnées
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				las = 1,col = "grey55")
		
		#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)
		
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

dev.off()


###############################################################################################
#---------------------------- Pendant la période des projections -----------------------------# 
###############################################################################################

#Récupération des caluls réalisés dans le cadre de la modélisation sans déversement sur le modèle standard
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_03_23_ProjectionSansRepeuplementAlagnon_InteractionReciproque_2017_04_25.RData")

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



png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_03_23_4zones_Interaction/RepartitionSpatialeGéniteurs_ratio_proj20years.png",width=800, height=1500, units = "px",type="cairo")
par(mfrow=c(5,1))
#..........
# Nb total
#..........
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,4000),ylab="",main="Total number of spawners")

# trace l'axe des ordonnées
axis(2,at = c(0,1000,2000,3000,4000),labels=c(0,1000,2000,3000,4000),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		las = 1,col = "grey55")

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_q[i+T,5],i+0.15,N_vichy_q[i+T,5])
	segments(i,N_vichy_q[i+T,4],i,N_vichy_q[i+T,5])
	#5%
	segments(i-0.15,N_vichy_q[i+T,1],i+0.15,N_vichy_q[i+T,1])
	segments(i,N_vichy_q[i+T,2],i,N_vichy_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_q[i+T,2],N_vichy_q[i+T,2],N_vichy_q[i+T,4],N_vichy_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,N_vichy_q[i+T,3],i+0.3,N_vichy_q[i+T,3])
}

#........
# Vichy
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Vichy-Langeac")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		las = 1,col = "grey55")

#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_vichy_q[i+T,5],i+0.15,ratio_S_vichy_q[i+T,5])
	segments(i,ratio_S_vichy_q[i+T,4],i,ratio_S_vichy_q[i+T,5])
	#5%
	segments(i-0.15,ratio_S_vichy_q[i+T,1],i+0.15,ratio_S_vichy_q[i+T,1])
	segments(i,ratio_S_vichy_q[i+T,2],i,ratio_S_vichy_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_vichy_q[i+T,2],ratio_S_vichy_q[i+T,2],ratio_S_vichy_q[i+T,4],ratio_S_vichy_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_vichy_q[i+T,3],i+0.3,ratio_S_vichy_q[i+T,3])
}


abline(h=1,lty=3)

#........
# Alagnon
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Alagnon")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		las = 1,col = "grey55")

#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_alagnon_q[i+T,5],i+0.15,ratio_S_alagnon_q[i+T,5])
	segments(i,ratio_S_alagnon_q[i+T,4],i,ratio_S_alagnon_q[i+T,5])
	#5%
	segments(i-0.15,ratio_S_alagnon_q[i+T,1],i+0.15,ratio_S_alagnon_q[i+T,1])
	segments(i,ratio_S_alagnon_q[i+T,2],i,ratio_S_alagnon_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_alagnon_q[i+T,2],ratio_S_alagnon_q[i+T,2],ratio_S_alagnon_q[i+T,4],ratio_S_alagnon_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_alagnon_q[i+T,3],i+0.3,ratio_S_alagnon_q[i+T,3])
}


abline(h=1,lty=3)

#........
# Langeac
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Langeac-Poutes")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		las = 1,col = "grey55")

#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_langeac_q[i+T,5],i+0.15,ratio_S_langeac_q[i+T,5])
	segments(i,ratio_S_langeac_q[i+T,4],i,ratio_S_langeac_q[i+T,5])
	#5%
	segments(i-0.15,ratio_S_langeac_q[i+T,1],i+0.15,ratio_S_langeac_q[i+T,1])
	segments(i,ratio_S_langeac_q[i+T,2],i,ratio_S_langeac_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_langeac_q[i+T,2],ratio_S_langeac_q[i+T,2],ratio_S_langeac_q[i+T,4],ratio_S_langeac_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_langeac_q[i+T,3],i+0.3,ratio_S_langeac_q[i+T,3])
}


abline(h=1,lty=3)

#........
# Poutes
#........

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,20.5), xlab="Years", ylim=c(0,1), ylab="" ,main="Ratio spawners Amont Poutes")

# trace l'axe des ordonnées
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1974+T+1,1974+T+5,1974+T+10,1974+T+15,1974+T+20)
axis(1,at = c(1,5,10,15,20),
		labels=lab1,
		las = 1,col = "grey55")

#text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)

for(i in 1:20){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_poutes_q[i+T,5],i+0.15,ratio_S_poutes_q[i+T,5])
	segments(i,ratio_S_poutes_q[i+T,4],i,ratio_S_poutes_q[i+T,5])
	#5%
	segments(i-0.15,ratio_S_poutes_q[i+T,1],i+0.15,ratio_S_poutes_q[i+T,1])
	segments(i,ratio_S_poutes_q[i+T,2],i,ratio_S_poutes_q[i+T,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_poutes_q[i+T,2],ratio_S_poutes_q[i+T,2],ratio_S_poutes_q[i+T,4],ratio_S_poutes_q[i+T,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_poutes_q[i+T,3],i+0.3,ratio_S_poutes_q[i+T,3])
}


abline(h=1,lty=3)


dev.off()



#====================================================
# Figure avec période jusqu'à T puis de T à T+20
#====================================================

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2017_03_23_4zones_Interaction/RepartitionSpatialeGéniteurs_ratio_serieTemp&proj20years.png",width=800, height=1500, units = "px",type="cairo")
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
