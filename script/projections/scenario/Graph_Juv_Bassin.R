# GRAPH de la densité de juvénile (sauvage, élevage, totale) à l'échelle du bassin
# 
# Author: marion.legrand
###############################################################################
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2015_01_24_thin200")
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2015_11_26")
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2015_12_04_standard")
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_01_20_standard_thin200")

library(coda)
library(boot)
T=40

surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),(T-8)))
S_juv_JP<-matrix(surf,nrow=3)

####################################################################################
#--------------------- Graphique des juvéniles totaux------------------------------#
####################################################################################
dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)

juv_tot=array(rep(0,195000),dim=c(5000,T))#296000
juv_tot_q=array(rep(0,T*5),dim=c(T,5))

for (t in 2:12){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1,t] + dmoy_tot_L[,t-1] * S_juv_JP[2,t]
}
for (t in 13:T){
	juv_tot[,t]= dmoy_tot_V[,t-1] *S_juv_JP[1,t] + dmoy_tot_L[,t-1] *S_juv_JP[2,t] + dmoy_tot_P[,t-12] * S_juv_JP[3,t]	
}

for (i in 1:T){
	juv_tot_q[i,]=quantile(juv_tot[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

max(juv_tot_q)
png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/ProdJuvTot.png",width=800, height=800, units = "px",type="cairo")
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,500000),ylab="",main="total 0+ juvenile production (wild + stocked)")

# trace l'axe des ordonn�es
axis(2,at = c(0,100000,200000,300000,400000,500000),
		labels=c(
				expression(0),
				expression(paste(100," x ", 10^3)),
				expression(paste(200," x ", 10^3)),
				expression(paste(300," x ", 10^3)),
				expression(paste(400," x ", 10^3)),
				expression(paste(500," x ", 10^3))),
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


dev.off()


######################################################################################
#--------------------- Graphique des juvéniles sauvages------------------------------#
######################################################################################

dmoy_wild_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt",5001,10000)
dmoy_wild_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt",5001,10000)
dmoy_wild_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt",5001,10000)

juv_wild_tot=array(rep(0,195000),dim=c(5000,T))#296000
juv_wild_tot_q=array(rep(0,T*5),dim=c(T,5))

for (t in 2:12){
	juv_wild_tot[,t]= dmoy_wild_V[,t-1] * S_juv_JP[1,t] + dmoy_wild_L[,t-1] * S_juv_JP[2,t]
}
for (t in 13:T){
	juv_wild_tot[,t]= dmoy_wild_V[,t-1] *S_juv_JP[1,t] + dmoy_wild_L[,t-1] *S_juv_JP[2,t] + dmoy_wild_P[,t-12] * S_juv_JP[3,t]	
}

for (i in 1:T){
	juv_wild_tot_q[i,]=quantile(juv_wild_tot[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

max(juv_wild_tot_q)

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/ProdJuvTot_WILD.png",width=800, height=800, units = "px",type="cairo")
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,500000),ylab="",main="total wild 0+ juvenile production")

# trace l'axe des ordonn�es
axis(2,at = c(0,100000,200000,300000,400000,500000),
		labels=c(
				expression(0),
				expression(paste(100," x ", 10^3)),
				expression(paste(200," x ", 10^3)),
				expression(paste(300," x ", 10^3)),
				expression(paste(400," x ", 10^3)),
				expression(paste(500," x ", 10^3))),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,T),
		labels=lab1,
		cex.axis = 0.9,las = 1,col = "grey55")




for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,juv_wild_tot_q[i,5],i+0.15,juv_wild_tot_q[i,5])
	segments(i,juv_wild_tot_q[i,4],i,juv_wild_tot_q[i,5])
	#5%
	segments(i-0.15,juv_wild_tot_q[i,1],i+0.15,juv_wild_tot_q[i,1])
	segments(i,juv_wild_tot_q[i,2],i,juv_wild_tot_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_wild_tot_q[i,2],juv_wild_tot_q[i,2],juv_wild_tot_q[i,4],juv_wild_tot_q[i,4]),col="grey85")
	#median
	segments(i-0.3,juv_wild_tot_q[i,3],i+0.3,juv_wild_tot_q[i,3])
}


dev.off()

###########################################################################################################
#--------------------- Graphique répartition spatiale des juvéniles totaux------------------------------#
###########################################################################################################
juv_V=array(rep(0,195000),dim=c(5000,T))
juv_L=array(rep(0,195000),dim=c(5000,T))
juv_P=array(rep(0,195000),dim=c(5000,T))

juv_V_q=array(rep(0,T*5),dim=c(T,5))
juv_L_q=array(rep(0,T*5),dim=c(T,5))
juv_P_q=array(rep(0,T*5),dim=c(T,5))

for (t in 2:12){
	juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[1,t]
	juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[2,t]
}

for (t in 13:T){
	juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[1,t]
	juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[2,t]
	juv_P[,t]<-dmoy_tot_P[,t-12] * S_juv_JP[3,t]
}

for (i in 1:T){
	juv_V_q[i,]=quantile(juv_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	juv_L_q[i,]=quantile(juv_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	juv_P_q[i,]=quantile(juv_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

max(juv_V_q)#352108.6
max(juv_L_q)#183536.9
max(juv_P_q)#225391.8

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/RepartitionSpatialeProdJuvTot.png",width=800, height=800, units = "px",type="cairo")
par(mfrow=c(3,1))
	#Vichy
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,400000),ylab="",main="total 0+ juvenile production at Vichy (wild+stocked)")
		
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
			segments(i-0.15,juv_V_q[i,5],i+0.15,juv_V_q[i,5])
			segments(i,juv_V_q[i,4],i,juv_V_q[i,5])
			#5%
			segments(i-0.15,juv_V_q[i,1],i+0.15,juv_V_q[i,1])
			segments(i,juv_V_q[i,2],i,juv_V_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_V_q[i,2],juv_V_q[i,2],juv_V_q[i,4],juv_V_q[i,4]),col="grey85")
			#median
			segments(i-0.3,juv_V_q[i,3],i+0.3,juv_V_q[i,3])
		}
	
	#Langeac
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,400000),ylab="",main="total 0+ juvenile production at Langeac (wild+stocked)")
		
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
			segments(i-0.15,juv_L_q[i,5],i+0.15,juv_L_q[i,5])
			segments(i,juv_L_q[i,4],i,juv_L_q[i,5])
			#5%
			segments(i-0.15,juv_L_q[i,1],i+0.15,juv_L_q[i,1])
			segments(i,juv_L_q[i,2],i,juv_L_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_L_q[i,2],juv_L_q[i,2],juv_L_q[i,4],juv_L_q[i,4]),col="grey85")
			#median
			segments(i-0.3,juv_L_q[i,3],i+0.3,juv_L_q[i,3])
		}
		
	#Poutes
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,400000),ylab="",main="total 0+ juvenile production at Poutes (wild+stocked)")
		
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
			segments(i-0.15,juv_P_q[i,5],i+0.15,juv_P_q[i,5])
			segments(i,juv_P_q[i,4],i,juv_P_q[i,5])
			#5%
			segments(i-0.15,juv_P_q[i,1],i+0.15,juv_P_q[i,1])
			segments(i,juv_P_q[i,2],i,juv_P_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_P_q[i,2],juv_P_q[i,2],juv_P_q[i,4],juv_P_q[i,4]),col="grey85")
			#median
			segments(i-0.3,juv_P_q[i,3],i+0.3,juv_P_q[i,3])
		}

dev.off()

###############################################################################################################
#--------------------- Graphique répartition spatiale des juvéniles totaux RATIO------------------------------#
###############################################################################################################
dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)

juv_tot=array(rep(0,195000),dim=c(5000,T))#296000
juv_tot_q=array(rep(0,T*5),dim=c(T,5))

for (t in 2:12){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1,t] + dmoy_tot_L[,t-1] * S_juv_JP[2,t]
}
for (t in 13:T){
	juv_tot[,t]= dmoy_tot_V[,t-1] *S_juv_JP[1,t] + dmoy_tot_L[,t-1] *S_juv_JP[2,t] + dmoy_tot_P[,t-12] * S_juv_JP[3,t]	
}

for (i in 1:T){
	juv_tot_q[i,]=quantile(juv_tot[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

max(juv_tot_q)#401774.1
max(juv_V_q)#352108.6
max(juv_L_q)#183536.9
max(juv_P_q)#225391.8

	##Par secteurs calcul des ratios
	juv_V=array(rep(0,195000),dim=c(5000,T))
	juv_L=array(rep(0,195000),dim=c(5000,T))
	juv_P=array(rep(0,195000),dim=c(5000,T))
	
	ratio_juv_V=array(0,c(5000,T))
	ratio_juv_L=array(0,c(5000,T))
	ratio_juv_P=array(0,c(5000,T))
	
	ratio_juv_V_q=array(rep(0,T*5),dim=c(T,5))
	ratio_juv_L_q=array(rep(0,T*5),dim=c(T,5))
	ratio_juv_P_q=array(rep(0,T*5),dim=c(T,5))
	
	
	for (t in 2:12){
		juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[1,t]
		juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[2,t]
	}
	
	for (t in 13:T){
		juv_V[,t]<-dmoy_tot_V[,t-1] * S_juv_JP[1,t]
		juv_L[,t]<-dmoy_tot_L[,t-1] * S_juv_JP[2,t]
		juv_P[,t]<-dmoy_tot_P[,t-12] * S_juv_JP[3,t]
	}

	for (t in 2:T){
		ratio_juv_V[,t]=juv_V[,t]/juv_tot[,t]
		ratio_juv_L[,t]=juv_L[,t]/juv_tot[,t]
		ratio_juv_P[,t]=juv_P[,t]/juv_tot[,t]
	}
	
	for (t in 1:T){
		ratio_juv_V_q[t,]=quantile(ratio_juv_V[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
		ratio_juv_L_q[t,]=quantile(ratio_juv_L[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
		ratio_juv_P_q[t,]=quantile(ratio_juv_P[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	}
	
	
png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/RepartitionSpatiale_juv_totaux_ratio.png",width=800, height=1000, units = "px",type="cairo")
par(mfrow=c(4,1))

	#Juv totaux tous secteurs
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,500000),ylab="",main="total 0+ juvenile production (wild + stocked)")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,100000,200000,300000,400000,500000),
				labels=c(
						expression(0),
						expression(paste(100," x ", 10^3)),
						expression(paste(200," x ", 10^3)),
						expression(paste(300," x ", 10^3)),
						expression(paste(400," x ", 10^3)),
						expression(paste(500," x ", 10^3))),
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

########################################################################################################################################
#--------------------- Graphique répartition spatiale des juvéniles totaux avec impact DEVALAISON - RATIO------------------------------#
########################################################################################################################################
dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)

juv_tot=array(rep(0,195000),dim=c(5000,T))#296000
juv_tot_q=array(rep(0,T*5),dim=c(T,5))

river=c(rep(c(0.66, 0.14, 0.2),29),rep(c(0.43, 0.44, 0.13),30))
ratio_river_V<-matrix(river,nrow=3)
interbar=c(rep(c(0, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),11),rep(c(1, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),18),
		rep(c(1, 1,0.0884, 0.281, 0.303, 0.174, 0.395, 0.029, 0.012, 0.06, 0.156),30))
ratio_interbar<-matrix(interbar,nrow=11)
rho_surv=c(0.678, 0.926, 0.916, 0.902, 0.847, 0.886, 0.838, 0.915, 0.907, 0.9, 0.853)

Juv_surv_Vall_3<-array(0,dim=c(5000,T+1))
Juv_surv_Vall_4<-array(0,dim=c(5000,T+1))
Juv_surv_Vala_5<-array(0,dim=c(5000,T+1))
Juv_surv_Vala_6<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_7<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_8<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_9<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_10<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_11<-array(0,dim=c(5000,T+1))
Juv_surv_V<-array(0,dim=c(5000,T+1))
Juv_surv_L<-array(0,dim=c(5000,T+1))
Juv_surv_P<-array(0,dim=c(5000,T+1))

for (t in 2:T){
	Juv_surv_Vall_3[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[1,t]*ratio_interbar[3,t]*(rho_surv[3]*rho_surv[4])
	Juv_surv_Vall_4[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[1,t]*ratio_interbar[4,t]*(rho_surv[3]*rho_surv[4])
	
	Juv_surv_Vala_5[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[2,t]*ratio_interbar[5,t]*(rho_surv[5]*rho_surv[6])
	Juv_surv_Vala_6[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[2,t]*ratio_interbar[6,t]*(rho_surv[5]*rho_surv[6])
	
	Juv_surv_Vdor_7[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[7,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_8[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[8,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_9[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[9,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_10[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[10,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_11[,t] = dmoy_tot_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[11,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	
	Juv_surv_V[,t] = Juv_surv_Vall_3[,t]+Juv_surv_Vall_4[,t]+Juv_surv_Vala_5[,t]+Juv_surv_Vala_6[,t]
					+Juv_surv_Vdor_7[,t]+Juv_surv_Vdor_8[,t]+Juv_surv_Vdor_9[,t]+Juv_surv_Vdor_10[,t]+Juv_surv_Vdor_11[,t]
	Juv_surv_L[,t] = dmoy_tot_L[,t-1] * S_juv_JP[2,t]*ratio_interbar[2,t]*(rho_surv[2]*rho_surv[3]*rho_surv[4])
}

for (t in 13:T){
Juv_surv_P[,t] = dmoy_tot_P[,t-12] * S_juv_JP[3,t]*ratio_interbar[1,t]*(rho_surv[1]*rho_surv[2]*rho_surv[3]*rho_surv[4])
}

for (t in 2:12){
	juv_tot[,t]= Juv_surv_V[,t] + Juv_surv_L[,t]
}
for (t in 13:T){
	juv_tot[,t]= Juv_surv_V[,t] + Juv_surv_L[,t] + Juv_surv_P[,t]
}

for (i in 1:T){
	juv_tot_q[i,]=quantile(juv_tot[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

max(juv_tot_q)#201753.7


##Par secteurs calcul des ratios
juv_V=array(rep(0,195000),dim=c(5000,T))
juv_L=array(rep(0,195000),dim=c(5000,T))
juv_P=array(rep(0,195000),dim=c(5000,T))

ratio_juv_V=array(0,c(5000,T))
ratio_juv_L=array(0,c(5000,T))
ratio_juv_P=array(0,c(5000,T))

ratio_juv_V_q=array(rep(0,T*5),dim=c(T,5))
ratio_juv_L_q=array(rep(0,T*5),dim=c(T,5))
ratio_juv_P_q=array(rep(0,T*5),dim=c(T,5))


for (t in 2:12){
	juv_V[,t]<-Juv_surv_V[,t]
	juv_L[,t]<-Juv_surv_L[,t]
}

for (t in 13:T){
	juv_V[,t]<-Juv_surv_V[,t]
	juv_L[,t]<-Juv_surv_L[,t]
	juv_P[,t]<-Juv_surv_P[,t]
}

for (t in 2:T){
	ratio_juv_V[,t]=juv_V[,t]/juv_tot[,t]
	ratio_juv_L[,t]=juv_L[,t]/juv_tot[,t]
	ratio_juv_P[,t]=juv_P[,t]/juv_tot[,t]
}

for (t in 1:T){
	ratio_juv_V_q[t,]=quantile(ratio_juv_V[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_L_q[t,]=quantile(ratio_juv_L[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_P_q[t,]=quantile(ratio_juv_P[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}


png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/RepartitionSpatiale_juv_totaux_ratio_acDevalaison.png",width=800, height=1000, units = "px",type="cairo")
par(mfrow=c(4,1))

#Juv totaux tous secteurs
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,500000),ylab="",main="total 0+ juvenile production (wild + stocked) with mortalities at the hydropower dams")

# trace l'axe des ordonn�es
axis(2,at = c(0,100000,200000,300000,400000,500000),
		labels=c(
				expression(0),
				expression(paste(100," x ", 10^3)),
				expression(paste(200," x ", 10^3)),
				expression(paste(300," x ", 10^3)),
				expression(paste(400," x ", 10^3)),
				expression(paste(500," x ", 10^3))),
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
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Vichy (ratio total 0+ juvenile production)")

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
#Langeac juv totaux ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Langeac (ratio total 0+ juvenile production)")

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
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Poutes (ratio total 0+ juvenile production)")

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


###########################################################################################################
#--------------------- Graphique répartition spatiale des juvéniles sauvages------------------------------#
###########################################################################################################
juv_wild_V=array(rep(0,195000),dim=c(5000,T))
juv_wild_L=array(rep(0,195000),dim=c(5000,T))
juv_wild_P=array(rep(0,195000),dim=c(5000,T))

juv_wild_V_q=array(rep(0,T*5),dim=c(T,5))
juv_wild_L_q=array(rep(0,T*5),dim=c(T,5))
juv_wild_P_q=array(rep(0,T*5),dim=c(T,5))

for (t in 2:12){
	juv_wild_V[,t]<-dmoy_wild_V[,t-1] * S_juv_JP[1,t]
	juv_wild_L[,t]<-dmoy_wild_L[,t-1] * S_juv_JP[2,t]
}

for (t in 13:T){
	juv_wild_V[,t]<-dmoy_wild_V[,t-1] * S_juv_JP[1,t]
	juv_wild_L[,t]<-dmoy_wild_L[,t-1] * S_juv_JP[2,t]
	juv_wild_P[,t]<-dmoy_wild_P[,t-12] * S_juv_JP[3,t]
}


for (i in 1:T){
	juv_wild_V_q[i,]=quantile(juv_wild_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	juv_wild_L_q[i,]=quantile(juv_wild_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	juv_wild_P_q[i,]=quantile(juv_wild_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

max(juv_wild_V_q)#336430.2
max(juv_wild_L_q)#183536.9
max(juv_wild_P_q)#123462.4

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/RepartitionSpatialeProdJuvTot_WILD.png",width=800, height=800, units = "px",type="cairo")
par(mfrow=c(3,1))
#Vichy
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,400000),ylab="",main="total wild 0+ juvenile production at Vichy")

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
	segments(i-0.15,juv_wild_V_q[i,5],i+0.15,juv_wild_V_q[i,5])
	segments(i,juv_wild_V_q[i,4],i,juv_wild_V_q[i,5])
	#5%
	segments(i-0.15,juv_wild_V_q[i,1],i+0.15,juv_wild_V_q[i,1])
	segments(i,juv_wild_V_q[i,2],i,juv_wild_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_wild_V_q[i,2],juv_wild_V_q[i,2],juv_wild_V_q[i,4],juv_wild_V_q[i,4]),col="grey85")
	#median
	segments(i-0.3,juv_wild_V_q[i,3],i+0.3,juv_wild_V_q[i,3])
}

#Langeac
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,400000),ylab="",main="total wild 0+ juvenile production at Langeac")

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
	segments(i-0.15,juv_wild_L_q[i,5],i+0.15,juv_wild_L_q[i,5])
	segments(i,juv_wild_L_q[i,4],i,juv_wild_L_q[i,5])
	#5%
	segments(i-0.15,juv_wild_L_q[i,1],i+0.15,juv_wild_L_q[i,1])
	segments(i,juv_wild_L_q[i,2],i,juv_wild_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_wild_L_q[i,2],juv_wild_L_q[i,2],juv_wild_L_q[i,4],juv_wild_L_q[i,4]),col="grey85")
	#median
	segments(i-0.3,juv_wild_L_q[i,3],i+0.3,juv_wild_L_q[i,3])
}

#Poutes
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,400000),ylab="",main="total wild 0+ juvenile production at Poutes")

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
	segments(i-0.15,juv_wild_P_q[i,5],i+0.15,juv_wild_P_q[i,5])
	segments(i,juv_wild_P_q[i,4],i,juv_wild_P_q[i,5])
	#5%
	segments(i-0.15,juv_wild_P_q[i,1],i+0.15,juv_wild_P_q[i,1])
	segments(i,juv_wild_P_q[i,2],i,juv_wild_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_wild_P_q[i,2],juv_wild_P_q[i,2],juv_wild_P_q[i,4],juv_wild_P_q[i,4]),col="grey85")
	#median
	segments(i-0.3,juv_wild_P_q[i,3],i+0.3,juv_wild_P_q[i,3])
}

dev.off()


##################################################################################################################
#--------------------- Graphique répartition spatiale des juvéniles sauvages RATIO ------------------------------#
##################################################################################################################
juv_wild_V=array(rep(0,195000),dim=c(5000,T))
juv_wild_L=array(rep(0,195000),dim=c(5000,T))
juv_wild_P=array(rep(0,195000),dim=c(5000,T))
juv_wild_tot=array(rep(0,195000),dim=c(5000,T))
juv_wild_tot_q=array(rep(0,T*5),dim=c(T,5))


for (t in 2:12){
	juv_wild_V[,t]<-dmoy_wild_V[,t-1] * S_juv_JP[1,t]
	juv_wild_L[,t]<-dmoy_wild_L[,t-1] * S_juv_JP[2,t]
}

for (t in 13:T){
	juv_wild_V[,t]<-dmoy_wild_V[,t-1] * S_juv_JP[1,t]
	juv_wild_L[,t]<-dmoy_wild_L[,t-1] * S_juv_JP[2,t]
	juv_wild_P[,t]<-dmoy_wild_P[,t-12] * S_juv_JP[3,t]
}

for (t in 1:T){
	juv_wild_tot[,t]<-juv_wild_V[,t]+juv_wild_L[,t]+juv_wild_P[,t]
}

for (t in 1:T){
	juv_wild_tot_q[t,]=quantile(juv_wild_tot[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

	##Ratio par secteurs
	ratio_juv_wild_V=array(0,c(5000,T))
	ratio_juv_wild_L=array(0,c(5000,T))
	ratio_juv_wild_P=array(0,c(5000,T))
	
	ratio_juv_wild_V_q=array(rep(0,T*5),dim=c(T,5))
	ratio_juv_wild_L_q=array(rep(0,T*5),dim=c(T,5))
	ratio_juv_wild_P_q=array(rep(0,T*5),dim=c(T,5))
	
	
	for (t in 2:T){
		ratio_juv_wild_V[,t]=juv_wild_V[,t]/juv_wild_tot[,t]
		ratio_juv_wild_L[,t]=juv_wild_L[,t]/juv_wild_tot[,t]
		ratio_juv_wild_P[,t]=juv_wild_P[,t]/juv_wild_tot[,t]
	}
	
	for (t in 1:T){
		ratio_juv_wild_V_q[t,]=quantile(ratio_juv_wild_V[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
		ratio_juv_wild_L_q[t,]=quantile(ratio_juv_wild_L[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
		ratio_juv_wild_P_q[t,]=quantile(ratio_juv_wild_P[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	}

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/RepartitionSpatialeProdJuvTot_WILD_ratio_même échelle.png",width=800, height=1000, units = "px",type="cairo")
par(mfrow=c(4,1))
	#Juv tot sauvage
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,500000),ylab="",main="total wild 0+ juvenile production")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,100000,200000,300000,400000,500000),
				labels=c(
						expression(0),
						expression(paste(100," x ", 10^3)),
						expression(paste(200," x ", 10^3)),
						expression(paste(300," x ", 10^3)),
						expression(paste(400," x ", 10^3)),
						expression(paste(500," x ", 10^3))),
				cex.axis = 0.8,las = 1,col = "grey55")
		# trace l'axe des abscisses
		lab1=c(1975,1980,1990,2000,1974+T)
		axis(1,at = c(1,6,16,26,T),
				labels=lab1,
				cex.axis = 0.9,las = 1,col = "grey55")
		
		for(i in 2:T){
			#whiskers
			#95%
			segments(i-0.15,juv_wild_tot_q[i,5],i+0.15,juv_wild_tot_q[i,5])
			segments(i,juv_wild_tot_q[i,4],i,juv_wild_tot_q[i,5])
			#5%
			segments(i-0.15,juv_wild_tot_q[i,1],i+0.15,juv_wild_tot_q[i,1])
			segments(i,juv_wild_tot_q[i,2],i,juv_wild_tot_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_wild_tot_q[i,2],juv_wild_tot_q[i,2],juv_wild_tot_q[i,4],juv_wild_tot_q[i,4]),col="grey85")
			#median
			segments(i-0.3,juv_wild_tot_q[i,3],i+0.3,juv_wild_tot_q[i,3])
		}

	#Vichy ratio
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Vichy (ratio wild 0+ production)")
		
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
			segments(i-0.15,ratio_juv_wild_V_q[i,5],i+0.15,ratio_juv_wild_V_q[i,5])
			segments(i,ratio_juv_wild_V_q[i,4],i,ratio_juv_wild_V_q[i,5])
			#5%
			segments(i-0.15,ratio_juv_wild_V_q[i,1],i+0.15,ratio_juv_wild_V_q[i,1])
			segments(i,ratio_juv_wild_V_q[i,2],i,ratio_juv_wild_V_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_wild_V_q[i,2],ratio_juv_wild_V_q[i,2],ratio_juv_wild_V_q[i,4],ratio_juv_wild_V_q[i,4]),col="grey85")
			#median
			segments(i-0.3,ratio_juv_wild_V_q[i,3],i+0.3,ratio_juv_wild_V_q[i,3])
		}
		abline(h=1,lty=3,col = "grey55")
		
	#Langeac ratio
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Langeac (ratio wild 0+ production)")
		
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
			segments(i-0.15,ratio_juv_wild_L_q[i,5],i+0.15,ratio_juv_wild_L_q[i,5])
			segments(i,ratio_juv_wild_L_q[i,4],i,ratio_juv_wild_L_q[i,5])
			#5%
			segments(i-0.15,ratio_juv_wild_L_q[i,1],i+0.15,ratio_juv_wild_L_q[i,1])
			segments(i,ratio_juv_wild_L_q[i,2],i,ratio_juv_wild_L_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_wild_L_q[i,2],ratio_juv_wild_L_q[i,2],ratio_juv_wild_L_q[i,4],ratio_juv_wild_L_q[i,4]),col="grey85")
			#median
			segments(i-0.3,ratio_juv_wild_L_q[i,3],i+0.3,ratio_juv_wild_L_q[i,3])
		}
		abline(h=1,lty=3,col = "grey55")
	#Poutes ratio
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Poutes (ratio wild 0+ production)")
		
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
			segments(i-0.15,ratio_juv_wild_P_q[i,5],i+0.15,ratio_juv_wild_P_q[i,5])
			segments(i,ratio_juv_wild_P_q[i,4],i,ratio_juv_wild_P_q[i,5])
			#5%
			segments(i-0.15,ratio_juv_wild_P_q[i,1],i+0.15,ratio_juv_wild_P_q[i,1])
			segments(i,ratio_juv_wild_P_q[i,2],i,ratio_juv_wild_P_q[i,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_wild_P_q[i,2],ratio_juv_wild_P_q[i,2],ratio_juv_wild_P_q[i,4],ratio_juv_wild_P_q[i,4]),col="grey85")
			#median
			segments(i-0.3,ratio_juv_wild_P_q[i,3],i+0.3,ratio_juv_wild_P_q[i,3])
		}
		abline(h=1,lty=3,col = "grey55")

dev.off()

#############################################################################################################################
#--------------------- Graphique répartition spatiale des juvéniles sauvages RATIO DEVALAISON ------------------------------#
#############################################################################################################################
juv_wild_V=array(rep(0,195000),dim=c(5000,T))
juv_wild_L=array(rep(0,195000),dim=c(5000,T))
juv_wild_P=array(rep(0,195000),dim=c(5000,T))
juv_wild_tot=array(rep(0,195000),dim=c(5000,T))
juv_wild_tot_q=array(rep(0,T*5),dim=c(T,5))

rho_surv=c(0.678, 0.926, 0.916, 0.902, 0.847, 0.886, 0.838, 0.915, 0.907, 0.9, 0.853)
ratio_interbar=c(rep(c(0,1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),11),rep(c(1, 1, 0.0884, 0.281, 0, 0, 0.395, 0.029, 0.012, 0.06, 0.156),18),rep(c( 1, 1,0.0884, 0.281, 0.303, 0.174, 0.395, 0.029, 0.012, 0.06, 0.156),10))
ratio_interbar<-matrix(ratio_interbar,nrow=11)
ratio_river_V=c(rep(c(0.66, 0.14, 0.2),29),rep(c(0.43, 0.44, 0.13),11))
ratio_river_V<-matrix(ratio_river_V,nrow=3)

Juv_surv_Vall_3<-array(0,dim=c(5000,T+1))
Juv_surv_Vall_4<-array(0,dim=c(5000,T+1))
Juv_surv_Vala_5<-array(0,dim=c(5000,T+1))
Juv_surv_Vala_6<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_7<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_8<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_9<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_10<-array(0,dim=c(5000,T+1))
Juv_surv_Vdor_11<-array(0,dim=c(5000,T+1))
Juv_surv_V<-array(0,dim=c(5000,T+1))
Juv_surv_L<-array(0,dim=c(5000,T+1))
Juv_surv_P<-array(0,dim=c(5000,T+1))

for (t in 2:T){
	Juv_surv_Vall_3[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[1,t]*ratio_interbar[3,t]*(rho_surv[3]*rho_surv[4])
	Juv_surv_Vall_4[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[1,t]*ratio_interbar[4,t]*(rho_surv[3]*rho_surv[4])
	
	Juv_surv_Vala_5[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[2,t]*ratio_interbar[5,t]*(rho_surv[5]*rho_surv[6])
	Juv_surv_Vala_6[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[2,t]*ratio_interbar[6,t]*(rho_surv[5]*rho_surv[6])
	
	Juv_surv_Vdor_7[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[7,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_8[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[8,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_9[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[9,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_10[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[10,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	Juv_surv_Vdor_11[,t] = dmoy_wild_V[,t-1] * S_juv_JP[1,t]*ratio_river_V[3,t]*ratio_interbar[11,t]*(rho_surv[7]*rho_surv[8]*rho_surv[9]*rho_surv[10]*rho_surv[11])
	
	Juv_surv_V[,t] = Juv_surv_Vall_3[,t]+Juv_surv_Vall_4[,t]+Juv_surv_Vala_5[,t]+Juv_surv_Vala_6[,t]
	+Juv_surv_Vdor_7[,t]+Juv_surv_Vdor_8[,t]+Juv_surv_Vdor_9[,t]+Juv_surv_Vdor_10[,t]+Juv_surv_Vdor_11[,t]
	Juv_surv_L[,t] = dmoy_wild_L[,t-1] * S_juv_JP[2,t]*ratio_interbar[2,t]*(rho_surv[2]*rho_surv[3]*rho_surv[4])
	Juv_surv_P[,t] = dmoy_wild_P[,t-1] * S_juv_JP[3,t]*ratio_interbar[1,t]*(rho_surv[1]*rho_surv[2]*rho_surv[3]*rho_surv[4])
}



for (t in 2:12){
	juv_wild_V[,t]<-Juv_surv_V[,t]
	juv_wild_L[,t]<-Juv_surv_L[,t]
}

for (t in 13:T){
	juv_wild_V[,t]<-Juv_surv_V[,t]
	juv_wild_L[,t]<-Juv_surv_L[,t]
	juv_wild_P[,t]<-Juv_surv_P[,t]
}

for (t in 1:T){
	juv_wild_tot[,t]<-juv_wild_V[,t]+juv_wild_L[,t]+juv_wild_P[,t]
}

for (t in 1:T){
	juv_wild_tot_q[t,]=quantile(juv_wild_tot[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

##Ratio par secteurs
ratio_juv_wild_V=array(0,c(5000,T))
ratio_juv_wild_L=array(0,c(5000,T))
ratio_juv_wild_P=array(0,c(5000,T))

ratio_juv_wild_V_q=array(rep(0,T*5),dim=c(T,5))
ratio_juv_wild_L_q=array(rep(0,T*5),dim=c(T,5))
ratio_juv_wild_P_q=array(rep(0,T*5),dim=c(T,5))


for (t in 2:T){
	ratio_juv_wild_V[,t]=juv_wild_V[,t]/juv_wild_tot[,t]
	ratio_juv_wild_L[,t]=juv_wild_L[,t]/juv_wild_tot[,t]
	ratio_juv_wild_P[,t]=juv_wild_P[,t]/juv_wild_tot[,t]
}

for (t in 1:T){
	ratio_juv_wild_V_q[t,]=quantile(ratio_juv_wild_V[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_wild_L_q[t,]=quantile(ratio_juv_wild_L[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	ratio_juv_wild_P_q[t,]=quantile(ratio_juv_wild_P[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/RepartitionSpatialeProdJuvTot_WILD_ratio_acDevalaison.png",width=800, height=1000, units = "px",type="cairo")
par(mfrow=c(4,1))
#Juv tot sauvage
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,500000),ylab="",main="total wild 0+ juvenile production with mortalities at the hydropower dams")

# trace l'axe des ordonn�es
axis(2,at = c(0,100000,200000,300000,400000,500000),
		labels=c(
				expression(0),
				expression(paste(100," x ", 10^3)),
				expression(paste(200," x ", 10^3)),
				expression(paste(300," x ", 10^3)),
				expression(paste(400," x ", 10^3)),
				expression(paste(500," x ", 10^3))),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,juv_wild_tot_q[i,5],i+0.15,juv_wild_tot_q[i,5])
	segments(i,juv_wild_tot_q[i,4],i,juv_wild_tot_q[i,5])
	#5%
	segments(i-0.15,juv_wild_tot_q[i,1],i+0.15,juv_wild_tot_q[i,1])
	segments(i,juv_wild_tot_q[i,2],i,juv_wild_tot_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(juv_wild_tot_q[i,2],juv_wild_tot_q[i,2],juv_wild_tot_q[i,4],juv_wild_tot_q[i,4]),col="grey85")
	#median
	segments(i-0.3,juv_wild_tot_q[i,3],i+0.3,juv_wild_tot_q[i,3])
}

#Vichy ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Vichy (ratio wild 0+ production)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_wild_V_q[i,5],i+0.15,ratio_juv_wild_V_q[i,5])
	segments(i,ratio_juv_wild_V_q[i,4],i,ratio_juv_wild_V_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_wild_V_q[i,1],i+0.15,ratio_juv_wild_V_q[i,1])
	segments(i,ratio_juv_wild_V_q[i,2],i,ratio_juv_wild_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_wild_V_q[i,2],ratio_juv_wild_V_q[i,2],ratio_juv_wild_V_q[i,4],ratio_juv_wild_V_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_juv_wild_V_q[i,3],i+0.3,ratio_juv_wild_V_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

#Langeac ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Langeac (ratio wild 0+ production)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_wild_L_q[i,5],i+0.15,ratio_juv_wild_L_q[i,5])
	segments(i,ratio_juv_wild_L_q[i,4],i,ratio_juv_wild_L_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_wild_L_q[i,1],i+0.15,ratio_juv_wild_L_q[i,1])
	segments(i,ratio_juv_wild_L_q[i,2],i,ratio_juv_wild_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_wild_L_q[i,2],ratio_juv_wild_L_q[i,2],ratio_juv_wild_L_q[i,4],ratio_juv_wild_L_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_juv_wild_L_q[i,3],i+0.3,ratio_juv_wild_L_q[i,3])
}
abline(h=1,lty=3,col = "grey55")
#Poutes ratio
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab="",main="Poutes (ratio wild 0+ production)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.8,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 2:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_wild_P_q[i,5],i+0.15,ratio_juv_wild_P_q[i,5])
	segments(i,ratio_juv_wild_P_q[i,4],i,ratio_juv_wild_P_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_wild_P_q[i,1],i+0.15,ratio_juv_wild_P_q[i,1])
	segments(i,ratio_juv_wild_P_q[i,2],i,ratio_juv_wild_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_wild_P_q[i,2],ratio_juv_wild_P_q[i,2],ratio_juv_wild_P_q[i,4],ratio_juv_wild_P_q[i,4]),col="grey85")
	#median
	segments(i-0.3,ratio_juv_wild_P_q[i,3],i+0.3,ratio_juv_wild_P_q[i,3])
}
abline(h=1,lty=3,col = "grey55")

dev.off()


###############################################################################################
#--------------------------- Géniteurs totaux + ratio par secteur ----------------------------#
###############################################################################################

	#---------- Sans projection --------------#
S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")

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
		53,39,14,26,118,59,45,57)

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
		
	}
}
for (t in 12:T){
	for( i in 1:5000){
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


for (i in 1:T){
	S_vichy_q[i,]=quantile(S_vichy_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	S_langeac_q[i,]=quantile(S_langeac_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

S_stocking=c(
		0,0,14,34,44,
		52,48,64,38,5,
		40,10,20,9,5,
		8,10,16,4,0,
		0,15,8,3,20,
		20,25,40,103,63,
		47,27,17,39,49,
		14,50,50,50,24)

C_dwn=c(
		420,439,77,124,190,
		318,819,388,169,286,
		438,614,385,731,260,
		196,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0)

C_up=c(	1190,700,315,220,200,
		1280,514,1163,410,314,
		807,72,91,425,140,
		88,135,110,112,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0,
		0,0,0,0,0)

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
data_vichy=c(
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,NA,NA,NA,
		NA,NA,393,267,515,
		380,400,541,1238,NA,#662,
		510,950,572,421,491,
		227,755,861,819,595)

for (t in 24:T){
	S_tot_data[t]=max(data_vichy[t]-S_stocking[t],1)
}

for (t in 1:T){
	S_tot_q[t,]=quantile(S_tot[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/simulation/2015_12_04_standard/RepartitionSpatialeGéniteurs_ratio.png",width=800, height=1000, units = "px",type="cairo")
par(mfrow=c(4,1))
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



	#---------- Avec projection --------------#
#Récupération des caluls réalisés dans le cadre de la modélisation sans déversement sur le modèle standard
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2015_12_04_Standard_ProjectionSansRepeuplement_2015.12.08.RData")






