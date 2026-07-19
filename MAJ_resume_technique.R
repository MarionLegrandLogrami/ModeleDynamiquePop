# TODO: Add comment
#  SCRIPT de mise à jour des infos contenues dans le résumé technique de Guillaume Dauphin de 2012
# Author: marion.legrand
####################################################################################################

#Modèle 2017.08.29_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2016
setwd("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
#Modèle 2019.12.12_4zones_Interaction_ss_rho_poutes_MatriceVC_Maj2018
setwd("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2019_12_12/")
datawd<-("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2019_12_12/")

library(coda)
library(boot)

T=44

## On charge le RData qu'on a généré à la fin de la partie init du script CompareModel_interaction car dedans on a normalement tout pour faire tourner les figures du rapport
#load(file = "C:/Users/marion.legrand/workspace/ModeleDynamiquePop/2017_08_29_4zones_Interaction_matriceVC_maj2016_CopyOfCompare.RData")
##dans ce Rdata on a des paramètres "_un" qui ne nous intéressent pas puisque le modèle qu'on considère est le modèle 2 (dans la CompareModel_interaction)
##on enlève tous les paramètres finissant par "_un"
load(file = "C:/Users/marion.legrand/workspace/ModeleDynamiquePop/2019_12_12_4zones_Interaction_matriceVC_maj2018_CopyOfCompare.RData")
print(grep("*._un",ls()))	
rm(list = grep("*._un",ls(),value = TRUE))


###########################################
# 1.3. L’ajustement du modèle aux données #
###########################################

#-----------------------------------------------------------------------#
# graph densité dépendance --> fig. 4.8 du rapport Dauphin&Prévost 2012 #
#-----------------------------------------------------------------------#

##............
## figure
##............
par(mfrow=c(2,1),mar=c(4,5.5,2,1.5))
#Graph densite dependence juvenile sauvage
	plot(1,1,type="n",axes=FALSE,xlim=c(0,0.0025),xlab=expression(paste(S," ",(spawners.m^-2))),ylim=c(0,0.35),ylab=expression(paste(d^wild," (0+",.m^-2,")")),main="wild 0+ density dependence relationship",cex.main=1.5,cex.lab=1.8)
	# trace l'axe des ordonnées
	axis(1,at = c(0,0.00050,0.0010,0.0015, 0.002, 0.0025),
			labels=c(0,expression(paste(0.5," x ",10^-3)),expression(10^-3),expression(paste(1.5," x ",10^-3)),expression(paste(2," x ",10^-3)),expression(paste(2.5," x ",10^-3)) ),
			cex.axis = 1.2,las = 1,lwd=2)
	# trace l'axe des abscisses
	axis(2,at = c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35),
			labels=c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35),
			cex.axis = 1.2,las = 1,lwd=2)
	points(S_vichy_q_deux[1:(T-1),3]/S_juv_JP_new[1,1:(T-1)],dmoy_wild_V_q_deux[2:T,3],pch=16,col="coral3")#"gray15")
	points(S_alagnon_q_deux[1:(T-1),3]/S_juv_JP_new[2,1:(T-1)],dmoy_wild_A_q_deux[2:T,3],pch=16,col="blue")#"gray15")
	points(S_langeac_q_deux[1:(T-1),3]/S_juv_JP_new[3,1:(T-1)],dmoy_wild_L_q_deux[2:T,3],pch=16,col="darkolivegreen4")#"gray45")
	points(S_poutes_counter_deux[12:(T-1)]/S_juv_JP_new[4,12:(T-1)],dmoy_wild_P_q_deux[13:T,3],pch=16,col="darkolivegreen3")#"gray75")
	x=seq(0,0.0025,0.0000001)
	y=exp(log(x/(1/median(a_deux) + x * 1/median(Rmax_deux))) + median(nu_d_deux[,1]))
	points(x,y,type="l",col="coral3")#"gray15")
	y=exp(log(x/(1/median(a_deux) + x * 1/median(Rmax_deux))) + median(nu_d_deux[,2]))
	points(x,y,type="l",col="darkolivegreen4")#"gray45")
	text(0.0025,0.35,labels=expression(italic("a.")),col = "grey55",cex=1.5)
#Graphe densite dependence juvenile repeuplement
	plot(1,1,type="n",axes=FALSE,xlim=c(0,1.4),xlab=expression(paste(Stock^juv," ",(0+.m^-2))),ylim=c(0,0.35),ylab=expression(paste(d^juv," (0+",.m^-2,")")),main="stocked 0+ density dependence relationship",cex.main=1.5,cex.lab=1.8)
	# trace l'axe des abcisses
	axis(1,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4),
			labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4),
			cex.axis = 1.2,las = 1)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.05,0.10,0.15,0.2,0.25,0.3,0.35),
			labels=c(0,0.05,0.10,0.15,0.20,0.25,0.3,0.35),
			cex.axis = 1.2,las = 1)
	points(stocked_juv_V_d_deux[I_juv_V_deux],dmoy_juv_V_q_deux[I_juv_V_deux,3],pch=15,col="coral3")
	points(stocked_juv_A_d_deux[I_juv_A_deux],dmoy_juv_A_q_deux[I_juv_A_deux,3],pch=15,col="blue")
	points(stocked_juv_L_d_deux[I_juv_L_deux],dmoy_juv_L_q_deux[I_juv_L_deux,3],pch=15,col="darkolivegreen4")
	points(stocked_juv_P_d_deux[I_juv_P_deux],dmoy_juv_P_q_deux[I_juv_P_deux,3],pch=15,col="darkolivegreen3")
	x=seq(0,0.35,0.0001)
	points(x,x,type="l",lty=2,col="grey15")
	x=seq(0,1.4,0.0001)
	y=  x / ( ( 1 / exp(median(nu_d_deux[,1]))) * 1/median(a_juv_deux) + x * 1/median(Rmax_deux)  )   
	points(x,y,type="l",col="coral3")
	y=  x / ( ( 1 / exp(median(nu_d_deux[,2]))) * 1/median(a_juv_deux) + x * 1/median(Rmax_deux)  )   
	points(x,y,type="l",col="darkolivegreen4")
	text(1.4,0.35,labels=expression(italic("b.")),col = "grey55",cex=1.5)

##Comparaison moyenne recrutement naturel
y_avalLangeac=exp(log(x/(1/median(a_deux) + x * 1/median(Rmax_deux))) + median(nu_d_deux[,1]))
y_amontLangeac=exp(log(x/(1/median(a_deux) + x * 1/median(Rmax_deux))) + median(nu_d_deux[,2]))
mean(y_amontLangeac)/mean(y_avalLangeac) #2.653554

##Comparaison moyenne juv déversé
y_avalLangeac_dev=  x / ( ( 1 / exp(median(nu_d_deux[,1]))) * 1/median(a_juv_deux) + x * 1/median(Rmax_deux)  )   
y_amontLangeac_dev=  x / ( ( 1 / exp(median(nu_d_deux[,2]))) * 1/median(a_juv_deux) + x * 1/median(Rmax_deux)  )   
mean(y_amontLangeac_dev)/mean(y_avalLangeac_dev)#1.405766


############################
# 2. Analyse rétrospective #
############################

#----------------------------------------------------------------------------------#
# graph Géniteurs potentiels et ratio --> fig. 4.2 du rapport Dauphin&Prévost 2012 #
#----------------------------------------------------------------------------------#
par(mfrow=c(4,2),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
## Vichy
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,6500),ylab=expression(italic(S["t,1"])),main="Vichy-Langeac",cex.main=2,cex.lab=1.5)
	# trace l'axe des ordonnées
	axis(2,at = c(0,1000,2000,3000,4000,5000,6000),labels=c(0,1000,2000,3000,4000,5000,6000),cex.axis = 1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.4,las = 1,col = "grey55")
	text(T,6500,labels=expression(italic("a.")),col = "grey55")
	for(i in 3:T){
		#whiskers
		#95%
		segments(i-0.15,S_vichy_q_deux[i,5],i+0.15,S_vichy_q_deux[i,5])
		segments(i,S_vichy_q_deux[i,4],i,S_vichy_q_deux[i,5])
		#5%
		segments(i-0.15,S_vichy_q_deux[i,1],i+0.15,S_vichy_q_deux[i,1])
		segments(i,S_vichy_q_deux[i,2],i,S_vichy_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_vichy_q_deux[i,2],S_vichy_q_deux[i,2],S_vichy_q_deux[i,4],S_vichy_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,S_vichy_q_deux[i,3],i+0.3,S_vichy_q_deux[i,3])
	}
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Vichy-Langeac",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.4,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("e.")),col = "grey55")
	for(i in 3:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_V_q_deux[i,5],i+0.15,ratio_S_V_q_deux[i,5])
		segments(i,ratio_S_V_q_deux[i,4],i,ratio_S_V_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_V_q_deux[i,1],i+0.15,ratio_S_V_q_deux[i,1])
		segments(i,ratio_S_V_q_deux[i,2],i,ratio_S_V_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_V_q_deux[i,2],ratio_S_V_q_deux[i,2],ratio_S_V_q_deux[i,4],ratio_S_V_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,ratio_S_V_q_deux[i,3],i+0.3,ratio_S_V_q_deux[i,3])
	}
	segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)	
	segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	
	segments(0,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=11.5,lty=3)
## Alagnon
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,800),ylab=expression(italic(S["t,1"])),main="Alagnon",cex.main=2,cex.lab=1.5)
	# trace l'axe des ordonnées
	axis(2,at = c(0,200,400,600,800),labels=c(0,200,400,600,800),cex.axis = 1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.4,las = 1,col = "grey55")
	text(T,800,labels=expression(italic("b.")),col = "grey55")
	for(i in 3:22){
		#whiskers
		#95%
		segments(i-0.15,S_alagnon_q_deux[i,5],i+0.15,S_alagnon_q_deux[i,5])
		segments(i,S_alagnon_q_deux[i,4],i,S_alagnon_q_deux[i,5])
		#5%
		segments(i-0.15,S_alagnon_q_deux[i,1],i+0.15,S_alagnon_q_deux[i,1])
		segments(i,S_alagnon_q_deux[i,2],i,S_alagnon_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_alagnon_q_deux[i,2],S_alagnon_q_deux[i,2],S_alagnon_q_deux[i,4],S_alagnon_q_deux[i,4]),col="gray")
		#median
		segments(i-0.3,S_alagnon_q_deux[i,3],i+0.3,S_alagnon_q_deux[i,3])
	}
	for(i in 23:T){
		#whiskers
		#95%
		segments(i-0.15,S_alagnon_q_deux[i,5],i+0.15,S_alagnon_q_deux[i,5])
		segments(i,S_alagnon_q_deux[i,4],i,S_alagnon_q_deux[i,5])
		#5%
		segments(i-0.15,S_alagnon_q_deux[i,1],i+0.15,S_alagnon_q_deux[i,1])
		segments(i,S_alagnon_q_deux[i,2],i,S_alagnon_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_alagnon_q_deux[i,2],S_alagnon_q_deux[i,2],S_alagnon_q_deux[i,4],S_alagnon_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,S_alagnon_q_deux[i,3],i+0.3,S_alagnon_q_deux[i,3])
	}
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Alagnon",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.4,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("f.")),col = "grey55")
	for(i in 3:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_A_q_deux[i,5],i+0.15,ratio_S_A_q_deux[i,5])
		segments(i,ratio_S_A_q_deux[i,4],i,ratio_S_A_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_A_q_deux[i,1],i+0.15,ratio_S_A_q_deux[i,1])
		segments(i,ratio_S_A_q_deux[i,2],i,ratio_S_A_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_A_q_deux[i,2],ratio_S_A_q_deux[i,2],ratio_S_A_q_deux[i,4],ratio_S_A_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,ratio_S_A_q_deux[i,3],i+0.3,ratio_S_A_q_deux[i,3])
	}
	segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)		
	segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	
	segments(0,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=11.5,lty=3)
## Langeac
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1200),ylab=expression(italic(S["t,2"])),main="Langeac-Poutes",cex.main=2,cex.lab=1.5)
	# trace l'axe des ordonnées
	axis(2,at = c(0,300,600,900,1200),labels=c(0,300,600,900,1200),cex.axis = 1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.4,las = 1,col = "grey55")
	text(T,1200,labels=expression(italic("c.")),col = "grey55")
	for(i in 3:T){
		#whiskers
		#95%
		segments(i-0.15,S_langeac_q_deux[i,5],i+0.15,S_langeac_q_deux[i,5])
		segments(i,S_langeac_q_deux[i,4],i,S_langeac_q_deux[i,5])
		#5%
		segments(i-0.15,S_langeac_q_deux[i,1],i+0.15,S_langeac_q_deux[i,1])
		segments(i,S_langeac_q_deux[i,2],i,S_langeac_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_langeac_q_deux[i,2],S_langeac_q_deux[i,2],S_langeac_q_deux[i,4],S_langeac_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,S_langeac_q_deux[i,3],i+0.3,S_langeac_q_deux[i,3])
	}
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Poutes")
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis =1.4,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("g.")),col = "grey55")
	for(i in 3:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_L_q_deux[i,5],i+0.15,ratio_S_L_q_deux[i,5])
		segments(i,ratio_S_L_q_deux[i,4],i,ratio_S_L_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_L_q_deux[i,1],i+0.15,ratio_S_L_q_deux[i,1])
		segments(i,ratio_S_L_q_deux[i,2],i,ratio_S_L_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_q_deux[i,2],ratio_S_L_q_deux[i,2],ratio_S_L_q_deux[i,4],ratio_S_L_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,ratio_S_L_q_deux[i,3],i+0.3,ratio_S_L_q_deux[i,3])
	}
	segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)		
	segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	
	segments(0,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=11.5,lty=3)
## Poutes
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,200),ylab=expression(italic(S["t,3"])),main="Upstream Poutes",cex.main=2,cex.lab=1.5)
	# trace l'axe des ordonnées
	axis(2,at = c(0,100,200),labels=c(0,100,200),cex.axis = 1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.4,las = 1,col = "grey55")
	points(x=seq(12,T,1),y=S_poutes_counter_deux[12:T],pch=16,col="darkolivegreen3") 
	abline(v=11.5,lty=2)
	text(6,100,paste( "upstream Poutes\n inaccessible"),col = "grey55")
	text(T,200,labels=expression(italic("d.")),col = "grey55")
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Poutes",cex.main=2,cex.lab=1.5)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis =1.4,las = 1,col = "grey55")
	# trace l'axe des abscisses
	axis(1,at = c(1,6,16,26,T),
			labels=c(1975,1980,1990,2000,1974+T),
			cex.axis =1.4,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("h.")),col = "grey55")
	text(6,0.55,paste( "Amont Poutes\n inaccessible"),col = "grey55")
	for(i in 12:22){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_P_q_deux[i,5],i+0.15,ratio_S_P_q_deux[i,5])
		segments(i,ratio_S_P_q_deux[i,4],i,ratio_S_P_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_P_q_deux[i,1],i+0.15,ratio_S_P_q_deux[i,1])
		segments(i,ratio_S_P_q_deux[i,2],i,ratio_S_P_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_P_q_deux[i,2],ratio_S_P_q_deux[i,2],ratio_S_P_q_deux[i,4],ratio_S_P_q_deux[i,4]),col="darkolivegreen3")
		#median
		segments(i-0.3,ratio_S_P_q_deux[i,3],i+0.3,ratio_S_P_q_deux[i,3])
	}
	x=seq(23,T,1)
	points(x,ratio_S_P_q_deux[23:T,3],pch=16,col="darkolivegreen3")
	segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)		
	segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	
	segments(11.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=11.5,lty=3)

# proportion de géniteurs potentiels sur l'Allier en aval de Langeac avant et après la réouverture de Poutès
mean(ratio_S_V_q_deux[3:11,3])
mean(ratio_S_V_q_deux[12:T,3])
# proportion de géniteurs potentiels dans l'Alagnon avant et après la réouverture de Poutès
mean(ratio_S_A_q_deux[3:11,3])
mean(ratio_S_A_q_deux[12:T,3])
# proportion de géniteurs potentiels dans l'Allier entre Langeac et Poutès avant et après la réouverture de Poutès
mean(ratio_S_L_q_deux[3:11,3])
mean(ratio_S_L_q_deux[12:T,3])
# proportion de géniteurs potentiels dans l'Allier en amont de Poutès après la réouverture de Poutès
mean(ratio_S_P_q_deux[12:T,3])

#---------------------------------------------------------------------------------------------------------------------------------------#
# graph Ratio entre les densités issus des juvéniles déversés et les densités totales --> fig. 4.5 à4.7 du rapport Dauphin&Prévost 2012 #
#---------------------------------------------------------------------------------------------------------------------------------------#

par(mfrow=c(4,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
## Vichy-Langeac
	#juveniles
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Vichy-Langeac (ratio stocked juv/total)",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	x=which(I_juv_moy[2:T,1]==1)
	ratio_V_juv_deux=array(rep(0,T*5000),dim=c(5000,T))
	ratio_V_q_juv_deux=array(rep(0,T*5),dim=c(T,5))
	mean(ratio_V_q_juv_deux[(T-9):T,3])
	for(i in 1:30){
		ratio_V_juv_deux[,i]=stocking_juv_V_deux[,i]/dmoy_tot_V_deux[,i]
	}
	for(i in 30:(T-1)){
		ratio_V_juv_deux[,i]=stocking_juv_V_deux[,i]*S_juv_JP_dev[i+1,1]/S_juv_JP_new[1,i+1]/dmoy_tot_V_deux[,i]
	}
	for (i in 1:(T-1)){
		ratio_V_q_juv_deux[i,]=quantile(ratio_V_juv_deux[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	}
	for(i in 1:length(x)){
		#whiskers
		#95%
		segments(x[i]-0.15,ratio_V_q_juv_deux[x[i],5],x[i]+0.15,ratio_V_q_juv_deux[x[i],5])
		segments(x[i],ratio_V_q_juv_deux[x[i],4],x[i],ratio_V_q_juv_deux[x[i],5])
		#5%
		segments(x[i]-0.15,ratio_V_q_juv_deux[x[i],1],x[i]+0.15,ratio_V_q_juv_deux[x[i],1])
		segments(x[i],ratio_V_q_juv_deux[x[i],2],x[i],ratio_V_q_juv_deux[x[i],1])
		#boxplot
		polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_V_q_juv_deux[x[i],2],ratio_V_q_juv_deux[x[i],2],ratio_V_q_juv_deux[x[i],4],ratio_V_q_juv_deux[x[i],4]),col="coral3")
		#median
		segments(x[i]-0.3,ratio_V_q_juv_deux[x[i],3],x[i]+0.3,ratio_V_q_juv_deux[x[i],3])
	}
	text(T-1+0.5,1,labels=expression(italic("a.")),col = "grey55",cex=2)
	x=which(I_juv_moy[2:T,1]==0)
	points(x,y=rep(0,length(x)),pch=21,bg="bisque")
## Alagnon
	#juveniles
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Alagnon (ratio stocked juv/total)",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	x=which(I_juv_moy[2:T,2]==1)
	ratio_A_juv_deux=array(rep(0,T*5000),dim=c(5000,T))
	ratio_A_q_juv_deux=array(rep(0,T*5),dim=c(T,5))
	mean(ratio_A_q_juv_deux[(T-9):T,3])
	for(i in 1:30){
		ratio_A_juv_deux[,i]=stocking_juv_A_deux[,i]/dmoy_tot_A_deux[,i]
	}
	for(i in 30:(T-1)){
		ratio_A_juv_deux[,i]=stocking_juv_A_deux[,i]*S_juv_JP_dev[i+1,2]/S_juv_JP_new[2,i+1]/dmoy_tot_A_deux[,i]
	}
	for (i in 1:(T-1)){
		ratio_A_q_juv_deux[i,]=quantile(ratio_A_juv_deux[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	}
	for(i in 1:length(x)){
		#whiskers
		#95%
		segments(x[i]-0.15,ratio_A_q_juv_deux[x[i],5],x[i]+0.15,ratio_A_q_juv_deux[x[i],5])
		segments(x[i],ratio_A_q_juv_deux[x[i],4],x[i],ratio_A_q_juv_deux[x[i],5])
		#5%
		segments(x[i]-0.15,ratio_A_q_juv_deux[x[i],1],x[i]+0.15,ratio_A_q_juv_deux[x[i],1])
		segments(x[i],ratio_A_q_juv_deux[x[i],2],x[i],ratio_A_q_juv_deux[x[i],1])
		#boxplot
		polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_A_q_juv_deux[x[i],2],ratio_A_q_juv_deux[x[i],2],ratio_A_q_juv_deux[x[i],4],ratio_A_q_juv_deux[x[i],4]),col="darkolivegreen4")
		#median
		segments(x[i]-0.3,ratio_A_q_juv_deux[x[i],3],x[i]+0.3,ratio_A_q_juv_deux[x[i],3])
	}
	text(T+0.5,0.8,labels=expression(italic("b.")),col = "grey55",cex=2)
	x=which(I_juv_moy[2:T,2]==0)
	points(x,y=rep(0,length(x)),pch=21,bg="bisque")
##Langeac-Poutès
	#juveniles
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Langeac-Poutes (ratio stocked juv/total)",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	x=which(I_juv_moy[2:T,3]==1)
	ratio_L_juv_deux=array(0,dim=c(5000,T))
	ratio_L_q_juv_deux=array(0,dim=c(T,5))
	for(i in 1:30){
		ratio_L_juv_deux[,i]=stocking_juv_L_deux[,i]/dmoy_tot_L_deux[,i]
	}
	for(i in 30:(T-1)){
		ratio_L_juv_deux[,i]=stocking_juv_L_deux[,i]*S_juv_JP_dev[i+1,3]/S_juv_JP_new[3,i+1]/dmoy_tot_L_deux[,i]
	}
	for (i in 1:(T-1)){
		ratio_L_q_juv_deux[i,]=quantile(ratio_L_juv_deux[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	}
	for(i in 1:length(x)){
		#whiskers
		#95%
		segments(x[i]-0.15,ratio_L_q_juv_deux[x[i],5],x[i]+0.15,ratio_L_q_juv_deux[x[i],5])
		segments(x[i],ratio_L_q_juv_deux[x[i],4],x[i],ratio_L_q_juv_deux[x[i],5])
		#5%
		segments(x[i]-0.15,ratio_L_q_juv_deux[x[i],1],x[i]+0.15,ratio_L_q_juv_deux[x[i],1])
		segments(x[i],ratio_L_q_juv_deux[x[i],2],x[i],ratio_L_q_juv_deux[x[i],1])
		#boxplot
		polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_L_q_juv_deux[x[i],2],ratio_L_q_juv_deux[x[i],2],ratio_L_q_juv_deux[x[i],4],ratio_L_q_juv_deux[x[i],4]),col="darkolivegreen4")
		#median
		segments(x[i]-0.3,ratio_L_q_juv_deux[x[i],3],x[i]+0.3,ratio_L_q_juv_deux[x[i],3])
	}
	text(T+0.5,0.8,labels=expression(italic("c.")),col = "grey55",cex=2)
	x=which(I_juv_moy[2:T,3]==0)
	points(x,y=rep(0,length(x)),pch=21,bg="olivedrab2")
## Amont Poutès
	#juveniles
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Upstream Poutes (ratio stocked juv/total)",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	x=which(I_juv_moy[2:T,4]==1)[c(-1,-2)]
	abline(v=10,lty=2)
	ratio_P_deux=array(rep(0,T*5000),dim=c(5000,T))
	ratio_P_q_deux=array(rep(0,T*5),dim=c(T,5))
	mean(ratio_P_q_deux[(T-9):T,3])
	for(i in 1:(30-12)){
		ratio_P_deux[,(i+11)]=stocking_juv_P_deux[,i]/dmoy_tot_P_deux[,i]
	}
	for(i in (31-12):(T-12)){
		ratio_P_deux[,(i+11)]=stocking_juv_P_deux[,i]*S_juv_JP_dev[i+12,4]/S_juv_JP_new[4,i+12]/dmoy_tot_P_deux[,i]
	}
	for (i in 1:(T-1)){
		ratio_P_q_deux[i,]=quantile(ratio_P_deux[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	}
	for(i in 1:length(x)){
		#whiskers
		#95%
		segments(x[i]-0.15,ratio_P_q_deux[x[i],5],x[i]+0.15,ratio_P_q_deux[x[i],5])
		segments(x[i],ratio_P_q_deux[x[i],4],x[i],ratio_P_q_deux[x[i],5])
		#5%
		segments(x[i]-0.15,ratio_P_q_deux[x[i],1],x[i]+0.15,ratio_P_q_deux[x[i],1])
		segments(x[i],ratio_P_q_deux[x[i],2],x[i],ratio_P_q_deux[x[i],1])
		#boxplot
		polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_P_q_deux[x[i],2],ratio_P_q_deux[x[i],2],ratio_P_q_deux[x[i],4],ratio_P_q_deux[x[i],4]),col="darkolivegreen4")
		#median
		segments(x[i]-0.3,ratio_P_q_deux[x[i],3],x[i]+0.3,ratio_P_q_deux[x[i],3])
	}
	x=which(I_juv_moy[2:T,4]==0)
	points(x,y=rep(0,length(x)),pch=21,bg="olivedrab2")
	text(T-1+0.5,1,labels=expression(italic("d.")),col = "grey55",cex=2)

	
x_V=which(I_juv_moy[2:T,1]==1)	
x_A=which(I_juv_moy[2:T,2]==1)	
x_L=which(I_juv_moy[2:T,3]==1)	
x_P=which(I_juv_moy[2:T,4]==1)	

mean(ratio_V_q_juv_deux[x_V,3])
mean(ratio_A_q_juv_deux[x_A,3])
mean(ratio_L_q_juv_deux[x_L,3])
mean(ratio_P_q_deux[x_P,3])
#---------------------------------#
# graph taux de retour 0+/adulte  #
#---------------------------------#
I_surv_deux=read.coda(str_c(datawd,"simulation/I_survCODAchain1.txt"),str_c(datawd,"simulation/I_survCODAindex.txt"),5001,10000)
s_juv2ad_deux=read.coda(str_c(datawd,"simulation/s_juv2adCODAchain1.txt"),str_c(datawd,"simulation/s_juv2adCODAindex.txt"),5001,10000)
level_s_deux=read.coda(str_c(datawd,"simulation/level_sCODAchain1.txt"),str_c(datawd,"simulation/level_sCODAindex.txt"),5001,10000)
s_deux=array(0,c(5000,T))
s_smolt_deux=array(0,c(5000,T))
mu_s_smolt=0.000442
for (t in 1:7){
	s_deux[,t]=s_juv2ad_deux*exp(level_s_deux)
	s_smolt_deux[,t]=mu_s_smolt*exp(level_s_deux)
}
for (t in 8:T){
	s_deux[,t]=s_juv2ad_deux*exp(level_s_deux*I_surv_deux[,t-7])
	s_smolt_deux[,t]=mu_s_smolt*exp(level_s_deux*I_surv_deux[,t-7])
}
s_q_deux=array(0,c(T,5))
s_smolt_q_deux=array(0,c(T,5))
I_surv_q_deux=array(0,c(T,5))
for (t in 1:T){
	s_q_deux[t,]=quantile(s_deux[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	s_smolt_q_deux[t,]=quantile(s_smolt_deux[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
palette_s=rainbow(15,start=0.1,end=2/6)
palette_s=rev(palette_s)

##Taux de survie moyen de la dernière année
round(s_q_deux[T,3]*100,2)#0.27%
round(s_smolt_q_deux[T,3]*100,2)#0.04%

##............
## figure
##............
#juv 0+
	par(mfrow=c(2,1),mar=c(4,7.1,2,0.5),col.lab="grey25",col.axis="grey55",col.main="grey25")
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,(T-1)+0.5), xlab="Years", ylim=c(0,0.07), ylab="",main="average 0+ to returning adult survival",cex.lab=1.5,cex.main=2 )
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07),labels=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07),cex.axis = 1.5,las = 1,lwd=2,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.5,las = 1,lwd=2,col = "grey55")
	for(i in 7:21){
		#whiskers
		#95%
		segments(i-0.15,s_q_deux[i,5],i+0.15,s_q_deux[i,5])
		segments(i,s_q_deux[i,4],i,s_q_deux[i,5])
		#5%
		segments(i-0.15,s_q_deux[i,1],i+0.15,s_q_deux[i,1])
		segments(i,s_q_deux[i,2],i,s_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(s_q_deux[i,2],s_q_deux[i,2],s_q_deux[i,4],s_q_deux[i,4]),col=palette_s[i-6])
		#median
		segments(i-0.3,s_q_deux[i,3],i+0.3,s_q_deux[i,3])
	}
	
	for(i in 22:T){
		#whiskers
		#95%
		segments(i-0.15,s_q_deux[i,5],i+0.15,s_q_deux[i,5])
		segments(i,s_q_deux[i,4],i,s_q_deux[i,5])
		#5%
		segments(i-0.15,s_q_deux[i,1],i+0.15,s_q_deux[i,1])
		segments(i,s_q_deux[i,2],i,s_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(s_q_deux[i,2],s_q_deux[i,2],s_q_deux[i,4],s_q_deux[i,4]),col=palette_s[15])
		#median
		segments(i-0.3,s_q_deux[i,3],i+0.3,s_q_deux[i,3])
	}
##smolt
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,(T+0.5)), xlab="Years", ylim=c(0,0.07), ylab="",main="average stocked smolts to returning adult survival",cex.lab=1.5 ,cex.main=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07),labels=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07),cex.axis = 1.5,las = 1,lwd=2,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1975,1980,1990,2000,1974+T)
	axis(1,at = c(1,6,16,26,T),
			labels=lab1,
			cex.axis = 1.5,las = 1,lwd=2,col = "grey55")
	for(i in 7:21){
		#whiskers
		#95%
		segments(i-0.15,s_smolt_q_deux[i,5],i+0.15,s_smolt_q_deux[i,5])
		segments(i,s_smolt_q_deux[i,4],i,s_smolt_q_deux[i,5])
		#5%
		segments(i-0.15,s_smolt_q_deux[i,1],i+0.15,s_smolt_q_deux[i,1])
		segments(i,s_smolt_q_deux[i,2],i,s_smolt_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(s_smolt_q_deux[i,2],s_smolt_q_deux[i,2],s_smolt_q_deux[i,4],s_smolt_q_deux[i,4]),col=palette_s[i-6])
		#median
		segments(i-0.3,s_smolt_q_deux[i,3],i+0.3,s_smolt_q_deux[i,3])
	}
	for(i in 22:T){
		points(i,s_smolt_q_deux[i,3],pch=16,col=palette_s[15])
	}

	
#----------------------------------------------------------------------------------#
# graph Production relative de juv´eniles 0+  --> fig. 4.9 du rapport Dauphin&Prévost 2012 #
#----------------------------------------------------------------------------------#	

par(mfrow=c(4,3),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
# Vichy
	# Prod juveniles
	#................
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Vichy-Langeac",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("a.")),col = "grey55",cex=2)
	for(i in 7:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_juv_tot_V_q_deux[i,5],i+0.15,ratio_juv_tot_V_q_deux[i,5])
		segments(i,ratio_juv_tot_V_q_deux[i,4],i,ratio_juv_tot_V_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_juv_tot_V_q_deux[i,1],i+0.15,ratio_juv_tot_V_q_deux[i,1])
		segments(i,ratio_juv_tot_V_q_deux[i,2],i,ratio_juv_tot_V_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_V_q_deux[i,2],ratio_juv_tot_V_q_deux[i,2],ratio_juv_tot_V_q_deux[i,4],ratio_juv_tot_V_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,ratio_juv_tot_V_q_deux[i,3],i+0.3,ratio_juv_tot_V_q_deux[i,3])
	}
	segments(0,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")
	
	# Spawners
	#..........
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Vichy-Langeac",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("b.")),col = "grey55",cex=2)
	for(i in 1:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_V_q_deux[i,5],i+0.15,ratio_S_V_q_deux[i,5])
		segments(i,ratio_S_V_q_deux[i,4],i,ratio_S_V_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_V_q_deux[i,1],i+0.15,ratio_S_V_q_deux[i,1])
		segments(i,ratio_S_V_q_deux[i,2],i,ratio_S_V_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_V_q_deux[i,2],ratio_S_V_q_deux[i,2],ratio_S_V_q_deux[i,4],ratio_S_V_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,ratio_S_V_q_deux[i,3],i+0.3,ratio_S_V_q_deux[i,3])
	}
	segments(0,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")
	
	# diff ratio
	#.............
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Vichy-Langeac",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,0.5,labels=expression(italic("c.")),col = "grey55",cex=2)
	for(i in 7:T){
		#whiskers
		#95%
		segments(i-0.15,diff_ratio_V_q_deux[i,5],i+0.15,diff_ratio_V_q_deux[i,5])
		segments(i,diff_ratio_V_q_deux[i,4],i,diff_ratio_V_q_deux[i,5])
		#5%
		segments(i-0.15,diff_ratio_V_q_deux[i,1],i+0.15,diff_ratio_V_q_deux[i,1])
		segments(i,diff_ratio_V_q_deux[i,2],i,diff_ratio_V_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_V_q_deux[i,2],diff_ratio_V_q_deux[i,2],diff_ratio_V_q_deux[i,4],diff_ratio_V_q_deux[i,4]),col="coral3")
		#median
		segments(i-0.3,diff_ratio_V_q_deux[i,3],i+0.3,diff_ratio_V_q_deux[i,3])
	}
	abline(h=0,lty=2,col = "grey55")
	abline(v=12.5,lty=3,col = "grey55")
# Alagnon
	# Prod juveniles
	#.................
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Alagnon",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)
	for(i in 7:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_juv_tot_A_q_deux[i,5],i+0.15,ratio_juv_tot_A_q_deux[i,5])
		segments(i,ratio_juv_tot_A_q_deux[i,4],i,ratio_juv_tot_A_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_juv_tot_A_q_deux[i,1],i+0.15,ratio_juv_tot_A_q_deux[i,1])
		segments(i,ratio_juv_tot_A_q_deux[i,2],i,ratio_juv_tot_A_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_A_q_deux[i,2],ratio_juv_tot_A_q_deux[i,2],ratio_juv_tot_A_q_deux[i,4],ratio_juv_tot_A_q_deux[i,4]),col="darkolivegreen4")
		#median
		segments(i-0.3,ratio_juv_tot_A_q_deux[i,3],i+0.3,ratio_juv_tot_A_q_deux[i,3])
	}
	segments(0,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")
	
	# Spawners
	#............
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Alagnon",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("e.")),col = "grey55",cex=2)
	for(i in 1:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_A_q_deux[i,5],i+0.15,ratio_S_A_q_deux[i,5])
		segments(i,ratio_S_A_q_deux[i,4],i,ratio_S_A_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_A_q_deux[i,1],i+0.15,ratio_S_A_q_deux[i,1])
		segments(i,ratio_S_A_q_deux[i,2],i,ratio_S_A_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_A_q_deux[i,2],ratio_S_A_q_deux[i,2],ratio_S_A_q_deux[i,4],ratio_S_A_q_deux[i,4]),col="darkolivegreen4")
		#median
		segments(i-0.3,ratio_S_A_q_deux[i,3],i+0.3,ratio_S_A_q_deux[i,3])
	}
	segments(0,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")
	
	# diff ratio
	#.............
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Alagnon",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,0.5,labels=expression(italic("f.")),col = "grey55",cex=2)
	for(i in 7:T){
		#whiskers
		#95%
		segments(i-0.15,diff_ratio_A_q_deux[i,5],i+0.15,diff_ratio_A_q_deux[i,5])
		segments(i,diff_ratio_A_q_deux[i,4],i,diff_ratio_A_q_deux[i,5])
		#5%
		segments(i-0.15,diff_ratio_A_q_deux[i,1],i+0.15,diff_ratio_A_q_deux[i,1])
		segments(i,diff_ratio_A_q_deux[i,2],i,diff_ratio_A_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_A_q_deux[i,2],diff_ratio_A_q_deux[i,2],diff_ratio_A_q_deux[i,4],diff_ratio_A_q_deux[i,4]),col="darkolivegreen4")
		#median
		segments(i-0.3,diff_ratio_A_q_deux[i,3],i+0.3,diff_ratio_A_q_deux[i,3])
	}
	abline(h=0,lty=2,col = "grey55")
	abline(v=12.5,lty=3,col = "grey55")
# Langeac
	# Prod juveniles
	#...................
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Langeac-Poutes",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("g.")),col = "grey55",cex=2)
	for(i in 7:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_juv_tot_L_q_deux[i,5],i+0.15,ratio_juv_tot_L_q_deux[i,5])
		segments(i,ratio_juv_tot_L_q_deux[i,4],i,ratio_juv_tot_L_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_juv_tot_L_q_deux[i,1],i+0.15,ratio_juv_tot_L_q_deux[i,1])
		segments(i,ratio_juv_tot_L_q_deux[i,2],i,ratio_juv_tot_L_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_L_q_deux[i,2],ratio_juv_tot_L_q_deux[i,2],ratio_juv_tot_L_q_deux[i,4],ratio_juv_tot_L_q_deux[i,4]),col="darkolivegreen4")
		#median
		segments(i-0.3,ratio_juv_tot_L_q_deux[i,3],i+0.3,ratio_juv_tot_L_q_deux[i,3])
	}
	segments(0,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,12]),29.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,12]),(T+0.5),S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")

	# Spawners
	#............
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Poutes",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("h.")),col = "grey55",cex=2)
	for(i in 1:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_L_q_deux[i,5],i+0.15,ratio_S_L_q_deux[i,5])
		segments(i,ratio_S_L_q_deux[i,4],i,ratio_S_L_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_L_q_deux[i,1],i+0.15,ratio_S_L_q_deux[i,1])
		segments(i,ratio_S_L_q_deux[i,2],i,ratio_S_L_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_q_deux[i,2],ratio_S_L_q_deux[i,2],ratio_S_L_q_deux[i,4],ratio_S_L_q_deux[i,4]),col="darkolivegreen4")
		#median
		segments(i-0.3,ratio_S_L_q_deux[i,3],i+0.3,ratio_S_L_q_deux[i,3])
	}
	segments(0,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
	segments(11.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,12]),29.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,12]),(T+0.5),S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")

	# diff ratio
	#..............
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Langeac-Poutes",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,0.5,labels=expression(italic("i.")),col = "grey55",cex=2)
	for(i in 7:T){
		#whiskers
		#95%
		segments(i-0.15,diff_ratio_L_q_deux[i,5],i+0.15,diff_ratio_L_q_deux[i,5])
		segments(i,diff_ratio_L_q_deux[i,4],i,diff_ratio_L_q_deux[i,5])
		#5%
		segments(i-0.15,diff_ratio_L_q_deux[i,1],i+0.15,diff_ratio_L_q_deux[i,1])
		segments(i,diff_ratio_L_q_deux[i,2],i,diff_ratio_L_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_L_q_deux[i,2],diff_ratio_L_q_deux[i,2],diff_ratio_L_q_deux[i,4],diff_ratio_L_q_deux[i,4]),col="darkolivegreen4")
		#median
		segments(i-0.3,diff_ratio_L_q_deux[i,3],i+0.3,diff_ratio_L_q_deux[i,3])
	}
	abline(h=0,lty=2,col = "grey55")
	abline(v=12.5,lty=3,col = "grey55")
# Poutes
	# Prod juveniles
	#.................
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="upstream Poutes",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("j.")),col = "grey55",cex=2)
	for(i in 16:T){
		#whiskers
		#95%
		segments(i-0.15,ratio_juv_tot_P_q_deux[i,5],i+0.15,ratio_juv_tot_P_q_deux[i,5])
		segments(i,ratio_juv_tot_P_q_deux[i,4],i,ratio_juv_tot_P_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_juv_tot_P_q_deux[i,1],i+0.15,ratio_juv_tot_P_q_deux[i,1])
		segments(i,ratio_juv_tot_P_q_deux[i,2],i,ratio_juv_tot_P_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_P_q_deux[i,2],ratio_juv_tot_P_q_deux[i,2],ratio_juv_tot_P_q_deux[i,4],ratio_juv_tot_P_q_deux[i,4]),col="darkolivegreen3")
		#median
		segments(i-0.3,ratio_juv_tot_P_q_deux[i,3],i+0.3,ratio_juv_tot_P_q_deux[i,3])
	}
	points(x=c(13,14,15),y=c(0,0,0),pch=16,col="darkolivegreen3")
	segments(11.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")

	# Spawners
	#............			
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Poutes",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,1,labels=expression(italic("k.")),col = "grey55",cex=2)
	for(i in 13:23){
		#whiskers
		#95%
		segments(i-0.15,ratio_S_P_q_deux[i,5],i+0.15,ratio_S_P_q_deux[i,5])
		segments(i,ratio_S_P_q_deux[i,4],i,ratio_S_P_q_deux[i,5])
		#5%
		segments(i-0.15,ratio_S_P_q_deux[i,1],i+0.15,ratio_S_P_q_deux[i,1])
		segments(i,ratio_S_P_q_deux[i,2],i,ratio_S_P_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_P_q_deux[i,2],ratio_S_P_q_deux[i,2],ratio_S_P_q_deux[i,4],ratio_S_P_q_deux[i,4]),col="darkolivegreen3")
		#median
		segments(i-0.3,ratio_S_P_q_deux[i,3],i+0.3,ratio_S_P_q_deux[i,3])
	}
	x=seq(24,T,1)
	points(x,ratio_S_P_q_deux[24:T,3],pch=16,col="darkolivegreen3")
	segments(11.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
	segments(23.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
	segments(29.5,S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)
	abline(v=12.5,lty=3,col = "grey55")

	# diff ratio
	#..............
	plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="upstream Poutes",cex.main=2,cex.lab=2)
	# trace l'axe des ordonnées
	axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 2,las = 1,col = "grey55")
	# trace l'axe des abscisses
	lab1=c(1976,1980,1990,2000,1974+T)
	axis(1,at = c(1,5,15,25,(T-1)),
			labels=lab1,
			cex.axis = 2,las = 1,col = "grey55")
	text(T,0.5,labels=expression(italic("l.")),cex=2)
	for(i in 13:T){
		#whiskers
		#95%
		segments(i-0.15,diff_ratio_P_q_deux[i,5],i+0.15,diff_ratio_P_q_deux[i,5])
		segments(i,diff_ratio_P_q_deux[i,4],i,diff_ratio_P_q_deux[i,5])
		#5%
		segments(i-0.15,diff_ratio_P_q_deux[i,1],i+0.15,diff_ratio_P_q_deux[i,1])
		segments(i,diff_ratio_P_q_deux[i,2],i,diff_ratio_P_q_deux[i,1])
		#boxplot
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_P_q_deux[i,2],diff_ratio_P_q_deux[i,2],diff_ratio_P_q_deux[i,4],diff_ratio_P_q_deux[i,4]),col="darkolivegreen3")
		#median
		segments(i-0.3,diff_ratio_P_q_deux[i,3],i+0.3,diff_ratio_P_q_deux[i,3])
	}
	abline(h=0,lty=2,col = "grey55")
	abline(v=12.5,lty=3,col = "grey55")
	for (i in 1:T){
		S_vichy_q_deux[i,]=quantile(S_vichy_real_deux[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
		S_langeac_q_deux[i,]=quantile(S_langeac_real_deux[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}

#Calcul de la moyenne à Vichy-Langeac & Alagnon sur les 20 dernières années
mean(ratio_juv_tot_V_q_deux[(T-19):T,3])
mean(ratio_juv_tot_A_q_deux[(T-19):T,3])
mean(ratio_juv_tot_L_q_deux[(T-19):T,3])
mean(ratio_juv_tot_P_q_deux[(T-19):T,3])

#Calcul de la moyenne à Vichy et à Poutès du ratio de géniteurs durant respectivement les 20 dernières années et l'ensemble de la période
mean(ratio_S_V_q_deux[(T-19):T,3])
mean(ratio_S_P_q_deux[13:T,3])
#ratio habitat en amont de Poutès
S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30])

############################################
# 3.Viabilité de la population de l’Allier #
############################################

#================================
# Scenario Retour vers le futur
#================================
##On a besoin du jeu de données pour sortir la figure retour vers le futur. 
##On récupère le RData sauvé à la fin des calculs du script : SimulationWithoutStocking_RetrospectiveAnalysis_4zones
load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_AnalyseRetro_InteractionReciproqueMatriceVC_Maj2016_2017_12_12.RData")

#Calcul N_vichy
N_vichy_real_q=array(NA,dim=c((T+20-15),5))
N_vichy_q=array(NA,dim=c(T,5))
for (t in 1:T){
	N_vichy_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
#Attention à l'année 30 estimation des passages à Vichy car année jugée incomplète
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	N_vichy_real_q[(t+7),]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
data_vichy<-N[,1]
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
#On estime l'ann�e 2004 car comptage partiel. t-7 car ann�e 30 � la suite des 22 premi�res ann�e soit 23eme ligne de N_vichy_real
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
#--------#
# Figure #
#--------#
par(mfrow=c(2,2))
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="With stocking",cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,36,T),
		labels=c(1975,1980,1990,2000,2010,(1975+T-1)),
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
points(x=seq(23,T,1),data_vichy[23:T],pch=16)
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="Simulation without stocking",cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,36,T),
		labels=c(1975,1980,1990,2000,2010,(1975+T-1)),
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
axis(1,at = c(1,6,16,26,36,T),
		labels=c(1975,1980,1990,2000,2010,(1975+T-1)),
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
axis(1,at = c(1,6,16,26,36,T),
		labels=c(1975,1980,1990,2000,2010,(1975+T-1)),
		cex.axis = 1.2,las = 1,lwd=2,col = "black")
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

#Calcul de la contribution des déversements aux retours d'adulte des 10 dernières années
mean(pourcentage_N_vichy_q[(T-9):T,3])

#=================================
# Scenario Arrêt des déversements
#=================================
#On charge le jeu de données qu'on a créé à la fin du script ProjectionModelWithoutStocking_4zones_Interaction 
load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_2017_12_12.RData")

bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 ann�e de suivi station (soit T+20 - 15). Ne change rien car � part sur ces 23 ann�es sinon on a tjrs un eff exhaustifs indiqu� dans data_vichy
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	N_vichy_real_q[(t+7),]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
N_vichy_proj_q=array(0,dim=c(T+20,5))
for (t in (T+1):(T+20)){
	N_vichy_proj_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#=========
# Figure
#=========
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab=iconv("Ann�es","UTF-8","LATIN1"),ylim=c(0,9000),ylab=iconv("Retours � Vichy","UTF-8","LATIN1"),main=iconv("Projection � 20 ans sans repeuplement","UTF-8","LATIN1"),cex.lab=1.2)
# trace l'axe des ordonn�es
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

#Nombre de poissons moyen dans 20 ans
N_vichy_proj_q[(T+20),3]

##Calcul des proba d'être en dessous de certains seuils
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

#========
# Figure
#========

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab=iconv("Ann�es","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
lab2=c(1975+T,1975+T+9,1975+T+19)
axis(1,at = c(1,10,20),
		labels=lab2,
		cex.axis = 0.9,las = 1,col = "black")
x=seq(1,20,1)

points(x,p_under_50_vichy,col="grey75",pch=16)
segments(x[1:19],p_under_50_vichy[1:19],x[2:20],p_under_50_vichy[2:20],col="grey75")
points(x,p_under_100_vichy,col="grey65",pch=16)
segments(x[1:19],p_under_100_vichy[1:19],x[2:20],p_under_100_vichy[2:20],col="grey65")
points(x,p_under_250_vichy,col="grey55",pch=16)
segments(x[1:19],p_under_250_vichy[1:19],x[2:20],p_under_250_vichy[2:20],col="grey55")
points(x,p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],p_under_500_vichy[1:19],x[2:20],p_under_500_vichy[2:20],col="grey45")

legend(15,1,legend=c(expression(p^seuils < 500),expression(p^seuils < 250),expression(p^seuils < 100),expression(p^seuils < 50)),#,expression(p^seuils < 10)),
		pch=c(16,16,16,16),#16),
		col=c("grey45","grey55","grey65","grey75"), #,"grey75","grey85"),
		bty="n" )

#Proba d'être en dessous de 100 poissons en fin de période
p_under_100_vichy[20]

###########################################################################
# 4.Conditions nécessaires au rétablissement de la population de l’Allier
###########################################################################

## 50% d'augmentation de survie revient à remonter de combien la survie actuelle
bugs_level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt")
exp(mean(bugs_level_s)/2) #2,12 fois

## Récupérer RData dans scénario d'amélioration à hauteur de 50% de la survie
load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_AmeliorationSurvie50_2017_12_12.RData")

bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 ann�e de suivi station (soit T+20 - 15)
#Attention � l'ann�e 30 estimation des passages Vichy car ann�e jug�e incompl�te
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	N_vichy_real_q[(t+7),]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
N_vichy_proj_q=array(0,dim=c(T+20,5))
for (t in (T+1):(T+20)){
	N_vichy_proj_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#=========
# Figure
#=========
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",cex.lab=1.5)
# trace l'axe des ordonnées
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


##Moyenne retour d'adulte dans cette projection
mean(N_vichy_proj_q[(T+1):T,3])

##Probabilité de ne pas atteindre 250 poissons
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

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)))
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

#proba moyenne d'observer moins de 250 poissons à Vichy
mean(p_under_250_vichy)
#on ajoute un calcul pour voir proba d'observer plus de 1000 poissons
upper_1000_vichy=array(0,dim=c(8000,20))
for (t in (T+1):(T+20)){
	for (i in 1:5000){
		if(N_vichy[i,t] > 1000){upper_1000_vichy[i,t-T]=1}  
	}
}
p_upper_1000_vichy=rep(0,20)

for (t in 1:20){
	p_upper_1000_vichy[t]=mean(upper_1000_vichy[,t])
}

##Récupérer RData dans scénario Continuité Ecologique
load("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_ContinuiteEcologique_2017_12_12.RData")

bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#4 car il y a 17 ann�e de suivi station (soit T+20 - 15)
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	N_vichy_real_q[(t+7),]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
N_vichy_proj_q=array(0,dim=c(T+20,5))
for (t in (T+1):(T+20)){
	N_vichy_proj_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#=========
# Figure
#=========
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


## Récupérer RData dans scénario d'amélioration à hauteur de 100% de la survie
load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_AmeliorationSurvie100_2017_12_12.RData")
#On calcul proba d'être en dessous de 1000
under_1000_vichy=array(0,dim=c(8000,20))
for (t in (T+1):(T+20)){
	for (i in 1:5000){
		if(N_vichy[i,t] < 1000){under_1000_vichy[i,t-T]=1}  
	}
}
p_under_1000_vichy=rep(0,20)
for (t in 1:20){
	p_under_1000_vichy[t]=mean(under_1000_vichy[,t])
}

##proba moyenne d'être en dessous de 1000 poissons
mean(p_under_1000_vichy)

##Effectif moyen dans ce scénario
setwd("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")
datawd<-("C:/Users/marion.legrand/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Interaction_ss_rho_poutes_matriceVC/")

bugs_N_vichy_real=read.coda("simulation/N_vichy_realCODAchain1.txt","simulation/N_vichy_realCODAindex.txt")
N_vichy_real_q=array(NA,dim=c(44,5))#44 car il y a 16 ann�e de suivi station (soit T+20 - 15)
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for(t in 23:23){
	N_vichy_real_q[(t+7),]=quantile(bugs_N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
N_vichy_proj_q=array(0,dim=c(T+20,5))
for (t in (T+1):(T+20)){
	N_vichy_proj_q[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#=========
# Figure
#=========
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20+0.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",cex.lab=1.5)
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

#effectif moyenne sur les 20 ans de projection
mean(N_vichy_proj_q[(T+1):(T+20),3])
