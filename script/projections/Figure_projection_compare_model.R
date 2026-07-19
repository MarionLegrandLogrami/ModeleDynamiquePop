# TODO: Add comment
# 
# Author: LOGRAMI
###############################################################################

#On charge les modèles et on execute le code des figure
	#----------------------------------------------------------------------------------
	# 2017_07_19 (on charge le jeu de données issus du script ProjectionModelWithoutStocking_4zones_Interaction + on ajoute chemin vers données pour récupérer data.txt)
	#----------------------------------------------------------------------------------
	setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/")
	load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_07_19_ProjectionSansRepeuplementAlagnon_InteractionReciproque_Maj2016_2017_09_12.RData")
	datawd<-("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_07_19_4zones_Interaction_ss_rho_poutes/")
	library(coda)
	library(stringr)
	
	bugs_N_vichy_real_un=read.coda("2017_07_19_4zones_Interaction_ss_rho_poutes/simulation/N_vichy_realCODAchain1.txt","2017_07_19_4zones_Interaction_ss_rho_poutes/simulation/N_vichy_realCODAindex.txt")
	N_vichy_real_q_un=array(NA,dim=c(44,5))#44 car il y a 16 année de suivi station (soit T+20 - 15). Ne change rien car à part sur ces 23 années sinon on a tjrs un eff exhaustifs indiqué dans data_vichy
	#Attention à l'année 30 estimation des passages Vichy car année jugée incomplète
	for (t in 1:22){
		N_vichy_real_q_un[t,]=quantile(bugs_N_vichy_real_un[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	for(t in 23:23){
		N_vichy_real_q_un[(t+7),]=quantile(bugs_N_vichy_real_un[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	#N_vichy_proj_q=array(0,dim=c(43,5))
	N_vichy_proj_q_un=array(0,dim=c(T+20,5))
	for (t in (T+1):(T+20)){
		N_vichy_proj_q_un[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	library(stringr)
	library(coda)
	bugs2jags(str_c(datawd,"data.txt"),"data_4zones_Interaction_2017.07.19.R")
	source("data_4zones_Interaction_2017.07.19.R")
	data_vichy_un<-N[,1]
		
	under_10_vichy_un=array(0,dim=c(8000,20))
	under_50_vichy_un=array(0,dim=c(8000,20))
	under_100_vichy_un=array(0,dim=c(8000,20))
	under_250_vichy_un=array(0,dim=c(8000,20))
	under_500_vichy_un=array(0,dim=c(8000,20))
	
	
	for (t in (T+1):(T+20)){
		
		for (i in 1:5000){
			if(N_vichy[i,t] < 10){under_10_vichy_un[i,t-T]=1}  
			if(N_vichy[i,t] < 50){under_50_vichy_un[i,t-T]=1}
			if(N_vichy[i,t] < 100){under_100_vichy_un[i,t-T]=1}
			if(N_vichy[i,t] < 250){under_250_vichy_un[i,t-T]=1}
			if(N_vichy[i,t] < 500){under_500_vichy_un[i,t-T]=1}
			
		}
	}
	
	
	p_under_10_vichy_un=rep(0,20)
	p_under_50_vichy_un=rep(0,20)
	p_under_100_vichy_un=rep(0,20)
	p_under_250_vichy_un=rep(0,20)
	p_under_500_vichy_un=rep(0,20)
	
	
	for (t in 1:20){
		p_under_10_vichy_un[t]=mean(under_10_vichy_un[,t])
		p_under_50_vichy_un[t]=mean(under_50_vichy_un[,t])
		p_under_100_vichy_un[t]=mean(under_100_vichy_un[,t])
		p_under_250_vichy_un[t]=mean(under_250_vichy_un[,t])
		p_under_500_vichy_un[t]=mean(under_500_vichy_un[,t])
		
	}
	
	#---------------------------------------------------------------------------------------
	#2016_12_19 (on charge le jeu de données issus du script ProjectionModelWithoutStocking_4zones+ on ajoute chemin vers données pour récupérer data.txt)
	#---------------------------------------------------------------------------------------
	load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2016_12_19_ProjectionSansRepeuplementAlagnon_cor_juv_2016_12_20.RData")
	datawd<-("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon/")
	
	bugs_N_vichy_real_deux=read.coda(str_c(datawd,"simulation/N_vichy_realCODAchain1.txt"),str_c(datawd,"simulation/N_vichy_realCODAindex.txt"))
	N_vichy_real_q_deux=array(NA,dim=c(44,5))#44 car il y a 16 année de suivi station (soit T+20 - 15). Ne change rien car à part sur ces 23 années sinon on a tjrs un eff exhaustifs indiqué dans data_vichy
	#Attention à l'année 30 estimation des passages Vichy car année jugée incomplète
	for (t in 1:22){
		N_vichy_real_q_deux[t,]=quantile(bugs_N_vichy_real_deux[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	for(t in 23:23){
		N_vichy_real_q_deux[(t+7),]=quantile(bugs_N_vichy_real_deux[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	#N_vichy_proj_q=array(0,dim=c(43,5))
	N_vichy_proj_q_deux=array(0,dim=c(T+20,5))
	for (t in (T+1):(T+20)){
		N_vichy_proj_q_deux[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	library(stringr)
	library(coda)
	bugs2jags(str_c(datawd,"data.txt"),"data.R")
	source("data.R")
	data_vichy_deux<-N[,1]
	
	under_10_vichy_deux=array(0,dim=c(8000,20))
	under_50_vichy_deux=array(0,dim=c(8000,20))
	under_100_vichy_deux=array(0,dim=c(8000,20))
	under_250_vichy_deux=array(0,dim=c(8000,20))
	under_500_vichy_deux=array(0,dim=c(8000,20))
	
	
	for (t in (T+1):(T+20)){
		
		for (i in 1:5000){
			if(N_vichy[i,t] < 10){under_10_vichy_deux[i,t-T]=1}  
			if(N_vichy[i,t] < 50){under_50_vichy_deux[i,t-T]=1}
			if(N_vichy[i,t] < 100){under_100_vichy_deux[i,t-T]=1}
			if(N_vichy[i,t] < 250){under_250_vichy_deux[i,t-T]=1}
			if(N_vichy[i,t] < 500){under_500_vichy_deux[i,t-T]=1}
			
		}
	}
	
	
	p_under_10_vichy_deux=rep(0,20)
	p_under_50_vichy_deux=rep(0,20)
	p_under_100_vichy_deux=rep(0,20)
	p_under_250_vichy_deux=rep(0,20)
	p_under_500_vichy_deux=rep(0,20)
	
	
	for (t in 1:20){
		p_under_10_vichy_deux[t]=mean(under_10_vichy_deux[,t])
		p_under_50_vichy_deux[t]=mean(under_50_vichy_deux[,t])
		p_under_100_vichy_deux[t]=mean(under_100_vichy_deux[,t])
		p_under_250_vichy_deux[t]=mean(under_250_vichy_deux[,t])
		p_under_500_vichy_deux[t]=mean(under_500_vichy_deux[,t])
		
	}
	
	#---------------------------------------------------------------------------------------
	#2017_08_29 (on charge le jeu de données issus du script ProjectionModelWithoutStocking_4zones+ on ajoute chemin vers données pour récupérer data.txt)
	#---------------------------------------------------------------------------------------
	load(file = "C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/2017_08_29_ProjectionSansRepeuplementAlagnon_cor_juv_2016_12_20.RData")
	datawd<-("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/2017_08_29_Alagnon/")
	
	bugs_N_vichy_real_trois=read.coda(str_c(datawd,"simulation/N_vichy_realCODAchain1.txt"),str_c(datawd,"simulation/N_vichy_realCODAindex.txt"))
	N_vichy_real_q_trois=array(NA,dim=c(44,5))#44 car il y a 16 année de suivi station (soit T+20 - 15). Ne change rien car à part sur ces 23 années sinon on a tjrs un eff exhaustifs indiqué dans data_vichy
	#Attention à l'année 30 estimation des passages Vichy car année jugée incomplète
	for (t in 1:22){
		N_vichy_real_q_trois[t,]=quantile(bugs_N_vichy_real_trois[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	for(t in 23:23){
		N_vichy_real_q_trois[(t+7),]=quantile(bugs_N_vichy_real_trois[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	#N_vichy_proj_q=array(0,dim=c(43,5))
	N_vichy_proj_q_trois=array(0,dim=c(T+20,5))
	for (t in (T+1):(T+20)){
		N_vichy_proj_q_trois[t,]=quantile(N_vichy[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
	library(stringr)
	library(coda)
	bugs2jags(str_c(datawd,"data.txt"),"data.R")
	source("data.R")
	data_vichy_trois<-N[,1]
	
	under_10_vichy_trois=array(0,dim=c(8000,20))
	under_50_vichy_trois=array(0,dim=c(8000,20))
	under_100_vichy_trois=array(0,dim=c(8000,20))
	under_250_vichy_trois=array(0,dim=c(8000,20))
	under_500_vichy_trois=array(0,dim=c(8000,20))
	
	
	for (t in (T+1):(T+20)){
		
		for (i in 1:5000){
			if(N_vichy[i,t] < 10){under_10_vichy_trois[i,t-T]=1}  
			if(N_vichy[i,t] < 50){under_50_vichy_trois[i,t-T]=1}
			if(N_vichy[i,t] < 100){under_100_vichy_trois[i,t-T]=1}
			if(N_vichy[i,t] < 250){under_250_vichy_trois[i,t-T]=1}
			if(N_vichy[i,t] < 500){under_500_vichy_trois[i,t-T]=1}
			
		}
	}
	
	
	p_under_10_vichy_trois=rep(0,20)
	p_under_50_vichy_trois=rep(0,20)
	p_under_100_vichy_trois=rep(0,20)
	p_under_250_vichy_trois=rep(0,20)
	p_under_500_vichy_trois=rep(0,20)
	
	
	for (t in 1:20){
		p_under_10_vichy_trois[t]=mean(under_10_vichy_trois[,t])
		p_under_50_vichy_trois[t]=mean(under_50_vichy_trois[,t])
		p_under_100_vichy_trois[t]=mean(under_100_vichy_trois[,t])
		p_under_250_vichy_trois[t]=mean(under_250_vichy_trois[,t])
		p_under_500_vichy_trois[t]=mean(under_500_vichy_trois[,t])
		
	}
	
#===================================
# Figure seuil
#===================================
	png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/Simulation/Threshold_compareModel_2016_12_19_2017_07_19.png",width=800,height=800)
	
	par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
	
	plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab=iconv("Années","UTF8","LATIN1"),ylim=c(0,1),ylab=expression(italic(p^seuils)),cex.lab=1.5)
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
	points(x,p_under_50_vichy_un,col="lightskyblue",pch=16)
	segments(x[1:19],p_under_50_vichy_un[1:19],x[2:20],p_under_50_vichy_un[2:20],col="lightskyblue")
	
	points(x,p_under_100_vichy_un,col="cadetblue3",pch=16)
	segments(x[1:19],p_under_100_vichy_un[1:19],x[2:20],p_under_100_vichy_un[2:20],col="cadetblue3")
	
	points(x,p_under_250_vichy_un,col="cornflowerblue",pch=16)
	segments(x[1:19],p_under_250_vichy_un[1:19],x[2:20],p_under_250_vichy_un[2:20],col="cornflowerblue")
	
	points(x,p_under_500_vichy_un,col="royalblue",pch=16)
	segments(x[1:19],p_under_500_vichy_un[1:19],x[2:20],p_under_500_vichy_un[2:20],col="royalblue")
	
	
		
	points(x,p_under_50_vichy_deux,col="grey75",pch=16)
	segments(x[1:19],p_under_50_vichy_deux[1:19],x[2:20],p_under_50_vichy_deux[2:20],col="grey75")
	
	points(x,p_under_100_vichy_deux,col="grey65",pch=16)
	segments(x[1:19],p_under_100_vichy_deux[1:19],x[2:20],p_under_100_vichy_deux[2:20],col="grey65")
	
	points(x,p_under_250_vichy_deux,col="grey55",pch=16)
	segments(x[1:19],p_under_250_vichy_deux[1:19],x[2:20],p_under_250_vichy_deux[2:20],col="grey55")
	
	points(x,p_under_500_vichy_deux,col="grey45",pch=16)
	segments(x[1:19],p_under_500_vichy_deux[1:19],x[2:20],p_under_500_vichy_deux[2:20],col="grey45")
	
	
	
#	points(x,p_under_50_vichy_trois,col="darkolivegreen1",pch=16)
#	segments(x[1:19],p_under_50_vichy_trois[1:19],x[2:20],p_under_50_vichy_trois[2:20],col="grey75")
#	
#	points(x,p_under_100_vichy_trois,col="green1",pch=16)
#	segments(x[1:19],p_under_100_vichy_trois[1:19],x[2:20],p_under_100_vichy_trois[2:20],col="grey65")
#	
#	points(x,p_under_250_vichy_trois,col="green3",pch=16)
#	segments(x[1:19],p_under_250_vichy_trois[1:19],x[2:20],p_under_250_vichy_trois[2:20],col="grey55")
#	
#	points(x,p_under_500_vichy_trois,col="forestgreen",pch=16)
#	segments(x[1:19],p_under_500_vichy_trois[1:19],x[2:20],p_under_500_vichy_trois[2:20],col="grey45")
#	
	
	legend(15,1,legend=c(expression(p^seuils < 500),expression(p^seuils < 250),expression(p^seuils < 100),expression(p^seuils < 50)),#,expression(p^seuils < 10)),
			pch=c(16,16,16,16),#16),
			col=c("grey45","grey55","grey65","grey75"), #,"grey75","grey85"),
			bty="n" )
	legend(1,1,legend=c("Modele 2016_12_19","Modele 2017_07_19"),#"Modele 2017_08_29"),
			lty=1,lwd=2,
			col=c("grey55","cornflowerblue"),#"green3"),
			bty="n")
	
	
	dev.off()
	
