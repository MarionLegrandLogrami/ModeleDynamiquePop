# TODO: Add comment
# Figure de la proba d'avoir moins de 500 individus à Vichy en fonction des différents scénarii
# Author: marion.legrand
###################################################################################################


#On charge les sauvegardes des différentes simulations et on donne un autre nom à N_vichy puis on enlève N_vichy de la mémoire de la session 
#pour pouvoir charger les données d'une autre simulation
#=======================
# Sans amélioration
#=======================
	#2016_01_20_standard
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_01_20_ProjectionSansRepeuplement_2016.03.11.RData")
	
	#2016_12_06_Alagnon
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_09_ProjectionSansRepeuplementAlagnon.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_09_ProjectionSansRepeuplementAlagnon_cor_juv.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_ProjectionSansRepeuplementAlagnon_cor_juv.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_ProjectionSansRepeuplementAlagnon_cor_juv_2016_12_20.RData")
	
	#2017_03_23_4zones_Interaction
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_03_23_ProjectionSansRepeuplementAlagnon_InteractionReciproque_2017_04_25.RData")
	
	#2017_08_29_4zonesInteraction_ss_rho_poutes_MatriceVC
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_2017_12_12.RData")
	

sans_N_vichy<-N_vichy
rm(N_vichy)
#===================================
# Suppression mortalité dévalaison
#===================================
	#2016_01_20_standard
	#load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_01_20_DevalaisonAvecPoutes_Surf&Prod_2016_04_08.RData")
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_01_20_Devalaison_ss_rho_poutes_Surf_thin200_2016_11_03.RData")
	
	#2016_12_06_Alagnon
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_Devalaison_Surf&Prod_ac_rho_poutes.RData")#mean(N_vichy[i,t]=629.6972
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_Devalaison_Surf&Prod_ac_rho_poutes_cor_juv.RData")#mean(N_vichy[i,t]=629.6972
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_06_01_Devalaison_4zones.RData")
	
	#2017_05_02_4zones_Interaction
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_05_02_Devalaison_4zones_Interaction.RData")

	#2017_12_20_Devalaison_4zonesInteraction_ss_rho_poutes
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_12_20_Devalaison_ss_rho_poutes_Interaction_MatriceVC_2017_12_20.RData")

dev_N_vichy<-N_vichy
rm(N_vichy)

#============================
# Amélioration survie 50%
#============================
	#2016_01_20_standard	
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_01_20_AmeliorationSurvie50_2016_03_14.RData")
	#2016_12_06_Alagnon
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_AmeliorationSurvie50_2016_12_09.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_AmeliorationSurvie50_cor_juv_2016_12_14.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_AmeliorationSurvie50_cor_juv_2016_12_19.RData")
	
	#2017_03_23_4zones_Interaction
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_03_23_AmeliorationSurvie50_2017_04_28.RData")
	
	#2017_08_29_4zones_Interaction_ss_rho_poutes_MatriceVC
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_AmeliorationSurvie50_2017_12_12.RData")

surv50_N_vichy<-N_vichy
rm(N_vichy)

#============================
# Amélioration survie 100%
#============================
	#2016_01_20_standard
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_01_20_AmeliorationSurvie100_2016_03_14.RData")

	#2016_12_06_Alagnon
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_AmeliorationSurvie100_2016_12_09.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_AmeliorationSurvie100_cor_juv_2016_12_14.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_AmeliorationSurvie100_cor_juv_2016_12_19.RData")
	
	#2017_03_23_4zones_Interaction
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_03_23_AmeliorationSurvie100_2017_04_28.RData")

	#2017_08_29_4zones_Interaction_ss_rho_poutes_MatriceVC
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_AmeliorationSurvie100_2017_12_12.RData")
	
surv100_N_vichy<-N_vichy
rm(N_vichy)

#===========================
# Amélioration poutès 50%
#===========================
	#2016_01_20_standard
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_01_20_Standard_OuverturePoutes50_2016_03_11.RData")

	#2016_12_06_Alagnon
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_OuverturePoutes50_2016_12_09.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_OuverturePoutes50_cor_juv_2016_12_14.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_OuverturePoutes50_cor_juv_2016_12_19.RData")
	
	#2017_03_23_4zones_Interaction
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_03_23_OuverturePoutes50_2017_04_28.RData")

	#2017_08_29_4zones_Interaction_ss_rho_poutes_MatriceVC
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_OuverturePoutes50_2017_12_22.RData")
	
pou50_N_vichy<-N_vichy
rm(N_vichy)

#============================
# Amélioration poutès 100%
#============================
	#2016_01_20_standard	
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_01_20_OuverturePoutes100_2016_03_11.RData")

	#2016_12_06_Alagnon
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_OuverturePoutes100_2016_12_09.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_OuverturePoutes100_cor_juv_2016_12_09.RData")
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_OuverturePoutes100_cor_juv_2016_12_19.RData")
	
	#2017_03_23_4zones_Interaction
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_03_23_OuverturePoutes100_2017_04_28.RData")

	#2017_08_29_4zones_Interaction_ss_rho_poutes_MatriceVC
	load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_OuverturePoutes100_2017_12_22.RData")
	
pou100_N_vichy<-N_vichy
rm(N_vichy)

#============================
# Continuité écologique
#============================
	#2017_08_29_4zoneInteraction_ss_rho_poutes_MatriceVC
	load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2017_08_29_Projection_InteractionReciproqueMatriceVC_Maj2016_ContinuiteEcologique_2017_12_12.RData")

ecol_N_vichy<-N_vichy
rm(N_vichy)


#on créé les différentes variables vides
sans_under_500_vichy=array(0,dim=c(8000,20))
dev_under_500_vichy=array(0,dim=c(8000,20))
surv50_under_500_vichy=array(0,dim=c(8000,20))
surv100_under_500_vichy=array(0,dim=c(8000,20))
pou50_under_500_vichy=array(0,dim=c(8000,20))
pou100_under_500_vichy=array(0,dim=c(8000,20))
ecol_under_500_vichy=array(0,dim=c(8000,20))

for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		
		if(sans_N_vichy[i,t] < 500){sans_under_500_vichy[i,t-T]=1}
		if(dev_N_vichy[i,t] < 500){dev_under_500_vichy[i,t-T]=1}
		if(surv50_N_vichy[i,t] < 500){surv50_under_500_vichy[i,t-T]=1}
		if(surv100_N_vichy[i,t] < 500){surv100_under_500_vichy[i,t-T]=1}
		if(pou50_N_vichy[i,t] < 500){pou50_under_500_vichy[i,t-T]=1}
		if(pou100_N_vichy[i,t] < 500){pou100_under_500_vichy[i,t-T]=1}
		if(ecol_N_vichy[i,t] < 500){ecol_under_500_vichy[i,t-T]=1}
		
	}
}


sans_p_under_500_vichy=rep(0,20)
dev_p_under_500_vichy=rep(0,20)
surv50_p_under_500_vichy=rep(0,20)
surv100_p_under_500_vichy=rep(0,20)
pou50_p_under_500_vichy=rep(0,20)
pou100_p_under_500_vichy=rep(0,20)
ecol_p_under_500_vichy=rep(0,20)

for (t in 1:20){
	
	sans_p_under_500_vichy[t]=mean(sans_under_500_vichy[,t])
	dev_p_under_500_vichy[t]=mean(dev_under_500_vichy[,t])
	surv50_p_under_500_vichy[t]=mean(surv50_under_500_vichy[,t])
	surv100_p_under_500_vichy[t]=mean(surv100_under_500_vichy[,t])
	pou50_p_under_500_vichy[t]=mean(pou50_under_500_vichy[,t])
	pou100_p_under_500_vichy[t]=mean(pou100_under_500_vichy[,t])
	ecol_p_under_500_vichy[t]=mean(ecol_under_500_vichy[,t])
	
}


png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_08_29_Interaction_ss_rho_poutes_matriceVC/All_Threshold_2017_12_22.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main=iconv("Probabilité d'observer moins de 500 individus à Vichy\
en fonction des différents scénarii - modèle à 4 zones","UTF8"))

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
lab2=c(1975+T,1975+T+9,1975+T+19)
axis(1,at = c(1,10,20),
		labels=lab2,
		cex.axis = 0.9,las = 1,col = "black")
x=seq(1,20,1)


points(x,sans_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],sans_p_under_500_vichy[1:19],x[2:20],sans_p_under_500_vichy[2:20],col="blue")

points(x,dev_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],dev_p_under_500_vichy[1:19],x[2:20],dev_p_under_500_vichy[2:20],col="green")

points(x,surv50_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],surv50_p_under_500_vichy[1:19],x[2:20],surv50_p_under_500_vichy[2:20],col="yellow")

points(x,surv100_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],surv100_p_under_500_vichy[1:19],x[2:20],surv100_p_under_500_vichy[2:20],col="orange")

points(x,pou50_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],pou50_p_under_500_vichy[1:19],x[2:20],pou50_p_under_500_vichy[2:20],col="red")

points(x,pou100_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],pou100_p_under_500_vichy[1:19],x[2:20],pou100_p_under_500_vichy[2:20],col="black")

points(x,ecol_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],ecol_p_under_500_vichy[1:19],x[2:20],ecol_p_under_500_vichy[2:20],col="purple")

legend(10,1,legend=iconv(c("Sans amélioration","Amélioration à Poutès de 50%","Suppression de Poutès","Suppression des impacts à la dévalaison","Amélioration de la continuité écologique","Amélioration de la survie à 50% du niveau initial","Amélioration de la survie à 100% du niveau initial"),"UTF8"), #
		pch=c(16,16,16,16,16),col=c("blue","red","black","green","purple","yellow","orange"),bty="n" ) #

#legend(10,1,legend=iconv(c("Sans amélioration","Suppression des impacts à la dévalaison","Amélioration de la continuité écologique","Amélioration de la survie à 50% du niveau initial","Amélioration de la survie à 100% du niveau initial"),"UTF8"), #
#		pch=c(16,16,16,16,16),col=c("blue","green","purple","yellow","orange"),bty="n" ) #
#

dev.off()



##################################################
# GRAPH POUR COMPARER avec et sans interaction
##################################################

#===========================================================
# on charge les jeux de données 4 zones sans interaction

#=======================
# Sans amélioration
#=======================

#2016_12_06_Alagnon
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_ProjectionSansRepeuplementAlagnon_cor_juv_2016_12_20.RData")

sans_N_vichy_ss<-N_vichy
rm(N_vichy)
#===================================
# Suppression mortalité dévalaison
#===================================

#2016_12_06_Alagnon
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_06_Alagnon_Devalaison_Surf&Prod_ac_rho_poutes_cor_juv.RData")#mean(N_vichy[i,t]=629.6972

dev_N_vichy_ss<-N_vichy
rm(N_vichy)

#============================
# Amélioration survie 50%
#============================
#2016_12_06_Alagnon
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_AmeliorationSurvie50_cor_juv_2016_12_19.RData")

surv50_N_vichy_ss<-N_vichy
rm(N_vichy)

#============================
# Amélioration survie 100%
#============================

#2016_12_06_Alagnon
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_AmeliorationSurvie100_cor_juv_2016_12_19.RData")

surv100_N_vichy_ss<-N_vichy
rm(N_vichy)

#===========================
# Amélioration poutès 50%
#===========================

#2016_12_06_Alagnon
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_OuverturePoutes50_cor_juv_2016_12_19.RData")


pou50_N_vichy_ss<-N_vichy
rm(N_vichy)

#============================
# Amélioration poutès 100%
#============================

#2016_12_06_Alagnon
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/2016_12_19_Alagnon_OuverturePoutes100_cor_juv_2016_12_19.RData")

pou100_N_vichy_ss<-N_vichy
rm(N_vichy)

#on créé les différentes variables vides
sans_under_500_vichy_ss=array(0,dim=c(8000,20))
#dev_under_500_vichy_ss=array(0,dim=c(8000,20))
surv50_under_500_vichy_ss=array(0,dim=c(8000,20))
surv100_under_500_vichy_ss=array(0,dim=c(8000,20))
pou50_under_500_vichy_ss=array(0,dim=c(8000,20))
pou100_under_500_vichy_ss=array(0,dim=c(8000,20))

for (t in (T+1):(T+20)){
	
	for (i in 1:5000){
		
		if(sans_N_vichy_ss[i,t] < 500){sans_under_500_vichy_ss[i,t-T]=1}
		#if(dev_N_vichy_ss[i,t] < 500){dev_under_500_vichy_ss[i,t-T]=1}
		if(surv50_N_vichy_ss[i,t] < 500){surv50_under_500_vichy_ss[i,t-T]=1}
		if(surv100_N_vichy_ss[i,t] < 500){surv100_under_500_vichy_ss[i,t-T]=1}
		if(pou50_N_vichy_ss[i,t] < 500){pou50_under_500_vichy_ss[i,t-T]=1}
		if(pou100_N_vichy_ss[i,t] < 500){pou100_under_500_vichy_ss[i,t-T]=1}
		
	}
}


sans_p_under_500_vichy_ss=rep(0,20)
#dev_p_under_500_vichy_ss=rep(0,20)
surv50_p_under_500_vichy_ss=rep(0,20)
surv100_p_under_500_vichy_ss=rep(0,20)
pou50_p_under_500_vichy_ss=rep(0,20)
pou100_p_under_500_vichy_ss=rep(0,20)


for (t in 1:20){
	
	sans_p_under_500_vichy_ss[t]=mean(sans_under_500_vichy_ss[,t])
	#dev_p_under_500_vichy_ss[t]=mean(dev_under_500_vichy_ss[,t])
	surv50_p_under_500_vichy_ss[t]=mean(surv50_under_500_vichy_ss[,t])
	surv100_p_under_500_vichy_ss[t]=mean(surv100_under_500_vichy_ss[,t])
	pou50_p_under_500_vichy_ss[t]=mean(pou50_under_500_vichy_ss[,t])
	pou100_p_under_500_vichy_ss[t]=mean(pou100_under_500_vichy_ss[,t])
	
}




png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2017_03_23_4zones_Interaction/All_Threshold_With&WithoutInteraction_2017_04_28.png",width=800,height=800)

par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main=iconv("Probabilité d'observer moins de 500 individus à Vichy\
						en fonction des différents scénarii - modèle à 4 zones","UTF8"))

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
lab2=c(1975+T,1975+T+9,1975+T+19)
axis(1,at = c(1,10,20),
		labels=lab2,
		cex.axis = 0.9,las = 1,col = "black")
x=seq(1,20,1)

#============================
# courbe avec interaction

points(x,sans_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],sans_p_under_500_vichy[1:19],x[2:20],sans_p_under_500_vichy[2:20],col="blue")

#points(x,dev_p_under_500_vichy,col="grey45",pch=16)
#segments(x[1:19],dev_p_under_500_vichy[1:19],x[2:20],dev_p_under_500_vichy[2:20],col="green")

points(x,surv50_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],surv50_p_under_500_vichy[1:19],x[2:20],surv50_p_under_500_vichy[2:20],col="yellow")

points(x,surv100_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],surv100_p_under_500_vichy[1:19],x[2:20],surv100_p_under_500_vichy[2:20],col="orange")

points(x,pou50_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],pou50_p_under_500_vichy[1:19],x[2:20],pou50_p_under_500_vichy[2:20],col="red")

points(x,pou100_p_under_500_vichy,col="grey45",pch=16)
segments(x[1:19],pou100_p_under_500_vichy[1:19],x[2:20],pou100_p_under_500_vichy[2:20],col="black")

legend(0.5,1,legend=iconv(c("Sans amélioration","Amélioration à Poutès de 50%","Suppression de Poutès","Amélioration de la survie à 50% du niveau initial","Amélioration de la survie à 100% du niveau initial"),"UTF8"), #"Suppression des impacts à la dévalaison",
		pch=c(16,16,16,16,16), col=c("blue","red","black","yellow","orange"),bty="n" ) #"green", 

legend(13,1,legend=iconv(c("Avec Interaction"),"UTF8"), #"Suppression des impacts à la dévalaison",
		col=c("black"),lty=1,bty="n" ) #"green", 

#=============================
# courbe sans interaction

points(x,sans_p_under_500_vichy_ss,col="grey45",pch=16)
segments(x[1:19],sans_p_under_500_vichy_ss[1:19],x[2:20],sans_p_under_500_vichy_ss[2:20],col="blue",lty=3)

#points(x,dev_p_under_500_vichy_ss,col="grey45",pch=16)
#segments(x[1:19],dev_p_under_500_vichy_ss[1:19],x[2:20],dev_p_under_500_vichy_ss[2:20],col="green",lty=3)

points(x,surv50_p_under_500_vichy_ss,col="grey45",pch=16)
segments(x[1:19],surv50_p_under_500_vichy_ss[1:19],x[2:20],surv50_p_under_500_vichy_ss[2:20],col="yellow",lty=3)

points(x,surv100_p_under_500_vichy_ss,col="grey45",pch=16)
segments(x[1:19],surv100_p_under_500_vichy_ss[1:19],x[2:20],surv100_p_under_500_vichy_ss[2:20],col="orange",lty=3)

points(x,pou50_p_under_500_vichy_ss,col="grey45",pch=16)
segments(x[1:19],pou50_p_under_500_vichy_ss[1:19],x[2:20],pou50_p_under_500_vichy_ss[2:20],col="red",lty=3)

points(x,pou100_p_under_500_vichy_ss,col="grey45",pch=16)
segments(x[1:19],pou100_p_under_500_vichy_ss[1:19],x[2:20],pou100_p_under_500_vichy_ss[2:20],col="black",lty=3)

#legend(5,0.8,legend=iconv(c("Sans amélioration","Amélioration à Poutès de 50%","Suppression de Poutès","Amélioration de la survie à 50% du niveau initial","Amélioration de la survie à 100% du niveau initial"),"UTF8"), #"Suppression des impacts à la dévalaison",
#		col=c("blue","red","black","yellow","orange"),lty=3,bty="n") #"green", pch=c(16,16,16,16,16)
legend(13,0.97,legend=iconv(c("Sans Interaction"),"UTF8"), #"Suppression des impacts à la dévalaison",
		col=c("black"),lty=3,bty="n" ) #"green", 


dev.off()