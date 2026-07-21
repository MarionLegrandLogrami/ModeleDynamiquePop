# TODO: Add comment
# 
# Author: marion.legrand
###############################################################################


#Modèle 2015_01_24
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2015_01_24_thin200/")
library(coda)
library(boot)
T=39

surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)	

bugs_d_wild_moy_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt",5001,10000)
bugs_d_wild_moy_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt",5001,10000)
bugs_d_wild_moy_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt",5001,10000)
bugs_juv_tot_system=read.coda("simulation/juv_tot_systemCODAchain1.txt","simulation/juv_tot_systemCODAindex.txt",5001,10000)
bugs_rho_poutes=read.coda("simulation/rho_poutesCODAchain1.txt","simulation/rho_poutesCODAindex.txt",5001,10000)


Juv_wild_V=array(0,dim=c(5000,T))
Juv_wild_L=array(0,dim=c(5000,T))
Juv_wild_P=array(0,dim=c(5000,T))

Juv_tot_wild_V=array(0,dim=c(5000,T))
Juv_tot_wild_L=array(0,dim=c(5000,T))
Juv_tot_wild_P=array(0,dim=c(5000,T))

Juv_wild_system=array(0,dim=c(5000,T))
Juv_juv_system=array(0,dim=c(5000,T))

ratio=array(0,dim=c(5000,T))

Juv_tot_system_q=array(0,dim=c(T,5))
Juv_juv_system_q=array(0,dim=c(T,5))
Juv_wild_system_q=array(0,dim=c(T,5))


for (t in 1:(T-1)){
	#for (i in 1:5000){
	Juv_wild_V[,t+1]=bugs_d_wild_moy_V[,t]*S_juv_JP[1,t+1]
	Juv_wild_L[,t+1]=bugs_d_wild_moy_L[,t]*S_juv_JP[2,t+1]
	
}
for (t in 12:(T-1)){
	#for (i in 1:5000){
	Juv_wild_P[,t+1]=bugs_d_wild_moy_P[,t-11]*S_juv_JP[3,t+1]
}

for (t in 7:T){
	#for (i in 1:5000){
	Juv_tot_wild_V[,t]=1/3*Juv_wild_V[,t-3]+1/3*Juv_wild_V[,t-4]+1/3*Juv_wild_V[,t-5]
	Juv_tot_wild_L[,t]=1/3*Juv_wild_L[,t-3]+1/3*Juv_wild_L[,t-4]+1/3*Juv_wild_L[,t-5]
	Juv_tot_wild_P[,t]=1/3*Juv_wild_P[,t-3]+1/3*Juv_wild_P[,t-4]+1/3*Juv_wild_P[,t-5]
}

#for (t in 7:T){
#	Juv_wild_system[,t]=Juv_tot_wild_V[,t]+Juv_tot_wild_L[,t]+Juv_tot_wild_P[,t]*bugs_rho_poutes
#}

for (t in 7:15){
	#for (i in 1:5000){
	Juv_wild_system[,t]=Juv_tot_wild_V[,t]+Juv_tot_wild_L[,t]
}
for (t in 16:T){
	#for(i in 1:5000){
	Juv_wild_system[,t]=Juv_tot_wild_V[,t]+Juv_tot_wild_L[,t]+Juv_tot_wild_P[,t]*bugs_rho_poutes
}

for (t in 7:T){
	for (i in 1:5000){
	if (bugs_juv_tot_system[i,t-6]<Juv_wild_system[i,t]) Juv_juv_system[i,t]=0
	else
	Juv_juv_system[i,t]=bugs_juv_tot_system[i,t-6] - Juv_wild_system[i,t]
}
}
for (t in 1:T){
	Juv_juv_system_q[t,]=quantile(Juv_juv_system[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

for (t in 7:T){
	for (i in 1:5000){
		ratio[i,t]<-Juv_juv_system[i,t]/bugs_juv_tot_system[i,t-6]
	}
}

bugs_res_vichy=read.coda("simulation/res_vichyCODAchain1.txt","simulation/res_vichyCODAindex.txt",5001,10000)

#for (t in 7:T){
#	assign(paste("tab",t,sep=""),array(0,dim=c(5000,2)))
#}


library(reshape2)
#annee<-c(rep(1981:2013,each=5000))
#On passe bugs_res_vichy (class=mcmc) en data frame
data=as.data.frame(bugs_res_vichy)
#on renomme les colonnes de data
colnames(data)<-c(1981:2013)
#on renomme les lignes de data
data$id<-1:nrow(data)
data$id<-as.factor(data$id)
#on convertit le tableau long en tableau court pour avoir toutes les données dans la même colonne et en plus l'indication de l'identifiant
tab<-melt(data,id.vars="id")
#on merge avec Juv_juv_system
juvr=as.data.frame(ratio)
colnames(juvr)<-c(1975:2013)
juvr$id<-1:nrow(juvr)
juvr$id<-as.factor(juvr$id)
tab2<-melt(juvr,id.vars="id")

tab3<-merge(tab,tab2,by=c("id","variable"),all.y=FALSE)
#colnames(tab3[4])<-"juv"
#on réordonne
tab3<-tab3[order(tab3[,2], tab3[,1]), ]

pdf(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/2015_01_24_thin200/CorrSurvie0+Ad_AlvDev_2015.07.02_2.pdf",width=7,height=7)
		plot(as.matrix(tab3[3]),as.matrix(tab3[4]),xlab="residus survie 0+-Adulte",ylab="Ratio alevins deverses/juveniles total",col=rgb(0,0,0,0.1))

dev.off()

plot(as.matrix(tab3[4]),as.matrix(tab3[3]),xlab="Alevins deverses",ylab="residus survie 0+-Adulte")

#Vu avec Etienne il faut prendre toutes les iterations 1 de toutes les années pour data (=en faire un vecteur) et toutes les iterations 1 de ratio de toutes les
#années et faire le coef de correlation de ces 2 vecteurs pour les iterations 1 à 5000

cor=array(0,dim=c(5000,1))

for (i in 1:5000){
	cor[i]=cor(tab3[tab3$id==i,3],tab3[tab3$id==i,4])
}

pdf(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/2015_01_24_thin200/CorrSurvie0+Ad_AlvDev_2015.07.06.pdf",width=7,height=7)
hist(cor)
dev.off()

###################
# VERIF
###################
bugs_juv_tot_V=read.coda("simulation/juv_tot_VCODAchain1.txt","simulation/juv_tot_VCODAindex.txt",5001,10000)
bugs_juv_tot_L=read.coda("simulation/juv_tot_LCODAchain1.txt","simulation/juv_tot_LCODAindex.txt",5001,10000)
bugs_juv_tot_P=read.coda("simulation/juv_tot_PCODAchain1.txt","simulation/juv_tot_PCODAindex.txt",5001,10000)
bugs_d_juv_moy_V=read.coda("stocking_juv_VCODAchain1.txt","stocking_juv_VCODAindex.txt",5001,10000)
bugs_d_juv_moy_L=read.coda("stocking_juv_LCODAchain1.txt","stocking_juv_LCODAindex.txt",5001,10000)
bugs_d_juv_moy_P=read.coda("stocking_juv_PCODAchain1.txt","stocking_juv_PCODAindex.txt",5001,10000)
bugs_d_egg_moy_V=read.coda("stocking_egg_VCODAchain1.txt","stocking_egg_VCODAindex.txt",5001,10000)
bugs_d_egg_moy_L=read.coda("stocking_egg_LCODAchain1.txt","stocking_egg_LCODAindex.txt",5001,10000)
bugs_d_wild_moy_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt",5001,10000)
bugs_d_wild_moy_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt",5001,10000)
bugs_d_wild_moy_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt",5001,10000)
bugs_d_tot_moy_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
bugs_d_tot_moy_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
bugs_d_tot_moy_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)



#for (t in 1:T){
#	Juv_wild_system_q[t,]=quantile(Juv_wild_system[,(t)],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#}
#
#for (t in 1:T){
#	Juv_tot_system_q[t,]=quantile(bugs_juv_tot_system[,(t)],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#}
#
#for (t in 7:T){
#	Juv_juv_system_q[t]=Juv_tot_system_q[t,]-Juv_wild_system_q[t,]
#}
#

#for (t in 1:T){
#	Juv_juv_system_q[t,]=quantile(Juv_juv_system[,(t)],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#}

#Essai 2 mais c'est pas ça n'ont plus qu'on voulait
bugs_res_vichy=read.coda("simulation/res_vichyCODAchain1.txt","simulation/res_vichyCODAindex.txt")
bugs_juv_tot_system=read.coda("simulation/juv_tot_sysCODAchain1.txt","simulation/juv_tot_sysCODAindex.txt")

res_vichy=array(0,dim=c(T,5))
juv_tot_system=array(0,dim=c(T,5))
alv=array(0,dim=c(T,1))

for (t in 7:T){
	res_vichy[t,]=quantile(bugs_res_vichy[,(t-6)],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (t in 1:T){
	juv_tot_system[t,]=quantile(bugs_juv_tot_system[,(t)],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


bugs_dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
bugs_dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
bugs_dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)


surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)	
surf_dev=c(0,250441,147351,
		396109,250441,383049,
		857901,0,127087,
			1120006,0,0,
			974790,0,0,
			974790,0,0,
			935172,0,0,
			1043216,0,0,
			976540,0,0)
surf_dev_JP=matrix(surf_dev, nrow=3)

juv=array(rep(0,195000),dim=c(5000,T))
juv_tot=array(rep(0,195000),dim=c(5000,T))
juv_tot_q=array(rep(0,T*5),dim=c(T,5))


for (t in 2:12){
	juv[,t]= bugs_dmoy_tot_V[,t-1] * S_juv_JP[1,t] + bugs_dmoy_tot_L[,t-1] * S_juv_JP[2,t]
	
}

for (t in 13:T){
	juv[,t]= bugs_dmoy_tot_V[,t-1] * S_juv_JP[1,t] + bugs_dmoy_tot_L[,t-1] * S_juv_JP[2,t] + bugs_dmoy_tot_P[,t-12] * S_juv_JP[3,t]
}


for (t in 7:T){
	juv_tot[,t] <- (1/3) * juv[,t-3] + (1/3) * juv[,t-4] + (1/3) * juv[,t-5] 
}


for (t in 1:T){
	juv_tot_q[t,]=quantile(juv_tot[,t],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}




x=c(33333,26667,48000,28000,69650,58317,71650,31333,21333,1333,0,0,0,19470,63303,104970,103367,377533,689067,1032733,1036400,816993,725693,751218,796984,796762,
     715605,613379,543367,345520,407204,440402,592496)

for (t in 7:T){
	alv[t,]=x[(t-6)]/juv_tot_q[t,3]
}

t=cbind(alv,res_vichy[,3])

plot(t,ylab="residu survie",xlab="alv. deverses")

#1ere méthode mais on peut faire mieux notamment mettre juv déversé en proportion
#de ce qui est produit et faire nuage de point plutôt que superposition
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,39.5),xlab="Years",ylim=c(-2,2),ylab="res_vichy")

# trace l'axe des ordonn�es
axis(2,at = c(-2,-1,0,1,2),labels=c(-2,-1,0,1,2),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,36,39),
		labels=c(1975,1980,1990,2000,2010,2013))
#		cex.axis = 0.9,las = 1,col = "black")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,res_vichy[i,5],i+0.15,res_vichy[i,5])
	segments(i,res_vichy[i,4],i,res_vichy[i,5])
	#5%
	segments(i-0.15,res_vichy[i,1],i+0.15,res_vichy[i,1])
	segments(i,res_vichy[i,2],i,res_vichy[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_vichy[i,2],res_vichy[i,2],res_vichy[i,4],res_vichy[i,4]),col="light grey")
	#median
	segments(i-0.3,res_vichy[i,3],i+0.3,res_vichy[i,3])
}
par(new=TRUE)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),ylab="",xlab="",ylim=c(0,1200000))

axis(4,at = c(0,200000,400000,600000,800000,1000000,1200000),
		labels=c(0,200000,400000,600000,800000,1000000,1200000),
		cex.axis = 0.9,las = 1,col = "orange")
points(x=seq(7,T,1),juv_tot_q[7:T],pch=16,col="orange")


