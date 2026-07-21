# TODO: Add comment
# 
# Author: marion.legrand
###############################################################################

#Modèle 2015_01_24
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2015_01_24_thin200/")

#On charge les recalculs liés à Poutès 50% (script SimulationOuverturePoutes.R)
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2015_01_24_OuverturePoutes50_2015_02_02.RData")
#On charge les recalculs liés à Poutès 100% (script SimulationOuverturePoutes.R)
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2015_01_24_OuverturePoutes100_2015.02.02.RData")
#On charge les projections sans repeuplement à 20 ans
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2015_01_24_ProjectionSansRepeuplement_2015.02.02.RData")
#On charge les projections avec amélioration de la survie de 50%
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2015_01_24_AmeliorationSurvie50_2015.02.02.RData")
#On charge les projections avec amélioration de la survie de 100%
load("C:/Users/utilisateur/workspace/ModeleDynamiquePop/2015_01_24_AmeliorationSurvie100_2015.02.02.RData")

library(coda)
library(boot)
T=39

surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)


#----------------------------------------------------
# Figure : Redds_probaPassageStation with all years
#----------------------------------------------------
#charge L_mu_p_Poutes pour les 39-11 premières années
bugs_L_mu_p_poutes=read.coda("L_mu_p_poutesCODAchain1.txt","L_mu_p_poutesCODAindex.txt")

#On agglomère les L_mu_p_poutes (estimé dans openbugs et dans R) pour les stocker dans une seule et même variable
mu_p_poutes_m=array(0,dim=c(5000,48))
for (t in 1:28){
	for (i in 1:5000){
		mu_p_poutes_m[i,t]=bugs_L_mu_p_poutes[i,(t)]
	}
}
#on récupère les L_mu_p_poutes recalculés dans la simulation ouverture poutès
for (t in 29:48){
	for (i in 1:5000){
		mu_p_poutes_m[i,t]=L_mu_p_poutes[i,(t+11)]
	}
}

mu_p_poutes_m=inv.logit(mu_p_poutes_m)
mu_p_poutes_q=quantile(mu_p_poutes_m,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)

#charge p_poutes issu d'openbugs
bugs_p_poutes=read.coda("p_poutesCODAchain1.txt","p_poutesCODAindex.txt")
p_poutes_m=array(0,dim=c(5000,48))
for (t in 1:28){
	for (i in 1:5000){
		p_poutes_m[i,t]=bugs_p_poutes[i,(t)]
	}
}
#on récupère les p_poutes recalculés dans la simulation ouverture poutès
for (t in 29:48){
	for (i in 1:5000){
		p_poutes_m[i,t]=p_poutes[i,(t+11)]
	}
}

p_poutes_q=array(rep(0,59*5),dim=c(T+20,5))

for (i in 12:(T+20)){
	p_poutes_q[i,]=quantile(p_poutes_m[,i-11],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/ProbaPassagePoutesImprovementSurvival50_2015.02.02.png",width=800, height=800, units = "px",type="cairo")

#par(mfrow=c(1,1),mar=c(4,6.1,0,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")

#..........
#p_poutes
#..........
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^P)),main="")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

temp=c(11.5,12:(T+21))
temp1=c(rev(12:(T+21)),11.5)

xx=c(temp,temp1)

q_2_5=rep(mu_p_poutes_q[1],50)
q_97_5=rep(mu_p_poutes_q[5],50)

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="slategray1",border="NA")

q_25=rep(mu_p_poutes_q[2],50)
q_75=rep(mu_p_poutes_q[4],50)

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="slategray3",border="NA")

points(seq(11.5,T+21,0.5),rep(mu_p_poutes_q[3],(49*2),type="l",col="white"))

for(i in 12:T){
	#whiskers
	#95%
	segments(i-0.15,p_poutes_q[i,5],i+0.15,p_poutes_q[i,5])
	segments(i,p_poutes_q[i,4],i,p_poutes_q[i,5])
	#5%
	segments(i-0.15,p_poutes_q[i,1],i+0.15,p_poutes_q[i,1])
	segments(i,p_poutes_q[i,2],i,p_poutes_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(p_poutes_q[i,2],p_poutes_q[i,2],p_poutes_q[i,4],p_poutes_q[i,4]),col="steelblue4")
	#median
	segments(i-0.3,p_poutes_q[i,3],i+0.3,p_poutes_q[i,3])
}
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,p_poutes_q[i,5],i+0.15,p_poutes_q[i,5])
	segments(i,p_poutes_q[i,4],i,p_poutes_q[i,5])
	#5%
	segments(i-0.15,p_poutes_q[i,1],i+0.15,p_poutes_q[i,1])
	segments(i,p_poutes_q[i,2],i,p_poutes_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(p_poutes_q[i,2],p_poutes_q[i,2],p_poutes_q[i,4],p_poutes_q[i,4]),col="orange")
	#median
	segments(i-0.3,p_poutes_q[i,3],i+0.3,p_poutes_q[i,3])
}
abline(v=11.5,lty=2)

text(6,0.55,paste( "upstream Poutes\n inaccessible"),col = "grey25")

dev.off()

########################
# CHAP: SPAWNERS_REDDS 
########################
bugs_S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
bugs_S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")

S_vichy_q=array(rep(0,(T+20)*5),dim=c((T+20),5))
S_langeac_q=array(rep(0,(T+20)*5),dim=c((T+20),5))
S_poutes_q=array(rep(0,(T+20)*5),dim=c((T+20),5))

S_poutes_counter=c(
		0,0,
		0,0,0,0,0,
		0,0,0,0,10,
		43,110,21,4,3,
		11,9,23,6,67,
		35,31,130,112,53,
		40,154,89,74,153,
		53,39,14,26,118,59,45)

ratio_S_V=array(0,c(5000,T+20))
ratio_S_L=array(0,c(5000,T+20))
ratio_S_P=array(0,c(5000,T+20))

#ratio

for (t in 1:11){
	
	for(i in 1:5000){
		ratio_S_V[i,t] = bugs_S_vichy_real[i,t] / (bugs_S_vichy_real[i,t] + bugs_S_langeac_real[i,t])
		ratio_S_L[i,t] = 1 - ratio_S_V[i,t]
	}
}


for (t in 12:T){
	
	for( i in 1:5000){
		ratio_S_V[i,t] = bugs_S_vichy_real[i,t] / (bugs_S_vichy_real[i,t] + bugs_S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_L[i,t] = bugs_S_langeac_real[i,t] / (bugs_S_vichy_real[i,t] + bugs_S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_P[i,t] = 1 - ratio_S_V[i,t] -ratio_S_L[i,t]
	}
}
for (t in (T+1):(T+20)){
	
	for( i in 1:5000){
		ratio_S_V[i,t] = S_vichy[i,t] / (S_vichy[i,t] + S_langeac[i,t] + S_poutes[i,t])
		ratio_S_L[i,t] = S_langeac[i,t] / (S_vichy[i,t] + S_langeac[i,t] + S_poutes[i,t])
		ratio_S_P[i,t] = 1 - ratio_S_V[i,t] -ratio_S_L[i,t]
	}
}

ratio_S_V_q=array(0,c((T+20),5))
ratio_S_L_q=array(0,c((T+20),5))
ratio_S_P_q=array(0,c((T+20),5))



for (t in 1:(T+20)){
	
	ratio_S_V_q[t,]=quantile(ratio_S_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_L_q[t,]=quantile(ratio_S_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_S_P_q[t,]=quantile(ratio_S_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}


for (i in 1:T){
	S_vichy_q[i,]=quantile(bugs_S_vichy_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}
for (i in (T+1):(T+20)){
	S_vichy_q[i,]=quantile(S_vichy[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}
for (i in 1:T){
	S_langeac_q[i,]=quantile(bugs_S_langeac_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (i in (T+1):(T+20)){
	S_langeac_q[i,]=quantile(S_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (i in (T+1):(T+20)){
	S_poutes_q[i,]=quantile(S_poutes[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


#=============================================================
# SEC : Figure : SpawnersRedds_GeniteursPotentiels all years
#=============================================================

#---------------
# Spawners
#--------------
png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/SpawnersRedds_GeniteursPotentielsImprovementSurvival100_2015.02.02.png",width=800, height=800, units = "px",type="cairo")

par(mfrow=c(3,2),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
#........
# Vichy
#........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,6500),ylab=expression(italic(S["t,1"])),main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000),labels=c(0,1000,2000,3000,4000,5000,6000),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

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
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,S_vichy_q[i,5],i+0.15,S_vichy_q[i,5])
	segments(i,S_vichy_q[i,4],i,S_vichy_q[i,5])
	#5%
	segments(i-0.15,S_vichy_q[i,1],i+0.15,S_vichy_q[i,1])
	segments(i,S_vichy_q[i,2],i,S_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_vichy_q[i,2],S_vichy_q[i,2],S_vichy_q[i,4],S_vichy_q[i,4]),col="orange")
	#median
	segments(i-0.3,S_vichy_q[i,3],i+0.3,S_vichy_q[i,3])
}

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

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
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_V_q[i,5],i+0.15,ratio_S_V_q[i,5])
	segments(i,ratio_S_V_q[i,4],i,ratio_S_V_q[i,5])
	#5%
	segments(i-0.15,ratio_S_V_q[i,1],i+0.15,ratio_S_V_q[i,1])
	segments(i,ratio_S_V_q[i,2],i,ratio_S_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_V_q[i,2],ratio_S_V_q[i,2],ratio_S_V_q[i,4],ratio_S_V_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_V_q[i,3],i+0.3,ratio_S_V_q[i,3])
}
segments(0,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

abline(v=11.5,lty=3)

#..........
# Langeac
#..........
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,1200),ylab=expression(italic(S["t,2"])),main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,300,600,900,1200),labels=c(0,300,600,900,1200),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")


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
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,S_langeac_q[i,5],i+0.15,S_langeac_q[i,5])
	segments(i,S_langeac_q[i,4],i,S_langeac_q[i,5])
	#5%
	segments(i-0.15,S_langeac_q[i,1],i+0.15,S_langeac_q[i,1])
	segments(i,S_langeac_q[i,2],i,S_langeac_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_langeac_q[i,2],S_langeac_q[i,2],S_langeac_q[i,4],S_langeac_q[i,4]),col="orange")
	#median
	segments(i-0.3,S_langeac_q[i,3],i+0.3,S_langeac_q[i,3])
}

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

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
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_L_q[i,5],i+0.15,ratio_S_L_q[i,5])
	segments(i,ratio_S_L_q[i,4],i,ratio_S_L_q[i,5])
	#5%
	segments(i-0.15,ratio_S_L_q[i,1],i+0.15,ratio_S_L_q[i,1])
	segments(i,ratio_S_L_q[i,2],i,ratio_S_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_q[i,2],ratio_S_L_q[i,2],ratio_S_L_q[i,4],ratio_S_L_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_L_q[i,3],i+0.3,ratio_S_L_q[i,3])
}
segments(0,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

abline(v=11.5,lty=3)

#..........
# Poutes
#..........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,200),ylab=expression(italic(S["t,3"])),main="Upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,100,200),labels=c(0,100,200),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

points(x=seq(12,T,1),y=S_poutes_counter[12:T],pch=16,col="darkolivegreen3") 
abline(v=11.5,lty=2)
text(6,100,paste( "upstream Poutes\n inaccessible"),col = "grey55")

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,S_poutes_q[i,5],i+0.15,S_poutes_q[i,5])
	segments(i,S_poutes_q[i,4],i,S_poutes_q[i,5])
	#5%
	segments(i-0.15,S_poutes_q[i,1],i+0.15,S_poutes_q[i,1])
	segments(i,S_poutes_q[i,2],i,S_poutes_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_poutes_q[i,2],S_poutes_q[i,2],S_poutes_q[i,4],S_poutes_q[i,4]),col="orange")
	#median
	segments(i-0.3,S_poutes_q[i,3],i+0.3,S_poutes_q[i,3])
}


text(T+20,200,labels=expression(italic("c.")),col = "grey55")



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("f.")),col = "grey55")
text(6,0.55,paste( "upstream Poutes\n inaccessible"),col = "grey55")


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

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_P_q[i,5],i+0.15,ratio_S_P_q[i,5])
	segments(i,ratio_S_P_q[i,4],i,ratio_S_P_q[i,5])
	#5%
	segments(i-0.15,ratio_S_P_q[i,1],i+0.15,ratio_S_P_q[i,1])
	segments(i,ratio_S_P_q[i,2],i,ratio_S_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_P_q[i,2],ratio_S_P_q[i,2],ratio_S_P_q[i,4],ratio_S_P_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_P_q[i,3],i+0.3,ratio_S_P_q[i,3])
}

segments(11.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

abline(v=11.5,lty=3)

dev.off()

########################################
# CHAP: SPAWNERS - PRODUCTION JUVENILE 
########################################

S_vichy_q=array(0,dim=c(T,5))
S_langeac_q=array(0,dim=c(T,5))

S_poutes_counter=c(
		0,0,
		0,0,0,0,0,
		0,0,0,0,10,
		43,110,21,4,3,
		11,9,23,6,67,
		35,31,130,112,53,
		40,154,89,74,153,
		53,39,14,26,118,59,45)

#===============
# SEC: ratio
#===============
#-----------------
# Ratio Géniteurs
#-----------------
#cf.au-dessus

#------------------
# Ratio Juvénile
#------------------
bugs_juv_tot_V=read.coda("simulation/juv_tot_VCODAchain1.txt","simulation/juv_tot_VCODAindex.txt")
bugs_juv_tot_L=read.coda("simulation/juv_tot_LCODAchain1.txt","simulation/juv_tot_LCODAindex.txt")
bugs_juv_tot_P=read.coda("simulation/juv_tot_PCODAchain1.txt","simulation/juv_tot_PCODAindex.txt")

ratio_juv_tot_V=array(0,c(5000,T+20))
ratio_juv_tot_L=array(0,c(5000,T+20))
ratio_juv_tot_P=array(0,c(5000,T+20))

for (t in 7:15){			
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = bugs_juv_tot_V[i,t-6] / (bugs_juv_tot_V[i,t-6] + bugs_juv_tot_L[i,t-6])
		ratio_juv_tot_L[i,t] = 1 - ratio_juv_tot_V[i,t]
	}
}
for (t in 16:T){
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = bugs_juv_tot_V[i,t-6] / (bugs_juv_tot_V[i,t-6] + bugs_juv_tot_L[i,t-6] + bugs_juv_tot_P[i,t-15])
		ratio_juv_tot_L[i,t] = bugs_juv_tot_L[i,t-6] / (bugs_juv_tot_V[i,t-6] + bugs_juv_tot_L[i,t-6] + bugs_juv_tot_P[i,t-15])
		ratio_juv_tot_P[i,t] = 1 - ratio_juv_tot_V[i,t] - ratio_juv_tot_L[i,t]
	}
}
for (t in (T+1):(T+6)){
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = bugs_juv_tot_vichy[i,t] / (bugs_juv_tot_vichy[i,t] + bugs_juv_tot_langeac[i,t] + bugs_juv_tot_poutes[i,t])
		ratio_juv_tot_L[i,t] = bugs_juv_tot_langeac[i,t] / (bugs_juv_tot_vichy[i,t] + bugs_juv_tot_langeac[i,t] + bugs_juv_tot_poutes[i,t])
		ratio_juv_tot_P[i,t] = 1 - ratio_juv_tot_V[i,t] - ratio_juv_tot_L[i,t]
	}
}
for (t in (T+7):(T+20)){
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = juv_tot_vichy[i,t] / (juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t])
		ratio_juv_tot_L[i,t] = juv_tot_langeac[i,t] / (juv_tot_vichy[i,t] + juv_tot_langeac[i,t] + juv_tot_poutes[i,t])
		ratio_juv_tot_P[i,t] = 1 - ratio_juv_tot_V[i,t] - ratio_juv_tot_L[i,t]
	}
}

ratio_juv_tot_V_q=array(0,c(T+20,5))
ratio_juv_tot_L_q=array(0,c(T+20,5))
ratio_juv_tot_P_q=array(0,c(T+20,5))

for (t in 7:(T+20)){
	
	ratio_juv_tot_V_q[t,]=quantile(ratio_juv_tot_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_juv_tot_L_q[t,]=quantile(ratio_juv_tot_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (t in 16:(T+20)){
	ratio_juv_tot_P_q[t,]=quantile(ratio_juv_tot_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#--------------------------------
# Diff ratio géniteurs/juvéniles
#--------------------------------
diff_ratio_V=ratio_S_V-ratio_juv_tot_V
diff_ratio_L=ratio_S_L-ratio_juv_tot_L
diff_ratio_P=ratio_S_P-ratio_juv_tot_P

diff_ratio_V_q=array(0,c(T+20,5))
diff_ratio_L_q=array(0,c(T+20,5))
diff_ratio_P_q=array(0,c(T+20,5))

for (t in 1:(T+20)){
	diff_ratio_V_q[t,]=quantile(diff_ratio_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	diff_ratio_L_q[t,]=quantile(diff_ratio_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	diff_ratio_P_q[t,]=quantile(diff_ratio_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


#=============================================
# SEC: Figure : Spawners Production Juvenile
#=============================================
png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2015_01_24_thin200/SpawnersProdJuv_ratioImprovementSurvival100_2015.02.02.png",width=800, height=800, units = "px",type="cairo")
par(mfrow=c(3,3),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")

#----------
# Vichy
#---------
#.................
# Prod juveniles
#.................
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("a.")),col = "grey55")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_tot_V_q[i,5],i+0.15,ratio_juv_tot_V_q[i,5])
	segments(i,ratio_juv_tot_V_q[i,4],i,ratio_juv_tot_V_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_tot_V_q[i,1],i+0.15,ratio_juv_tot_V_q[i,1])
	segments(i,ratio_juv_tot_V_q[i,2],i,ratio_juv_tot_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_V_q[i,2],ratio_juv_tot_V_q[i,2],ratio_juv_tot_V_q[i,4],ratio_juv_tot_V_q[i,4]),col="coral3")
	#median
	segments(i-0.3,ratio_juv_tot_V_q[i,3],i+0.3,ratio_juv_tot_V_q[i,3])
}
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_tot_V_q[i,5],i+0.15,ratio_juv_tot_V_q[i,5])
	segments(i,ratio_juv_tot_V_q[i,4],i,ratio_juv_tot_V_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_tot_V_q[i,1],i+0.15,ratio_juv_tot_V_q[i,1])
	segments(i,ratio_juv_tot_V_q[i,2],i,ratio_juv_tot_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_V_q[i,2],ratio_juv_tot_V_q[i,2],ratio_juv_tot_V_q[i,4],ratio_juv_tot_V_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_tot_V_q[i,3],i+0.3,ratio_juv_tot_V_q[i,3])
}

segments(0,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

#segments(7,mean(ratio_juv_tot_V[,7:12]),12.5,mean(ratio_juv_tot_V[,7:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_juv_tot_V[,13:37]),37.5,mean(ratio_juv_tot_V[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")

#............
# Spawners
#............
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("b.")),col = "grey55")

for(i in 1:T){
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
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_V_q[i,5],i+0.15,ratio_S_V_q[i,5])
	segments(i,ratio_S_V_q[i,4],i,ratio_S_V_q[i,5])
	#5%
	segments(i-0.15,ratio_S_V_q[i,1],i+0.15,ratio_S_V_q[i,1])
	segments(i,ratio_S_V_q[i,2],i,ratio_S_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_V_q[i,2],ratio_S_V_q[i,2],ratio_S_V_q[i,4],ratio_S_V_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_V_q[i,3],i+0.3,ratio_S_V_q[i,3])
}
segments(0,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[1,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[1,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[1,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[1,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

#segments(0,mean(ratio_S_V[,1:12]),12.5,mean(ratio_S_V[,1:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_S_V[,13:37]),37.5,mean(ratio_S_V[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")

#............. 
# diff ratio
#.............
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,0.5,labels=expression(italic("c.")),col = "grey55")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,diff_ratio_V_q[i,5],i+0.15,diff_ratio_V_q[i,5])
	segments(i,diff_ratio_V_q[i,4],i,diff_ratio_V_q[i,5])
	#5%
	segments(i-0.15,diff_ratio_V_q[i,1],i+0.15,diff_ratio_V_q[i,1])
	segments(i,diff_ratio_V_q[i,2],i,diff_ratio_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_V_q[i,2],diff_ratio_V_q[i,2],diff_ratio_V_q[i,4],diff_ratio_V_q[i,4]),col="coral3")
	#median
	segments(i-0.3,diff_ratio_V_q[i,3],i+0.3,diff_ratio_V_q[i,3])
}
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,diff_ratio_V_q[i,5],i+0.15,diff_ratio_V_q[i,5])
	segments(i,diff_ratio_V_q[i,4],i,diff_ratio_V_q[i,5])
	#5%
	segments(i-0.15,diff_ratio_V_q[i,1],i+0.15,diff_ratio_V_q[i,1])
	segments(i,diff_ratio_V_q[i,2],i,diff_ratio_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_V_q[i,2],diff_ratio_V_q[i,2],diff_ratio_V_q[i,4],diff_ratio_V_q[i,4]),col="orange")
	#median
	segments(i-0.3,diff_ratio_V_q[i,3],i+0.3,diff_ratio_V_q[i,3])
}
abline(h=0,lty=2,col = "grey55")
abline(v=12.5,lty=3,col = "grey55")

#----------
# Langeac
#---------
#...................
# Prod juveniles
#...................
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("d.")),col = "grey55")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_tot_L_q[i,5],i+0.15,ratio_juv_tot_L_q[i,5])
	segments(i,ratio_juv_tot_L_q[i,4],i,ratio_juv_tot_L_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_tot_L_q[i,1],i+0.15,ratio_juv_tot_L_q[i,1])
	segments(i,ratio_juv_tot_L_q[i,2],i,ratio_juv_tot_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_L_q[i,2],ratio_juv_tot_L_q[i,2],ratio_juv_tot_L_q[i,4],ratio_juv_tot_L_q[i,4]),col="darkolivegreen4")
	#median
	segments(i-0.3,ratio_juv_tot_L_q[i,3],i+0.3,ratio_juv_tot_L_q[i,3])
}

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_tot_L_q[i,5],i+0.15,ratio_juv_tot_L_q[i,5])
	segments(i,ratio_juv_tot_L_q[i,4],i,ratio_juv_tot_L_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_tot_L_q[i,1],i+0.15,ratio_juv_tot_L_q[i,1])
	segments(i,ratio_juv_tot_L_q[i,2],i,ratio_juv_tot_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_L_q[i,2],ratio_juv_tot_L_q[i,2],ratio_juv_tot_L_q[i,4],ratio_juv_tot_L_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_tot_L_q[i,3],i+0.3,ratio_juv_tot_L_q[i,3])
}

segments(0,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

#segments(7,mean(ratio_juv_tot_L[,7:12]),12.5,mean(ratio_juv_tot_L[,7:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_juv_tot_L[,13:37]),37.5,mean(ratio_juv_tot_L[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")

#............
# Spawners
#............
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("e.")),col = "grey55")

for(i in 1:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_L_q[i,5],i+0.15,ratio_S_L_q[i,5])
	segments(i,ratio_S_L_q[i,4],i,ratio_S_L_q[i,5])
	#5%
	segments(i-0.15,ratio_S_L_q[i,1],i+0.15,ratio_S_L_q[i,1])
	segments(i,ratio_S_L_q[i,2],i,ratio_S_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_q[i,2],ratio_S_L_q[i,2],ratio_S_L_q[i,4],ratio_S_L_q[i,4]),col="darkolivegreen4")
	#median
	segments(i-0.3,ratio_S_L_q[i,3],i+0.3,ratio_S_L_q[i,3])
}
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_L_q[i,5],i+0.15,ratio_S_L_q[i,5])
	segments(i,ratio_S_L_q[i,4],i,ratio_S_L_q[i,5])
	#5%
	segments(i-0.15,ratio_S_L_q[i,1],i+0.15,ratio_S_L_q[i,1])
	segments(i,ratio_S_L_q[i,2],i,ratio_S_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_L_q[i,2],ratio_S_L_q[i,2],ratio_S_L_q[i,4],ratio_S_L_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_L_q[i,3],i+0.3,ratio_S_L_q[i,3])
}
segments(0,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),11.5,S_juv_JP[2,1]/(S_juv_JP[1,1]+S_juv_JP[2,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[2,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[2,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[2,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

#segments(0,mean(ratio_S_L[,1:12]),12.5,mean(ratio_S_L[,1:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_S_L[,13:37]),37.5,mean(ratio_S_L[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")

#..............
# diff ratio
#..............
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,0.5,labels=expression(italic("f.")),col = "grey55")

for(i in 7:T){
	#whiskers
	#95%
	segments(i-0.15,diff_ratio_L_q[i,5],i+0.15,diff_ratio_L_q[i,5])
	segments(i,diff_ratio_L_q[i,4],i,diff_ratio_L_q[i,5])
	#5%
	segments(i-0.15,diff_ratio_L_q[i,1],i+0.15,diff_ratio_L_q[i,1])
	segments(i,diff_ratio_L_q[i,2],i,diff_ratio_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_L_q[i,2],diff_ratio_L_q[i,2],diff_ratio_L_q[i,4],diff_ratio_L_q[i,4]),col="darkolivegreen4")
	#median
	segments(i-0.3,diff_ratio_L_q[i,3],i+0.3,diff_ratio_L_q[i,3])
}
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,diff_ratio_L_q[i,5],i+0.15,diff_ratio_L_q[i,5])
	segments(i,diff_ratio_L_q[i,4],i,diff_ratio_L_q[i,5])
	#5%
	segments(i-0.15,diff_ratio_L_q[i,1],i+0.15,diff_ratio_L_q[i,1])
	segments(i,diff_ratio_L_q[i,2],i,diff_ratio_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_L_q[i,2],diff_ratio_L_q[i,2],diff_ratio_L_q[i,4],diff_ratio_L_q[i,4]),col="orange")
	#median
	segments(i-0.3,diff_ratio_L_q[i,3],i+0.3,diff_ratio_L_q[i,3])
}

abline(h=0,lty=2,col = "grey55")
abline(v=12.5,lty=3,col = "grey55")


#----------
# Poutes
#----------
#.................
# Prod juveniles
#.................
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("g.")),col = "grey55")

for(i in 16:T){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_tot_P_q[i,5],i+0.15,ratio_juv_tot_P_q[i,5])
	segments(i,ratio_juv_tot_P_q[i,4],i,ratio_juv_tot_P_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_tot_P_q[i,1],i+0.15,ratio_juv_tot_P_q[i,1])
	segments(i,ratio_juv_tot_P_q[i,2],i,ratio_juv_tot_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_P_q[i,2],ratio_juv_tot_P_q[i,2],ratio_juv_tot_P_q[i,4],ratio_juv_tot_P_q[i,4]),col="darkolivegreen3")
	#median
	segments(i-0.3,ratio_juv_tot_P_q[i,3],i+0.3,ratio_juv_tot_P_q[i,3])
}

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_juv_tot_P_q[i,5],i+0.15,ratio_juv_tot_P_q[i,5])
	segments(i,ratio_juv_tot_P_q[i,4],i,ratio_juv_tot_P_q[i,5])
	#5%
	segments(i-0.15,ratio_juv_tot_P_q[i,1],i+0.15,ratio_juv_tot_P_q[i,1])
	segments(i,ratio_juv_tot_P_q[i,2],i,ratio_juv_tot_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_juv_tot_P_q[i,2],ratio_juv_tot_P_q[i,2],ratio_juv_tot_P_q[i,4],ratio_juv_tot_P_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_juv_tot_P_q[i,3],i+0.3,ratio_juv_tot_P_q[i,3])
}

points(x=c(13,14,15),y=c(0,0,0),pch=16,col="darkolivegreen3")

segments(11.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

#segments(12.5,mean(ratio_juv_tot_P[,16:37]),37.5,mean(ratio_juv_tot_L[,16:37]),col="red",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")

#............
# Spawners
#............			
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,1,labels=expression(italic("h.")),col = "grey55")

for(i in 13:23){
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

x=seq(24,T,1)
points(x,ratio_S_P_q[24:T,3],pch=16,col="darkolivegreen3")

for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,ratio_S_P_q[i,5],i+0.15,ratio_S_P_q[i,5])
	segments(i,ratio_S_P_q[i,4],i,ratio_S_P_q[i,5])
	#5%
	segments(i-0.15,ratio_S_P_q[i,1],i+0.15,ratio_S_P_q[i,1])
	segments(i,ratio_S_P_q[i,2],i,ratio_S_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_P_q[i,2],ratio_S_P_q[i,2],ratio_S_P_q[i,4],ratio_S_P_q[i,4]),col="orange")
	#median
	segments(i-0.3,ratio_S_P_q[i,3],i+0.3,ratio_S_P_q[i,3])
}

segments(11.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),23.5,S_juv_JP[3,12]/(S_juv_JP[1,12]+S_juv_JP[2,12]+S_juv_JP[3,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),29.5,S_juv_JP[3,24]/(S_juv_JP[1,24]+S_juv_JP[2,24]+S_juv_JP[3,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),(T+20.5),S_juv_JP[3,30]/(S_juv_JP[1,30]+S_juv_JP[2,30]+S_juv_JP[3,30]),col="grey35",lty=2,lwd=2)

#segments(12.5,mean(ratio_S_P[,13:37]),37.5,mean(ratio_S_P[,13:37]),col="red",lty=2,lwd=2)
abline(v=12.5,lty=3,col = "grey55")

#.............. 
# diff ratio
#..............
plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+20.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,T+7,T+20),
		labels=c(1975,1980,1990,2000,2013,2020,2033),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T+20,0.5,labels=expression(italic("i.")))

for(i in 13:T){
	#whiskers
	#95%
	segments(i-0.15,diff_ratio_P_q[i,5],i+0.15,diff_ratio_P_q[i,5])
	segments(i,diff_ratio_P_q[i,4],i,diff_ratio_P_q[i,5])
	#5%
	segments(i-0.15,diff_ratio_P_q[i,1],i+0.15,diff_ratio_P_q[i,1])
	segments(i,diff_ratio_P_q[i,2],i,diff_ratio_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_P_q[i,2],diff_ratio_P_q[i,2],diff_ratio_P_q[i,4],diff_ratio_P_q[i,4]),col="darkolivegreen3")
	#median
	segments(i-0.3,diff_ratio_P_q[i,3],i+0.3,diff_ratio_P_q[i,3])
}
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.15,diff_ratio_P_q[i,5],i+0.15,diff_ratio_P_q[i,5])
	segments(i,diff_ratio_P_q[i,4],i,diff_ratio_P_q[i,5])
	#5%
	segments(i-0.15,diff_ratio_P_q[i,1],i+0.15,diff_ratio_P_q[i,1])
	segments(i,diff_ratio_P_q[i,2],i,diff_ratio_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(diff_ratio_P_q[i,2],diff_ratio_P_q[i,2],diff_ratio_P_q[i,4],diff_ratio_P_q[i,4]),col="orange")
	#median
	segments(i-0.3,diff_ratio_P_q[i,3],i+0.3,diff_ratio_P_q[i,3])
}
abline(h=0,lty=2,col = "grey55")
abline(v=12.5,lty=3,col = "grey55")

dev.off()


