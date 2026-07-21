# TODO: Add comment
# 
# Author: marion.legrand
###############################################################################

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

#=============#
# CALIBRATION #
#=============#

IA=c(52,82,131,120,159,15,21,21,61)

d=read.coda("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/dCODAchain1.txt","C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/dCODAindex.txt",5001,10000)


q.d_1=quantile(d[,1],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_2=quantile(d[,2],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_3=quantile(d[,3],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_4=quantile(d[,4],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_5=quantile(d[,5],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_6=quantile(d[,6],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_7=quantile(d[,7],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_8=quantile(d[,8],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_9=quantile(d[,9],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)

q.d=rbind(
		q.d_1,q.d_2,q.d_3,q.d_4,q.d_5,q.d_6,q.d_7,q.d_8,q.d_9)


d_fake=read.coda("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/d_newCODAchain1.txt","C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/d_newCODAindex.txt",5001,10000)


#Graphes
#densities

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Calibration_relation.png",width=800, height=800, units = "px",type="cairo")

par(mfrow=c(1,2))

plot(1,1,type="n",axes=FALSE,xlim=c(0,300),xlab="IA 5 minutes",ylim=c(0,1.2),ylab="density (0+ / m-2)",main="IA vs 0+ density relationship")

# trace l'axe des ordonn�es
axis(2,at = seq(0,1.2,0.2),cex.axis = 0.8,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = seq(0,300,25),cex.axis = 0.8,las = 1,col = "black")


d_fake_q=array(rep(0,1500),dim=c(300,5) )

IA_fake=seq(1,300,1)

for (i in 1:300){
	
	d_fake_q[i,]=quantile(d_fake[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}

colnames(d_fake_q)=c("5%","25%","50%","75%","95%")

points(IA_fake,d_fake_q[,3],type="l",col="red",lwd=1.5)
points(IA_fake,d_fake_q[,1],type="l",col="red",lwd=1.5,lty=2)
points(IA_fake,d_fake_q[,5],type="l",col="red",lwd=1.5,lty=2)




k=2

for(i in 1:9){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="light grey")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
	
}

dev.off()

#autre m�thode plus rapide
q_d_v=array(rep(0,9*5),dim=c(9,5))
for(i in 1:9){
	q_d_v[i,]=quantile(d[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

write.table(d_fake_q, "C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/d_predict.csv")



#===========#
# REDDS	    #
#===========#

# parameters calculation

#Pas pr�sentes - � voir si utilis�
#mean_N_V=read.coda("mean_N_VCODAchain1.txt","mean_N_VCODAindex.txt")
#sigma_Vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

zone_effect_V=read.coda("zone_effect_VCODAchain1.txt","zone_effect_VCODAindex.txt")
zone_effect_A=read.coda("zone_effect_ACODAchain1.txt","zone_effect_ACODAindex.txt")
zone_effect_L=read.coda("zone_effect_LCODAchain1.txt","zone_effect_LCODAindex.txt")
zone_effect_P=read.coda("zone_effect_PCODAchain1.txt","zone_effect_PCODAindex.txt")


zone_effect_V_q=array(rep(0,T*5),dim=c(T,5))
zone_effect_A_q=array(rep(0,T*5),dim=c(T,5))
zone_effect_L_q=array(rep(0,T*5),dim=c(T,5))
zone_effect_P_q=array(rep(0,T*5),dim=c(T,5))



for (i in 1:T){
	zone_effect_V_q[i,]=quantile(zone_effect_V[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	zone_effect_A_q[i,]=quantile(zone_effect_A[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	zone_effect_L_q[i,]=quantile(zone_effect_L[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

for (i in 12:T){
	zone_effect_P_q[i,]=quantile(zone_effect_P[,i-11],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


mu_zone=read.coda("mu_zoneCODAchain1.txt","mu_zoneCODAindex.txt")
beta_zone=read.coda("beta_zoneCODAchain1.txt","beta_zoneCODAindex.txt")

mu_zone_q=array(rep(0,10),dim=c(1,5))

mu_zone_q[1,]=quantile(mu_zone[,1],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#mu_zone_q[2,]=quantile(mu_zone[,2],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
beta_zone_q=quantile(beta_zone,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)



N_alagnon=read.coda("N_ACODAchain1.txt","N_ACODAindex.txt",5001,10000)
N_langeac=read.coda("N_LCODAchain1.txt","N_LCODAindex.txt",5001,10000)


library(stringr)
bugs2jags("data.txt","data_4zones_2016.12.19.R")
source("data_4zones_2016.12.19.R")
C_langeac<-min_L[min_L>1]

p_count_L=array(rep(0,30000),dim=c(5000,6))

for (t in 1:6){
	p_count_L[,t]=C_langeac[t]/N_langeac[,26+t]
}

p_count_L_q=array(rep(0,35),dim=c(6,5))

for (t in 1:6){
	p_count_L_q[t,]=quantile(p_count_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

save.image(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/data/Redds_kappa_2016.12.19.RData")
load(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/data/Redds_kappa_2016.12.19.RData")

###########################
#plot zone_effect /kappa
###############################

pdf(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/illustrations/2015_01_24/Redds_kappa_2015_02_10.pdf")#,width=800, height=800, units = "px",type="cairo"


par(mfrow=c(4,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Ann�es",ylim=c(0,3),ylab=expression(italic( kappa["t,1"])),main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,1,2,3),labels=c(0,1,2,3),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(T-T+1,T-T+1+5,T-T+1+15,T-T+1+25,T-T+1+35,T),
		labels=c(1975,1980,1990,2000,2010,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,3,labels=expression(italic("a.")),col = "grey55")




xx=c(0:(T+1),(T+1):0)

q_2_5=rep(mu_zone_q[1,1],(T+2))
q_97_5=rep(mu_zone_q[1,5],(T+2))

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="rosybrown1",border="NA")

q_25=rep(mu_zone_q[1,2],(T+2))
q_75=rep(mu_zone_q[1,4],(T+2))

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="rosybrown3",border="NA")

points(x=seq(0,(T+1),1),y=rep(mu_zone_q[1,3],(T+2)),type="l",col="white")


for(i in 1:T){
	#whiskers
	#95%
	segments(i-0.15,zone_effect_V_q[i,5],i+0.15,zone_effect_V_q[i,5])
	segments(i,zone_effect_V_q[i,4],i,zone_effect_V_q[i,5])
	#5%
	segments(i-0.15,zone_effect_V_q[i,1],i+0.15,zone_effect_V_q[i,1])
	segments(i,zone_effect_V_q[i,2],i,zone_effect_V_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(zone_effect_V_q[i,2],zone_effect_V_q[i,2],zone_effect_V_q[i,4],zone_effect_V_q[i,4]),col="coral3")
	#median
	segments(i-0.3,zone_effect_V_q[i,3],i+0.3,zone_effect_V_q[i,3])
}

#####################
#####################
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Ann�es",ylim=c(0,3),ylab=expression(italic( kappa["t,1"])),main="Alagnon",cex.main=2,cex.lab=2)

# trace l'axe des ordonn�es
axis(2,at = c(0,1,2,3),labels=c(0,1,2,3),cex.axis=2,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,(T-1)),
		labels=lab1,
		cex.axis=2,las = 1,col = "grey55")

text(T,3,labels=expression(italic("b.")),col = "grey55")

xx=c(0:(T+1),(T+1):0)

q_2_5=rep(mu_zone_q[1,1],(T+2))
q_97_5=rep(mu_zone_q[1,5],(T+2))

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="rosybrown1",border="NA")

q_25=rep(mu_zone_q[1,2],(T+2))
q_75=rep(mu_zone_q[1,4],(T+2))

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="rosybrown3",border="NA")

points(x=seq(0,(T+1),1),y=rep(mu_zone_q[1,3],(T+2)),type="l",col="white")

for(i in 1:T){
	#whiskers
	#95%
	segments(i-0.15,zone_effect_A_q[i,5],i+0.15,zone_effect_A_q[i,5])
	segments(i,zone_effect_A_q[i,4],i,zone_effect_A_q[i,5])
	#5%
	segments(i-0.15,zone_effect_A_q[i,1],i+0.15,zone_effect_A_q[i,1])
	segments(i,zone_effect_A_q[i,2],i,zone_effect_A_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(zone_effect_A_q[i,2],zone_effect_A_q[i,2],zone_effect_A_q[i,4],zone_effect_A_q[i,4]),col="coral3")
	#median
	segments(i-0.3,zone_effect_A_q[i,3],i+0.3,zone_effect_A_q[i,3])
}

#####################
#####################

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,(T+0.5)),xlab="Ann�es",ylim=c(0,3),ylab=expression(italic(kappa["t,2"])),main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,1,2,3),labels=c(0,1,2,3),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(T-T+1,T-T+1+5,T-T+1+15,T-T+1+25,T-T+1+35,T),
		labels=c(1975,1980,1990,2000,2010,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


text(T,4,labels=expression(italic("b.")),col = "grey55")


xx=c(0:(T+1),(T+1):0)

q_2_5=rep(mu_zone_q[1,1],T+2)
q_97_5=rep(mu_zone_q[1,5],T+2)

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="rosybrown1",border="NA")


q_25=rep(mu_zone_q[1,2],T+2)
q_75=rep(mu_zone_q[1,4],T+2)

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="rosybrown3",border="NA")

points(x=seq(0,T+1,1),y=rep(mu_zone_q[1,3],T+2),type="l",col="white")





for(i in 1:T){
	#whiskers
	#95%
	segments(i-0.15,zone_effect_L_q[i,5],i+0.15,zone_effect_L_q[i,5])
	segments(i,zone_effect_L_q[i,4],i,zone_effect_L_q[i,5])
	#5%
	segments(i-0.15,zone_effect_L_q[i,1],i+0.15,zone_effect_L_q[i,1])
	segments(i,zone_effect_L_q[i,2],i,zone_effect_L_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(zone_effect_L_q[i,2],zone_effect_L_q[i,2],zone_effect_L_q[i,4],zone_effect_L_q[i,4]),col="coral3")
	#median
	segments(i-0.3,zone_effect_L_q[i,3],i+0.3,zone_effect_L_q[i,3])
}



#####################
#####################

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Ann�es",ylim=c(0,6),ylab=expression(italic(kappa["t,3"])),main="Amont de Pout�s")

# trace l'axe des ordonn�es
axis(2,at = c(0,1,2,3,4,5,6),labels=c(0,1,2,3,4,5,6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(T-T+1,T-T+1+5,T-T+1+15,T-T+1+25,T-T+1+35,T),
		labels=c(1975,1980,1990,2000,2010,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


text(T,6,labels=expression(italic("c.")),col = "grey55")



#mu_zone[2]=1
#temp=c(11.5,12:(T+1))
#temp1=c(rev(12:(T+1)),11.5)
#xx=c(temp,temp1)
#q_2_5=rep(mu_zone_q[2,1],(T-11))
#q_97_5=rep(mu_zone_q[2,5],(T-11))
#yy=c(q_2_5,rev(q_97_5))
#polygon(xx,yy,col="yellowgreen",border="NA")
#q_25=rep(mu_zone_q[2,2],T+1-12+2)
#q_75=rep(mu_zone_q[2,4],T+1-12+2)
#yy=c(q_25,rev(q_75))
#polygon(xx,yy,col="palegreen4",border="NA")
#points(seq(11.5,T+1,0.5),rep(mu_zone_q[2,3],(T+1-11)*2),type="l",col="white")

points(seq(11.5,T+1,0.5),rep(1,(T-10)*2),type="l",col="palegreen4")


for(i in 12:T){
	#whiskers
	#95%
	segments(i-0.15,zone_effect_P_q[i,5],i+0.15,zone_effect_P_q[i,5])
	segments(i,zone_effect_P_q[i,4],i,zone_effect_P_q[i,5])
	#5%
	segments(i-0.15,zone_effect_P_q[i,1],i+0.15,zone_effect_P_q[i,1])
	segments(i,zone_effect_P_q[i,2],i,zone_effect_P_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(zone_effect_P_q[i,2],zone_effect_P_q[i,2],zone_effect_P_q[i,4],zone_effect_P_q[i,4]),col="darkolivegreen3")
	#median
	segments(i-0.3,zone_effect_P_q[i,3],i+0.3,zone_effect_P_q[i,3])
}

abline(v=11.5,lty=2)

text(4,3,paste( "Amont de Poutes\n inaccessible"))

dev.off()




#plot mu_zone -->ne sert plus car mu_zone[2]=1
#png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Redds_MuZone.png",width=800, height=800, units = "px",type="cairo")
#par(mfrow=c(1,2),mar=c(4,5,2,2))
#plot(NULL,ylab="Density",xlab=expression(mu^kappa),xlim=c(0.2,2),ylim=c(0,3.5))
#d_1=density(mu_zone[,1],bw=0.04)
#d_2=density(mu_zone[,2],bw=0.04)
#polygon(d_1, col=rgb(139,137,137,50,maxColorValue=255))
#polygon(d_2, col=rgb(139,137,137,50,maxColorValue=255))
#text(0.84,3.4,expression(mu^kappa_down))
#text(1.5,2.3,expression(mu^kappa_up))
#plot(as.numeric(mu_zone[,1]),as.numeric(mu_zone[,2]),xlim=c(0.2,2.5),ylim=c(0.2,2.5),xlab=expression(mu^kappa_down),ylab=expression(mu^kappa_up),col=rgb(139,137,137,75,maxColorValue=255),pch=16)
#plot(as.numeric(mu_zone[,1]),xlim=c(0.2,2.5),ylim=c(0.2,2.5),xlab=expression(mu^kappa_down),ylab=expression(mu^kappa_up),col=rgb(139,137,137,75,maxColorValue=255),pch=16)
#x=seq(0.2,2.5,0.0001)
#points(x,x,type="l",lty=2,lwd=2)
#dev.off()


######################################################################
## Calcul des probas de passer aux differentes stations de comptage ##
###################################################################### 

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

# Extraction of CODAs
library(boot)

L_mu_p_langeac=read.coda("L_mu_p_langeacCODAchain1.txt","L_mu_p_langeacCODAindex.txt")
L_mu_p_poutes=read.coda("L_mu_p_poutesCODAchain1.txt","L_mu_p_poutesCODAindex.txt")

mu_p_langeac=inv.logit(L_mu_p_langeac)
mu_p_poutes=inv.logit(L_mu_p_poutes)

mu_p_langeac_q=quantile(mu_p_langeac,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
mu_p_poutes_q=quantile(mu_p_poutes,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)


sigma_p_langeac=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
sigma_p_poutes=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")


sigma_p_langeac_q=quantile(sigma_p_langeac,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
sigma_p_poutes_q=quantile(sigma_p_poutes,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)


p_langeac=read.coda("p_langeacCODAchain1.txt","p_langeacCODAindex.txt")
p_poutes=read.coda("p_poutesCODAchain1.txt","p_poutesCODAindex.txt")

p_langeac_q=array(rep(0,T*5),dim=c(T,5))
p_poutes_q=array(rep(0,T*5),dim=c(T,5))


for (i in 1:T){
	p_langeac_q[i,]=quantile(p_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


for (i in 12:T){
	p_poutes_q[i,]=quantile(p_poutes[,i-11],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}





#Graph with all years

#####################
#p_langeac
#####################

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Redds_ProbaPassageStation.png",width=800, height=800, units = "px",type="cairo")


par(mfrow=c(2,1),mar=c(4,6.1,0,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="",ylim=c(0,1),ylab=expression(italic(p^L)),main="")
text(T+0.5,1,labels=expression(italic("a.")),col = "grey55")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")



xx=c(0:(T+1),(T+1):0)

q_2_5=rep(mu_p_langeac_q[1],T+2)
q_97_5=rep(mu_p_langeac_q[5],T+2)

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="slategray1",border="NA")


q_25=rep(mu_p_langeac_q[2],T+2)
q_75=rep(mu_p_langeac_q[4],T+2)

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="slategray3",border="NA")

points(x=seq(0,T+1,1),y=rep(mu_p_langeac_q[3],T+2),type="l",col="white")





for(i in 1:T){
	#whiskers
	#95%
	segments(i-0.15,p_langeac_q[i,5],i+0.15,p_langeac_q[i,5])
	segments(i,p_langeac_q[i,4],i,p_langeac_q[i,5])
	#5%
	segments(i-0.15,p_langeac_q[i,1],i+0.15,p_langeac_q[i,1])
	segments(i,p_langeac_q[i,2],i,p_langeac_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(p_langeac_q[i,2],p_langeac_q[i,2],p_langeac_q[i,4],p_langeac_q[i,4]),col="steelblue4")
	#median
	segments(i-0.3,p_langeac_q[i,3],i+0.3,p_langeac_q[i,3])
}

#####################
#p_poutes
#####################


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^P)),main="")

text(T+0.5,1,labels=expression(italic("b.")),col = "grey55")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


temp=c(11.5,12:(T+1))

temp1=c(rev(12:(T+1)),11.5)


xx=c(temp,temp1)

q_2_5=rep(mu_p_poutes_q[1],T+1-12+2)
q_97_5=rep(mu_p_poutes_q[5],T+1-12+2)

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="slategray1",border="NA")


q_25=rep(mu_p_poutes_q[2],T+1-12+2)
q_75=rep(mu_p_poutes_q[4],T+1-12+2)

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="slategray3",border="NA")



points(seq(11.5,T+1,0.5),rep(mu_p_poutes_q[3],(T+1-12+1)*2),type="l",col="white")





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

abline(v=11.5,lty=2)

text(6,0.55,paste( "upstream Poutès\n inaccessible"),col = "grey25")

dev.off()

#=======================#
# CODE_R_SPAWNERS_REDDS #
#=======================#

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_alagnon_real=read.coda("S_alagnonCODAchain1.txt","S_alagnonCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")

S_vichy_q=array(rep(0,T*5),dim=c(T,5))
S_alagnon_q=array(rep(0,T*5),dim=c(T,5))
S_langeac_q=array(rep(0,T*5),dim=c(T,5))


library(stringr)
bugs2jags("data.txt","data_4zones_2016.12.19.R")
source("data_4zones_2016.12.19.R")
S_poutes_counter=N[,4]


ratio_S_V=array(0,c(5000,T))
ratio_S_A=array(0,c(5000,T))
ratio_S_L=array(0,c(5000,T))
ratio_S_P=array(0,c(5000,T))


#ratio

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
		ratio_S_P[i,t] = S_poutes_counter[t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
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


for (i in 1:T){
	S_vichy_q[i,]=quantile(S_vichy_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	S_alagnon_q[i,]=quantile(S_alagnon_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	S_langeac_q[i,]=quantile(S_langeac_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}


save.image(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/data/SpawnersRedds_GeniteursPotentiels_2016.12.19.RData")


#Graph with all years
surf=c(rep(c(916866,250441,0),11),rep(c(916866,250441,301101),12),rep(c(916866,250441,383049),6),rep(c(1202540,250441,383049),31))
S_juv_JP<-matrix(surf,nrow=3)	


pdf(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/illustrations/2015_01_24/SpawnersRedds_GeniteursPotentiels_2015_02_10.pdf")

par(mfrow=c(4,2),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")
#........
# Vichy
#........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,6500),ylab=expression(italic(S["t,1"])),main="Vichy-Langeac",cex.main=2,cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000),labels=c(0,1000,2000,3000,4000,5000,6000),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,T),
		labels=lab1,
		cex.axis = 1.4,las = 1,col = "grey55")

text(T,6500,labels=expression(italic("a.")),col = "grey55",cex=2)

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


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Vichy-Langeac",cex.main=2,cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,1974+T),
		cex.axis = 1.4,las = 1,col = "grey55")

text(T,1,labels=expression(italic("d.")),col = "grey55",cex=2)

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


segments(0,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[1,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[1,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[1,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[1,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)

segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)		
segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	

abline(v=11.5,lty=3)

#........
# Alagnon
#........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,800),ylab=expression(italic(S["t,1"])),main="Alagnon",cex.main=2,cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,200,400,600,800),labels=c(0,200,400,600,800),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,T),
		labels=lab1,
		cex.axis = 1.4,las = 1,col = "grey55")

text(T,1000,labels=expression(italic("b.")),col = "grey55")

for(i in 3:22){
	#whiskers
	#95%
	segments(i-0.15,S_alagnon_q[i,5],i+0.15,S_alagnon_q[i,5])
	segments(i,S_alagnon_q[i,4],i,S_alagnon_q[i,5])
	#5%
	segments(i-0.15,S_alagnon_q[i,1],i+0.15,S_alagnon_q[i,1])
	segments(i,S_alagnon_q[i,2],i,S_alagnon_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_alagnon_q[i,2],S_alagnon_q[i,2],S_alagnon_q[i,4],S_alagnon_q[i,4]),col="gray")
	#median
	segments(i-0.3,S_alagnon_q[i,3],i+0.3,S_alagnon_q[i,3])
}

for(i in 23:T){
	#whiskers
	#95%
	segments(i-0.15,S_alagnon_q[i,5],i+0.15,S_alagnon_q[i,5])
	segments(i,S_alagnon_q[i,4],i,S_alagnon_q[i,5])
	#5%
	segments(i-0.15,S_alagnon_q[i,1],i+0.15,S_alagnon_q[i,1])
	segments(i,S_alagnon_q[i,2],i,S_alagnon_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(S_alagnon_q[i,2],S_alagnon_q[i,2],S_alagnon_q[i,4],S_alagnon_q[i,4]),col="coral3")
	#median
	segments(i-0.3,S_alagnon_q[i,3],i+0.3,S_alagnon_q[i,3])
}


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Alagnon",cex.main=2,cex.lab=2)

# trace l'axe des ordonn�es
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
	segments(i-0.15,ratio_S_A_q[i,5],i+0.15,ratio_S_A_q[i,5])
	segments(i,ratio_S_A_q[i,4],i,ratio_S_A_q[i,5])
	#5%
	segments(i-0.15,ratio_S_A_q[i,1],i+0.15,ratio_S_A_q[i,1])
	segments(i,ratio_S_A_q[i,2],i,ratio_S_A_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(ratio_S_A_q[i,2],ratio_S_A_q[i,2],ratio_S_A_q[i,4],ratio_S_A_q[i,4]),col="coral3")
	#median
	segments(i-0.3,ratio_S_A_q[i,3],i+0.3,ratio_S_A_q[i,3])
}

segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)		
segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	

segments(0,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[2,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[2,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[2,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[2,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)


abline(v=11.5,lty=3)

#..........
# Langeac
#..........
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,1200),ylab=expression(italic(S["t,2"])),main="Langeac-Poutes",cex.main=2,cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,300,600,900,1200),labels=c(0,300,600,900,1200),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,T),
		labels=lab1,
		cex.axis = 1.4,las = 1,col = "grey55")


text(T,1200,labels=expression(italic("b.")),col = "grey55",cex=2)



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

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,T),
		labels=lab1,
		cex.axis =1.4,las = 1,col = "grey55")

text(T,1,labels=expression(italic("e.")),col = "grey55",cex=2)

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

segments(0,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),11.5,S_juv_JP_new[3,1]/(S_juv_JP_new[1,1]+S_juv_JP_new[2,1]+S_juv_JP_new[3,1]),col="grey35",lty=2,lwd=2)
segments(11.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[3,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[3,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[3,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)

segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)		
segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	

abline(v=11.5,lty=3)
#..........
# Poutes
#..........

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,200),ylab=expression(italic(S["t,3"])),main="Upstream Poutes",cex.main=2,cex.lab=1.5)

# trace l'axe des ordonn�es
axis(2,at = c(0,100,200),labels=c(0,100,200),cex.axis = 1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
lab1=c(1975,1980,1990,2000,1974+T)
axis(1,at = c(1,6,16,26,T),
		labels=lab1,
		cex.axis = 1.4,las = 1,col = "grey55")

points(x=seq(12,T,1),y=S_poutes_counter[12:T],pch=16,col="darkolivegreen3") 
abline(v=11.5,lty=2)
text(6,100,paste( "upstream Poutes\n inaccessible"),col = "grey55")


text(T,200,labels=expression(italic("c.")),col = "grey55",cex=2)

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Poutes",cex.main=2,cex.lab=2)

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis =1.4,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,1974+T),
		cex.axis =1.4,las = 1,col = "grey55")

text(T,1,labels=expression(italic("f.")),col = "grey55",cex=2)
text(6,0.55,paste( "Amont Poutes\n inaccessible"),col = "grey55")


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

segments(0,0.5,40,0.5,col="deepskyblue",lty=2,lwd=2)		
segments(0,0.2,40,0.2,col="deepskyblue",lty=2,lwd=2)	

segments(11.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),23.5,S_juv_JP_new[4,12]/(S_juv_JP_new[1,12]+S_juv_JP_new[2,12]+S_juv_JP_new[3,12]+S_juv_JP_new[4,12]),col="grey35",lty=2,lwd=2)
segments(23.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),29.5,S_juv_JP_new[4,24]/(S_juv_JP_new[1,24]+S_juv_JP_new[2,24]+S_juv_JP_new[3,24]+S_juv_JP_new[4,24]),col="grey35",lty=2,lwd=2)
segments(29.5,S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),(T+0.5),S_juv_JP_new[4,30]/(S_juv_JP_new[1,30]+S_juv_JP_new[2,30]+S_juv_JP_new[3,30]+S_juv_JP_new[4,30]),col="grey35",lty=2,lwd=2)

abline(v=11.5,lty=3)

dev.off()


#########################
# Relation stock recrutement
#########################

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

dmoy_wild_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt",5001,10000)
dmoy_wild_A=read.coda("dmoywild_ACODAchain1.txt","dmoywild_ACODAindex.txt",5001,10000)
dmoy_wild_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt",5001,10000)
dmoy_wild_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt",5001,10000)

stocking_juv_V=read.coda("stocking_juv_VCODAchain1.txt","stocking_juv_VCODAindex.txt",5001,10000)
stocking_juv_A=read.coda("stocking_juv_ACODAchain1.txt","stocking_juv_ACODAindex.txt",5001,10000)
stocking_juv_L=read.coda("stocking_juv_LCODAchain1.txt","stocking_juv_LCODAindex.txt",5001,10000)
stocking_juv_P=read.coda("stocking_juv_PCODAchain1.txt","stocking_juv_PCODAindex.txt",5001,10000)

a=read.coda("aCODAchain1.txt","aCODAindex.txt",5001,10000)
a_juv=read.coda("a_juvCODAchain1.txt","a_juvCODAindex.txt",5001,10000)

Rmax=read.coda("RmaxCODAchain1.txt","RmaxCODAindex.txt",5001,10000)

nu_wild=read.coda("nu_wildCODAchain1.txt","nu_wildCODAindex.txt",5001,10000)

dmoy_wild_V_q=array(rep(0,T*5),dim=c(T,5))
dmoy_wild_A_q=array(rep(0,T*5),dim=c(T,5))
dmoy_wild_L_q=array(rep(0,T*5),dim=c(T,5))
dmoy_wild_P_q=array(rep(0,T*5),dim=c(T,5))

dmoy_juv_V_q=array(rep(0,T*5),dim=c(T,5))
dmoy_juv_A_q=array(rep(0,T*5),dim=c(T,5))
dmoy_juv_L_q=array(rep(0,T*5),dim=c(T,5))
dmoy_juv_P_q=array(rep(0,T*5),dim=c(T,5))

for (i in 1:(T-1)){
	dmoy_wild_V_q[i+1,]=quantile(dmoy_wild_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_wild_A_q[i+1,]=quantile(dmoy_wild_A[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_wild_L_q[i+1,]=quantile(dmoy_wild_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
	dmoy_juv_V_q[i+1,]=quantile(stocking_juv_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_juv_A_q[i+1,]=quantile(stocking_juv_A[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_juv_L_q[i+1,]=quantile(stocking_juv_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}


for (i in 1:(T-12)){
	
	dmoy_wild_P_q[i+12,]=quantile(dmoy_wild_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_juv_P_q[i+12,]=quantile(stocking_juv_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

library(stringr)
bugs2jags("data.txt","data_4zones_2016.12.19.R")
source("data_4zones_2016.12.19.R")
I_juv_V=which(I_juv_moy[,1]==1)
I_juv_A=which(I_juv_moy[,2]==1)
I_juv_L=which(I_juv_moy[,3]==1)
I_juv_P=which(I_juv_moy[,4]==1)
		

stocked_juv_V=stock_juv[,1]
stocked_juv_A=stock_juv[,2]
stocked_juv_L=stock_juv[,3]
stocked_juv_P=stock_juv[,4]

Mat=cbind(stocked_juv_V,stocked_juv_A,stocked_juv_L,stocked_juv_P)

stocked_juv_V_d=array(0,T)
stocked_juv_A_d=array(0,T)
stocked_juv_L_d=array(0,T)
stocked_juv_P_d=array(0,T)

for(t in 1:T){
	stocked_juv_V_d[t]=Mat[t,1]/S_juv_JP[t,1]
	stocked_juv_A_d[t]=Mat[t,2]/S_juv_JP[t,2]
	stocked_juv_L_d[t]=Mat[t,3]/S_juv_JP[t,3]
	stocked_juv_P_d[t]=Mat[t,4]/S_juv_JP[t,4]
	}



save.image(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/data/dd_2016.12.19.RData")


pdf(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/illustrations/2015_01_24/dd_2016_03_08.pdf")#,width=800,height=800)
par(mfrow=c(2,1),mar=c(4,5.5,2,1.5))

#Graphe densite dependence juvenile sauvage
plot(1,1,type="n",axes=FALSE,xlim=c(0,0.0025),xlab=expression(paste(S," ",(geniteurs.m^-2))),ylim=c(0,0.35),ylab=expression(paste(d^sauvage," (0+",.m^-2,")")),main="relation de densité dépendance des 0+ sauvages",cex.main=1.5,cex.lab=1.8)
# trace l'axe des ordonnées
axis(1,at = c(0,0.00050,0.0010,0.0015, 0.002, 0.0025),
		labels=c(0,expression(paste(0.5," x ",10^-3)),expression(10^-3),expression(paste(1.5," x ",10^-3)),expression(paste(2," x ",10^-3)),expression(paste(2.5," x ",10^-3)) ),
		cex.axis = 1.2,las = 1,lwd=2)
# trace l'axe des abscisses
axis(2,at = c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35),
		labels=c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35),
		cex.axis = 1.2,las = 1,lwd=2)

points(S_vichy_q[1:(T-1),3]/S_juv_JP[1,1:(T-1)],dmoy_wild_V_q[2:T,3],pch=16,col="coral3")#"gray15")

points(S_langeac_q[1:(T-1),3]/S_juv_JP[2,1:(T-1)],dmoy_wild_L_q[2:T,3],pch=16,col="darkolivegreen4")#"gray45")

points(S_poutes_counter[12:(T-1)]/S_juv_JP[3,12:(T-1)],dmoy_wild_P_q[13:T,3],pch=16,col="darkolivegreen3")#"gray75")

x=seq(0,0.0025,0.0000001)

y=exp(log(x/(1/median(a) + x * 1/median(Rmax))) + median(nu_wild[,1]))
points(x,y,type="l",col="coral3")#"gray15")

y=exp(log(x/(1/median(a) + x * 1/median(Rmax))) + median(nu_wild[,2]))
points(x,y,type="l",col="darkolivegreen4")#"gray45")


text(0.0025,0.35,labels=expression(italic("a.")),col = "grey55",cex=1.5)


#Graphe densite dependence juvenile repeuplement

plot(1,1,type="n",axes=FALSE,xlim=c(0,1.4),xlab=expression(paste(Stock^juv," ",(0+.m^-2))),ylim=c(0,0.35),ylab=expression(paste(d^juv," (0+",.m^-2,")")),main="relation de densité dépendance des 0+ repeuplés",cex.main=1.5,cex.lab=1.8)
# trace l'axe des abcisses
axis(1,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4),
		labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4),
		cex.axis = 1.2,las = 1)
# trace l'axe des ordonnées
axis(2,at = c(0,0.05,0.10,0.15,0.2,0.25,0.3,0.35),
		labels=c(0,0.05,0.10,0.15,0.20,0.25,0.3,0.35),
		cex.axis = 1.2,las = 1)

points(stocked_juv_V_d[I_juv_V],dmoy_juv_V_q[I_juv_V,3],pch=15,col="coral3")

points(stocked_juv_L_d[I_juv_L],dmoy_juv_L_q[I_juv_L,3],pch=15,col="darkolivegreen4")

points(stocked_juv_P_d[I_juv_P],dmoy_juv_P_q[I_juv_P,3],pch=15,col="darkolivegreen3")

#x=seq(0,0.35,0.0001)

points(x,x,type="l",lty=2,col="grey15")

x=seq(0,1.4,0.0001)

y=  x / ( ( 1 / exp(median(nu_wild[,1]))) * 1/median(a_juv) + x * 1/median(Rmax)  )   
points(x,y,type="l",col="coral3")


y=  x / ( ( 1 / exp(median(nu_wild[,2]))) * 1/median(a_juv) + x * 1/median(Rmax)  )   
points(x,y,type="l",col="darkolivegreen4")


text(1.4,0.35,labels=expression(italic("b.")),col = "grey55",cex=1.5)

#legend(0,0.2,bg="white",legend=c("Vichy-Langeac","Langeac-Poutes","upstream Poutes","downstream Langeac","upstream Langeac"),lty=c(NA,NA,NA,1,1),pch=c(15,15,15,NA,NA),col=c("gray15","gray45","gray75","gray15","gray75"))

dev.off()

moy_juv1=mean(x / ( ( 1 / exp(median(nu_wild[,1]))) * 1/median(a_juv) + x * 1/median(Rmax)  ) )
moy_wild1=mean(exp(log(x/(1/median(a) + x * 1/median(Rmax))) + median(nu_wild[,1])))
moy_juv2=mean( x / ( ( 1 / exp(median(nu_wild[,2]))) * 1/median(a_juv) + x * 1/median(Rmax)  )   )
moy_wild2=mean(exp(log(x/(1/median(a) + x * 1/median(Rmax))) + median(nu_wild[,2])))
tab<-cbind(moy_juv1,moy_juv2,moy_wild1,moy_wild2)
#calcul des CVs

CV_V=rep(0,T)
CV_L=rep(0,T)

for (i in 1:T){
	CV_V[i]=sd(S_vichy_real[,i])/mean(S_vichy_real[,i])
	CV_L[i]=sd(S_langeac_real[,i])/mean(S_langeac_real[,i])
}


x=seq(1,T,1)



data_vichy=seq(23,T)
#c(23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)

no_data_vichy=seq(1,22)
#no_data_vichy=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)

#data_langeac=c(27,28,29,30,31,32)
data_langeac=seq(27,32)
#no_data_langeac=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,33,34,35,36,37) 
no_data_langeac=seq(1,T) 

par(mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(0,0,type="n",axes=FALSE,ylim=c(0,1.2),xlim=c(0,T),pch=16,ylab="CV",xlab="Years")

axis(2,at = c(0,0.2,0.4,0.6,0.8,1,1.2),labels=c(0,0.2,0.4,0.6,0.8,1,1.2),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c("1975",1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")




points(data_langeac,CV_L[data_langeac[]], pch=15,col="grey70")
points(no_data_langeac,CV_L[no_data_langeac[]], pch=0,col="grey70")

segments(x[1:(T-1)]+0.15,CV_L[1:(T-1)],x[2:T]-0.15,CV_L[2:T],col="grey70",lty=2)


points(data_vichy,CV_V[data_vichy[]], pch=16)
points(no_data_vichy,CV_V[no_data_vichy[]], pch=1)

segments(x[1:(T-1)]+0.15,CV_V[1:(T-1)],x[2:T]-0.15,CV_V[2:T],lty=2)


mean(CV_V[no_data_vichy[]])
mean(CV_L[no_data_langeac[]])

mean(CV_V[data_vichy[]])
mean(CV_L[data_langeac[]])




points(x[21:T],CV_V[21:T],col="red", pch=16)



plot(x,CV_L,axes=FALSE,main="CV spawners Langeac",ylim=c(0,0.7),pch=16,ylab="",xlab="Years")

axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,37),
		labels=c("",1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "black")

points(x[27:32],CV_L[27:32],col="red", pch=16)


p_redd_V=c(0.429,0.673,0.551,0.296,0.898,0,0.898,0.306,0.898,0.265,
		0.265,0.276,0.429,0.429,0.429,0.429,0.429,0.429,0,0,0.612,0.898,
		0.745,0.673,0.704,0,0,0.889,0.746,0.873,1,0,0.865,0,0)
p_redd_L=c(0.448,0.414,0.414,0.862,1,0,1,0.862,1,1,1,1,1,0.862,0.862,
		0.793,0.690,0.690,0,0,1,1,1,1,1,0,0,1,1,1,1,0,1,0,0)


plot(p_redd_V,CV_V,pch=16)
plot(p_redd_L,CV_L,pch=16)



#================================#
# SPAWNERS - PRODUCTION JUVENILE #
#================================#
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41


S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_alagnon_real=read.coda("S_alagnonCODAchain1.txt","S_alagnonCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")



S_vichy_q=array(0,dim=c(T,5))
S_alagnon_q=array(0,dim=c(T,5))
S_langeac_q=array(0,dim=c(T,5))


library(stringr)
bugs2jags("data.txt","data_4zones_2016.12.19.R")
source("data_4zones_2016.12.19.R")

S_poutes_counter=N[,4]



ratio_S_V=array(0,c(5000,T))
ratio_S_A=array(0,c(5000,T))
ratio_S_L=array(0,c(5000,T))
ratio_S_P=array(0,c(5000,T))


#ratio

for (t in 1:12){
	
	for(i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] +  S_alagnon_real[i,t] + S_langeac_real[i,t])
		ratio_S_A[i,t] = S_alagnon_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t])
		ratio_S_L[i,t] = S_langeac_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t])
	}
}


for (t in 13:T){
	
	for( i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_A[i,t] = S_alagnon_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_L[i,t] = S_langeac_real[i,t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_P[i,t] = S_poutes_counter[t] / (S_vichy_real[i,t] + S_alagnon_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
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



juv_tot_V=read.coda("simulation/juv_tot_VCODAchain1.txt","simulation/juv_tot_VCODAindex.txt")
juv_tot_A=read.coda("simulation/juv_tot_ACODAchain1.txt","simulation/juv_tot_ACODAindex.txt")
juv_tot_L=read.coda("simulation/juv_tot_LCODAchain1.txt","simulation/juv_tot_LCODAindex.txt")
juv_tot_P=read.coda("simulation/juv_tot_PCODAchain1.txt","simulation/juv_tot_PCODAindex.txt")


ratio_juv_tot_V=array(0,c(5000,T))
ratio_juv_tot_A=array(0,c(5000,T))
ratio_juv_tot_L=array(0,c(5000,T))
ratio_juv_tot_P=array(0,c(5000,T))


#ratio

for (t in 7:15){
	
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = juv_tot_V[i,t-6] / (juv_tot_V[i,t-6] +  juv_tot_A[i,t-6] +juv_tot_L[i,t-6])
		ratio_juv_tot_A[i,t] = juv_tot_A[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_A[i,t-6] + juv_tot_L[i,t-6])
		ratio_juv_tot_L[i,t] = juv_tot_L[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_A[i,t-6] + juv_tot_L[i,t-6])
	}
}

for (t in 16:T){
	
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = juv_tot_V[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_A[i,t-6] + juv_tot_L[i,t-6] + juv_tot_P[i,t-15])
		ratio_juv_tot_A[i,t] = juv_tot_A[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_A[i,t-6] + juv_tot_L[i,t-6] + juv_tot_P[i,t-15])
		ratio_juv_tot_L[i,t] = juv_tot_L[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_A[i,t-6] + juv_tot_L[i,t-6] + juv_tot_P[i,t-15])
		ratio_juv_tot_P[i,t] = 1 - ratio_juv_tot_V[i,t] - ratio_juv_tot_A[i,t] - ratio_juv_tot_L[i,t]
	}
}





ratio_juv_tot_V_q=array(0,c(T,5))
ratio_juv_tot_A_q=array(0,c(T,5))
ratio_juv_tot_L_q=array(0,c(T,5))
ratio_juv_tot_P_q=array(0,c(T,5))

for (t in 7:T){
	
	ratio_juv_tot_V_q[t,]=quantile(ratio_juv_tot_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_juv_tot_A_q[t,]=quantile(ratio_juv_tot_A[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_juv_tot_L_q[t,]=quantile(ratio_juv_tot_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
	
}

for (t in 16:T){
	ratio_juv_tot_P_q[t,]=quantile(ratio_juv_tot_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}



diff_ratio_V=ratio_S_V-ratio_juv_tot_V
diff_ratio_A=ratio_S_A-ratio_juv_tot_A
diff_ratio_L=ratio_S_L-ratio_juv_tot_L
diff_ratio_P=ratio_S_P-ratio_juv_tot_P

diff_ratio_V_q=array(0,c(T,5))
diff_ratio_A_q=array(0,c(T,5))
diff_ratio_L_q=array(0,c(T,5))
diff_ratio_P_q=array(0,c(T,5))

for (t in 1:T){
	
	diff_ratio_V_q[t,]=quantile(diff_ratio_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	diff_ratio_A_q[t,]=quantile(diff_ratio_A[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	diff_ratio_L_q[t,]=quantile(diff_ratio_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	diff_ratio_P_q[t,]=quantile(diff_ratio_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
	
}

save.image(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/data/JuvProdRatioGeni_2016.12.19.RData")



#Graph with all years

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/SpawnersProdJuv_ratio.png",width=800, height=800, units = "px",type="cairo")


par(mfrow=c(3,3),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")



S_juv_JP_old=c(2574236,434552,655661)

#----------
# Vichy
#---------

#########
# Prod juveniles


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,1,labels=expression(italic("a.")),col = "grey55")



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


segments(7,S_juv_JP[1]/sum(S_juv_JP[1:2]),12.5,S_juv_JP[1]/sum(S_juv_JP[1:2]),col="grey25",lty=2,lwd=2)
segments(12.5,S_juv_JP[1]/sum(S_juv_JP[]),T+0.5,S_juv_JP[1]/sum(S_juv_JP[]),col="grey25",lty=2,lwd=2)



#segments(7,mean(ratio_juv_tot_V[,7:12]),12.5,mean(ratio_juv_tot_V[,7:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_juv_tot_V[,13:37]),37.5,mean(ratio_juv_tot_V[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")


## 
# Spawners

plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,1,labels=expression(italic("b.")),col = "grey55")



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

segments(0,S_juv_JP[1]/sum(S_juv_JP[1:2]),12.5,S_juv_JP[1]/sum(S_juv_JP[1:2]),col="grey25",lty=2,lwd=2)
segments(12.5,S_juv_JP[1]/sum(S_juv_JP[]),T+0.5,S_juv_JP[1]/sum(S_juv_JP[]),col="grey25",lty=2,lwd=2)

#segments(0,mean(ratio_S_V[,1:12]),12.5,mean(ratio_S_V[,1:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_S_V[,13:37]),37.5,mean(ratio_S_V[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")


## 
# diff ratio



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
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


abline(h=0,lty=2,col = "grey55")
abline(v=12.5,lty=3,col = "grey55")

#----------
# Langeac
#---------


#########
# Prod juveniles


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,1,labels=expression(italic("d.")),col = "grey55")



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


segments(7,S_juv_JP[2]/sum(S_juv_JP[1:2]),12.5,S_juv_JP[2]/sum(S_juv_JP[1:2]),col="grey25",lty=2,lwd=2)
segments(12.5,S_juv_JP[2]/sum(S_juv_JP[]),T+0.5,S_juv_JP[2]/sum(S_juv_JP[]),col="grey25",lty=2,lwd=2)

#segments(7,mean(ratio_juv_tot_L[,7:12]),12.5,mean(ratio_juv_tot_L[,7:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_juv_tot_L[,13:37]),37.5,mean(ratio_juv_tot_L[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")







plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,1,labels=expression(italic("e.")),col = "grey55")



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



segments(0,S_juv_JP[2]/sum(S_juv_JP[1:2]),12.5,S_juv_JP[2]/sum(S_juv_JP[1:2]),col="grey25",lty=2,lwd=2)
segments(12.5,S_juv_JP[2]/sum(S_juv_JP[]),T+0.5,S_juv_JP[2]/sum(S_juv_JP[]),col="grey25",lty=2,lwd=2)

#segments(0,mean(ratio_S_L[,1:12]),12.5,mean(ratio_S_L[,1:12]),col="grey25",lty=2,lwd=2)
#segments(12.5,mean(ratio_S_L[,13:37]),37.5,mean(ratio_S_L[,13:37]),col="grey25",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")


## 
# diff ratio



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,0.5,labels=expression(italic("f.")),col = "grey55")



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


abline(h=0,lty=2,col = "grey55")
abline(v=12.5,lty=3,col = "grey55")


#----------
# Poutes
#----------


#########
# Prod juveniles


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,1,labels=expression(italic("g.")),col = "grey55")



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


points(x=c(13,14,15),y=c(0,0,0),pch=16,col="darkolivegreen3")




segments(12.5,S_juv_JP[3]/sum(S_juv_JP[]),T+0.5,S_juv_JP[3]/sum(S_juv_JP[]),col="grey25",lty=2,lwd=2)


#segments(12.5,mean(ratio_juv_tot_P[,16:37]),37.5,mean(ratio_juv_tot_L[,16:37]),col="red",lty=2,lwd=2)

abline(v=12.5,lty=3,col = "grey55")



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,1,labels=expression(italic("h.")),col = "grey55")



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



segments(12.5,S_juv_JP[3]/sum(S_juv_JP[]),T+0.5,S_juv_JP[3]/sum(S_juv_JP[]),col="grey25",lty=2,lwd=2)


#segments(12.5,mean(ratio_S_P[,13:37]),37.5,mean(ratio_S_P[,13:37]),col="red",lty=2,lwd=2)
abline(v=12.5,lty=3,col = "grey55")




## 
# diff ratio



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(-0.5,-0.25,0,0.25,0.5),labels=c(-0.5,-0.25,0,0.25,0.5),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

text(T,0.5,labels=expression(italic("i.")))



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


abline(h=0,lty=2,col = "grey55")
abline(v=12.5,lty=3,col = "grey55")


for (i in 1:T){
	S_vichy_q[i,]=quantile(S_vichy_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	S_langeac_q[i,]=quantile(S_langeac_real[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}



dev.off()



#===================#
# CODE_R_PARAMETERS #
#===================#

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

mu_zone=read.coda("mu_zoneCODAchain1.txt","mu_zoneCODAindex.txt")
beta_zone=read.coda("beta_zoneCODAchain1.txt","beta_zoneCODAindex.txt")

mu_zone_q=array(rep(0,10),dim=c(1,5))

mu_zone_q[1,]=quantile(mu_zone[,1],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#mu_zone_q[2,]=quantile(mu_zone[,2],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
beta_zone_q=quantile(beta_zone,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)


a=read.coda("aCODAchain1.txt","aCODAindex.txt")
a_juv=read.coda("a_juvCODAchain1.txt","a_juvCODAindex.txt")
Rmax=read.coda("RmaxCODAchain1.txt","RmaxCODAindex.txt")
nu_wild=read.coda("nu_wildCODAchain1.txt","nu_wildCODAindex.txt")


Rmax_up= Rmax * exp(nu_wild[,2])
Rmax_down= Rmax * exp(nu_wild[,1])

a_wild_up= a* exp(nu_wild[,2])
a_wild_down= a* exp(nu_wild[,1])

a_juv_up= a_juv* exp(nu_wild[,2])
a_juv_down= a_juv* exp(nu_wild[,1])


mean(Rmax_up)
sd(Rmax_up)
Rmax_up_q=quantile(Rmax_up,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
Rmax_up_q

mean(Rmax_down)
sd(Rmax_down)
Rmax_down_q=quantile(Rmax_down,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
Rmax_down_q

mean(a_wild_up)
sd(a_wild_up)
a_wild_up_q=quantile(a_wild_up,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
a_wild_up_q

mean(a_wild_down)
sd(a_wild_down)
a_wild_down_q=quantile(a_wild_down,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
a_wild_down_q

mean(a_juv_up)
sd(a_juv_up)
a_juv_up_q=quantile(a_juv_up,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
a_juv_up_q

mean(a_juv_down)
sd(a_juv_down)
a_juv_down_q=quantile(a_juv_down,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
a_juv_down_q



p_reach_V=read.coda("p_reach_VCODAchain1.txt","p_reach_VCODAindex.txt")

sigma_p_L=read.coda("sigma_p_langeacCODAchain1.txt","sigma_p_langeacCODAindex.txt")
sigma_p_P=read.coda("sigma_p_poutesCODAchain1.txt","sigma_p_poutesCODAindex.txt")

tau_p_L=1/(sigma_p_L*sigma_p_L)
tau_p_P=1/(sigma_p_P*sigma_p_P)


rho=read.coda("rho_stationCODAchain1.txt","rho_stationCODAindex.txt")
alpha_L=read.coda("adjust_p_LCODAchain1.txt","adjust_p_LCODAindex.txt")
alpha_P=read.coda("adjust_p_PCODAchain1.txt","adjust_p_PCODAindex.txt")
s_juv2ad=read.coda("s_juv2adCODAchain1.txt","s_juv2adCODAindex.txt")

hel_effect=read.coda("hel_effectCODAchain1.txt","hel_effectCODAindex.txt")

sigma_V=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")
tau_vichy=1/(sigma_V*sigma_V)

#MANQUE CODA
#mean(p_reach_V)
#sd(p_reach_V)
#p_reach_V_q=quantile(p_reach_V,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#p_reach_V_q


mean(tau_vichy)
sd(tau_vichy)
tau_vichy_q=quantile(tau_vichy,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
tau_vichy_q



mean(tau_p_L)
sd(tau_p_L)
tau_p_L_q=quantile(tau_p_L,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
tau_p_L_q


mean(tau_p_P)
sd(tau_p_P)
tau_p_P_q=quantile(tau_p_P,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
tau_p_P_q

#MANQUE CODA
#mean(rho)
#sd(rho)
#rho_q=quantile(rho,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#rho_q

#MANQUE CODA
#mean(alpha_L)
#sd(alpha_L)
#alpha_L_q=quantile(alpha_L,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#alpha_L_q


#mean(alpha_P)
#sd(alpha_P)
#alpha_P_q=quantile(alpha_P,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
#alpha_P_q


mean(s_juv2ad)
sd(s_juv2ad)
s_juv2ad_q=quantile(s_juv2ad,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
s_juv2ad_q



mean(hel_effect)
sd(hel_effect)
hel_effect_q=quantile(hel_effect,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
hel_effect_q


#----------------------------------------------------
#CREATION TABLE AVEC TOUS LES PARAMETRES D'INTERET
#----------------------------------------------------

moy=rbind(mean(a_juv_down),mean(a_juv_up),mean(a_wild_down),mean(a_wild_up),mean(hel_effect),
		mean(Rmax_down),mean(Rmax_up),mean(s_juv2ad),mean(tau_p_L),mean(tau_p_P),mean(tau_vichy))

ET=rbind(sd(a_juv_down),sd(a_juv_up),sd(a_wild_down),sd(a_wild_up),sd(hel_effect),
		sd(Rmax_down),sd(Rmax_up),sd(s_juv2ad),sd(tau_p_L),sd(tau_p_P),sd(tau_vichy))

quantile=rbind(a_juv_down_q,a_juv_up_q,a_wild_down_q,a_wild_up_q,hel_effect_q,
		Rmax_down_q,Rmax_up_q,s_juv2ad_q,tau_p_L_q,tau_p_P_q,tau_vichy_q)

tab=cbind(moy,ET,quantile[,1],quantile[,2],quantile[,3],quantile[,4],quantile[,5])
colnames(tab)=c("mean","sd","2.5th","25th","median","75th","97.5th")
rownames(tab)=c("a_juv_down","a_juv_up","a_wild_down","a_wild_up","hel_effect","Rmax_down","Rmax_up","s_juv2ad","tau_p_L","tau_p_P","tau_vichy")

write.table(tab,file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/TableauParametres.csv")


#================#
#CODE_R_SPAWNERS #
#================#

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

N_vichy_real=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")


N_vichy_real_q=array(NA,dim=c(42,5))
#Attention à l'année 30 estimation des passages Vichy car année jugée incomplète
for (t in 1:22){
	N_vichy_real_q[t,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}
for (t in 23:23){
	N_vichy_real_q[t+7,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


#projection 20 years


CV_V=rep(0,42)
for (t in 1:42){
	CV_V[t]=sd(N_vichy_real[,t])/mean(N_vichy_real[,t])
}

min(CV_V[3:22])
max(CV_V[3:22])
median(CV_V[3:22])
mean(CV_V[3:22])

par(mar=c(4,7.1,2,4),col.lab="grey25",col.axis="grey55",col.main="grey25")

max(N_vichy_real_q[3:22,3])
min(N_vichy_real_q[3:22,3])
which(N_vichy_real_q[,3]==min(N_vichy_real_q[3:22, 3]))


#sans projections

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Spawners_ReturnsVichy.png",width=800, height=800, units = "px",type="cairo")
x11()
par(mar=c(4,7.1,2,4),col.lab="grey25",col.axis="grey55",col.main="grey25")

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab=iconv("Années","UTF-8","latin1"),ylim=c(0,9000),ylab=iconv("Effectif à Vichy","UTF8","latin1"),main=iconv("Retours des adultes à Vichy","UTF8","latin1"))

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


for(i in 3:30){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_real_q[i,5],i+0.15,N_vichy_real_q[i,5])
	segments(i,N_vichy_real_q[i,4],i,N_vichy_real_q[i,5])
	#5%
	segments(i-0.15,N_vichy_real_q[i,1],i+0.15,N_vichy_real_q[i,1])
	segments(i,N_vichy_real_q[i,2],i,N_vichy_real_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i,2],N_vichy_real_q[i,2],N_vichy_real_q[i,4],N_vichy_real_q[i,4]),col="grey85")
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



par(new=TRUE)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),ylab="",xlab="",ylim=c(0,1))

axis(4,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.9,las = 1,col = "grey55")


points(seq(3,22,1),CV_V[3:22],pch=16,col="darkorange")
segments(seq(3,21,1),CV_V[3:21],seq(4,22,1),CV_V[4:22],lty=2,pch=16,col="darkorange")

par(srt=-90) 
mtext(side=4,"CV",line=2.5,cex = 0.9)

dev.off()

# avec projections a 20 ans sans repeuplement

dev.new(width=30, height=25)
png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Spawners_proj.png",width=800, height=800, units = "px",type="cairo")




par(mar=c(4,7.1,2,0.5),col.lab="black",col.axis="grey55",col.main="black")




plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+20.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="Adults returns at Vichy and 20 years projection without stocking")

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T,46,58),
		labels=c(1975,1980,1990,2000,2013,2020,2032),
		cex.axis = 0.9,las = 1,col = "grey55")




for(i in 3:22){
	#whiskers
	#95%
	segments(i-0.15,N_vichy_real_q[i,5],i+0.15,N_vichy_real_q[i,5])
	segments(i,N_vichy_real_q[i,4],i,N_vichy_real_q[i,5])
	#5%
	segments(i-0.15,N_vichy_real_q[i,1],i+0.15,N_vichy_real_q[i,1])
	segments(i,N_vichy_real_q[i,2],i,N_vichy_real_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i,2],N_vichy_real_q[i,2],N_vichy_real_q[i,4],N_vichy_real_q[i,4]),col="grey85")
	#median
	segments(i-0.3,N_vichy_real_q[i,3],i+0.3,N_vichy_real_q[i,3])
}





points(x=seq(23,T,1),data_vichy[23:T],pch=16,col="grey25")


#Attention dans la boucle for il faut que pour l'ann�e T+1 il lise la 23eme ligne du tableau N_vichy_real_q
#pour avoir la suite des estimation (estimation sur les 22 premi�res ann�es, puis donn�es r�elles, puis estimation sur les 20 ann�es suivantes
#ici 40-17=23eme ligne
for(i in (T+1):(T+20)){
	#whiskers
	#95%
	segments(i-0.17,N_vichy_real_q[i-17,5],i+0.17,N_vichy_real_q[i-17,5])
	segments(i,N_vichy_real_q[i-17,4],i,N_vichy_real_q[i-17,5])
	#5%
	segments(i-0.17,N_vichy_real_q[i-17,1],i+0.17,N_vichy_real_q[i-17,1])
	segments(i,N_vichy_real_q[i-17,2],i,N_vichy_real_q[i-17,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i-17,2],N_vichy_real_q[i-17,2],N_vichy_real_q[i-17,4],N_vichy_real_q[i-17,4]),col="darkgoldenrod2")
	#median
	segments(i-0.3,N_vichy_real_q[i-17,3],i+0.3,N_vichy_real_q[i-17,3])
}

dev.off()


#==================================#
#CODE_R_PRODUCTION_JUVENILE_TOTALE #
#==================================#

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)

dmoy_wild_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt")
dmoy_wild_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt")
dmoy_wild_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt")

stocking_juv_V=read.coda("stocking_juv_VCODAchain1.txt","stocking_juv_VCODAindex.txt")
stocking_juv_L=read.coda("stocking_juv_LCODAchain1.txt","stocking_juv_LCODAindex.txt")
stocking_juv_P=read.coda("stocking_juv_PCODAchain1.txt","stocking_juv_PCODAindex.txt")


stocking_egg_V=read.coda("stocking_egg_VCODAchain1.txt","stocking_egg_VCODAindex.txt")
stocking_egg_L=read.coda("stocking_egg_LCODAchain1.txt","stocking_egg_LCODAindex.txt")


#D�j� r�cup�r� plus haut
#S_juv_JP=c(2574236,434552,655661)


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



#sans projections


png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/ProdJuvTot.png",width=800, height=800, units = "px",type="cairo")



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,900000),ylab="",main="total 0+ juvenile production (wild + stocked)")

# trace l'axe des ordonn�es
axis(2,at = c(0,100000,200000,300000,400000,500000,600000,700000,800000,900000),
		labels=c(
				expression(0),
				expression(paste(100," x ", 10^3)),
				expression(paste(200," x ", 10^3)),
				expression(paste(300," x ", 10^3)),
				expression(paste(400," x ", 10^3)),
				expression(paste(500," x ", 10^3)),
				expression(paste(600," x ", 10^3)),
				expression(paste(700," x ", 10^3)),
				expression(paste(800," x ", 10^3)),
				expression(paste(900," x ", 10^3))),
		cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
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


#========================#
# CODE_R_SURVIVAL_JUV2AD #
#========================#

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

I_surv=read.coda("simulation/I_survCODAchain1.txt","simulation/I_survCODAindex.txt",5001,10000)
s_juv2ad=read.coda("simulation/s_juv2adCODAchain1.txt","simulation/s_juv2adCODAindex.txt",5001,10000)
level_s=read.coda("simulation/level_sCODAchain1.txt","simulation/level_sCODAindex.txt",5001,10000)

library(stringr)
bugs2jags("data.txt","data_4zones_2016.12.19.R")
source("data_4zones_2016.12.19.R")
mu_s_smolt=s_smolt

s=array(0,c(5000,T))
s_smolt=array(0,c(5000,T))

for (t in 1:7){
	s[,t]=s_juv2ad*exp(level_s)
	s_smolt[,t]=mu_s_smolt*exp(level_s)
}

for (t in 8:T){
	s[,t]=s_juv2ad*exp(level_s*I_surv[,t-7])
	s_smolt[,t]=mu_s_smolt*exp(level_s*I_surv[,t-7])
	
}


s_q=array(0,c(T,5))
s_smolt_q=array(0,c(T,5))

#I_surv_q=array(0,c(T,5))

for (t in 1:T){
	s_q[t,]=quantile(s[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	s_smolt_q[t,]=quantile(s_smolt[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	#I_surv_q[t+7,]=quantile(I_surv[,t],probs=c(0.01,0.25,0.5,0.75,0.99),names=FALSE)
}



save.image(file = "C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/data/SurvivalJuv2Ad_2016.12.19.RData")



palette_s=rainbow(15,start=0.1,end=2/6)
palette_s=rev(palette_s)


pdf(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/script/lateX/illustrations/2015_01_24/SurvivalJuv2Ad_2015_02_10.pdf")#,width=800, height=800, units = "px",type="cairo")


par(mfrow=c(2,1),mar=c(4,7.1,2,0.5),col.lab="grey25",col.axis="grey55",col.main="grey25")


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,39.5), xlab="Années", ylim=c(0,0.05), ylab="",main="Survie moyenne du 0+ à l'adulte de retour" )

# trace l'axe des ordonn�es
axis(2,at = c(0,0.01,0.02,0.03,0.04,0.05),labels=c(0,0.01,0.02,0.03,0.04,0.05),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,39),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


for(i in 7:21){
	#whiskers
	#95%
	segments(i-0.15,s_q[i,5],i+0.15,s_q[i,5])
	segments(i,s_q[i,4],i,s_q[i,5])
	#5%
	segments(i-0.15,s_q[i,1],i+0.15,s_q[i,1])
	segments(i,s_q[i,2],i,s_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(s_q[i,2],s_q[i,2],s_q[i,4],s_q[i,4]),col=palette_s[i-6])
	#median
	segments(i-0.3,s_q[i,3],i+0.3,s_q[i,3])
}



for(i in 22:39){
	#whiskers
	#95%
	segments(i-0.15,s_q[i,5],i+0.15,s_q[i,5])
	segments(i,s_q[i,4],i,s_q[i,5])
	#5%
	segments(i-0.15,s_q[i,1],i+0.15,s_q[i,1])
	segments(i,s_q[i,2],i,s_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(s_q[i,2],s_q[i,2],s_q[i,4],s_q[i,4]),col=palette_s[15])
	#median
	segments(i-0.3,s_q[i,3],i+0.3,s_q[i,3])
}





plot(1,1,type="n",axes=FALSE, xlim=c(0.5,39.5), xlab="Années", ylim=c(0,0.05), ylab="",main="Survie moyenne du smolt déversé à l'adulte de retour" )

# trace l'axe des ordonn�es
axis(2,at = c(0,0.01,0.02,0.03,0.04,0.05),labels=c(0,0.01,0.02,0.03,0.04,0.05),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,39),
		labels=c(1975,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


for(i in 7:21){
	#whiskers
	#95%
	segments(i-0.15,s_smolt_q[i,5],i+0.15,s_smolt_q[i,5])
	segments(i,s_smolt_q[i,4],i,s_smolt_q[i,5])
	#5%
	segments(i-0.15,s_smolt_q[i,1],i+0.15,s_smolt_q[i,1])
	segments(i,s_smolt_q[i,2],i,s_smolt_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(s_smolt_q[i,2],s_smolt_q[i,2],s_smolt_q[i,4],s_smolt_q[i,4]),col=palette_s[i-6])
	#median
	segments(i-0.3,s_smolt_q[i,3],i+0.3,s_smolt_q[i,3])
}



for(i in 22:39){
	
	points(i,s_smolt_q[i,3],pch=16,col=palette_s[15])
	#whiskers
	#95%
	#segments(i-0.15,s_smolt_q[i,5],i+0.15,s_smolt_q[i,5])
	#segments(i,s_smolt_q[i,4],i,s_smolt_q[i,5])
	#5%
	#segments(i-0.15,s_smolt_q[i,1],i+0.15,s_smolt_q[i,1])
	#segments(i,s_smolt_q[i,2],i,s_smolt_q[i,1])
	#boxplot
	#polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(s_smolt_q[i,2],s_smolt_q[i,2],s_smolt_q[i,4],s_smolt_q[i,4]),col=palette_s[15])
	#median
	#segments(i-0.3,s_smolt_q[i,3],i+0.3,s_smolt_q[i,3])
}

dev.off()




juv_tot_q=array(0,c(39,5))


for( t in 1:39){
	juv_tot_q[t,]=quantile(juv_tot_sys[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
	
}

#===============#
# RESIDUS VICHY #
#===============#
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

res_vichy=read.coda("simulation/res_vichyCODAchain1.txt","simulation/res_vichyCODAindex.txt")



res_vichy_q=array(0,dim=c(T,5))

for (t in 7:T){
	res_vichy_q[t,]=quantile(res_vichy[,t-6],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/ResidusVichy.png",width=800, height=800, units = "px",type="cairo")



#sans projections



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,37.5),xlab="Years",ylim=c(-2,2),ylab="residuals",main="Adults returns at Vichy")

# trace l'axe des ordonn�es
axis(2,at = c(-2,-1,0,1,2),labels=c(-2,-1,0,1,2),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,37),
		labels=c(1975,1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "grey55")


abline(h=0,lty=2)

for(i in 7:37){
	#whiskers
	#95%
	segments(i-0.15,res_vichy_q[i,5],i+0.15,res_vichy_q[i,5])
	segments(i,res_vichy_q[i,4],i,res_vichy_q[i,5])
	#5%
	segments(i-0.15,res_vichy_q[i,1],i+0.15,res_vichy_q[i,1])
	segments(i,res_vichy_q[i,2],i,res_vichy_q[i,1])
	#boxplot
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_vichy_q[i,2],res_vichy_q[i,2],res_vichy_q[i,4],res_vichy_q[i,4]),col="grey85")
	#median
	segments(i-0.3,res_vichy_q[i,3],i+0.3,res_vichy_q[i,3])
}

dev.off()

#============================
# CODE_R_JUVENILE_PRODUCTION
#============================

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2016_12_19_Alagnon")

library(lattice)
library(coda)
library(boot)

T=41

dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt",5001,10000)
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt",5001,10000)
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt",5001,10000)

dmoy_wild_V=read.coda("dmoywild_VCODAchain1.txt","dmoywild_VCODAindex.txt",5001,10000)
dmoy_wild_L=read.coda("dmoywild_LCODAchain1.txt","dmoywild_LCODAindex.txt",5001,10000)
dmoy_wild_P=read.coda("dmoywild_PCODAchain1.txt","dmoywild_PCODAindex.txt",5001,10000)

stocking_juv_V=read.coda("stocking_juv_VCODAchain1.txt","stocking_juv_VCODAindex.txt",5001,10000)
stocking_juv_L=read.coda("stocking_juv_LCODAchain1.txt","stocking_juv_LCODAindex.txt",5001,10000)
stocking_juv_P=read.coda("stocking_juv_PCODAchain1.txt","stocking_juv_PCODAindex.txt",5001,10000)


stocking_egg_V=read.coda("stocking_egg_VCODAchain1.txt","stocking_egg_VCODAindex.txt",5001,10000)
stocking_egg_L=read.coda("stocking_egg_LCODAchain1.txt","stocking_egg_LCODAindex.txt",5001,10000)





res_wild_V=read.coda("res_wild_VCODAchain1.txt","res_wild_VCODAindex.txt")
res_wild_L=read.coda("res_wild_LCODAchain1.txt","res_wild_LCODAindex.txt")
res_wild_P=read.coda("res_wild_PCODAchain1.txt","res_wild_PCODAindex.txt")

res_juv_V=read.coda("res_juv_VCODAchain1.txt","res_juv_VCODAindex.txt")
res_juv_L=read.coda("res_juv_LCODAchain1.txt","res_juv_LCODAindex.txt")
res_juv_P=read.coda("res_juv_PCODAchain1.txt","res_juv_PCODAindex.txt")


res_egg_V=read.coda("res_egg_VCODAchain1.txt","res_egg_VCODAindex.txt")
res_egg_L=read.coda("res_egg_LCODAchain1.txt","res_egg_LCODAindex.txt")



dmoy_tot_V_q=array(rep(0,T*5),dim=c(T,5))
dmoy_tot_L_q=array(rep(0,T*5),dim=c(T,5))
dmoy_tot_P_q=array(rep(0,T*5),dim=c(T,5))

dmoy_wild_V_q=array(rep(0,T*5),dim=c(T,5))
dmoy_wild_L_q=array(rep(0,T*5),dim=c(T,5))
dmoy_wild_P_q=array(rep(0,T*5),dim=c(T,5))


dmoy_juv_V_q=array(rep(0,T*5),dim=c(T,5))
dmoy_juv_L_q=array(rep(0,T*5),dim=c(T,5))
dmoy_juv_P_q=array(rep(0,T*5),dim=c(T,5))

dmoy_egg_V_q=array(rep(0,T*5),dim=c(T,5))
dmoy_egg_L_q=array(rep(0,T*5),dim=c(T,5))



res_wild_V_q=array(rep(0,T*5),dim=c(T,5))
res_wild_L_q=array(rep(0,T*5),dim=c(T,5))
res_wild_P_q=array(rep(0,T*5),dim=c(T,5))

res_juv_V_q=array(rep(0,T*5),dim=c(T,5))
res_juv_L_q=array(rep(0,T*5),dim=c(T,5))
res_juv_P_q=array(rep(0,T*5),dim=c(T,5))

res_egg_V_q=array(rep(0,T*5),dim=c(T,5))
res_egg_L_q=array(rep(0,T*5),dim=c(T,5))






for (i in 1:(T-1)){
	dmoy_tot_V_q[i+1,]=quantile(dmoy_tot_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_tot_L_q[i+1,]=quantile(dmoy_tot_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
	dmoy_wild_V_q[i+1,]=quantile(dmoy_wild_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_wild_L_q[i+1,]=quantile(dmoy_wild_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
	dmoy_juv_V_q[i+1,]=quantile(stocking_juv_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_juv_L_q[i+1,]=quantile(stocking_juv_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
	res_wild_V_q[i+1,]=quantile(res_wild_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	res_wild_L_q[i+1,]=quantile(res_wild_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
	res_juv_V_q[i+1,]=quantile(res_juv_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	res_juv_L_q[i+1,]=quantile(res_juv_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
}


for (i in 1:(T-12)){
	dmoy_tot_P_q[i+12,]=quantile(dmoy_tot_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_wild_P_q[i+12,]=quantile(dmoy_wild_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	dmoy_juv_P_q[i+12,]=quantile(stocking_juv_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	res_wild_P_q[i+12,]=quantile(res_wild_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	res_juv_P_q[i+12,]=quantile(res_juv_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
	res_egg_V_q[i+12,]=quantile(res_egg_V[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	res_egg_L_q[i+12,]=quantile(res_egg_L[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
	
}

for (i in 21:(T-1)){
	dmoy_egg_V_q[i,]=quantile(stocking_egg_V[,i-11],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

for (i in 23:33){
	dmoy_egg_L_q[i,]=quantile(stocking_egg_L[,i-11],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}






###########################################
#############################################
##                                         ##
## #     # #  ###      #      ##   ##   #  ##
##  #   #  # #         #     #  #  # #  #  ##
##  #   #  # #    ###  #    #    # #  # #  ## 
##   # #   # #    ###  #    ###### #  # #  ##
##   # #   # #         #    #    # #   ##  ##
##    #    #  ###      #### #    # #    #  ##
##                                         ##
#############################################
###########################################

png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/juvenile_production_VL.png",width=4800,height=2400)

#######################################################
# Contribution moyenne sur la periode 1995 a 2009
########################################################


par( mfrow=c(3,4) ,mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4, oma = c( 0, 0, 2, 0 ),col.lab="grey25",col.axis="grey55",col.main="grey25")


#layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 3, 4, byrow = TRUE),widths=c(3,3,3,1), heights=c(1,1,1) )


### Vichy - Langeac ###

plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



# residuals wild

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals density dependence")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(1,0,1,0,1,1,1,0,1,1,0,0,0,0,0,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)


abline(h=0,lty=2)

for(i in 1:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,res_wild_V_q[i+1,5],i+0.15,res_wild_V_q[i+1,5])
	segments(i,res_wild_V_q[i+1,4],i,res_wild_V_q[i+1,5])
	#5%
	segments(i-0.15,res_wild_V_q[i+1,1],i+0.15,res_wild_V_q[i+1,1])
	segments(i,res_wild_V_q[i+1,2],i,res_wild_V_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_wild_V_q[i+1,2],res_wild_V_q[i+1,2],res_wild_V_q[i+1,4],res_wild_V_q[i+1,4]),col="coral3")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_wild_V_q[i+1,2],res_wild_V_q[i+1,2],res_wild_V_q[i+1,4],res_wild_V_q[i+1,4]),col="bisque")
	}
	#median
	segments(i-0.3,res_wild_V_q[i+1,3],i+0.3,res_wild_V_q[i+1,3])
}


text(T-1+0.5,4,labels=expression(italic("b.")),col = "grey55")



# residuals juv

x=c(1,3,5,6,7,9,10,17,20,21,22,23,24,25,26,27,28,29,31,32,33,34,35,36,37,38)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals stocking juv")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

abline(h=0,lty=2)


for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,res_juv_V_q[x[i]+1,5],x[i]+0.15,res_juv_V_q[x[i]+1,5])
	segments(x[i],res_juv_V_q[x[i]+1,4],x[i],res_juv_V_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,res_juv_V_q[x[i]+1,1],x[i]+0.15,res_juv_V_q[x[i]+1,1])
	segments(x[i],res_juv_V_q[x[i]+1,2],x[i],res_juv_V_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(res_juv_V_q[x[i]+1,2],res_juv_V_q[x[i]+1,2],res_juv_V_q[x[i]+1,4],res_juv_V_q[x[i]+1,4]),col="coral3")
	#median
	segments(x[i]-0.3,res_juv_V_q[x[i]+1,3],x[i]+0.3,res_juv_V_q[x[i]+1,3])
}


text(T-1+0.5,4,labels=expression(italic("c.")),col = "grey55")



# residuals egg


x=c(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals stocking egg")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

abline(h=0,lty=2)


for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,res_egg_V_q[x[i]+1,5],x[i]+0.15,res_egg_V_q[x[i]+1,5])
	segments(x[i],res_egg_V_q[x[i]+1,4],x[i],res_egg_V_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,res_egg_V_q[x[i]+1,1],x[i]+0.15,res_egg_V_q[x[i]+1,1])
	segments(x[i],res_egg_V_q[x[i]+1,2],x[i],res_egg_V_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(res_egg_V_q[x[i]+1,2],res_egg_V_q[x[i]+1,2],res_egg_V_q[x[i]+1,4],res_egg_V_q[x[i]+1,4]),col="coral3")
	#median
	segments(x[i]-0.3,res_egg_V_q[x[i]+1,3],x[i]+0.3,res_egg_V_q[x[i]+1,3])
}


text(T-1+0.5,4,labels=expression(italic("d.")),col = "grey55")




#Graph with total density


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^tot*" (0+."*m^-2*")")),main="Vichy-Langeac (total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(1,0,1,0,1,1,1,0,1,1,0,0,0,0,0,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)




for(i in 1:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,dmoy_tot_V_q[i+1,5],i+0.15,dmoy_tot_V_q[i+1,5])
	segments(i,dmoy_tot_V_q[i+1,4],i,dmoy_tot_V_q[i+1,5])
	#5%
	segments(i-0.15,dmoy_tot_V_q[i+1,1],i+0.15,dmoy_tot_V_q[i+1,1])
	segments(i,dmoy_tot_V_q[i+1,2],i,dmoy_tot_V_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_tot_V_q[i+1,2],dmoy_tot_V_q[i+1,2],dmoy_tot_V_q[i+1,4],dmoy_tot_V_q[i+1,4]),col="coral3")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_tot_V_q[i+1,2],dmoy_tot_V_q[i+1,2],dmoy_tot_V_q[i+1,4],dmoy_tot_V_q[i+1,4]),col="bisque")
	}
	#median
	segments(i-0.3,dmoy_tot_V_q[i+1,3],i+0.3,dmoy_tot_V_q[i+1,3])
}


text(T-1+0.5,0.6,labels=expression(italic("a.")),col = "grey55")




#Graph wild density


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^wild*" (0+."*m^-2*")")),main="Vichy-Langeac (wild)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(1,0,1,0,1,1,1,0,1,1,0,0,0,0,0,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)


for(i in 1:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,dmoy_wild_V_q[i+1,5],i+0.15,dmoy_wild_V_q[i+1,5])
	segments(i,dmoy_wild_V_q[i+1,4],i,dmoy_wild_V_q[i+1,5])
	#5%
	segments(i-0.15,dmoy_wild_V_q[i+1,1],i+0.15,dmoy_wild_V_q[i+1,1])
	segments(i,dmoy_wild_V_q[i+1,2],i,dmoy_wild_V_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_wild_V_q[i+1,2],dmoy_wild_V_q[i+1,2],dmoy_wild_V_q[i+1,4],dmoy_wild_V_q[i+1,4]),col="coral3")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_wild_V_q[i+1,2],dmoy_wild_V_q[i+1,2],dmoy_wild_V_q[i+1,4],dmoy_wild_V_q[i+1,4]),col="bisque")
	}
	#median
	segments(i-0.3,dmoy_wild_V_q[i+1,3],i+0.3,dmoy_wild_V_q[i+1,3])
}



text(T-1+0.5,0.6,labels=expression(italic("e.")),col = "grey55")


# graph stocking
#juvenile stocking vector

x=c(1,3,5,6,7,9,10,17,20,21,22,23,24,25,26,27,28,29,31,32,33,34,35,36,37,38)



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^juv*" (0+."*m^-2*")")),main="Vichy-Langeac (juv stocking)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,dmoy_juv_V_q[x[i]+1,5],x[i]+0.15,dmoy_juv_V_q[x[i]+1,5])
	segments(x[i],dmoy_juv_V_q[x[i]+1,4],x[i],dmoy_juv_V_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,dmoy_juv_V_q[x[i]+1,1],x[i]+0.15,dmoy_juv_V_q[x[i]+1,1])
	segments(x[i],dmoy_juv_V_q[x[i]+1,2],x[i],dmoy_juv_V_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(dmoy_juv_V_q[x[i]+1,2],dmoy_juv_V_q[x[i]+1,2],dmoy_juv_V_q[x[i]+1,4],dmoy_juv_V_q[x[i]+1,4]),col="coral3")
	#median
	segments(x[i]-0.3,dmoy_juv_V_q[x[i]+1,3],x[i]+0.3,dmoy_juv_V_q[x[i]+1,3])
}


text(T-1+0.5,0.6,labels=expression(italic("f.")),col = "grey55")


#eggs stocking vector

x=c(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^egg*" (0+."*m^-2*")")),main="Vichy-Langeac (egg stocking)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,dmoy_egg_V_q[x[i],5],x[i]+0.15,dmoy_egg_V_q[x[i],5])
	segments(x[i],dmoy_egg_V_q[x[i],4],x[i],dmoy_egg_V_q[x[i],5])
	#5%
	segments(x[i]-0.15,dmoy_egg_V_q[x[i],1],x[i]+0.15,dmoy_egg_V_q[x[i],1])
	segments(x[i],dmoy_egg_V_q[x[i],2],x[i],dmoy_egg_V_q[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(dmoy_egg_V_q[x[i],2],dmoy_egg_V_q[x[i],2],dmoy_egg_V_q[x[i],4],dmoy_egg_V_q[x[i],4]),col="coral3")
	#median
	segments(x[i]-0.3,dmoy_egg_V_q[x[i]+1,3],x[i]+0.3,dmoy_egg_V_q[x[i]+1,3])
}


text(T-1+0.5,0.6,labels=expression(italic("g.")),col = "grey55")


plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



# graph ratio wild/total

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Vichy-Langeac (ratio wild/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(1,3,5,6,7,9,10,17,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)


ratio_V_wild=array(rep(0,T*5000),dim=c(5000,T))

ratio_V_q_wild=array(rep(0,T*5),dim=c(T,5))


for(i in 1:(T-1)){
	ratio_V_wild[,i]=dmoy_wild_V[,i]/dmoy_tot_V[,i]
}



for (i in 1:(T-1)){
	ratio_V_q_wild[i,]=quantile(ratio_V_wild[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

x=c(1,3,5,6,7,9,10,17,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_V_q_wild[x[i],5],x[i]+0.15,ratio_V_q_wild[x[i],5])
	segments(x[i],ratio_V_q_wild[x[i],4],x[i],ratio_V_q_wild[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_V_q_wild[x[i],1],x[i]+0.15,ratio_V_q_wild[x[i],1])
	segments(x[i],ratio_V_q_wild[x[i],2],x[i],ratio_V_q_wild[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_V_q_wild[x[i],2],ratio_V_q_wild[x[i],2],ratio_V_q_wild[x[i],4],ratio_V_q_wild[x[i],4]),col="coral3")
	#median
	segments(x[i]-0.3,ratio_V_q_wild[x[i],3],x[i]+0.3,ratio_V_q_wild[x[i],3])
}



text(T-1+0.5,1,labels=expression(italic("h.")),col = "grey55")



x=c(2,4,8,11,12,13,14,15,16,18,19)

points(x,y=rep(1,length(x)),pch=21,bg="bisque")


#juveniles
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Vichy-Langeac (ratio stocked juv/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(1,3,5,6,7,9,10,17,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)


ratio_V_juv=array(rep(0,T*5000),dim=c(5000,T))

ratio_V_q_juv=array(rep(0,T*5),dim=c(T,5))




for(i in 1:(T-1)){
	ratio_V_juv[,i]=stocking_juv_V[,i]/dmoy_tot_V[,i]
}


for (i in 1:(T-1)){
	ratio_V_q_juv[i,]=quantile(ratio_V_juv[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

x=c(1,3,5,6,7,9,10,17,20,21,22,23,24,25,26,27,28,29,31,32,33,34,35,36,37,38)

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_V_q_juv[x[i],5],x[i]+0.15,ratio_V_q_juv[x[i],5])
	segments(x[i],ratio_V_q_juv[x[i],4],x[i],ratio_V_q_juv[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_V_q_juv[x[i],1],x[i]+0.15,ratio_V_q_juv[x[i],1])
	segments(x[i],ratio_V_q_juv[x[i],2],x[i],ratio_V_q_juv[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_V_q_juv[x[i],2],ratio_V_q_juv[x[i],2],ratio_V_q_juv[x[i],4],ratio_V_q_juv[x[i],4]),col="coral3")
	#median
	segments(x[i]-0.3,ratio_V_q_juv[x[i],3],x[i]+0.3,ratio_V_q_juv[x[i],3])
}


text(T-1+0.5,1,labels=expression(italic("i.")),col = "grey55")



x=c(2,4,8,11,12,13,14,15,16,18,19,30)

points(x,y=rep(0,length(x)),pch=21,bg="bisque")


#eggs

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Vichy-Langeac (ratio stocked eggs/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)


ratio_V_egg=array(0,dim=c(5000,T))

ratio_V_q_egg=array(rep(0,T*5),dim=c(T,5))




for(i in 1:length(x)){
	ratio_V_egg[,i+20]=stocking_egg_V[,i+9]/dmoy_tot_V[,i+20]
}


for (i in 1:(T-1)){
	ratio_V_q_egg[i,]=quantile(ratio_V_egg[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}




x=c(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_V_q_egg[x[i],5],x[i]+0.15,ratio_V_q_egg[x[i],5])
	segments(x[i],ratio_V_q_egg[x[i],4],x[i],ratio_V_q_egg[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_V_q_egg[x[i],1],x[i]+0.15,ratio_V_q_egg[x[i],1])
	segments(x[i],ratio_V_q_egg[x[i],2],x[i],ratio_V_q_egg[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_V_q_egg[x[i],2],ratio_V_q_egg[x[i],2],ratio_V_q_egg[x[i],4],ratio_V_q_egg[x[i],4]),col="coral3")
	#median
	segments(x[i]-0.3,ratio_V_q_egg[x[i],3],x[i]+0.3,ratio_V_q_egg[x[i],3])
}


text(T-1+0.5,1,labels=expression(italic("j.")),col = "grey55")



x=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

points(x,y=rep(0,length(x)),pch=21,bg="bisque")

dev.off()







##############################################
################################################
##                                            ##
## #      ##   ##   #       ###   ###  #   #  ## 
## #     #  #  # #  #       #  # #   # #   #  ##
## #    #    # #  # #  ###  #  # #   # #   #  ##
## #    ###### #  # #  ###  ###  #   # #   #  ##
## #    #    # #   ##       #    #   # #   #  ##
## #### #    # #    #       #     ###   ###   ##
##                                            ##
################################################
##############################################



png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/juvenile_production_LP.png",width=4800,height=2400)



#######################################################
# Contribution moyenne sur la periode 1995 a 2009
########################################################



par( mfrow=c(3,4) ,mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4, oma = c( 0, 0, 2, 0 ),col.lab="grey25",col.axis="grey55",col.main="grey25")



#layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 3, 4, byrow = TRUE),widths=c(3,3,3,1), heights=c(1,1,1) )


### Langeac - Poutes ###

plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



# residuals wild




plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals density dependence")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0)


abline(h=0,lty=2)

for(i in 1:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,res_wild_L_q[i+1,5],i+0.15,res_wild_L_q[i+1,5])
	segments(i,res_wild_L_q[i+1,4],i,res_wild_L_q[i+1,5])
	#5%
	segments(i-0.15,res_wild_L_q[i+1,1],i+0.15,res_wild_L_q[i+1,1])
	segments(i,res_wild_L_q[i+1,2],i,res_wild_L_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_wild_L_q[i+1,2],res_wild_L_q[i+1,2],res_wild_L_q[i+1,4],res_wild_L_q[i+1,4]),col="darkolivegreen4")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_wild_L_q[i+1,2],res_wild_L_q[i+1,2],res_wild_L_q[i+1,4],res_wild_L_q[i+1,4]),col="olivedrab2")
	}
	#median
	segments(i-0.3,res_wild_L_q[i+1,3],i+0.3,res_wild_L_q[i+1,3])
}

text(T-1+0.5,4,labels=expression(italic("b.")),col = "grey55")


# residuals juv yellowgreen


x=c(5,16,20,21,22,23,25,26,27,28,30,31)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals stocking juv")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

abline(h=0,lty=2)


for(i in 1:12){
	#whiskers
	#95%
	segments(x[i]-0.15,res_juv_L_q[x[i]+1,5],x[i]+0.15,res_juv_L_q[x[i]+1,5])
	segments(x[i],res_juv_L_q[x[i]+1,4],x[i],res_juv_L_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,res_juv_L_q[x[i]+1,1],x[i]+0.15,res_juv_L_q[x[i]+1,1])
	segments(x[i],res_juv_L_q[x[i]+1,2],x[i],res_juv_L_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(res_juv_L_q[x[i]+1,2],res_juv_L_q[x[i]+1,2],res_juv_L_q[x[i]+1,4],res_juv_L_q[x[i]+1,4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,res_juv_L_q[x[i]+1,3],x[i]+0.3,res_juv_L_q[x[i]+1,3])
}



text(T-1+0.5,4,labels=expression(italic("c.")),col = "grey55")


# residuals egg


x=c(23,24,25,26,27,28,29,30,31,32,33)



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals stocking egg")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

abline(h=0,lty=2)


for(i in 1:11){
	#whiskers
	#95%
	segments(x[i]-0.15,res_egg_L_q[x[i]+1,5],x[i]+0.15,res_egg_L_q[x[i]+1,5])
	segments(x[i],res_egg_L_q[x[i]+1,4],x[i],res_egg_L_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,res_egg_L_q[x[i]+1,1],x[i]+0.15,res_egg_L_q[x[i]+1,1])
	segments(x[i],res_egg_L_q[x[i]+1,2],x[i],res_egg_L_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(res_egg_L_q[x[i]+1,2],res_egg_L_q[x[i]+1,2],res_egg_L_q[x[i]+1,4],res_egg_L_q[x[i]+1,4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,res_egg_L_q[x[i]+1,3],x[i]+0.3,res_egg_L_q[x[i]+1,3])
}

text(T-1+0.5,4,labels=expression(italic("d.")),col = "grey55")


#Graph with total density


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^tot*" (0+."*m^-2*")")),main="Langeac-Poutes (total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0)


for(i in 1:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,dmoy_tot_L_q[i+1,5],i+0.15,dmoy_tot_L_q[i+1,5])
	segments(i,dmoy_tot_L_q[i+1,4],i,dmoy_tot_L_q[i+1,5])
	#5%
	segments(i-0.15,dmoy_tot_L_q[i+1,1],i+0.15,dmoy_tot_L_q[i+1,1])
	segments(i,dmoy_tot_L_q[i+1,2],i,dmoy_tot_L_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_tot_L_q[i+1,2],dmoy_tot_L_q[i+1,2],dmoy_tot_L_q[i+1,4],dmoy_tot_L_q[i+1,4]),col="darkolivegreen4")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_tot_L_q[i+1,2],dmoy_tot_L_q[i+1,2],dmoy_tot_L_q[i+1,4],dmoy_tot_L_q[i+1,4]),col="olivedrab2")
	}
	#median
	segments(i-0.3,dmoy_tot_L_q[i+1,3],i+0.3,dmoy_tot_L_q[i+1,3])
}

text(T-1+0.5,0.6,labels=expression(italic("a.")),col = "grey55")


#Graph wild density


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^wild*" (0+."*m^-2*")")),main="Langeac-Poutes (wild)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0)


for(i in 1:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,dmoy_wild_L_q[i+1,5],i+0.15,dmoy_wild_L_q[i+1,5])
	segments(i,dmoy_wild_L_q[i+1,4],i,dmoy_wild_L_q[i+1,5])
	#5%
	segments(i-0.15,dmoy_wild_L_q[i+1,1],i+0.15,dmoy_wild_L_q[i+1,1])
	segments(i,dmoy_wild_L_q[i+1,2],i,dmoy_wild_L_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_wild_L_q[i+1,2],dmoy_wild_L_q[i+1,2],dmoy_wild_L_q[i+1,4],dmoy_wild_L_q[i+1,4]),col="darkolivegreen4")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_wild_L_q[i+1,2],dmoy_wild_L_q[i+1,2],dmoy_wild_L_q[i+1,4],dmoy_wild_L_q[i+1,4]),col="olivedrab2")
	}
	#median
	segments(i-0.3,dmoy_wild_L_q[i+1,3],i+0.3,dmoy_wild_L_q[i+1,3])
}

text(T-1+0.5,0.6,labels=expression(italic("e.")),col = "grey55")




# graph stocking
#juvenile stocking vector

x=c(5,16,20,21,22,23,25,26,27,28,30,31)



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^juv*" (0+."*m^-2*")")),main="Langeac-Poutes (juv stocking)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,dmoy_juv_L_q[x[i]+1,5],x[i]+0.15,dmoy_juv_L_q[x[i]+1,5])
	segments(x[i],dmoy_juv_L_q[x[i]+1,4],x[i],dmoy_juv_L_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,dmoy_juv_L_q[x[i]+1,1],x[i]+0.15,dmoy_juv_L_q[x[i]+1,1])
	segments(x[i],dmoy_juv_L_q[x[i]+1,2],x[i],dmoy_juv_L_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(dmoy_juv_L_q[x[i]+1,2],dmoy_juv_L_q[x[i]+1,2],dmoy_juv_L_q[x[i]+1,4],dmoy_juv_L_q[x[i]+1,4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,dmoy_juv_L_q[x[i]+1,3],x[i]+0.3,dmoy_juv_L_q[x[i]+1,3])
}

text(T-1+0.5,0.6,labels=expression(italic("f.")),col = "grey55")


#eggs stocking vector

x=c(23,24,25,26,27,28,29,30,31,32,33)



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^egg*" (0+."*m^-2*")")),main="Langeac-Poutes (egg stocking)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,dmoy_egg_L_q[x[i],5],x[i]+0.15,dmoy_egg_L_q[x[i],5])
	segments(x[i],dmoy_egg_L_q[x[i],4],x[i],dmoy_egg_L_q[x[i],5])
	#5%
	segments(x[i]-0.15,dmoy_egg_L_q[x[i],1],x[i]+0.15,dmoy_egg_L_q[x[i],1])
	segments(x[i],dmoy_egg_L_q[x[i],2],x[i],dmoy_egg_L_q[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(dmoy_egg_L_q[x[i],2],dmoy_egg_L_q[x[i],2],dmoy_egg_L_q[x[i],4],dmoy_egg_L_q[x[i],4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,dmoy_egg_L_q[x[i],3],x[i]+0.3,dmoy_egg_L_q[x[i],3])
}


text(T-1+0.5,0.5,labels=expression(italic("g.")),col = "grey55")


plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



# graph ratio wild/total

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Langeac-Poutes (ratio wild/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(5,16,20,21,22,23,24,25,26,27,28,29,30,31,32,33)



ratio_L_wild=array(0,dim=c(5000,T))

ratio_L_q_wild=array(0,dim=c(T,5))


for(i in 1:(T-1)){
	ratio_L_wild[,i]=dmoy_wild_L[,i]/dmoy_tot_L[,i]
}

for (i in 1:(T-1)){
	ratio_L_q_wild[i,]=quantile(ratio_L_wild[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_L_q_wild[x[i],5],x[i]+0.15,ratio_L_q_wild[x[i],5])
	segments(x[i],ratio_L_q_wild[x[i],4],x[i],ratio_L_q_wild[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_L_q_wild[x[i],1],x[i]+0.15,ratio_L_q_wild[x[i],1])
	segments(x[i],ratio_L_q_wild[x[i],2],x[i],ratio_L_q_wild[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_L_q_wild[x[i],2],ratio_L_q_wild[x[i],2],ratio_L_q_wild[x[i],4],ratio_L_q_wild[x[i],4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,ratio_L_q_wild[x[i],3],x[i]+0.3,ratio_L_q_wild[x[i],3])
}



x=c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,17,18,19,34,35,36,37,38)

points(x,y=rep(1,length(x)),pch=21,bg="grey90")

text(T-1+0.5,1,labels=expression(italic("h.")),col = "grey55")


#juveniles
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Langeac-Poutes (ratio stocked juv/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(5,16,20,21,22,23,25,26,27,28,30,31)



ratio_L_juv=array(0,dim=c(5000,T))

ratio_L_q_juv=array(0,dim=c(T,5))




for(t in 1:(T-1)){
	
	ratio_L_juv[,t]=stocking_juv_L[,t]/dmoy_tot_L[,t]
	
}


for (i in 1:(T-1)){
	ratio_L_q_juv[i,]=quantile(ratio_L_juv[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}



for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_L_q_juv[x[i],5],x[i]+0.15,ratio_L_q_juv[x[i],5])
	segments(x[i],ratio_L_q_juv[x[i],4],x[i],ratio_L_q_juv[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_L_q_juv[x[i],1],x[i]+0.15,ratio_L_q_juv[x[i],1])
	segments(x[i],ratio_L_q_juv[x[i],2],x[i],ratio_L_q_juv[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_L_q_juv[x[i],2],ratio_L_q_juv[x[i],2],ratio_L_q_juv[x[i],4],ratio_L_q_juv[x[i],4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,ratio_L_q_juv[x[i],3],x[i]+0.3,ratio_L_q_juv[x[i],3])
}


text(T-1+0.5,1,labels=expression(italic("i.")),col = "grey55")


x=c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,17,18,19,24,29,32,33,34,35,36,37,38)

points(x,y=rep(0,length(x)),pch=21,bg="olivedrab2")


#eggs

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Langeac-Poutes (ratio stocked eggs/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(23,24,25,26,27,28,29,30,31,32,33)


ratio_L_egg=array(0,dim=c(5000,T))

ratio_L_q_egg=array(0,dim=c(T,5))




for(i in 1:length(x)){
	ratio_L_egg[,i+22]=stocking_egg_L[,i+9]/dmoy_tot_L[,i+22]
}


for (i in 1:(T-1)){
	ratio_L_q_egg[i,]=quantile(ratio_L_egg[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}



for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_L_q_egg[x[i],5],x[i]+0.15,ratio_L_q_egg[x[i],5])
	segments(x[i],ratio_L_q_egg[x[i],4],x[i],ratio_L_q_egg[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_L_q_egg[x[i],1],x[i]+0.15,ratio_L_q_egg[x[i],1])
	segments(x[i],ratio_L_q_egg[x[i],2],x[i],ratio_L_q_egg[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_L_q_egg[x[i],2],ratio_L_q_egg[x[i],2],ratio_L_q_egg[x[i],4],ratio_L_q_egg[x[i],4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,ratio_L_q_egg[x[i],3],x[i]+0.3,ratio_L_q_egg[x[i],3])
}


text(T-1+0.5,1,labels=expression(italic("j.")),col = "grey55")


x=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,34,35,36,37,38)

points(x,y=rep(0,length(x)),pch=21,bg="olivedrab2")



dev.off()




##########################################
############################################
##                                        ##
## #   # ###   ##       ###   ###  #   #  ## 
## #   # #  # #         #  # #   # #   #  ##
## #   # #  # #    ###  #  # #   # #   #  ##
## #   # ###   #   ###  ###  #   # #   #  ##
## #   # #      #       #    #   # #   #  ##
##  ###  #    ###       #     ###   ###   ##
##                                        ##
############################################
##########################################


png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/juvenile_production_upP.png",width=4800,height=2400)



#######################################################
# Contribution moyenne sur la periode 1995 a 2009
########################################################


par( mfrow=c(3,4) ,mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4, oma = c( 0, 0, 2, 0 ))


#layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 3, 4, byrow = TRUE),widths=c(3,3,3,1), heights=c(1,1,1) )


### upstream Poutes ###

plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



# residuals wild


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals density dependence")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,0,0,0,0,0,0,0)


abline(h=0,lty=2)

abline(v=10,lty=2)

for(i in 12:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,res_wild_P_q[i+1,5],i+0.15,res_wild_P_q[i+1,5])
	segments(i,res_wild_P_q[i+1,4],i,res_wild_P_q[i+1,5])
	#5%
	segments(i-0.15,res_wild_P_q[i+1,1],i+0.15,res_wild_P_q[i+1,1])
	segments(i,res_wild_P_q[i+1,2],i,res_wild_P_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_wild_P_q[i+1,2],res_wild_P_q[i+1,2],res_wild_P_q[i+1,4],res_wild_P_q[i+1,4]),col="darkolivegreen4")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(res_wild_P_q[i+1,2],res_wild_P_q[i+1,2],res_wild_P_q[i+1,4],res_wild_P_q[i+1,4]),col="olivedrab2")
	}
	#median
	segments(i-0.3,res_wild_P_q[i+1,3],i+0.3,res_wild_P_q[i+1,3])
}

text(T-1+0.5,4,labels=expression(italic("b.")),col = "grey55")


# residuals juv


x=c(17,18,19,20,21,22,23,24,26,27,28,30,31,32)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(-4,4),ylab="",main="residuals stocking juv")

# trace l'axe des ordonn�es
axis(2,at = c(-4,-3,-2,-1,0,1,2,3,4),labels=c(-4,-3,-2,-1,0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

abline(h=0,lty=2)

abline(v=10,lty=2)

for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,res_juv_P_q[x[i]+1,5],x[i]+0.15,res_juv_P_q[x[i]+1,5])
	segments(x[i],res_juv_P_q[x[i]+1,4],x[i],res_juv_P_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,res_juv_P_q[x[i]+1,1],x[i]+0.15,res_juv_P_q[x[i]+1,1])
	segments(x[i],res_juv_P_q[x[i]+1,2],x[i],res_juv_P_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(res_juv_P_q[x[i]+1,2],res_juv_P_q[x[i]+1,2],res_juv_P_q[x[i]+1,4],res_juv_P_q[x[i]+1,4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,res_juv_P_q[x[i]+1,3],x[i]+0.3,res_juv_P_q[x[i]+1,3])
}


text(T-1+0.5,4,labels=expression(italic("c.")),col = "grey55")



# residuals egg (none upstream poutes)


plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



#Graph with total density


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^tot*" (0+."*m^-2*")")),main="Upstream Poutes (total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,0,0,0,0,0,0,0)

abline(v=10,lty=2)

for(i in 12:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,dmoy_tot_P_q[i+1,5],i+0.15,dmoy_tot_P_q[i+1,5])
	segments(i,dmoy_tot_P_q[i+1,4],i,dmoy_tot_P_q[i+1,5])
	#5%
	segments(i-0.15,dmoy_tot_P_q[i+1,1],i+0.15,dmoy_tot_P_q[i+1,1])
	segments(i,dmoy_tot_P_q[i+1,2],i,dmoy_tot_P_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_tot_P_q[i+1,2],dmoy_tot_P_q[i+1,2],dmoy_tot_P_q[i+1,4],dmoy_tot_P_q[i+1,4]),col="darkolivegreen4")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_tot_P_q[i+1,2],dmoy_tot_P_q[i+1,2],dmoy_tot_P_q[i+1,4],dmoy_tot_P_q[i+1,4]),col="olivedrab2")
	}
	#median
	segments(i-0.3,dmoy_tot_P_q[i+1,3],i+0.3,dmoy_tot_P_q[i+1,3])
}

text(T-1+0.5,0.6,labels=expression(italic("a.")),col = "grey55")


#Graph wild density


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^wild*" (0+."*m^-2*")")),main="Upstream Poutes (wild)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")

x=c(0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,0,0,0,0,0,0,0)

abline(v=10,lty=2)

for(i in 12:(T-1)){
	#whiskers
	#95%
	segments(i-0.15,dmoy_wild_P_q[i+1,5],i+0.15,dmoy_wild_P_q[i+1,5])
	segments(i,dmoy_wild_P_q[i+1,4],i,dmoy_wild_P_q[i+1,5])
	#5%
	segments(i-0.15,dmoy_wild_P_q[i+1,1],i+0.15,dmoy_wild_P_q[i+1,1])
	segments(i,dmoy_wild_P_q[i+1,2],i,dmoy_wild_P_q[i+1,1])
	#boxplot
	if (x[i] == 1){
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_wild_P_q[i+1,2],dmoy_wild_P_q[i+1,2],dmoy_wild_P_q[i+1,4],dmoy_wild_P_q[i+1,4]),col="darkolivegreen4")
	}
	else{
		polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(dmoy_wild_P_q[i+1,2],dmoy_wild_P_q[i+1,2],dmoy_wild_P_q[i+1,4],dmoy_wild_P_q[i+1,4]),col="olivedrab2")
	}
	#median
	segments(i-0.3,dmoy_wild_P_q[i+1,3],i+0.3,dmoy_wild_P_q[i+1,3])
}


text(T-1+0.5,0.6,labels=expression(italic("d.")),col = "grey55")



# graph stocking
#juvenile stocking vector

x=c(8,9,17,18,19,20,21,22,23,24,26,27,28,30,31,32)


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,0.6),ylab=expression(italic(D^juv*" (0+."*m^-2*")")),main="Upstream-Poutes (juv stocking)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


abline(v=10,lty=2)

for(i in 3:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,dmoy_juv_P_q[x[i]+1,5],x[i]+0.15,dmoy_juv_P_q[x[i]+1,5])
	segments(x[i],dmoy_juv_P_q[x[i]+1,4],x[i],dmoy_juv_P_q[x[i]+1,5])
	#5%
	segments(x[i]-0.15,dmoy_juv_P_q[x[i]+1,1],x[i]+0.15,dmoy_juv_P_q[x[i]+1,1])
	segments(x[i],dmoy_juv_P_q[x[i]+1,2],x[i],dmoy_juv_P_q[x[i]+1,1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(dmoy_juv_P_q[x[i]+1,2],dmoy_juv_P_q[x[i]+1,2],dmoy_juv_P_q[x[i]+1,4],dmoy_juv_P_q[x[i]+1,4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,dmoy_juv_P_q[x[i]+1,3],x[i]+0.3,dmoy_juv_P_q[x[i]+1,3])
}

text(T-1+0.5,0.6,labels=expression(italic("e.")),col = "grey55")


#eggs stocking vector(no egges upstream poutes

plot(1,1,type="n",axes=FALSE,xlab="",ylab="")


plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



# graph ratio wild/total

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Upstream Poutes (ratio wild/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(17,18,19,20,21,22,23,24,26,27,28,30,31,32)

abline(v=10,lty=2)


ratio_P=array(rep(0,5000*T),dim=c(5000,T))

ratio_P_q=array(rep(0,T*5),dim=c(T,5))






for(i in 1:length(x)){
	ratio_P[,x[i]]=dmoy_wild_P[,x[i]-11]/dmoy_tot_P[,x[i]-11]
}



for (i in 2:(T-1)){
	ratio_P_q[i,]=quantile(ratio_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}


for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_P_q[x[i],5],x[i]+0.15,ratio_P_q[x[i],5])
	segments(x[i],ratio_P_q[x[i],4],x[i],ratio_P_q[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_P_q[x[i],1],x[i]+0.15,ratio_P_q[x[i],1])
	segments(x[i],ratio_P_q[x[i],2],x[i],ratio_P_q[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_P_q[x[i],2],ratio_P_q[x[i],2],ratio_P_q[x[i],4],ratio_P_q[x[i],4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,ratio_P_q[x[i],3],x[i]+0.3,ratio_P_q[x[i],3])
}



x=c(12,13,14,15,16,25,29,33,34,35,36,37,38)

points(x,y=rep(1,length(x)),pch=21,bg="olivedrab2")

text(T-1+0.5,1,labels=expression(italic("f.")),col = "grey55")


#juveniles
plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T-1+0.5),xlab="Years",ylim=c(0,1),ylab="ratio",main="Upstream Poutes (ratio stocked juv/total)")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,5,15,25,38),
		labels=c(1976,1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


x=c(17,18,19,20,21,22,23,24,26,27,28,30,31,32)

abline(v=10,lty=2)


ratio_P=array(rep(0,T*5000),dim=c(5000,T))

ratio_P_q=array(rep(0,T*5),dim=c(T,5))




for(i in 1:length(x)){
	ratio_P[,x[i]]=stocking_juv_P[,x[i]-11]/dmoy_tot_P[,x[i]-11]
	
	#ratio_P[,i+11]=stocking_juv_P[,i]/dmoy_tot_P[,i]
}


for (i in 1:(T-1)){
	ratio_P_q[i,]=quantile(ratio_P[,i],probs=c(0.05,0.25,0.5,0.75,0.95),names=FALSE)
}



for(i in 1:length(x)){
	#whiskers
	#95%
	segments(x[i]-0.15,ratio_P_q[x[i],5],x[i]+0.15,ratio_P_q[x[i],5])
	segments(x[i],ratio_P_q[x[i],4],x[i],ratio_P_q[x[i],5])
	#5%
	segments(x[i]-0.15,ratio_P_q[x[i],1],x[i]+0.15,ratio_P_q[x[i],1])
	segments(x[i],ratio_P_q[x[i],2],x[i],ratio_P_q[x[i],1])
	#boxplot
	polygon(c(x[i]-0.3,x[i]+0.3,x[i]+0.3,x[i]-0.3),c(ratio_P_q[x[i],2],ratio_P_q[x[i],2],ratio_P_q[x[i],4],ratio_P_q[x[i],4]),col="darkolivegreen4")
	#median
	segments(x[i]-0.3,ratio_P_q[x[i],3],x[i]+0.3,ratio_P_q[x[i],3])
}



x=c(12,13,14,15,16,25,29,33,34,35,36,37,38)

points(x,y=rep(0,length(x)),pch=21,bg="olivedrab2")

text(T-1+0.5,1,labels=expression(italic("g.")),col = "grey55")


#eggs (no eggs upstream poutes)

plot(1,1,type="n",axes=FALSE,xlab="",ylab="")



dev.off()


######################################################


stock_juv=structure(.Data = c(
				30000,	2,		20000,
				20000,	2,		2,
				2,		   2,     	2,
				80000,	2,		2,
				2,		   2,		2,
				32000,	32000,  2,
				20000,	2,	     2,
				124950,   2,	     2,
				2,		   2,		30000,
				20000,	2,		40000,
				
				4000,	  2,		2,
				2,		   2,		2,
				2,		   2,		2,
				2,		   2,		2,
				2,	   	2,		2,
				2,	 	  2,		2,
				2,		   58410,  2,
				17150,	2,		114350,
				2,		   2,		125000,
				2,		   2,		53600,
				
				212500,	327000,	414500,
				410600,	311000,	338000,
				522600,	309000,	253000,
				611000,	103000,	251000,
				146500,	2,			254880,
				597700,	213000,	2,
				651793,	236170,	153610,
				92114,	 83144,	   363421,
				330480,	174429,	305126,
				194156,	2,			2,
				2,			85929,	146070,
				210000,	96000,	294000,
				152560,	2,		   52000,
				417052,	2,			2,
				699594,	2,			2,
				660841,	2,			2
		),  
		.Dim=c(3,36))



par(mfrow=c(2,2))

x=c(1,3,5,6,7,9,10,17,20,21,22,23,24,25,26,27,28,29,31,32,33,34)


plot(0,0,type="n",xlim=c(0,700000),ylim=c(-5,5),xlab="0+ stocking",ylab="median of residuals",main="Vichy-Langeac")


for (i in 1:23){
	for (k in 1:8000){
		points(stock_juv[1,x[i]+1],res_juv_V[k,x[i]],pch =16,col=rgb(139,137,137,10,maxColorValue=255))
	}
}

abline(h=0,lty=2,col="red")

temp=rbind(stock_juv[1,],stock_juv[1,])

for(t in 1:7998){
	temp=rbind(temp,stock_juv[1,])
	
}

dim(temp)	 



a=loess.smooth(temp[,x[]+1],res_juv_V[,x[]])
points(a$x,a$y,col="green",type="l")



plot(0,0,type="n",xlim=c(0,400000),ylim=c(-5,5),xlab="0+ stocking",ylab="median of residuals",main="Langeac-Poutes")
x=c(5,16,20,21,22,23,25,26,27,28,30,31)


for (i in 1:12){
	for (k in 1:8000){
		points(stock_juv[2,x[i]+1],res_juv_L[k,x[i]],pch =16,col=rgb(139,137,137,10,maxColorValue=255))
	}
}

abline(h=0,lty=2,col="red")


temp=rbind(stock_juv[2,],stock_juv[2,])

for(t in 1:7998){
	temp=rbind(temp,stock_juv[2,])
	
}

dim(temp)	 



a=loess.smooth(temp[,x[]+1],res_juv_L[,x[]])
points(a$x,a$y,col="green",type="l")




plot(0,0,type="n",xlim=c(0,450000),ylim=c(-5,5),xlab="0+ stocking",ylab="median of residuals",main="Upstream-Poutes")

x=c(17,18,19,20,21,22,23,24,26,27,28,30,31,32)

x=c(5,6,7,8,9,10,11,12,14,15,16,18,19,20)


for (i in 1:14){
	for (k in 1:8000){
		points(stock_juv[3,x[i]+1],res_juv_P[k,x[i]],pch =16,col=rgb(139,137,137,10,maxColorValue=255))
	}
}




abline(h=0,lty=2,col="red")


temp=rbind(stock_juv[3,],stock_juv[3,])

for(t in 1:7998){
	temp=rbind(temp,stock_juv[3,])
	
}

dim(temp)	 



a=loess.smooth(temp[,x[]+1],res_juv_P[,x[]])
points(a$x,a$y,col="green",type="l")







#==========================================
#ANCIEN CODE GUILLAUME POUR LES GENITEURS
#==========================================
###################
# Spawners        #
###################


S_vichy=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_langeac=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")


S_vichy_q=array(rep(0,(T-2)*5),dim=c(T-2,5))
S_langeac_q=array(rep(0,(T-2)*5),dim=c(T-2,5))



for (i in 1:(T-2)){
	S_vichy_q[i,]=quantile(S_vichy[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
}
for (i in 1:(T-2)){
	S_langeac_q[i,]=quantile(S_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


S_poutes_counter=c(0,0,0,0,0,0,0,0,0,10,43,110,21,4,3,11,9,23,6,67,35,31,130,112,53,40,154,89,74,153,53,39,14,26,118,59,45)



#calcul des CVs

CV_V=rep(0,T-2)
CV_L=rep(0,T-2)

for (i in 1:(T-2)){
	CV_V[i]=sd(S_vichy[,i])/mean(S_vichy[,i])
	CV_L[i]=sd(S_langeac[,i])/mean(S_langeac[,i])
}


x=seq(1,T-2,1)


par(mfrow=c(1,2))
plot(x,CV_V,axes=FALSE,main="CV spawners Vichy",ylim=c(0,0.7),pch=16,ylab="CV",xlab="Years")

axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(T-T+1,T-T+5,T-T+15,T-T+25,T-2),
		labels=c("",1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")

points(x[23:(T-2)],CV_V[23:(T-2)],col="red", pch=16)



plot(x,CV_L,axes=FALSE,main="CV spawners Langeac",ylim=c(0,0.7),pch=16,ylab="",xlab="Years")

axis(2,at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,4,14,24,T-2),
		labels=c("",1980,1990,2000,2013),
		cex.axis = 0.9,las = 1,col = "black")

points(x[27:32],CV_L[27:32],col="red", pch=16)

dev.off()

#p_redd correspond � p_area (surface prospect�e / surface totale)
#valeur r�cup�r�e en faisant copier/coller depuis "C:\Mes Documents\Tableau de bord\PROJETS\Mod�leDynamiquePopSaumon\Donn�esPourMAJ\surface prospect�e par secteur_maj_LineaireTotal_2014.05.20.xlsx"

p_area_V=c(0.237,0.372,0.304,0.163,0.496,0,0.496,0.169,0.496,0.146,
		0.146,0.152,0.237,0.237,0.237,0.237,0.237,0.237,0,0,
		0.338,0.496,0.411,0.372,0.389,0,0,0.507,0.425,0.498,
		0.570,0,0.493,0,0.588235294,0.909502262,0.357466063)
p_area_L=c(0.448,0.414,0.414,0.862,1.000,0,1.000,0.862,1.000,1.000,
		1.000,1.000,1.000,0.862,0.862,0.793,0.690,0.690,0,0,
		1.000,1.000,1.000,1.000,1.000,0,0,1.000,1.000,1.000,
		1.000,0,1.000,0,0,1.00,1.00)


#Valeur Guillaume avant remise des surfaces sur lin�aire total
#p_redd_V=c(0.429,0.673,0.551,0.296,0.898,0,0.898,0.306,0.898,0.265,
#		0.265,0.276,0.429,0.429,0.429,0.429,0.429,0.429,0,0,0.612,0.898,
#		0.745,0.673,0.704,0,0,0.889,0.746,0.873,1,0,0.865,0,0)
#p_redd_L=c(0.448,0.414,0.414,0.862,1,0,1,0.862,1,1,1,1,1,0.862,0.862,
#		0.793,0.690,0.690,0,0,1,1,1,1,1,0,0,1,1,1,1,0,1,0,0)


plot(p_area_V,CV_V,pch=16)
plot(p_area_L,CV_L,pch=16)


#Graph with all years : G�niteurs selon les secteurs

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Redds_G�niteurs.png",width=800, height=800, units = "px",type="cairo")



par(mfrow=c(3,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,35.5),xlab="Years",ylim=c(0,1200),ylab=expression(italic(S["t,1"])),main="Vichy-Langeac")

# trace l'axe des ordonn�es
axis(2,at = c(0,200,400,600,800,1000,1200),labels=c(0,200,400,600,800,1000,1200),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,4,14,24,35),
		labels=c(1977,1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "black")

text(T-2,1200,labels=expression(italic("a.")))



for(i in 1:(T-2)){
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

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,35.5),xlab="Years",ylim=c(0,600),ylab=expression(italic(S["t,2"])),main="Langeac-Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,200,400,600),labels=c(0,200,400,600),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,4,14,24,35),
		labels=c(1977,1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "black")


text(35,600,labels=expression(italic("b.")))



for(i in 1:35){
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


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,35.5),xlab="Years",ylim=c(0,200),ylab=expression(italic(S["t,3"])),main="Upstream Poutes")

# trace l'axe des ordonn�es
axis(2,at = c(0,100,200),labels=c(0,100,200),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,4,14,24,35),
		labels=c(1977,1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "black")

points(x=seq(10,35,1),y=S_poutes_counter[10:35],pch=16) 
abline(v=9.5,lty=2)
text(4,160,paste( "upstream Poutes\n inaccessible"))


text(35,200,labels=expression(italic("c.")))

