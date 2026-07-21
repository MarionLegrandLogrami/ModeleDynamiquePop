# TODO: Add comment
# 
# Author: marion.legrand
###############################################################################

setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/2014_05_27")

library(lattice)
library(coda)
library(boot)

T=39

#===========#
# REDDS	    #
#===========#

# parameters calculation

#Pas pr�sentes - � voir si utilis�
#mean_N_V=read.coda("mean_N_VCODAchain1.txt","mean_N_VCODAindex.txt")
#sigma_Vichy=read.coda("sigma_vichyCODAchain1.txt","sigma_vichyCODAindex.txt")

zone_effect_V=read.coda("zone_effect_VCODAchain1.txt","zone_effect_VCODAindex.txt")
zone_effect_L=read.coda("zone_effect_LCODAchain1.txt","zone_effect_LCODAindex.txt")
zone_effect_P=read.coda("zone_effect_PCODAchain1.txt","zone_effect_PCODAindex.txt")


zone_effect_V_q=array(rep(0,T*5),dim=c(T,5))
zone_effect_L_q=array(rep(0,T*5),dim=c(T,5))
zone_effect_P_q=array(rep(0,T*5),dim=c(T,5))



for (i in 1:T){
	zone_effect_V_q[i,]=quantile(zone_effect_V[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
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




N_langeac=read.coda("N_LCODAchain1.txt","N_LCODAindex.txt")

C_langeac=c(189,216,142,246,107,95)
p_count_L=array(rep(0,30000),dim=c(5000,6))

for (t in 1:6){
	p_count_L[,t]=C_langeac[t]/N_langeac[,26+t]
}

p_count_L_q=array(rep(0,35),dim=c(6,5))

for (t in 1:6){
	p_count_L_q[t,]=quantile(p_count_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}



###########################
#plot zone_effect /kappa
###############################

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Redds_kappa.png",width=800, height=800, units = "px",type="cairo")


par(mfrow=c(3,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,3),ylab=expression(italic( kappa["t,1"])),main="Vichy-Langeac")

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

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,(T+0.5)),xlab="Years",ylim=c(0,4),ylab=expression(italic(kappa["t,2"])),main="Langeac-Pout�s")

# trace l'axe des ordonn�es
axis(2,at = c(0,1,2,3,4),labels=c(0,1,2,3,4),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(T-T+1,T-T+1+5,T-T+1+15,T-T+1+25,T-T+1+35,T),
		labels=c(1975,1980,1990,2000,2010,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


text(T,4,labels=expression(italic("b.")),col = "grey55")




xx=c(0:(T+1),(T+1):0)

q_2_5=rep(mu_zone_q[1,1],T+2)
q_97_5=rep(mu_zone_q[1,5],T+2)

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="yellowgreen",border="NA")


q_25=rep(mu_zone_q[1,2],T+2)
q_75=rep(mu_zone_q[1,4],T+2)

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="palegreen4",border="NA")

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
	polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(zone_effect_L_q[i,2],zone_effect_L_q[i,2],zone_effect_L_q[i,4],zone_effect_L_q[i,4]),col="darkolivegreen4")
	#median
	segments(i-0.3,zone_effect_L_q[i,3],i+0.3,zone_effect_L_q[i,3])
}



#####################
#####################

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),xlab="Years",ylim=c(0,8),ylab=expression(italic(kappa["t,3"])),main="Upstream Pout�s")

# trace l'axe des ordonn�es
axis(2,at = c(0,1,2,3,4,5,6,7,8),labels=c(0,1,2,3,4,5,6,7,8),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(T-T+1,T-T+1+5,T-T+1+15,T-T+1+25,T-T+1+35,T),
		labels=c(1975,1980,1990,2000,2010,2013),
		cex.axis = 0.9,las = 1,col = "grey55")


text(T,6,labels=expression(italic("c.")),col = "grey55")




temp=c(11.5,12:(T+1))

temp1=c(rev(12:(T+1)),11.5)


xx=c(temp,temp1)

#q_2_5=rep(mu_zone_q[2,1],T+1-12+2)
#q_97_5=rep(mu_zone_q[2,5],T+1-12+2)

#yy=c(q_2_5,rev(q_97_5))

#polygon(xx,yy,col="yellowgreen",border="NA")


#q_25=rep(mu_zone_q[2,2],T+1-12+2)
#q_75=rep(mu_zone_q[2,4],T+1-12+2)

#yy=c(q_25,rev(q_75))

#polygon(xx,yy,col="palegreen4",border="NA")

#points(seq(11.5,T+1,0.5),rep(mu_zone_q[2,3],(T+1-11)*2),type="l",col="white")
points(seq(11.5,T+1,0.5),rep(1,(T+1-11)*2),type="l",col="black")


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

text(4,3,paste( "upstream Pout�s\n inaccessible"))

dev.off()




#plot mu_zone --> plus d'intéret car zone 2 fixée à 1

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


# Extraction of CODAs
adjust_p_L=read.coda("adjust_p_LCODAchain1.txt","adjust_p_LCODAindex.txt",5001,10000)

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

p_langeac_q=array(rep(0,185),dim=c(37,5))
p_poutes_q=array(rep(0,185),dim=c(37,5))


for (i in 1:37){
	p_langeac_q[i,]=quantile(p_langeac[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}


for (i in 12:37){
	p_poutes_q[i,]=quantile(p_poutes[,i-11],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}





#Graph with all years

#####################
#p_langeac
#####################

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Redds_ProbaPassageStation.png",width=800, height=800, units = "px",type="cairo")


par(mfrow=c(2,1),mar=c(4,6.1,0,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,38),xlab="",ylim=c(0,1),ylab=expression(italic(p^L)),main="")
text(37.5,1,labels=expression(italic("a.")),col = "grey55")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,37),
		labels=c(1975,1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "grey55")



xx=c(0:38,38:0)

q_2_5=rep(mu_p_langeac_q[1],39)
q_97_5=rep(mu_p_langeac_q[5],39)

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="slategray1",border="NA")


q_25=rep(mu_p_langeac_q[2],39)
q_75=rep(mu_p_langeac_q[4],39)

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="slategray3",border="NA")

points(x=seq(0,38,1),y=rep(mu_p_langeac_q[3],39),type="l",col="white")





for(i in 1:37){
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


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,37.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^P)),main="")
text(37.5,1,labels=expression(italic("b.")),col = "grey55")

# trace l'axe des ordonn�es
axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,37),
		labels=c(1975,1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "grey55")


temp=c(11.5,12:38)

temp1=c(rev(12:38),11.5)


xx=c(temp,temp1)

q_2_5=rep(mu_p_poutes_q[1],28)
q_97_5=rep(mu_p_poutes_q[5],28)

yy=c(q_2_5,rev(q_97_5))

polygon(xx,yy,col="slategray1",border="NA")


q_25=rep(mu_p_poutes_q[2],28)
q_75=rep(mu_p_poutes_q[4],28)

yy=c(q_25,rev(q_75))

polygon(xx,yy,col="slategray3",border="NA")



points(seq(11.5,38,0.5),rep(mu_p_poutes_q[3],54),type="l",col="white")





for(i in 12:37){
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

text(6,0.55,paste( "upstream Pout�s\n inaccessible"),col = "grey25")

dev.off()

#De quoi s'agit-il ?
par(mar=c(4,6.1,0,0.5),cex.lab=1.4, cex.lab=1.4)

plot(as.numeric(mu_p_langeac),as.numeric(mu_p_poutes),xlab=expression(mu^Langeac),ylab=expression(mu^Pout�s),xlim=c(0,0.8),ylim=c(0,0.8))

x=seq(-2.5,0.8,0.0001)

points(x,x,type="l",lty=2,lwd=2,col="red")






#================================#
# SPAWNERS - PRODUCTION JUVENILE #
#================================#


S_vichy_real=read.coda("S_vichyCODAchain1.txt","S_vichyCODAindex.txt")
S_langeac_real=read.coda("S_langeacCODAchain1.txt","S_langeacCODAindex.txt")



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



ratio_S_V=array(0,c(5000,T))
ratio_S_L=array(0,c(5000,T))
ratio_S_P=array(0,c(5000,T))


#ratio

for (t in 1:12){
	
	for(i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] + S_langeac_real[i,t])
		ratio_S_L[i,t] = 1 - ratio_S_V[i,t]
	}
}


for (t in 13:T){
	
	for( i in 1:5000){
		ratio_S_V[i,t] = S_vichy_real[i,t] / (S_vichy_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
		ratio_S_L[i,t] = S_langeac_real[i,t] / (S_vichy_real[i,t] + S_langeac_real[i,t] + S_poutes_counter[t])
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



juv_tot_V=read.coda("juv_tot_VCODAchain1.txt","juv_tot_VCODAindex.txt")
juv_tot_L=read.coda("juv_tot_LCODAchain1.txt","juv_tot_LCODAindex.txt")
juv_tot_P=read.coda("juv_tot_PCODAchain1.txt","juv_tot_PCODAindex.txt")


ratio_juv_tot_V=array(0,c(5000,T))
ratio_juv_tot_L=array(0,c(5000,T))
ratio_juv_tot_P=array(0,c(5000,T))


#ratio

for (t in 7:15){
	
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = juv_tot_V[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_L[i,t-6])
		ratio_juv_tot_L[i,t] = 1 - ratio_juv_tot_V[i,t]
	}
}

for (t in 16:T){
	
	for(i in 1:5000){
		ratio_juv_tot_V[i,t] = juv_tot_V[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_L[i,t-6] + juv_tot_P[i,t-15])
		ratio_juv_tot_L[i,t] = juv_tot_L[i,t-6] / (juv_tot_V[i,t-6] + juv_tot_L[i,t-6] + juv_tot_P[i,t-15])
		ratio_juv_tot_P[i,t] = 1 - ratio_juv_tot_V[i,t] - ratio_juv_tot_L[i,t]
	}
}





ratio_juv_tot_V_q=array(0,c(T,5))
ratio_juv_tot_L_q=array(0,c(T,5))
ratio_juv_tot_P_q=array(0,c(T,5))

for (t in 7:T){
	
	ratio_juv_tot_V_q[t,]=quantile(ratio_juv_tot_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	ratio_juv_tot_L_q[t,]=quantile(ratio_juv_tot_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
	
}

for (t in 16:T){
	ratio_juv_tot_P_q[t,]=quantile(ratio_juv_tot_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}



diff_ratio_V=ratio_S_V-ratio_juv_tot_V
diff_ratio_L=ratio_S_L-ratio_juv_tot_L
diff_ratio_P=ratio_S_P-ratio_juv_tot_P

diff_ratio_V_q=array(0,c(T,5))
diff_ratio_L_q=array(0,c(T,5))
diff_ratio_P_q=array(0,c(T,5))

for (t in 1:T){
	
	diff_ratio_V_q[t,]=quantile(diff_ratio_V[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	diff_ratio_L_q[t,]=quantile(diff_ratio_L[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	diff_ratio_P_q[t,]=quantile(diff_ratio_P[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	
	
}




#Graph with all years

png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/SpawnersProdJuv_ratio.png",width=800, height=800, units = "px",type="cairo")


par(mfrow=c(3,3),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4,col.lab="grey25",col.axis="grey55",col.main="grey25")



S_juv_JP=c(2574236,434552,655661)

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


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="Langeac-Pout�s")

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







plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="Langeac-Pout�s")

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



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="Langeac-Pout�s")

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
# Pout�s
#----------


#########
# Prod juveniles


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio juvenile prod")) ,main="upstream Pout�s")

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



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(0,1), ylab=expression(italic("ratio Spawners")) ,main="upstream Pout�s")

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



plot(1,1,type="n",axes=FALSE, xlim=c(0.5,T+0.5), xlab="Years", ylim=c(-0.5,0.5), ylab=expression(italic("difference ratio S - ratio juv")) ,main="upstream Pout�s")

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


mean(p_reach_V)
sd(p_reach_V)
p_reach_V_q=quantile(p_reach_V,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
p_reach_V_q


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

mean(rho)
sd(rho)
rho_q=quantile(rho,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
rho_q

mean(alpha_L)
sd(alpha_L)
alpha_L_q=quantile(alpha_L,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
alpha_L_q


mean(alpha_P)
sd(alpha_P)
alpha_P_q=quantile(alpha_P,probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
alpha_P_q


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

N_vichy_real=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")


N_vichy_real_q=array(0,dim=c(42,5))

for (t in 1:42){
	N_vichy_real_q[t,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#projection 20 years


CV_V=rep(0,42)
for (t in 1:42){
	CV_V[t]=sd(N_vichy_real[,t])/mean(N_vichy_real[,t])
}



par(mar=c(4,7.1,2,4),col.lab="grey25",col.axis="grey55",col.main="grey25")





#sans projections



plot(1,1,type="n",axes=FALSE,xlim=c(0.5,37.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="Adults returns at Vichy")

# trace l'axe des ordonn�es
axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 0.9,las = 1,col = "grey55")
# trace l'axe des abscisses
axis(1,at = c(1,6,16,26,T),
		labels=c(1975,1980,1990,2000,2013),
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


par(new=TRUE)

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,T+0.5),ylab="",xlab="",ylim=c(0,1))

axis(4,at = c(0,0.2,0.4,0.6,0.8,1),
		labels=c(0,0.2,0.4,0.6,0.8,1),
		cex.axis = 0.9,las = 1,col = "grey55")


points(seq(3,22,1),CV_V[3:22],pch=16,col="darkorange")
segments(seq(3,21,1),CV_V[3:21],seq(4,22,1),CV_V[4:22],lty=2,pch=16,col="darkorange")

par(srt=-90) 
mtext(side=4,"CV",line=2.5,cex = 0.9)

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

dmoy_tot_V=read.coda("dmoytot_VCODAchain1.txt","dmoytot_VCODAindex.txt")
dmoy_tot_L=read.coda("dmoytot_LCODAchain1.txt","dmoytot_LCODAindex.txt")
dmoy_tot_P=read.coda("dmoytot_PCODAchain1.txt","dmoytot_PCODAindex.txt")

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


juv_tot=array(rep(0,296000),dim=c(5000,T))


juv_tot_q=array(rep(0,T*5),dim=c(T,5))


for (t in 2:12){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1] + dmoy_tot_L[,t-1] * S_juv_JP[2]
}
for (t in 13:T){
	juv_tot[,t]= dmoy_tot_V[,t-1] * S_juv_JP[1] + dmoy_tot_L[,t-1] * S_juv_JP[2] + dmoy_tot_P[,t-12] * S_juv_JP[3]
	
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

I_surv=read.coda("I_survCODAchain1.txt","I_survCODAindex.txt")
s_juv2ad=read.coda("s_juv2adCODAchain1.txt","s_juv2adCODAindex.txt")
level_s=read.coda("level_sCODAchain1.txt","level_sCODAindex.txt")

s=array(0,c(5000,39))
s_smolt=array(0,c(5000,39))

mu_s_smolt=0.000545


for (t in 1:7){
	s[,t]=s_juv2ad*exp(level_s)
	s_smolt[,t]=mu_s_smolt*exp(level_s)
}

for (t in 8:39){
	s[,t]=s_juv2ad*exp(level_s*I_surv[,t-7])
	s_smolt[,t]=mu_s_smolt*exp(level_s*I_surv[,t-7])
	
}


s_q=array(0,c(39,5))
s_smolt_q=array(0,c(39,5))

I_surv_q=array(0,c(39,5))

for (t in 1:39){
	s_q[t,]=quantile(s[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	s_smolt_q[t,]=quantile(s_smolt[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	I_surv_q[t,]=quantile(I_surv[,t],probs=c(0.01,0.25,0.5,0.75,0.99),names=FALSE)
}


s_q



palette_s=rainbow(15,start=0.1,end=2/6)
palette_s=rev(palette_s)


png(file="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/SurvivalJuv2Ad.png",width=800, height=800, units = "px",type="cairo")



par(mfrow=c(2,1),mar=c(4,7.1,2,0.5),col.lab="grey25",col.axis="grey55",col.main="grey25")


plot(1,1,type="n",axes=FALSE, xlim=c(0.5,39.5), xlab="Years", ylim=c(0,0.02), ylab="",main="average 0+ to returning adult survival" )

# trace l'axe des ordonn�es
axis(2,at = c(0,0.01,0.02),labels=c(0,0.01,0.02),cex.axis = 0.9,las = 1,col = "grey55")
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





plot(1,1,type="n",axes=FALSE, xlim=c(0.5,39.5), xlab="Years", ylim=c(0,0.01), ylab="",main="average stocked smolts to returning adult survival" )

# trace l'axe des ordonn�es
axis(2,at = c(0,0.005,0.01),labels=c(0,0.005,0.01),cex.axis = 0.9,las = 1,col = "grey55")
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

res_vichy=read.coda("res_vichyCODAchain1.txt","res_vichyCODAindex.txt")



res_vichy_q=array(0,dim=c(37,5))

for (t in 7:37){
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

plot(1,1,type="n",axes=FALSE,xlim=c(0.5,35.5),xlab="Years",ylim=c(0,600),ylab=expression(italic(S["t,2"])),main="Langeac-Pout�s")

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


plot(1,1,type="n",axes=FALSE,xlim=c(0.5,35.5),xlab="Years",ylim=c(0,200),ylab=expression(italic(S["t,3"])),main="Upstream Pout�s")

# trace l'axe des ordonn�es
axis(2,at = c(0,100,200),labels=c(0,100,200),cex.axis = 0.9,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = c(1,4,14,24,35),
		labels=c(1977,1980,1990,2000,2011),
		cex.axis = 0.9,las = 1,col = "black")

points(x=seq(10,35,1),y=S_poutes_counter[10:35],pch=16) 
abline(v=9.5,lty=2)
text(4,160,paste( "upstream Pout�s\n inaccessible"))


text(35,200,labels=expression(italic("c.")))

