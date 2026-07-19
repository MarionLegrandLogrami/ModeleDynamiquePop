# TODO: Add comment
# 
# Author: guillaume.dauphin
###############################################################################

#setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2015_10_06/")
#setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2016_11_16/")
#setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2017_09_28/")
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2017_09_28/CalibrationTousLesPoints+Sioule/")

library(coda)

#IA Allier
IA=c(52,82,131,120,159,15,21,21,61,61,108,49,5,176,26,66,24,148,63,33,11,33)
calib=22

#IA Gartempe
IA=c(18,27,15,4,18,4,12,5,15,11,12,3)
year=c(rep("2003",4),rep("2004",4),rep("2005",4))
col.year=c(rep("royalblue",4),rep("yellow",4),rep("green",4))
col.year=c(rep(brewer.pal(n = 3, name="Paired"),each=4))
col.year2=c(rep(brewer.pal(n = 4, name="Set1"),3))
library("RColorBrewer")
display.brewer.all()

d=read.coda("dCODAchain1.txt","dCODAindex.txt",2001,10000)

#q_d_v=array(rep(0,9*5),dim=c(9,5))
#Gartempe
q_d_v=array(rep(1,3*5),dim=c(12,5))
for(i in 1:12){
	q_d_v[i,]=quantile(d[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

q.d_1=quantile(d[,1],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_2=quantile(d[,2],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_3=quantile(d[,3],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_4=quantile(d[,4],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_5=quantile(d[,5],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_6=quantile(d[,6],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_7=quantile(d[,7],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_8=quantile(d[,8],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_9=quantile(d[,9],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_10=quantile(d[,10],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_11=quantile(d[,11],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_12=quantile(d[,12],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)

q.d=rbind(
		q.d_1,q.d_2,q.d_3,q.d_4,q.d_5,q.d_6,q.d_7,q.d_8,q.d_9,q.d_10,q.d_11,q.d_12)


#Allier
d=read.coda("dCODAchain1.txt","dCODAindex.txt",2001,10000)
q_d_v=array(rep(1,3*5),dim=c(calib,5))
for(i in 1:calib){
	q_d_v[i,]=quantile(d[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

q.d_1=quantile(d[,1],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_2=quantile(d[,2],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_3=quantile(d[,3],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_4=quantile(d[,4],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_5=quantile(d[,5],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_6=quantile(d[,6],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_7=quantile(d[,7],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_8=quantile(d[,8],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_9=quantile(d[,9],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_10=quantile(d[,10],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_11=quantile(d[,11],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_12=quantile(d[,12],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_13=quantile(d[,13],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_14=quantile(d[,14],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_15=quantile(d[,15],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_16=quantile(d[,16],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_17=quantile(d[,17],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_18=quantile(d[,18],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_19=quantile(d[,19],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_20=quantile(d[,20],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_21=quantile(d[,21],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_22=quantile(d[,22],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)




q.d=rbind(
		q.d_1,q.d_2,q.d_3,q.d_4,q.d_5,q.d_6,q.d_7,q.d_8,
		q.d_9,q.d_10,q.d_11,q.d_12,q.d_13,q.d_14,q.d_15,
		q.d_16,q.d_17,q.d_18,q.d_19,q.d_20,q.d_21,q.d_22)




d_fake=read.coda("d_fakeCODAchain1.txt","d_fakeCODAindex.txt",5001,10000)
#d_fake_old=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2016_11_16/d_fakeCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2016_11_16/d_fakeCODAindex.txt")
d_fake_old=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2017_09_28/d_fakeCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2017_09_28/d_fakeCODAindex.txt")

#Pour ancienne relation Allier avec seulement les 9 points :
d_fake_old_onema=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2014_05_24_GuillaumeDauphin/d_fakeCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2014_05_24_GuillaumeDauphin/d_fakeCODAindex.txt")

#Graphes
#densities

d_fake_q=array(rep(0,1500),dim=c(300,5) )
d_fake_old_q=array(rep(0,1500),dim=c(300,5))
d_fake_old_onema_q=array(rep(0,1500),dim=c(300,5))

IA_fake=seq(1,300,1)
IA_fake_old=seq(1,300,1)
IA_fake_old_onema=seq(1,300,1)

for (i in 1:300){
	
	d_fake_q[i,]=quantile(d_fake[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	#d_fake_old_q[i,]=quantile(d_fake_old[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	#d_fake_old_onema_q[i,]=quantile(d_fake_old_onema[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

#Pour Allier
png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/calibration/2017_09_28/CalibrationTousLesPoints+Sioule/RelationCalibration_new&old.png")
plot(1,1,type="n",axes=FALSE,xlim=c(0,300),xlab="IA 5 minutes",ylim=c(0,1.4),ylab=iconv("densit� (0+ / m-2)","UTF8"),main=iconv("Relation IA vs densit� de 0+","UTF8"))

# trace l'axe des ordonnées
axis(2,at = seq(0,1.4,0.2),cex.axis = 0.8,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = seq(0,300,25),cex.axis = 0.8,las = 1,col = "black")

points(IA_fake,d_fake_q[,3],type="l",col="red",lwd=1.5)
points(IA_fake,d_fake_q[,1],type="l",col="red",lwd=1.5,lty=2)
points(IA_fake,d_fake_q[,5],type="l",col="red",lwd=1.5,lty=2)

points(IA_fake_old,d_fake_old_q[,3],type="l",col="grey",lwd=1.5)
points(IA_fake_old,d_fake_old_q[,1],type="l",col="grey",lwd=1.5,lty=2)
points(IA_fake_old,d_fake_old_q[,5],type="l",col="grey",lwd=1.5,lty=2)

#points(IA_fake_old_onema,d_fake_old_onema_q[,3],type="l",col="black",lwd=1.5)
#points(IA_fake_old_onema,d_fake_old_onema_q[,1],type="l",col="black",lwd=1.5,lty=2)
#points(IA_fake_old_onema,d_fake_old_onema_q[,5],type="l",col="black",lwd=1.5,lty=2)


k=2


#Pour ALLIER - points CSP
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

#points logrami 2015 - Allier
for(i in 10:11){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
}

#points logrami 2015 - Alagnon
for(i in 12:12){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
}

#points logrami 2015 - Allier
for(i in 13:13){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
}

#points logrami 2016 - Allier
for(i in 14:15){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
}

#points logrami 2016 - Alagnon
for(i in 16:17){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
}

#points logrami 2017 - Allier
for(i in 18:19){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
}

#points logrami 2017 - Sioule
#for(i in 22:22){
#	
#	#whiskers
#	#95%
#	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
#	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
#	
#	#5%
#	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
#	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
#	
#	
#	#boxplot
#	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="red")
#	
#	#median
#	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])		
#}

#points logrami 2017 - Alagnon
for(i in 20:21){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])		
}


dev.off()
#pour avoir l'équation de la droite continue (=relation médiane)
lm(d_fake_q[,3]~IA_fake) #0.592
lm(d_fake_old_q[,3]~IA_fake_old) #0.542
lm(d_fake_old_onema_q[,3]~IA_fake_old_onema) #0.473


# graph sans distinction couleur des points
png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/calibration/2017_09_28/CalibrationTousLesPoints+Sioule/RelationCalibration_all_in_grey.png")
plot(1,1,type="n",axes=FALSE,xlim=c(0,300),xlab="IA 5 minutes",ylim=c(0,1.4),ylab=iconv("densité (0+ / m-2)","UTF8"),main=iconv("Relation IA vs densité de 0+","UTF8"))

# trace l'axe des ordonnées
axis(2,at = seq(0,1.4,0.2),cex.axis = 0.8,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = seq(0,300,25),cex.axis = 0.8,las = 1,col = "black")

points(IA_fake,d_fake_q[,3],type="l",col="red",lwd=1.5)
points(IA_fake,d_fake_q[,1],type="l",col="red",lwd=1.5,lty=2)
points(IA_fake,d_fake_q[,5],type="l",col="red",lwd=1.5,lty=2)


#points(IA_fake_old_onema,d_fake_old_onema_q[,3],type="l",col="black",lwd=1.5)
#points(IA_fake_old_onema,d_fake_old_onema_q[,1],type="l",col="black",lwd=1.5,lty=2)
#points(IA_fake_old_onema,d_fake_old_onema_q[,5],type="l",col="black",lwd=1.5,lty=2)


k=2


#Pour ALLIER -
for(i in 1:21){
	
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

#-------------------------------------------------
# Allier sans la station de Monistrol 2017
#-------------------------------------------------
setwd("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2017_09_28/CalibrationSS_Monistrol/")

library(coda)

#IA Allier
IA=c(52,82,131,120,159,15,21,21,61,61,108,49,5,176,26,66,24,63,33,11)
calib=20
d=read.coda("dCODAchain1.txt","dCODAindex.txt",2001,10000)

q_d_v=array(rep(1,3*5),dim=c(calib,5))
for(i in 1:calib){
	q_d_v[i,]=quantile(d[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

q.d_1=quantile(d[,1],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_2=quantile(d[,2],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_3=quantile(d[,3],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_4=quantile(d[,4],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_5=quantile(d[,5],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_6=quantile(d[,6],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_7=quantile(d[,7],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_8=quantile(d[,8],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_9=quantile(d[,9],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_10=quantile(d[,10],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_11=quantile(d[,11],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_12=quantile(d[,12],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_13=quantile(d[,13],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_14=quantile(d[,14],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_15=quantile(d[,15],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_16=quantile(d[,16],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_17=quantile(d[,17],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_18=quantile(d[,18],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_19=quantile(d[,19],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
q.d_20=quantile(d[,20],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)


q.d=rbind(
		q.d_1,q.d_2,q.d_3,q.d_4,q.d_5,q.d_6,q.d_7,q.d_8,
		q.d_9,q.d_10,q.d_11,q.d_12,q.d_13,q.d_14,q.d_15,
		q.d_16,q.d_17,q.d_18,q.d_19,q.d_20)

d_fake=read.coda("d_fakeCODAchain1.txt","d_fakeCODAindex.txt",5001,10000)
d_fake_21=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2017_09_28/d_fakeCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2017_09_28/d_fakeCODAindex.txt")
d_fake_old=read.coda("C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2016_11_16/d_fakeCODAchain1.txt","C:/Users/LOGRAMI/workspace/ModeleDynamiquePop/data/CODA/calibration/2016_11_16/d_fakeCODAindex.txt")

d_fake_q=array(rep(0,1500),dim=c(300,5) )
d_fake_21_q=array(rep(0,1500),dim=c(300,5) )
d_fake_old_q=array(rep(0,1500),dim=c(300,5))

IA_fake=seq(1,300,1)
IA_fake_21=seq(1,300,1)
IA_fake_old=seq(1,300,1)

for (i in 1:300){
	
	d_fake_q[i,]=quantile(d_fake[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	d_fake_21_q[i,]=quantile(d_fake_21[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	d_fake_old_q[i,]=quantile(d_fake_old[,i],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
}

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/calibration/2017_09_28/CalibrationSS_Monistrol/RelationCalibration_new&old.png")
plot(1,1,type="n",axes=FALSE,xlim=c(0,300),xlab="IA 5 minutes",ylim=c(0,1.4),ylab=iconv("densité (0+ / m-2)","UTF8"),main=iconv("Relation IA vs densité de 0+","UTF8"))

# trace l'axe des ordonnées
axis(2,at = seq(0,1.4,0.2),cex.axis = 0.8,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = seq(0,300,25),cex.axis = 0.8,las = 1,col = "black")

points(IA_fake,d_fake_q[,3],type="l",col="red",lwd=1.5)
points(IA_fake,d_fake_q[,1],type="l",col="red",lwd=1.5,lty=2)
points(IA_fake,d_fake_q[,5],type="l",col="red",lwd=1.5,lty=2)

points(IA_fake_old,d_fake_old_q[,3],type="l",col="black",lwd=1.5)
points(IA_fake_old,d_fake_old_q[,1],type="l",col="black",lwd=1.5,lty=2)
points(IA_fake_old,d_fake_old_q[,5],type="l",col="black",lwd=1.5,lty=2)

#points(IA_fake_21,d_fake_21_q[,3],type="l",col="grey",lwd=1.5)
#points(IA_fake_21,d_fake_21_q[,1],type="l",col="grey",lwd=1.5,lty=2)
#points(IA_fake_21,d_fake_21_q[,5],type="l",col="grey",lwd=1.5,lty=2)

#points(IA_fake_old_onema,d_fake_old_onema_q[,3],type="l",col="black",lwd=1.5)
#points(IA_fake_old_onema,d_fake_old_onema_q[,1],type="l",col="black",lwd=1.5,lty=2)
#points(IA_fake_old_onema,d_fake_old_onema_q[,5],type="l",col="black",lwd=1.5,lty=2)


k=2


#Pour ALLIER
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

for(i in 10:11){
	
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

for(i in 12:12){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="royalblue")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
}

for(i in 13:13){
	
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

for(i in 14:15){
	
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

for(i in 16:17){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="royalblue")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
	
}

for(i in 18:18){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="black")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])
}

for(i in 19:20){
	
	#whiskers
	#95%
	segments(IA[i]-4,q.d[i,5],IA[i]+4,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-4,q.d[i,1],IA[i]+4,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-4,IA[i]+4,IA[i]+4,IA[i]-4),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col="cyan")
	
	#median
	segments(IA[i]-4,q.d[i,3],IA[i]+4,q.d[i,3])		
}

dev.off()
#pour avoir l'équation de la droite continue (=relation médiane)
lm(d_fake_q[,3]~IA_fake) #0.554


d_fake=read.coda("d_fakeCODAchain1.txt","d_fakeCODAindex.txt",5001,10000)


#Pour GARTEMPE

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/calibration/2016_10_07_Gartempe/RelationCalibrationGartempe2.png")
plot(1,1,type="n",axes=FALSE,xlim=c(0,50),xlab="IA 5 minutes",ylim=c(0,0.3),ylab="density (0+ / m-2)",main="IA vs 0+ density relationship")

# trace l'axe des ordonnées
axis(2,at = seq(0,0.3,0.05),cex.axis = 0.8,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = seq(0,50,5),cex.axis = 0.8,las = 1,col = "black")

points(IA_fake,d_fake_q[,3],type="l",col="red",lwd=1.5)
points(IA_fake,d_fake_q[,1],type="l",col="red",lwd=1.5,lty=2)
points(IA_fake,d_fake_q[,5],type="l",col="red",lwd=1.5,lty=2)


k=2


for(i in 1:12){
	
	#whiskers
	#95%
	segments(IA[i]-0.5,q.d[i,5],IA[i]+0.5,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-0.5,q.d[i,1],IA[i]+0.5,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-0.5,IA[i]+0.5,IA[i]+0.5,IA[i]-0.5),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col=col.year[i])
	
	#median
	segments(IA[i]-0.5,q.d[i,3],IA[i]+0.5,q.d[i,3])
}

dev.off()

	#Graphe avec couleur pour station

png(filename="C:/Users/logrami/workspace/ModeleDynamiquePop/img/calibration/2016_10_07_Gartempe/RelationCalibrationGartempe_couleurStation.png")
plot(1,1,type="n",axes=FALSE,xlim=c(0,50),xlab="IA 5 minutes",ylim=c(0,0.3),ylab="density (0+ / m-2)",main="IA vs 0+ density relationship")

# trace l'axe des ordonnées
axis(2,at = seq(0,0.3,0.05),cex.axis = 0.8,las = 1,col = "black")
# trace l'axe des abscisses
axis(1,at = seq(0,50,5),cex.axis = 0.8,las = 1,col = "black")

points(IA_fake,d_fake_q[,3],type="l",col="red",lwd=1.5)
points(IA_fake,d_fake_q[,1],type="l",col="red",lwd=1.5,lty=2)
points(IA_fake,d_fake_q[,5],type="l",col="red",lwd=1.5,lty=2)


k=2

for(i in 1:12){
	
	#whiskers
	#95%
	segments(IA[i]-0.5,q.d[i,5],IA[i]+0.5,q.d[i,5])
	segments(IA[i],q.d[i,4],IA[i],q.d[i,5])
	
	#5%
	segments(IA[i]-0.5,q.d[i,1],IA[i]+0.5,q.d[i,1])
	segments(IA[i],q.d[i,2],IA[i],q.d[i,1])
	
	
	#boxplot
	polygon(c(IA[i]-0.5,IA[i]+0.5,IA[i]+0.5,IA[i]-0.5),c(q.d[i,2],q.d[i,2],q.d[i,4],q.d[i,4]),col=col.year2[i])
	
	#median
	segments(IA[i]-0.5,q.d[i,3],IA[i]+0.5,q.d[i,3])
}

dev.off()

lm(d_fake_q[,3]~IA_fake)

write.table(d_fake_q, "d_predict.txt")


