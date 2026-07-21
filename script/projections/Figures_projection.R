# TODO: Add comment
# 
# Author: Marion LEGRAND
###############################################################################

#!!!!!!!!!!!!!!!!!!!!!!!!#
# FIXME : library + Path #
#!!!!!!!!!!!!!!!!!!!!!!!!#
library(coda)
setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/SimulationImprovementSurvival50%/2014_10_10_SurfErr+SurfDev")
#setwd("C:/Users/utilisateur/workspace/ModeleDynamiquePop/data/CODA/SimulationImprovementSurvival50%/2014_05_30")


##########################
# CHAP: Without stocking
##########################
#CF SimulationWithoutStocking_RetrospectiveAnalysis.R
	
#################################################
# CHAP: Improvement migratory transparancy only #
#################################################

	#=====================
	# SEC: Returns Vichy
	#=====================
	
	#======================
	# SEC: Threshold
	#======================

################################################
# CHAP: Improvement migration and survival 50% #
################################################
	
	#=====================
	# SEC: Returns Vichy
	#=====================

	#======================
	# SEC: Threshold
	#======================

##################################
# CHAP: Improvement survival 50% #
##################################
	
	#=====================
	# SEC: Returns Vichy
	#=====================
	N_vichy_real=read.coda("N_vichyCODAchain1.txt","N_vichyCODAindex.txt")#,2001,10000)
		
	N_vichy_real_q=array(0,dim=c(42,5))
	
	for (t in 1:42){
		N_vichy_real_q[t,]=quantile(N_vichy_real[,t],probs=c(0.025,0.25,0.5,0.75,0.975),names=FALSE)
	}
		#--------------------------------------------------------------------------------------
		#Figure : ANNUAL ADULT RETURN 20 years projection - improvement 50% old level survival
		#--------------------------------------------------------------------------------------
		png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/Simulation/2014.05.30/_outputAllier_SimulationImprovement50_.png",width=800,height=800)
		
		par(mfrow=c(1,1),col.lab="grey25",col.axis="grey55",col.main="grey25")
		
			
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,57.5),xlab="Years",ylim=c(0,9000),ylab="Returns Vichy",main="20 years projection - improvement 50% old level survival",cex.lab = 1.5)
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),labels=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000),cex.axis = 1.2,las = 1,lwd=2,col = "grey55")
		# trace l'axe des abscisses
		axis(1,at = c(1,6,16,26,39,46,59),
				labels=c(1975,1980,1990,2000,2013,2020,2033),
				cex.axis = 1.2,las = 1,lwd=2,col = "grey55")
				
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
				
		for(i in (T+1):(T+20)){
			#whiskers
			#95%
			segments(i-0.15,N_vichy_real_q[i-17,5],i+0.15,N_vichy_real_q[i-17,5])
			segments(i,N_vichy_real_q[i-17,4],i,N_vichy_real_q[i-17,5])
			#5%
			segments(i-0.15,N_vichy_real_q[i-17,1],i+0.15,N_vichy_real_q[i-17,1])
			segments(i,N_vichy_real_q[i-17,2],i,N_vichy_real_q[i-17,1])
			#boxplot
			polygon(c(i-0.3,i+0.3,i+0.3,i-0.3),c(N_vichy_real_q[i-17,2],N_vichy_real_q[i-17,2],N_vichy_real_q[i-17,4],N_vichy_real_q[i-17,4]),col="darkgoldenrod2")
			#median
			segments(i-0.3,N_vichy_real_q[i-17,3],i+0.3,N_vichy_real_q[i-17,3])
		}
		dev.off()
	#======================
	# SEC: Threshold
	#======================

	under_10_vichy=array(0,dim=c(8000,20))
	under_50_vichy=array(0,dim=c(8000,20))
	under_100_vichy=array(0,dim=c(8000,20))
	under_250_vichy=array(0,dim=c(8000,20))
	under_500_vichy=array(0,dim=c(8000,20))
	
	for (t in 23:42){
		
		for (i in 1:8000){
			if(N_vichy_real[i,t] < 10){under_10_vichy[i,t-22]=1} 
			if(N_vichy_real[i,t] < 50){under_50_vichy[i,t-22]=1}
			if(N_vichy_real[i,t] < 100){under_100_vichy[i,t-22]=1}
			if(N_vichy_real[i,t] < 250){under_250_vichy[i,t-22]=1}
			if(N_vichy_real[i,t] < 500){under_500_vichy[i,t-22]=1}
			
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
		
		#-----------------------------------------------------------------------------
		# Figure : THRESHOLD 20 years projection - improvement 50% old level survival
		#-----------------------------------------------------------------------------
png(filename="C:/Users/utilisateur/workspace/ModeleDynamiquePop/img/2014_09_26SurfERR+SurfDev/simulation/Threshold_50_trois.png",width=800,height=800)		

		par(mfrow=c(1,1),mar=c(4,6.1,2,0.5),cex.lab=1.4, cex.lab=1.4)
		
		plot(1,1,type="n",axes=FALSE,xlim=c(0.5,20.5),xlab="Years",ylim=c(0,1),ylab=expression(italic(p^threshold)),main="20 years projection - improvement 50% old level survival")
		
		# trace l'axe des ordonn�es
		axis(2,at = c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),cex.axis = 0.9,las = 1,col = "grey55")
		# trace l'axe des abscisses
		axis(1,at = c(1,9,19),
				labels=c(2012,2020,2030),
				cex.axis = 0.9,las = 1,col = "grey55")
		
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
		
		dev.off()

###################################
# CHAP: Improvement survival 100% #
###################################

	#=====================
	# SEC: Returns Vichy
	#=====================
	
	#======================
	# SEC: Threshold
	#======================




