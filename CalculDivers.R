# TODO: Add comment
# 
# Author: LOGRAMI
###############################################################################

library(faraway)
ilogit(0.8174449)
logit(0.446)
logit(0.887)
x<-c(0.249,0.391,0.320,0.172,0.521,NA,0.521,0.178,0.521,0.154,0.154,0.160,0.249,0.249,0.249,0.249,0.249,0.249,NA,NA,0.355,0.521,0.432,0.391,0.408,NA,NA,0.418,0.407,0.407,0.497,NA,0.435,NA,0.485875706,0.88700565,0.446327684,NA)
logit(x)



x <- seq(0, 1, length = 21)
dbeta(x, 1, 1)
pbeta(x, 1, 1)
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
	if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
		eps <- 1e-10
		x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
	} else {
		x <- seq(0, 1, length = 1025)
	}
	fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
	f <- fx; f[fx == Inf] <- 1e100
	matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
			main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
	abline(0,1,     col="gray", lty=3)
	abline(h = 0:1, col="gray", lty=3)
	legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
			col=1:3, lty=1:3, bty = "n")
	invisible(cbind(x, fx))
}
pl.beta(2,2)

x <- seq(0, 1, length = 1025)
dbeta(x, 2,2)
plot(dbeta(x, 2,2))
summary(dbeta(x, 2,2))


curve(dbeta(x,2,2))
abline(v=qbeta(.025,2,2))
abline(v=qbeta(.975,2,2))

summary(x, probs=seq(0,1,0.1))
