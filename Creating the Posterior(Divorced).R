library(MASS)
library(invgamma)

y<-divorced$detail_age
n<-length(y)
ybar<-mean(y)

m<-0
v<-10^2
a<-0.01
b<-0.01

#Create vectors
mu<-numeric()
sig2<-numeric()

nRep<-10000

sig2[1]<-var(y)
mu[1]<-ybar


for(j in 2:nRep) {
  #update mu based on sigma^2
  vstar<-1/(n/sig2[j-1]+1/v)
  mstar<-vstar*(n*ybar/sig2[j-1]+m/v)
  mu[j]<-rnorm(1,mstar,sqrt(vstar))
  
  #update sigma^2 based on updated mu
  astar<-n/2+a
  bstar<-sum((y-mu[j])^2)+b
  sig2[j]<-rinvgamma(1,astar,rate=bstar)
}

divorcedMu <- mu
divorcedSig2 <- sig2

save(divorcedMu, divorcedSig2, file="divorced.RData")


par(mfrow=c(2,2))
plot(divorcedMu[-c(1:500)], type='l', main = "Mean of Divorced Age of Death", ylab="Mean of Age of Death")
acf(divorcedMu[-c(1:500)], main = "Mean of Divorced Age of Death")
plot(divorcedSig2[-c(1:500)], type='l', main = "Sigma^2 of Divorced Age of Death", ylab="Mean of Age of Death")
acf(divorcedSig2[-c(1:500)], main = "Sigma^2 of Divorced Age of Death")
par(mfrow=c(1,1))

mean(divorcedMu[-c(1:500)])
sqrt(mean(divorcedSig2[-c(1:500)]))

quantile(divorcedMu[-c(1:500)], c(0.025, 0.975))
quantile(divorcedSig2[-c(1:500)], c(0.025, 0.975))