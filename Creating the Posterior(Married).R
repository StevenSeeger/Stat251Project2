library(MASS)
library(invgamma)

y<-married$detail_age
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

marriedMu <- mu
marriedSig2 <- sig2

save(marriedMu, marriedSig2, file="married.RData")

plot(marriedMu, type='l')
plot(marriedSig2, type='l')
plot(acf(marriedMu[-c(1:500)]))
plot(acf(marriedSig2[-c(1:500)]))

mean(marriedMu[-c(1:500)])
sqrt(mean(marriedSig2[-c(1:500)]))

quantile(marriedMu[-c(1:500)], c(0.025, 0.975))
quantile(marriedSig2[-c(1:500)], c(0.025, 0.975))

par(mfrow=c(2,2))
plot(marriedMu[-c(1:500)], type='l', main = "Mean of Divorced Age of Death", ylab="Mean of Age of Death")
acf(marriedMu[-c(1:500)], main = "Mean of Divorced Age of Death")
plot(marriedSig2[-c(1:500)], type='l', main = "Sigma^2 of Divorced Age of Death", ylab="Mean of Age of Death")
acf(marriedSig2[-c(1:500)], main = "Sigma^2 of Divorced Age of Death")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(marriedMu[-c(1:500)], type='l', main = "Trace Plot of Mean of Married Age of Death", ylab="Mean of Age of Death")
acf(marriedMu[-c(1:500)], main = "Mean of Married Age of Death ACF")
plot(marriedSig2[-c(1:500)], type='l', main = " Trace Plot of Sigma^2 of Married Age of Death", ylab="Mean of Age of Death")
acf(marriedSig2[-c(1:500)], main = "Sigma^2 of Married Age of Death ACF")
par(mfrow=c(1,1))
