library(MASS)
library(invgamma)

y<-widow$detail_age
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
#The order of sig2 and mu doesn't matter--the starting value doesn't either
sig2[1]<-var(y)
mu[1]<-ybar #optional--you only need to start with one of the two
#mu[1]<--3000

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

single18Mu <- mu
single18Sig2 <- sig2

plot(single18Mu, type='l')
plot(single18Sig2, type='l')

mean(single18Mu[-c(1:500)])
sqrt(mean(single18Sig2[-c(1:500)]))


