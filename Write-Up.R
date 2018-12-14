## ----setup, echo=FALSE---------------------------------------------------
library(knitr)
library(kableExtra)
library(invgamma)
library(ggplot2)
load("divorced.RData")
load("married.RData")
load("Summary Table.RData")

## ---- echo=FALSE, warning=FALSE,message=FALSE----------------------------
df <- rnorm(10000, 0, 10)

df <- as.data.frame(df)

ggplot(df, aes(x=df)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 1)+
 geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Prior N(0,10^2)")+
  xlab("x")

x <- seq(0, 5, .01)
qplot(x, dinvgamma(x, 0.01, 0.01), geom = "line") +
  ggtitle("Prior IG(0.01,0.01)")+
  ylab("Density")

## ----label1, out.width = "100%", fig.cap = "Boxplot", echo = FALSE-------
include_graphics("Report Files/Boxplot.pdf")

## ----SummaryStats, echo=FALSE--------------------------------------------
kable(TableSummary, caption = "Summary Statistics (age of death)")%>%
kable_styling(latex_options = c("striped", "hold_position", "scale_down"))

## ----label, out.width = "100%", fig.cap = "Density Plots", echo = FALSE----
include_graphics("Report Files/Density Plot.pdf")

## ---- echo=FALSE, results='hide', message=FALSE--------------------------
diff<-marriedMu[-c(1:500)]-divorcedMu[-c(1:500)]
diffMean <- mean(diff)
ci <- quantile(diff,c(0.025,0.975))

marriedD <- data.frame(Age=marriedMu[-c(1:500)])
marriedD$type <- rep("Married",length(marriedMu[-c(1:500)]), by=1)

divorcedD <- data.frame(Age=divorcedMu[-c(1:500)])
divorcedD$type <- rep("Divorced",length(divorcedMu[-c(1:500)]), by=1)

MDD <- rbind(marriedD, divorcedD)

library(plyr)
mu <- ddply(MDD, "type", summarise, grp.mean=mean(Age))

prior <- rnorm(length(divorcedMu[-c(1:500)]), 0,10)

priorD <- data.frame(Age=prior)
priorD$type <- rep("Prior",length(marriedMu[-c(1:500)]), by=1)

marriedD1 <- data.frame(Age=marriedMu[-c(1:500)])
marriedD1$type <- rep("Posterior",length(marriedMu[-c(1:500)]), by=1)

marriedDP <- rbind(marriedD1, priorD)

divorcedD1 <- data.frame(Age=divorcedMu[-c(1:500)])
divorcedD1$type <- rep("Posterior",length(divorcedMu[-c(1:500)]), by=1)

divorcedDP <- rbind(divorcedD1,priorD)


## ---- echo=FALSE---------------------------------------------------------
ggplot(MDD,aes(x=Age, color=type)) + geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="dashed")+
  ggtitle("Posterior Distribution of Married and Divorced")

## ---- echo=FALSE---------------------------------------------------------
ggplot(marriedDP,aes(x=Age, color=type)) + geom_density()+
  ggtitle("Prior Versus Married Posterior")

ggplot(divorcedDP,aes(x=Age, color=type)) + geom_density()+
  ggtitle("Prior Versus Divorced Posterior")

## ---- echo=FALSE---------------------------------------------------------

marriedN <- marriedMu[-c(1:500)]
divorcedN <- divorcedMu[-c(1:500)]
Mci <- quantile(marriedN,c(0.025,0.975))
Dci <- quantile(divorcedN,c(0.025,0.975))

outputTable <- rbind(round(Mci, 2), round(Dci, 2), round(ci, 2))

rownames(outputTable) <- c("Married", "Divorced", "Difference")

kable(outputTable, caption = "Posterior Credible Intervals of Means (on Age of Death)")%>%
kable_styling(latex_options = c("striped", "hold_position"))


## ---- echo=FALSE---------------------------------------------------------
ggplot(data.frame(diff), aes(x=diff, color='red'))+
  geom_density()+
  geom_vline(xintercept=diffMean, color='black')+
  ggtitle("Difference between  Married and Divorced Average Age of Death")+
  theme(legend.position = 'none')

