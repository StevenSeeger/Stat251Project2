#Credible Interval of means
quantile(divorcedMu[-c(1:50)] - marriedMu[-c(1:50)], c(0.025, .5, 0.975))