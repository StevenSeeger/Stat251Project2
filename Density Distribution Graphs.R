library(ggplot2)

Sgraph <- ggplot(single, aes(x=detail_age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5)+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(detail_age)),
             color="lightsteelblue3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(detail_age)),
             color="darkseagreen3", linetype="dashed", size=1) +
  xlab("Fms") + 
  ylab("Density") +
  ggtitle("Single Age of Death Density Distribution")

Mgraph <- ggplot(married, aes(x=detail_age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5)+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(detail_age)),
             color="lightsteelblue3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(detail_age)),
             color="darkseagreen3", linetype="dashed", size=1) +
  xlab("Fms") + 
  ylab("Density") +
  ggtitle("Married Age of Death Density Distribution")

Wgraph <- ggplot(widow, aes(x=detail_age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5)+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(detail_age)),
             color="lightsteelblue3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(detail_age)),
             color="darkseagreen3", linetype="dashed", size=1) +
  xlab("Fms") + 
  ylab("Density") +
  ggtitle("Widow Age of Death Density Distribution")

Dgraph <- ggplot(divorced, aes(x=detail_age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5)+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(detail_age)),
             color="lightsteelblue3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(detail_age)),
             color="darkseagreen3", linetype="dashed", size=1) +
  xlab("Fms") + 
  ylab("Density") +
  ggtitle("Divorced Age of Death Density Distribution")
