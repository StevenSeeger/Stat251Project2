makeGraph <- function(df, nms=NULL)
{
  if(!require(foreach)){
    install.packages("foreach")
    library(foreach)
  }
  
  if(!require(doParallel)){
    install.packages("doParallel")
    library(doParallel)
  }
  
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  
  if(is.null(nms))
  {
    names <- c(rep("No Title"), length(df))
  }
  
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  m.list <- list()
  

  m.list <- foreach(i = 1:length(df),
                    .packages = "ggplot2") %dopar% {
                      breaks <- pretty(range(na.omit(df[,i])), n = nclass.FD(na.omit(df[,i])), min.n = 1)
                      bwidth <- breaks[2]-breaks[1]
                      
                      gmean <- mean(na.omit(df[,i]))
                      gmedian <- median(na.omit(df[,i]))
                      
                      graph <- ggplot(na.omit(df[i]), aes_string(x=colnames(df[i]))) + 
                        geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = bwidth)+
                        geom_density(alpha=.2, fill="#FF6666") + 
                        geom_vline(aes(xintercept=mean(gmean)),
                                   color="lightsteelblue3", linetype="dashed", size=1) +
                        geom_vline(aes(xintercept=median(gmedian)),
                                   color="darkseagreen3", linetype="dashed", size=1) +
                        ggtitle(nms[i]) +
                        xlab("Fms") + 
                        ylab("Density")
                      
                      graph
                    }
  
  stopCluster(cl)
  stopImplicitCluster()
  
  return(m.list)
  
}
