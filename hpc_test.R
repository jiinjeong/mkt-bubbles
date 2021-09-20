dependencies = c("geckor", "ggplot2", "tseries", "forecast", "xts", "PerformanceAnalytics",
                 "pracma", "evir", "AutoSEARCH", "moments", "aTSA")
for (depen in dependencies) {
  if(depen %in% rownames(installed.packages()) == FALSE){
    install.packages(depen, dependencies = TRUE)
  }
  library(depen, character.only = TRUE) 
}
