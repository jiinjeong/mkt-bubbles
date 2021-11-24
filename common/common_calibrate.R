# =================== STEP 1. Collect historic data. ===================
# ----------- a. Bitcoin Daily Data in USD
collect_btc <- function(){
  btc = coin_history(
    coin_id = "bitcoin",
    vs_currency = "usd",
    days = "max",
    interval = "daily")
  
  btc = btc[order(as.Date(btc$timestamp, format="%Y-%m-%d")),]  # Order by date
  
  # Calculate simple and CC (log) returns
  btc.cc = diff(log(btc$price))
  btc.simple = exp(btc.cc) - 1
  btc.date = as.Date(btc$timestamp[2:length(btc$timestamp)])
  
  btc.df = data.frame(
    "date" = btc.date,
    "price" = as.vector(btc$price[2:length(btc$price)]),
    "simple" = btc.simple,
    "cc" = btc.cc
  )
  btc.ts = crypto_to_ts(btc.df)  # Time series data
  # write.csv(btc.df, file="btc.csv")
}

# ----------- b. Vanguard S&P 500 Index Daily Data
collect_sp500 <- function(){
  sp500 = get.hist.quote(instrument="vfinx", quote="AdjClose",
                         provider="yahoo", origin="1970-01-01",
                         compression="d", retclass="zoo")
  # start=start.date, end=end.date, 
  sp500
  sp500.cc = diff(log(sp500$Adjusted))
  sp500.simple = exp(sp500.cc) - 1
  sp500.date = as.Date(index(sp500)[2:length(sp500$Adjusted)])
  sp500.df = data.frame(
    "date" = sp500.date,
    "price" = as.vector(sp500$Adjusted[2:length(sp500$Adjusted)]),
    "simple" = sp500.simple,
    "cc" = sp500.cc
  )
  rownames(sp500.df) = NULL
  sp500.df
  sp500.ts = crypto_to_ts(sp500.df)  # Time series data
  # write.csv(sp500.df, file="sp500.csv")
}


# =================== STEP 2. Basic Plots and Stats ===================
basic_plots <- function(df){
  # Time series plot of historical Bitcoin price and returns
  price_plot <- ggplot(df, aes(x=date, y=price)) + geom_line()
  return_plot <- ggplot(df, aes(x=date)) + 
    geom_line(aes(y=cc), color="red") +
    geom_line(aes(y=simple), color="black")
  
  print(price_plot)
  print(return_plot)
  
  # Histogram + QQPlot + More
  par(mfrow=c(2,2))
  hist(df$cc,main="Daily Returns",
       xlab="btc", probability=T, col="slateblue1")
  boxplot(df$cc,outchar=T,col="slateblue1")
  plot(density(df$cc), main="smoothed density", 
       type="l",xlab="daily return",
       ylab="density estimate")
  qqnorm(df$cc)
  qqline(df$cc)
  par(mfrow=c(1,1))
}

basic_stats <- function(ts){
  sd(ts)
  skewness(ts)
  kurtosis(ts)
  summary(ts)
  
  calc_q_ratio(ts)
  calc_hurst(ts)
  calc_hill(ts)
  calc_jb(ts)
}
