f <- function(x,r=2){
  return(-x^2*r)
}

optimize(f, r=5, interval=c(-4,4),maximum=T)


m <- matrix(1:9,3,3)
ranking <- rank(m)/length(m)
p <- 10:18
p[ranking>0.8]
t <- c(NA,NA,1,2,3,NA)
rank(t)
paste0(t)

paste0("Periode",mon.index,"ranking =", ranking[1,])

p[(3:7) &(p > 15)]
cdax.mret[1,prod.ret.rp>0]
t <- na.omit(t)
t*2
sum(t)

m1 <- matrix(1:9,9,1)
m2 <- matrix(1:9,9,1)
m1/m2

years(cdax.mret[2,1])
plot(1:10)
strftime(cdax.mret[2,1], format="%y")

2*t 
t/t-1

m1/m2


sum <- function(x=4){
  return(x)
}

require(quantstrat)

#Load ETFs from yahoo
currency("USD")
symbols = c("XLY", "XLP", "XLE", "XLF")
stock(symbols, currency="USD",multiplier=1)
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='2000-01-01')

install.packages("quantstrat", repos="http://R-Forge.R-project.org")
install.packages("PerformanceAnalytics", repos="http://R-Forge.R-project.org")
install.packages("foreach", repos="https://R-Forge.R-project.org")

install.packages("/Users/sahra/Downloads/quantstrat_0.9.1739.zip")
install.packages("FinancialInstrument")
install.packages("PerformanceAnalytics")
install.packages("foreach")


A <- matrix(1:9,3,3)
B <- matrix(100:92,3,3)
C <- avetstat(A,B)

vola <- function(data, begin){ 
  #Input: daily prices in data.frame
  #Output: sigma^2_t in data.frame for all t and i
  delta <- 60/61 #such that delta/(1-delta) = 60
  x <- dayexret(data)
  #create vector with the last days in each month
  lastdays <- rep(F,nrow(x)) #new column in x indicating wether date of the row is the last day in the month or not
  for(i in begin:(nrow(x)-1)){
    if(months(x$Date[i])!=months(x$Date[i+1])) lastdays[i] <- T
  }
  x$lastday <- lastdays
  nlastdays <- sum(x$lastday==T)
  vol <- x[x$lastday==T,]
  seq.lastdays <- lastdays * seq(begin,nrow(x))
  rrow <- 0 #running index for the current row in vol
  for(t in begin:(nrow(x)-1)){
    ave <- scaled_ret(x,t) #exponentially weighted average return
    xmexc <- x[1:t,2:ncol(x)] #construct xmexc in size t*ncol(x)-1
    for(i in 1:t) xmexc[t-i+1,] <- x[i,2:ncol(x)] - ave[,2:ncol(ave)] #xmexc_t = R_it-1-s - bar(R)_it-1 
    xmexc <- (1-delta) * delta^(0:(t-1))*xmexc^2 #xmexc_t = (1-delta) * delta * (R_it-1-s - bar(R)_it-1)^2
    sum <- x[t,2:ncol(x)]
    for(j in 1:t) {
      for(i in 1:(ncol(x)-1)) if(is.na(xmexc[j,i])==F) sum[1,i] <- sum[1,i] + xmexc[j,i]
    }
    vol[t,2:ncol(x)] <- 261 * as.numeric(sum[1,]) #volatility 
  }
  return(vol)
}