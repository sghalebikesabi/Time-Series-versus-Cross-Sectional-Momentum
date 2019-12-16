#checked
#---------------------------------------------------------volatility scaling
f.delta <- function(delta){
  return(abs(sum((1-delta)*delta^(0:59))-0.98))
}

delta <- optimize(f.delta,interval=c(0,1))$minimum
  
vola <- function(data){ 
  #Input: data - daily prices in data.frame, nmon - numbers of the last months considered
  #Output: sigma^2_t in data.frame for all t and i
  #nmon <- nrow(removena(monexret(data)))
  vol <- monexret(data)
  vol <- vol[1:(nrow(vol)-1),] #vol is used lagged, we do not need two last  periods
  nmon <- nrow(vol) #nmon are all months we need
  x <- dayexret(data)
  #create vector with the last days in each month
  lastdays <- rep(F,nrow(x)) #new column in x indicating wether date of the row is the last day in the month or not
  i <- 0 #running index for each row of x
  while(months(x$Date[nrow(x)-i])==months(x$Date[nrow(x)-i-1])) i <- i+1 
  i <- i+1 #we do not need the last month, as lagged volatility is used
  while(nmon > 0 & i <nrow(x)-1){
    if(months(x$Date[nrow(x)-i])!=months(x$Date[nrow(x)-i-1])){
      lastdays[nrow(x)-i-1] <- T #is lastday
      nmon <- nmon-1 #one month found
    }
    i <- i+1
  }
  seq.lastdays <- lastdays * seq(1,nrow(x)) 
  seq.ld <- seq.lastdays[seq.lastdays!=0] #saves the row numbers of last days
  if(length(seq.ld)<nrow(vol)){ #if last day in data set is last day of month
    i <- 0 #running index for each row of x
    while(months(x$Date[nrow(x)-i])==months(x$Date[nrow(x)-i-1])) i <- i+1 
    i <- i+1 #we do not need the last month, as lagged volatility is used
    while(months(x$Date[nrow(x)-i])==months(x$Date[nrow(x)-i-1])) i <- i+1 
    seq.ld[length(seq.ld)+1] <- nrow(x)-i-1
  } 
  rrow <- 1 #running index for the current row in vol
  for(t in seq.ld){
    ave <- scaled_ret(x,t) #exponentially weighted average return
    xmexc <- x[t:1,2:ncol(x)] - matrix(as.numeric(ave[1,]),t,ncol(x)-1,byrow=T) #xmexc_t = R_it-1-s - bar(R)_it-1 
    xmexc <- (1-delta) * delta^(0:(t-1))*xmexc^2 #xmexc_t = (1-delta) * delta * (R_it-1-s - bar(R)_it-1)^2
    sum <- matrix(NA,1,ncol(x)-1)
    for(i in 1:ncol(xmexc)) sum[1,i] <- sum(xmexc[,i][is.na(xmexc[,i])==F])
    vol[rrow,2:ncol(x)] <- sqrt(261 * sum[1,]) #volatility
    rrow <- rrow + 1
  }
  vol[vol==0] <- NA
  return(vol)
}

scaled_ret <- function(x, t){
  #Input: dayexret(data), time t until average shall be calculated
  #Output: bar(R)_it of the equation for calculating the volatility
  ave <- x[1,2:ncol(x)] #no date
  for(i in 2:ncol(x)){
    entries <- delta^((t-1):0) * x[1:t,i]
    ave[1,i-1] <- sum(entries[is.na(entries)==F])/sum(delta^((t-1):0)[is.na(entries)==F]*(1-delta))
  }
  return(ave * (1-delta))
}
