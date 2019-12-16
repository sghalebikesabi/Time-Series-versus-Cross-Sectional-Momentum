#---------------------------------------------------------returns
dayexret <- function(x){
  #Input: daily prices 
  #Output: daily excess returns
  rf.ret <- rf
  #rf.ret[,2] <- rf[2:nrow(rf),2]/rf[1:(nrow(rf)-1),2]-1
  x.ret <- x[2:nrow(x),]
  x.ret[,2:ncol(x)] <- x[2:nrow(x),2:ncol(x)]/x[1:(nrow(x)-1),2:ncol(x)]-1
  if(nrow(x.ret) > nrow(rf.ret)){
    result <- x.ret[match(rf.ret[1,1], x.ret[,1]):nrow(x.ret),]
    result[,2:ncol(result)] <- result[,2:ncol(result)] - as.matrix(rf.ret[2])
  } else{
    result <- x.ret
    result[,2:ncol(result)] <- result[,2:ncol(result)] - rf.ret[match(x.ret[1,1], rf.ret[,1]),2]
  }
  return (result)
}

monexret <- function(x){ 
  #Input: daily prices 
  #Output: monthly excess returns
  x.ret <- monthly(x)
  #rf.ret <- monthly(rf)
  rf.ret <- rf.mon
  if(nrow(x.ret) > nrow(rf.ret)){
    result <- x.ret[(nrow(x.ret)-nrow(rf.ret)+1):nrow(x.ret),] ####
    result[,2:ncol(result)] <- result[,2:ncol(result)] - rf.ret[,2]
  } else{
    result <- x.ret
    result[,2:ncol(result)] <- result[,2:ncol(result)] - rf.ret[-nrow(x.ret)+nrow(rf.ret)+1,2]
  }
  return(result)
}

monthly <- function(x){
  #Input: data.frame
  #Output: calculates monthly returns
  n <- ncol(x)
  data <- data.frame(x[1,]) 
  i <- 1 #index running over the rows
  mon.index <- 1 #saves the number of months found
  while(i <= nrow(x)){
    begin <- x[i,2:n] #saves the last price in the preceding month
    i <- i + 1
    while(months(x$Date[i])==months(x$Date[i+1]) & i<nrow(x)) {i <- i + 1}
    end <- x[i,2:n] #the last price in the new month
    data[mon.index,2:n] <- end/begin - 1
    data[mon.index,1] <-  x[i,1]
    mon.index <- mon.index + 1
  }
  return(data[1:(nrow(data)-1),])
}

prodret <- function(x,p,m){ 
  #Input: x - panel data, p - p periods long, m - from time m
  #Output: multiplied excess returns + 1 
  result <- matrix(1, 1, ncol(x)-1)
  for(i in m:(m+p-1)) result <- result * (1 + x[i,2:ncol(x)])
  return(result-1)
}

cumulate <- function(x){ #if you invest $1 in x, how much do you get in the end
  cum <- data.frame(Date=x[1,1]-1*60*60*24*30) #minus one month
  cum[,2:ncol(x)] <- 1 
  cum[2:(nrow(x)+1),1] <- x[,1]
  x[is.na(x)] <- 0
  for(i in 1:nrow(x)){
    cum[i+1,2:ncol(cum)] <- cum[i,2:ncol(cum)]*(1+x[i,2:ncol(cum)])^(1/12)
  }
  return(cum)
}

#risk-adjusted returns for TS and CS strategies
adjust.risk <- function(x){
  
}
