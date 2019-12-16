#---------------------------------------------------------analysis of riskfree rates
#par(mfrow=c(3,7))
#for (i in 2:ncol(rf)) plot(monthly(rf[,c(1,i)]),type="l",ylim=c(-2,2))
#par(mfrow=c(1,1))
#we take first column, has no outliers

#---------------------------------------------------------outlier analysis
outliers <- function(y){
 ol <- data.frame(row=0, col=0, val=0) #outlier; first column row, second column column, third column value
  nol <- 0 #number of outliers
  for(i in 1:nrow(y)){
    for(j in 2:ncol(y)){
      if(is.na(y[i,j])==F & (y[i,j] > 10 || y[i,j] < -10)){
        nol <- nol + 1
        ol[nol,1] <- i
        ol[nol,2] <- j
        ol[nol,3] <- y[i,j]
      }
    }
  }
}

#---------------------------------------------------------strategies analysis
ana <- function(data){
  y <- data[,2:ncol(data)]
  averages <- numeric(9)
  for(i in 1:9) averages[i] <- mean(na.omit(y[,i]))
  tstatistics <- numeric(9)
  for(i in 1:9) tstatistics[i] <- t.test(y[is.na(y)==F,i])$statistic
  return(data.frame(ave=averages, tstat=tstatistics))
}

#---------------------------------------------------------sum stats for strats
stats <- function(x.strats, scaled=F){
  risk <- risk.adjusted(x.strats,scaled)
  x.stats <- matrix("",4,ncol(x.strats)-1)
  #rownames(x.stats) <- c("Mean","","$\alpha_{CAPM}$","","$\alpha_{FF3}$","","Median","SD","SharpeRatio","Skewness","Kurtosis","Drawdown")
  rownames(x.stats) <- c("Mean","","$\alpha_{CAPM}$","")
  for(i in 1:(ncol(x.strats)-1)){
    data <- x.strats[,i+1]
    data <- data[is.na(data)==F]
    x.stats[1,i] <- paste0(round(mean(data),4))
    x.stats[2,i] <- paste0("(",round(t.test(data)$statistic,3),")")
    x.stats[3,i] <- risk[i,1]
    x.stats[4,i] <- risk[i,2]
    #x.stats[5,i] <- risk[i+9,1]
    #x.stats[6,i] <- risk[i+9,2]
    #x.stats[7,i] <- paste0(round(median(data),4))
    #x.stats[8,i] <- paste0(round(sd(data),4))
    #x.stats[9,i] <- paste0(round(mean(data)/sd(data),4))
    #x.stats[10,i] <- paste0(round(skewness(data),4))
    #x.stats[11,i] <- paste0(round(kurtosis(data),3))
    #x.stats[12,i] <- paste0(round(maxDrawdown(data),4))
  }
  xtable(x.stats)
}

stats2 <- function(x){
  x.stats <- matrix(NA,7,ncol(x)-1)
  rownames(x.stats) <- c("Mean","Median","SD","SharpeRatio","Skewness","Kurtosis","Drawdown")
  for(i in 2:ncol(x)){
    data <- x[,i]
    data <- data[is.na(data)==F]
    x.stats[1,i-1] <-  mean(data)
    x.stats[2,i-1] <-  median(data)
    x.stats[3,i-1] <-  sd(data)
    x.stats[4,i-1] <-  mean(data)/sd(data)
    x.stats[5,i-1] <-  skewness(data)
    x.stats[6,i-1] <-  kurtosis(data)
    x.stats[7,i-1] <-  maxDrawdown(data)
  }
  return(x.stats)
}

stats3 <- function(x.strats){
  x.stats <- matrix("",2*length(x.strats),ncol(x.strats[[1]])-1)
  for(j in 1:length(x.strats)){
    risk <- risk.adjusted(x.strats[[j]])
    for(i in 1:(ncol(x.strats[[1]])-1)){
      data <- x.strats[[j]][,i+1]
      data <- data[is.na(data)==F]
      x.stats[2*j-1,i] <- risk[i,1]
      x.stats[2*j,i] <- risk[i,2]
    }
  }
  xtable(x.stats)
}

s <- round(cbind(anacs,anats),4)
s[,2] <- paste0("(",as.character(s[,2]),")")
s[,4] <- paste0("(",as.character(s[,4]),")")
