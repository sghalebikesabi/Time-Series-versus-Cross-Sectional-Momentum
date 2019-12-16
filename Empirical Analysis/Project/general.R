#---------------------------------------------------------general
read <- function(file){
  #Input: filename as String
  #Output: return data.frame
  data <- read.csv(paste0(file), header=T, dec=",", sep=";") #read Data
  #change the type of date as.POSIXct
  colnames(data)[1] <- "Date"
  data$Date <- as.POSIXct(data$Date,format="%m/%d/%y") 
  return(data)
}

read2 <- function(file, d="," ,s=";"){
  #Input: filename as String
  #Output: return data.frame
  data <- read.csv(paste0(file), header=T, dec=d, sep=s, na=0) #read Data
  #change the type of date as.POSIXct
  colnames(data)[1] <- "Date"
  data$Date <- as.POSIXct(data$Date,format="%d.%m.%Y") 
  return(data)
}

plotx <- function(x){
  #Input: time series x
  #Output: plot of time series x with type l
  plot(x, type = "l")
}

plotstrat <- function(x, strat ="TS MOM"){
  #Input: x - data of 3*3 momentum strat, strat - strategy as string with first letter as capital letter
  #Output: 3*3 plots
  old_par <- par()
  par(mfrow=c(3,3))
  par(oma = c(4, 4, 0, 0)) 
  par(mar = c(2, 2, 1, 1))
  for(i in 2:10) {
    plot(x[,c(1,i)],type="l",ylab="Excess returns", xlab="Time")
    if(i-1==1) mtext("1", side=2, line = 2, outer = F, adj=.5)  
    if(i-1==4) mtext("6", side=2, line = 2, outer = F, adj=.5)
    if(i-1==7) {mtext("12", side=2, line = 2, outer = F, adj=.5); mtext("1", side=1, line = 2, outer = F, adj=.5)}
    if(i-1==8) mtext("6", side=1, line = 2, outer = F, adj=.5)
    if(i-1==9) mtext("12", side=1, line = 2, outer = F, adj=.5)
  }
  mtext('Holding Period', side = 1, outer = TRUE, line = 2)
  mtext('Ranking Period', side = 2, outer = TRUE, line = 2)
  par(old_par)
  return()
}

print.out <- function(x){
  y <- matrix("",2,ncol(x)-1)
  y[1,] <- paste0(round(x[1,2:ncol(x)],4))
  y[2,] <- paste0("(",round(x[2,2:ncol(x)],3),")")
  xtable(y)
}

ana <- function(data){
  y <- data[,2:ncol(data)]
  averages <- numeric(9)
  tstatistics <- numeric(9)
  for(i in 1:9){
    save <- y[,i]
    averages[i] <- mean(save[is.na(save)==F])
    tstatistics[i] <- t.test(save[is.na(save)==F])$statistic
  } 
  return(data.frame(ave=averages, tstat=tstatistics))
}

twomean <- function(strat,strat2){
  Twotest <- numeric(ncol(strat)-1)
  for(i in 1:(ncol(strat)-1)){
    Twotest[i] <- paste0(round(t.test(strat[,i+1],strat2[,i+1])$statistic,3),"&")
    if(i==9) Twotest[i] <- paste0(round(t.test(strat[,i+1],strat2[,i+1])$statistic,3),"\\")
  }
  return(Twotest)
}

twomeang <- function(strat,strat2){
  Twotest <- numeric(ncol(strat)-1)
  for(i in 1:(ncol(strat)-1)){
    Twotest[i] <- t.test(strat[,i+1],strat2[,i+1],alternative="greater")$statistic
  }
  return(Twotest)
}


twomean(ps.cmom,ps.tmom)
twomean(ps.cmom,ps.bah)
twomean(ps.tmom,ps.bah)
twomean(ps.cmom.netlong2, ps.tmom.netlong)
twomean(ps.cmom.netlong2,ps.cmom)
twomean(ps.tmom.netlong,ps.tmom)
twomean(ps.cmom, ps.tmom.netlong)
twomean(ps.cmom.netlong2,ps.bah)
twomean(ps.tmom.netlong,ps.bah)
twomean(ps.tmom.long,ps.cmom.long)
twomean(ps.tmom.long,matrix(0.5,nrow(ps.tmom.long),ncol(ps.tmom.long)))
twomean(ps.cmom.long,matrix(0.5,nrow(ps.tmom.long),ncol(ps.tmom.long)))
twomean(ps.cmom.netlong2,ps.cmom.netlong3)
twomean(ps.cmom.netlong3,ps.tmom.netlong)
twomean(ps.cmom.netlong3,ps.bah)
twomean(ps.cmom.tvm,ps.bah)
twomean(ps.cmom.netlong4,ps.bah)
twomean(ps.cmom.tvm,ps.cmom.netlong3)
twomean(ps.cmom.tvm,ps.tmom.netlong)
twomean(tvm,ps.bah)
twomean(ps.cmom.turnover, ps.cmom.scaled.turnover)
twomean(ps.cmom.turnover, ps.tmom.turnover)
twomean(ps.tmom.turnover, ps.tmom.scaled.turnover)
twomean(ps.cmom.turnover, ps.bah.scaled.turnover)
twomean(ps.tmom.turnover, ps.bah.scaled.turnover)
twomean(ps.tmom.turnover,matrix(0,nrow(ps.tmom.long),ncol(ps.tmom.long)))
twomean(tmom.scaled[[2]],matrix(1,nrow(ps.tmom.long),ncol(ps.tmom.long)))
twomean(ps.tmom.long, ps.tmom.scaled.long)
twomean(ps.cmom, ps.cmom.scaled)
twomean(ps.tmom, ps.tmom.scaled)
twomean(ps.cmom.netlong3, ps.cmom.scaled.netlong3)
twomean(ps.cmom.tvm, ps.cmom.scaled.tvm)
twomean(ps.tmom.netlong, ps.tmom.scaled.netlong)
twomean(ps.bah, ps.bah.scaled)

clean.data <- function(x){
  #Input: list of data, important is the first element in the list
  #Output: cleaned data.set
  vec <- x[[1]][,c(1,i)]
  for(k in 1:nrow(vec)){
    for(j in 2:ncol(vec)){
      if(is.na(vec[k,j])==F & abs(vec[k,j]>3)) vec[k,j] <- NA
    }
  }
  return(vec)
}

avetstat <- function(A,B){
  #Input: two matrizes
  #Output: a matrix with class type character with the entires of B in brackets right after the entries of A
  C <- matrix(NA, ncol=ncol(A), nrow=nrow(A))
  for(i in 1:nrow(A)){
    for(j in 1:ncol(A)){
      C[i,j] <- paste0(A[i,j],"(",B[i,j],")")
    }
  }
  return(C)
}

avetstat2 <- function(A,B){
  #Input: two matrizes
  #Output: a matrix with class type character with the entires of B in brackets in the column next to the entries of A
  C <- matrix(NA, ncol=2*ncol(A), nrow=nrow(A))
  for(i in 1:nrow(A)){
    for(j in seq(1,2*ncol(A),2)){
      C[i,j] <- A[i,(j+1)/2]
      C[i,j + 1] <- paste0("(",B[i,(j+1)/2],")")
    }
  }
  return(C)
}

removena <- function(data){
  #Input: data.frame
  #Output: deletes all the rows with more than 20% NAs
  n <- ncol(data)
  data$na_count <- apply(data, 1, function(x) sum(is.na(x))) #column na_count counts NAs per row
  removed <- data[data$na_count/(n-1) < 0.2,] #removed does not contain any rows with more than 20% NAs
  removed <- removed[,1:(ncol(removed)-1)]
  return(removed)
}
