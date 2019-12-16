#th=5 cmom, th=0 tmom, th=-10000000 buy and hold
#netlong=0: CS as in GOY17
#netlong=1: TVM
#netlong=2: TS as in GOY17
#netlong=3: cs invests equal amount in long and short as ts
#netlong=4: tvm such that total investment is 1 and tvm position is equal to netlong pos of ts

#---------------------------------------------------------CS
cmom <- function(data=ps, scaled=F, netlong=0, th=5, calc.turnover=F){
  #Input: x - data.frame of daily prices
  #Output: data.frame with time series of CS returns in columns
  #x <- ps.mon
  x <- new.mon
  rankper <- c(1,6,12) 
  holdper <- c(1,6,12)
  total <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
  turnover <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
  cmom <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
  if(netlong!=0) {
    long <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
    netlongpos <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
    tvm <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
  }
  #if(scaled==T) x.vola <- ps.vola
  if(scaled==T) x.vola <- new.vola
  c <- 2
  for(r in rankper){
    for(h in holdper){
      old <- 0 #old time varying investment into the market
      companies.long.new <- rep(0,ncol(x)-1)
      companies.short.new <- rep(0,ncol(x)-1)
      for(mon.index in 2:(nrow(x)-r-h+1)){
        prod.ret.hp <- prodret(x,h,mon.index+r)
        if(scaled==T) weights <- 0.4/x.vola[mon.index+r-1,2:ncol(x.vola)] else weights <- matrix(1,1,ncol(x)-1)
        prod.ret.rp <- prodret(x,r,mon.index)*weights
        if(th==5) th <- sum(prod.ret.rp[is.na(prod.ret.rp)==F & is.na(weights)==F]*weights[is.na(prod.ret.rp)==F & is.na(weights)==F])/sum(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F]))
        if(calc.turnover==F){
        if(netlong==0 | netlong==1 | netlong==4){
          if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])==0){ 
          cmom[mon.index,c] <- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]
                                   *weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          } else{
          if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])==0){ 
            cmom[mon.index,c] <- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]
                                     *weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          } else cmom[mon.index,c] <- 1/2*(sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]
                                               *weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]) 
                                           - sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]
                                                 *weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]))
          #goy 17: else cmom[mon.index,c] <- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])
          }
          if(netlong==0) cmom[mon.index,c] <- cmom[mon.index,c]*mean(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
        }
        if(netlong==1 | netlong==4){
          #long[mon.index,c] <-  2*sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          netlongpos[mon.index,c] <- (sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])
                                      -sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0 & is.na(weights)==F]))/length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          tvm[mon.index,c] <- th
          if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0 & is.na(weights)==F])==0) netlongpos[mon.index,c] <- 1
          if(netlong==4) cmom[mon.index,c] <- cmom[mon.index,c]*(1-netlongpos[mon.index,c]) + netlongpos[mon.index,c]*tvm[mon.index,c]
          else cmom[mon.index,c] <- cmom[mon.index,c] + netlongpos[mon.index,c]*tvm[mon.index,c]
          #goy17: netlongpos[mon.index,c] <- (sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])-sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0 & is.na(weights)==F]))/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          #we: cmom[mon.index,c] <- (cmom[mon.index,c] + netlongpos[mon.index,c]*(sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(weights)==F])))/(1+abs(netlongpos[mon.index,c])) #we
        }
        if(netlong==2){
          #goy17: cmom[mon.index,c] <- 2/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])*(-sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]) + sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]))
          #cmom[mon.index,c] <- 1/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])*(-sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]) + sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])) #we
          total[mon.index,c] <-  mean(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          long[mon.index,c] <-  sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])>0) {
            #netlongpos[mon.index,c] <- sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])
            cmom[mon.index,c] <- cmom[mon.index,c] + 
              sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]
                  *weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])
          } else long[mon.index,c] <-  0
          if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])>0){ 
            cmom[mon.index,c] <- cmom[mon.index,c] - 
              sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]
                  *weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]) #we
            #netlongpos[mon.index,c] <- netlongpos[mon.index,c] - sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])
          }
          #netlongpos[mon.index,c] <- netlongpos[mon.index,c]/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          cmom[mon.index,c] <- cmom[mon.index,c]/length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          #goy17: netlongpos[mon.index,c] <- 2*(sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])-sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F]))
          #netlongpos[mon.index,c] <- 2*(sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])-sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F]))
        }
        if(netlong==3){
          long.temp <- sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])
          if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])==0) long.temp <- 0
          short.temp <- sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0 & is.na(weights)==F])
          if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0 & is.na(weights)==F])==0) short.temp <- 0
          cmom[mon.index,c] <- long.temp*sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/max(sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]),0.00000001) - short.temp*sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/max(sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]),0.00000001)
        }
      }
      companies.long.old <- companies.long.new
      companies.short.old <- companies.short.new
      #netlong=0: CS as in GOY17
      #netlong=1: TVM
      #netlong=2: TS as in GOY17
      #netlong=3: cs invests equal amount in long and short as ts
      #netlong=4: tvm such that total investment is 1 and tvm position is equal to netlong pos of ts
      weights[is.na(weights)==T] <- 0
      companies.long.new <- 1/2*(is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F)*weights/length(weights[(is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F)])
      companies.short.new <- 1/2*(is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F)*weights/length(weights[(is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F)])
      if(netlong==2){
        companies.long.new <- (is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F)*weights/length(weights[(is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F)])
        companies.short.new <- (is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F)*weights/length(weights[(is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F)])
      }
      if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])==0) companies.long.new <- rep(0,ncol(ps.mon)-1)
      if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])==0) companies.short.new <- rep(0,ncol(ps.mon)-1)
      turnover[mon.index,c] <- sum(abs(companies.long.new-companies.long.old)) + sum(abs(companies.short.new-companies.short.old))
      if(netlong==1){
        netlongpos[mon.index,c] <- (sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])-sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0 & is.na(weights)==F]))/(2*length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F]))
        if(is.nan(netlongpos[mon.index,c])==T) netlongpos[mon.index,c] <- 1 * (length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])==T) - 1 * (length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0 & is.na(weights)==F])==T)
        turnover[mon.index,c] <- turnover[mon.index,c] + abs(netlongpos[mon.index,c] - old)
        old <- netlongpos[mon.index,c]
      }}
      c <- c + 1
  }}
  cmom[,c(2,3,5,6,8,9)] <- (1+cmom[,c(2,3,5,6,8,9)])^(12/h) - 1 #annualize returns with holding period not 12 months
  if(calc.turnover==T) return(turnover[2:nrow(turnover),]) else{
    if(netlong==2) return(list(cmom[2:nrow(cmom),],total[2:nrow(cmom),],long[2:nrow(cmom),])) else{
      if(netlong==4) return(list(cmom[2:nrow(cmom),],tvm[2:nrow(cmom),])) else return(cmom[2:nrow(cmom),])
    }
  }
}

#(1+cmom[,c])^12-1
th <- function(data=ps, scaled=F){
  #Input: data = dataset with daily prices, scaled = volatility scaling or not
  #Output: result = threshold values of CMOM strategy over time for each month and ranking period in rankper
  x <- ps.mon
  rankper=c(1,6,12)
  result <- data.frame(Date=x[,1],r1=rep(NA,nrow(x)),r2=rep(NA,nrow(x)),r3=rep(NA,nrow(x)))
  c <- 2
  for(r in 1:(length(rankper))){
    for(mon.index in 2:(nrow(x)-r)){
      if(scaled==T) weights <- 0.4/ps.vola[mon.index+rankper[r]-1,2:ncol(ps.vola)]
      else weights <- matrix(1,1,ncol(x)-1)
      prod.ret.rp <- prodret(x,rankper[r],mon.index)
      result[mon.index,c] <- sum(prod.ret.rp[is.na(prod.ret.rp)==F & is.na(weights)==F]*weights[is.na(prod.ret.rp)==F & is.na(weights)==F])/sum(weights[is.na(prod.ret.rp)==F & is.na(weights)==F])
    }
    c <- c+1
  }
  return(result)
}

#determine a threshold
optimal.th <- function(ps){
  #Input: data = dataset with daily prices
  #Output: the optimal threshold in each month such that the following returns are maximized ex-post in [[1]], the resulting returns in [[2]]
  holdper <- c(1,6,12)
  rankper <- c(1,6,12)
  x <- ps.mon
  result <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
  result.val <- result
  c <- 2
  for(r in rankper){
    for(h in holdper){
      for(mon.index in 2:(nrow(x)-r-h+1)){
        prod.ret.hp <- prodret(x,h,mon.index+r)
        prod.ret.rp <- prodret(x,r,mon.index)
        opt <- optimize(f, ret.hp=prod.ret.hp, ret.rp=prod.ret.rp, interval=c(-3,3), maximum=T)
        result[mon.index,c] <- opt$maximum
        result.val[mon.index,c] <- (1+opt$objective)^(12/h)-1
      }
      c <- c+1
    }
  }
  return(list(result,result.val))
  #return(result)
}

f <- function(th,ret.hp,ret.rp){ #function to be optimized in optimal.th
  result <- mean(ret.hp[is.na(ret.rp)==F & is.na(ret.hp)==F & ret.rp>=th])-mean(ret.hp[is.na(ret.rp)==F & is.na(ret.hp)==F & ret.rp<th])
  if(th<min(ret.rp[is.na(ret.rp)==F & is.na(ret.hp)==F])) result <- mean(ret.hp[is.na(ret.rp)==F & is.na(ret.hp)==F & ret.rp>=th])
  if(th>max(ret.rp[is.na(ret.rp)==F & is.na(ret.hp)==F])) result <- mean(ret.hp[is.na(ret.rp)==F & is.na(ret.hp)==F & ret.rp<th])
  return(result)
}

#---------------------------------------------------------TS
tmom <- function(data=ps, scaled=F, netlong=2, calc.turnover=F){
  #Input: data - data.frame of daily prices, scaled - F no volatility scaling, T volatility scaling
  #       netlong - T netlong as in MOS12, F no netlong
  #Output: data.frame with time series of tmom returns in columns
  return(cmom(data, scaled, netlong, th=0, calc.turnover))
}

#-----------------------------------------------------------------BAH  IS ALREADY NETLONG (buy and hold)
bah <- function(scaled=F, calc.turnover=F, netlong=2){
  #Input: x - data.frame of daily prices
  #Output: data.frame with time series of bah returns in columns
  return(cmom(ps, scaled, netlong, th=-1000000, calc.turnover))
}

