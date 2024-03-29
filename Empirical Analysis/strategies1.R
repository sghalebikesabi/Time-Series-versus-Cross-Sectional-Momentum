cmom <- function(netlong=0, th=5){
  #Input: x - data.frame of daily prices
  #Output: data.frame with time series of CS returns in columns
  old <- 0 #old time varying investment into the market
  #x <- ps.mon
  x <- ps.mon
  rankper <- c(1,6,12) 
  holdper <- c(1,6,12)
  cmom <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
  if(netlong!=0) {
    long <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
    netlongpos <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
    tvm <- data.frame(Date=x[,1],OneOne=rep(NA,nrow(x)),OneSix=rep(NA,nrow(x)),OneTwelve=rep(NA,nrow(x)),SixOne=rep(NA,nrow(x)),SixSix=rep(NA,nrow(x)),SixTwelve=rep(NA,nrow(x)),TwelveOne=rep(NA,nrow(x)),TwelveSix=rep(NA,nrow(x)),TwelveTwelve=rep(NA,nrow(x)))
  }
  c <- 2
  for(r in rankper){
    for(h in holdper){
      for(mon.index in 2:(nrow(x)-r-h+1)){
        prod.ret.hp <- prodret(x,h,mon.index+r)
        weights <- matrix(1,1,ncol(x)-1)
        prod.ret.rp <- prodret(x,r,mon.index)*weights
        if(th==5) th <- sum(prod.ret.rp[is.na(prod.ret.rp)==F & is.na(weights)==F]*weights[is.na(prod.ret.rp)==F & is.na(weights)==F])/sum(weights[is.na(prod.ret.rp)==F & is.na(weights)==F])
            if(netlong==0 | netlong==1 | netlong==4){
            if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])==0){ 
              cmom[mon.index,c] <- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
            } else{
              if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])==0){ 
                cmom[mon.index,c] <- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
              } else cmom[mon.index,c] <- 1/2*(sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]) - sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]))
              #goy 17: else cmom[mon.index,c] <- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])
            }
          }
          if(netlong==2){
            #goy17: cmom[mon.index,c] <- 2/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])*(-sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]) + sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]))
            #cmom[mon.index,c] <- 1/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])*(-sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]) + sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])) #we
            netlongpos[mon.index,c] <- 0
            cmom[mon.index,c] <- 0
            long[mon.index,c] <-  sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
            if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])>0) {
              #netlongpos[mon.index,c] <- sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])
              cmom[mon.index,c] <- cmom[mon.index,c] + sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])
            } else long[mon.index,c] <-  0
            if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])>0){ 
              cmom[mon.index,c] <- cmom[mon.index,c] - sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]) #we
              #netlongpos[mon.index,c] <- netlongpos[mon.index,c] - sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])
            }
            cmom[mon.index,c] <- cmom[mon.index,c]/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
          }
          if(netlong==3){
            long.temp <- sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & is.na(weights)==F])
            if(length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0 & is.na(weights)==F])==0) long.temp <- 0
            cmom[mon.index,c] <- long.temp*sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F])/max(sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th & is.na(weights)==F]),0.00000001) - (1-long.temp)*sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F])/max(sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th & is.na(weights)==F]),0.00000001)
          }
        }
        weights <- 0.4/ps.vola[mon.index+r-1,2:ncol(ps.vola)]
        cmom[mon.index,c] <- (1+cmom[mon.index,c]*0.4*mean(weights[is.na(weights)==F]))^(12/h)-1
        c <- c + 1
        }
    }
    return(cmom[2:nrow(cmom),])
}
