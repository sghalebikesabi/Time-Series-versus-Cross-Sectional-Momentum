#---------------------------------------------------------returns
reg <- function(A,B){
  #Input: A, B strats
  #Output: betas and tstats for regressing each column of A against B and vv
  reg1 <- matrix(NA,2,ncol(A))
  for(i in 1:ncol(A)){
    reg1[1,i] <- paste0(round(summary(lm(A[,i]~B[,i]))$coefficients[2,1],4))
    reg1[2,i] <- paste0("(",round(summary(lm(A[,i]~B[,i]))$coefficients[2,3],3),")")
  }
  reg2 <- rep(NA,ncol(A)-1)
  for(i in 2:ncol(A)){
    reg2[i-1] <- paste0(summary(lm(B[,i]~A[,i]))$coefficients[1,1],"(",summary(lm(B[,i]~A[,i]))$coefficients[1,3],")")
  }
  #return(list(reg1,reg2))
  xtable(reg1)
}

reg(ps.cmom[,2:10]-ps.cmom.scaled[,2:10], ps.cmom.turnover[,2:10]-ps.cmom.scaled.turnover[,2:10])
reg(ps.cmom.tvm[,2:10]-ps.cmom.scaled.tvm[,2:10], ps.tvm.turnover[,2:10]-ps.tvm.scaled.turnover[,2:10])
reg(ps.tmom[,2:10]-ps.tmom.scaled[,2:10], ps.tmom.turnover[,2:10]-ps.tmom.scaled.turnover[,2:10])
reg(ps.tmom[,2:10]-ps.cmom[,2:10], ps.tmom.turnover[,2:10]-ps.cmom.turnover[,2:10])
reg(ps.tmom.scaled[,2:10]-ps.cmom.scaled[,2:10], ps.tmom.scaled.turnover[,2:10]-ps.cmom.scaled.turnover[,2:10])

tmom.scaled.long <- tmom.scaled[[3]]
tmom.scaled.total <- tmom.scaled[[2]]
ps.tmom.short <- tmom.scaled.total
ps.tmom.short.[,2:ncol(ps.tmom.short)] <- ps.tmom.short.[,2:ncol(ps.tmom.short)] - ps.tmom.long[,2:ncol(ps.tmom.short)]
reg(ps.bah, ps.tmom.scaled.long)
reg(ps.bah, ps.tmom.scaled.total)
reg(ps.bah, ps.tmom.scaled.short)

reg.st <- function(A,B){
  #Input: A cmom, B tmom
  #Output: alphas and tstats for regressing each column of A against B 
  reg.v <- matrix(NA,2,ncol(A)-1)
  for(i in 2:ncol(A)){
    reg.v[1,i-1] <- paste0(round(summary(lm(A[,i]~B[,i]))$coefficients[2,1],4))
    reg.v[2,i-1] <- paste0("(",round(summary(lm(A[,i]~B[,i]))$coefficients[2,3],4),")")
  }
  return(reg.v)
}

risk.adjusted <- function(x, scaled=F){
  #Input: x- Strategy returns
  #Output: risk adjusted alphas (first half with CAPM, second half with FF-3)
  if(scaled==T) factors <- riskfac.scaled
  else factors <- riskfac
  alphas <- matrix("",2*ncol(x)-2,2)
  for(j in 2:ncol(x)){
    lm1 <- summary(lm(x[,j]~factors[[1]][,j]+factors[[2]][,j]+factors[[3]][,j]))
    lm2 <- summary(lm(x[,j]~factors[[1]][,j]))
    alphas[j-1,1] <- paste0(round(lm2$coefficients[1,1],4))
    alphas[ncol(x)-1+j-1,1] <- paste0(round(lm1$coefficients[1,1],4))
    alphas[j-1,2] <- paste0("(",round(lm2$coefficients[1,3],3),")")
    alphas[ncol(x)-1+j-1,2] <- paste0("(",round(lm1$coefficients[1,3],3),")")
  }
  return(alphas)
}

cmom.risk.adjusted <- risk.adjusted(ps.cmom)
tmom.risk.adjusted <- risk.adjusted(ps.tmom)
alphas <- cbind(cmom.risk.adjusted,tmom.risk.adjusted)

riskfactors <- function(scaled=F){
  #Input: x - data.frame of strategy returns
  #Output: data.frame with time series of CS returns in columns
  x <- ps.mon
  mv <- data.frame(Date=x[,1],OneOne=rep(0,nrow(x)),OneSix=rep(0,nrow(x)),OneTwelve=rep(0,nrow(x)),SixOne=rep(0,nrow(x)),SixSix=rep(0,nrow(x)),SixTwelve=rep(0,nrow(x)),TwelveOne=rep(0,nrow(x)),TwelveSix=rep(0,nrow(x)),TwelveTwelve=rep(0,nrow(x)))
  btm <- data.frame(Date=x[,1],OneOne=rep(0,nrow(x)),OneSix=rep(0,nrow(x)),OneTwelve=rep(0,nrow(x)),SixOne=rep(0,nrow(x)),SixSix=rep(0,nrow(x)),SixTwelve=rep(0,nrow(x)),TwelveOne=rep(0,nrow(x)),TwelveSix=rep(0,nrow(x)),TwelveTwelve=rep(0,nrow(x)))
  msci.mon <- monexret(msci)
  msci.mon <- msci.mon[(nrow(msci.mon)-nrow(ps.mon)):nrow(msci.mon),]
  msci.comp <- data.frame(Date=x[,1],OneOne=rep(0,nrow(x)),OneSix=rep(0,nrow(x)),OneTwelve=rep(0,nrow(x)),SixOne=rep(0,nrow(x)),SixSix=rep(0,nrow(x)),SixTwelve=rep(0,nrow(x)),TwelveOne=rep(0,nrow(x)),TwelveSix=rep(0,nrow(x)),TwelveTwelve=rep(0,nrow(x)))
  c <- 2
  for(r in rankper){
    for(h in holdper){
      for(mon.index in 2:(nrow(x)-r-h+1)){
        if(scaled==T) weights <- 0.4/ps.vola[mon.index+r-1,2:ncol(ps.vola)]
        else weights <- matrix(1,1,ncol(x)-1)
        mv.save <- ps.mv[(mon.index+r):(mon.index+r+h-1),]
        mv.save <- as.matrix(mv.save[,2:ncol(mv.save)])
        mv.save <- as.numeric(mv.save[mv.save!="NA"])
        mv[mon.index,c] <- mean(mv.save[is.na(mv.save)==F])
        btm.save <- ps.btm[(mon.index+r):(mon.index+r+h-1),]
        btm.save <- as.matrix(btm.save[,2:ncol(btm.save)])
        btm.save <- as.numeric(btm.save[btm.save!="NA"])
        btm[mon.index,c] <- mean(as.numeric(btm.save[is.na(btm.save)==F]))
        msci.comp[mon.index,c] <- prodret(msci.mon,h,mon.index+r)
      }
      msci.comp[,c] <- (1+msci.comp[,c])^(12/h)-1
      c <- c + 1
    }
  }
  return(list(msci.comp[2:nrow(msci.comp),],mv[2:nrow(mv),],btm[2:nrow(mv),]))
}

riskfac <- riskfactors()
riskfac.scaled <- riskfactors(scaled=T)
