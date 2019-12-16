#---------------------------------------------------------CS
#th=5 cmom, th=0 tmom, th=-10000000 buy and hold
cmom <- function(data, scaled=F, netlong=0, th=5){
  #Input: x - data.frame of daily prices
  #Output: data.frame with time series of CS returns in columns
  x <- monexret(data)
  rankper <- c(1,6,12) #ranking periods 1,6,12
  holdper <- c(1,6,12) #ranking periods 1,6,12
  cmom <- data.frame(Date=x[,1]) #saves in each row the cross sectional momentum of the strategy
  #the first 3 columns have rankper 1, etc.
  netlongpos <- data.frame(Date=x[,1]) #save netlong positions
  #long <- data.frame(Date=x[,1])
  #short <- data.frame(Date=x[,1])
  if(scaled==T) {x.vola <- vola(data)}  #vola for the last nrow(x) months of data, 
  c <- 2 #running index for columns, runs if holdper changes
  for(r in rankper){
    for(h in holdper){
      for(mon.index in 2:(nrow(x)-r-h+1)){ #mon.index is the start period of the rankper, only runs to that
        #period such that one whole strategy is still possible
        prod.ret.hp <- prodret(x,h,mon.index+r) #saves the product of each company's returns in the holdper WITHOUT DATE
        if(scaled==T) weights <- 0.4/x.vola[mon.index+r-1,2:ncol(x)]
        else weights <- matrix(1,1,ncol(x)-1)
        prod.ret.rp <- prodret(x,r,mon.index) #saves the product of each company's returns in the rankper WITHOUT DATE
        if(th==5) th <- sum(prod.ret.rp[is.na(prod.ret.rp)==F]*weights[is.na(prod.ret.rp)==F])/sum(weights[is.na(prod.ret.rp)==F])  #threshhold for CSMOM (here: average in rankper)
        #---------------------the formula as in the paper:
       #cmom[mon.index,c] <- 0.5*sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F])/length(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F])*
          #(-sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th])*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0]/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0])
           #+ sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th])*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0]/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0]))
       #---------------------my formula:
        cmom[mon.index,c] <- -sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th])
          + sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th])
        if(netlong==1){
          netlongpos[mon.index,c] <- 2*(sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=0])
                                        -sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<0]))/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F])
          #---------------------my formula:
          cmom[mon.index,c] <- (2*(1+cmom[mon.index,c])^(12/h) - 1 + netlongpos[mon.index,c]*mean(1+prod.ret.hp[is.na(prod.ret.hp)==F])^(12/h) - 1)/(2+abs(netlongpos[mon.index,c]))
          #long[mon.index,c] <- 1 + netlongpos[mon.index,c]*(netlongpos[mon.index,c]>0)
          #short[mon.index,c] <- 1 + netlongpos[mon.index,c]*(netlongpos[mon.index,c]<0)
        } else {
            if(netlong==2){
          cmom[mon.index,c] <- 2/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F])*(-sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th])*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th]
          + sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th])*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th])
          #long[mon.index,c] <- 2/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp>=th])*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F]
          #short[mon.index,c] <- 2/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F & prod.ret.rp<th])*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F]
          }
          #else {
          #long[mon.index,c] <- 1
          #short[mon.index,c] <- 1
          #}
        }
      }
      c <- c + 1 #runs as holdper changes
    }
  }
  cmom[,c(2,3,5,6,8,9)] <- (1+cmom[,c(2,3,5,6,8,9)])^(12/h) - 1 #annualize returns with holding period not 12 months
  #return(list(cmom,netlongpos,long,short,x.vola))
  return(cmom)
}


#---------------------------------------------------------TS
tmom <- function(data, scaled=F, netlong=T){
  #Input: data - data.frame of daily prices, scaled - F no volatility scaling, T volatility scaling
  #       netlong - T netlong as in MOS12, F no netlong
  #Output: data.frame with time series of tmom returns in columns
  x <- monexret(removena(data)) #removes rows with over 20% NAs
  rankper <- c(1,6,12) #ranking periods 1,6,12
  holdper <- c(1,6,12) #holding periods 1,6,12
  tmom <- data.frame(Date=x[,1]) #saves in each row the time series momentum of the strategy
  #the first 3 columns have rankper 1, etc.
  if(scaled==T) x.vola <- vola(data, nrow(x)) #vola for the last nrow(x) months of data
  c <- 2 #running index for columns, runs if holdper changes
  for(r in rankper){
    for(h in holdper){
      for(mon.index in 1:(nrow(x)-r-h+1)){ #mon.index is the start period of the rankper, only runs to that
        #period such that one whole strategy is still possible
        prod.ret.rp <- prodret(x,r,mon.index) #saves the product of each company's returns in the rankper WITHOUT DATE
        if(scaled==F) prod.ret.hp <- prodret(x,h,mon.index+r) #saves the product of each company's returns in the holdper WITHOUT DATE
        else prod.ret.hp <- prodret(x,h,mon.index+r)/x.vola[match(x[1,mon.index+r-1],x.vola[,1])-1,2:ncol(x.vola)] 
        #saves the product of each company's returns in the holdper WITHOUT DATE divided by ex ante volatility
        if(netlong==T) tmom[mon.index,c] <- 2/(ncol(x)-1)*
            (sum(prod.ret.hp[is.na(prod.ret.rp)==F & is.na(prod.ret.hp)==F & prod.ret.rp>=0])
        -sum(prod.ret.hp[is.na(prod.ret.rp)==F & is.na(prod.ret.hp)==F & prod.ret.rp<0]))
        else tmom[mon.index,c] <- 1/2*
            (mean(prod.ret.hp[is.na(prod.ret.rp)==F & is.na(prod.ret.hp)==F & prod.ret.rp>=0])
        -mean(prod.ret.hp[is.na(prod.ret.rp)==F & is.na(prod.ret.hp)==F & prod.ret.rp<0]))
      }
      if(h!=12) tmom[,c] <- (1+tmom[,c])^(12/h) - 1 #annualize returns with holding period not 12 months
      c <- c + 1 #runs as holdper changes
    }
  }  
  return(tmom)
}

#-----------------------------------------------------------------BAH  IS ALREADY NETLONG (buy and hold)
bah <- function(data, scaled=F){
  #Input: x - data.frame of daily prices
  #Output: data.frame with time series of CS returns in columns
  x <- monexret(data)
  rankper <- c(1,6,12) #ranking periods 1,6,12
  holdper <- c(1,6,12) #ranking periods 1,6,12
  bah <- data.frame(Date=x[,1]) #saves in each row the cross sectional momentum of the strategy
  #the first 3 columns have rankper 1, etc.
  if(scaled==T) {x.vola <- vola(data)}  #vola for the last nrow(x) months of data, 
  c <- 2 #running index for columns, runs if holdper changes
  for(h in holdper){
      for(mon.index in 2:(nrow(x)-r-h+1)){ #mon.index is the start period of the rankper, only runs to that
        #period such that one whole strategy is still possible
        prod.ret.hp <- prodret(x,h,mon.index+r) #saves the product of each company's returns in the holdper WITHOUT DATE
        #else prod.ret.hp <-0.4*prodret(x,h,mon.index+r)/
        if(scaled==T) weights <- 0.4/x.vola[mon.index+r-1,2:ncol(x)]
        else weights <- matrix(1,1,ncol(x)-1)
        bah[mon.index,c] <- sum(prod.ret.hp[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F]*weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F])/sum(weights[is.na(prod.ret.hp)==F & is.na(prod.ret.rp)==F])
        if(h!=12) cmom[mon.index,c] <- (1+cmom[mon.index,c])^(12/h) - 1 #annualize returns with holding period not 12 months
      }
      c <- c + 1 #runs as holdper changes
    }
  return(cbind(bah,bah[,2:4],bah[,2:4]))
}
