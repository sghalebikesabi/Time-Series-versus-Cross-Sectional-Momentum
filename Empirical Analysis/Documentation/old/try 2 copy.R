#Arbeitsverzeichnis
setwd("/Users/sahra/Google Drive/Notability/FMT Seminar/Empirie/Data")

#packages
require(xtable)

#Daten einlesen
cdax <- read.table("cdax.csv", header=T, dec=",", sep=";")
colnames(cdax)[1] <- "Date"
#Variablen
cdax$Date <- as.Date(cdax$Date,format="%m/%d/%y") #Date in Datumsformat bringen
nconst <- ncol(cdax)-1
#Data.frame cdax.ret hat t??gliche Renditen f??r gesamten cdax eingespeichert
cdax.dret <- cdax
cdax.dret[nrow(cdax):2,(nconst+1):2] <- cdax[nrow(cdax):2,(nconst+1):2]/cdax[(nrow(cdax)-1):1,(nconst+1):2]-1
cdax.dret[1,2:(nconst+1)] <- 0

#Data.frame cdax.mret speichert monthly average returns
cdax.mret <- data.frame(cdax[1,]) #Data.frame erstellen
i <- 2 #Laufindex, der ??ber jede Zeile aus cdax.dret geht
acc.ret <- matrix(cdax.dret[1,2:nconst+1]) #speichert die Returns aus betrachteten Monat
ndays <- 1 #speichert, wie viele Tage vom betrachteten Monat bereits gefunden wurden
mon.index <- 1 #speichert Anzahl der bisher betrachteten Monate
while(i <= nrow(cdax)){
  if(months(cdax$Date[i-1])==months(cdax$Date[i])) {
    acc.ret <- acc.ret + cdax.dret[i,2:nconst+1]
    ndays <- ndays + 1
  } else{
  cdax.mret[mon.index, 1] <- cdax$Date[i-1]
  cdax.mret[mon.index, 2:nconst+1] <- acc.ret/ndays
  acc.ret <- matrix(cdax.dret[i,2:nconst+1])
  ndays <- 1
  mon.index <- mon.index + 1
  }
  i <- i + 1
}
cdax.mret$na_count <- apply(cdax.mret, 1, function(x) sum(is.na(x))) #Spalte na_count zaehlt NAs pro Reihe
cdax.mret <- cdax.mret[cdax.mret$na_count/nconst < 0.2,]#cdax.mret enth??lt keine Reihe mit mehr als 20% NAs

#define variables
rankper <- c(1,6,12) #rankingperiod
holdper <- c(1,6,12) #holdingperiod
cmom <- array(0,c(nrow(cdax.mret), 9))
tmom <- array(0,c(nrow(cdax.mret), 9))
c <- 1 #Laufindex f??r Spalte
for(r in rankper){
  for(h in holdper){
    for(mon.index in 1:(nrow(cdax.mret)-r-h+1)){
      #returns
      prod.ret.rp <- prodret(r,mon.index) #speichert f??r jedes Unternehmen das Produkt der Renditen in der Ranking Period
      prod.ret.hp <- prodret(h,mon.index+r) #speichert f??r jedes Unternehmen das Produkt der Renditen in der Holding Period
      #cmom
      cwinners <- 0 #h??chste 10%
      ncwinners <- 0 #Anzhal Winners
      closers <- 0 #niedrigste 10%
      nclosers <- 0 #Anzahl Losers
      ranking <- prod.ret.rp #Ranking zw 0 und 1 der nicht NA Werte
      nna <- 0 #Zahl der bisher begegneten NAs 
      for(j in 1:ncol(prod.ret.rp)){
        if(is.na(prod.ret.rp[1,j])==F) ranking[1,j] <- rank(prod.ret.rp[1,is.na(prod.ret.rp)==F])[j-nna]/length(prod.ret.rp[1,is.na(prod.ret.rp)==F])
        else nna <- nna + 1
      }
      for(i in 1:nconst){
        if(is.na(prod.ret.hp[1,i])==F & is.na(ranking[1,i])==F & ranking[1,i]>=0.9) { #oberste 10% returns addieren in holdper
          cwinners <- cwinners+prod(1+cdax.mret[(mon.index+r):(mon.index+r+h-1),i+1])-1
          ncwinners <- ncwinners + 1
        }
        if(is.na(prod.ret.hp[1,i])==F & is.na(prod.ret.hp[1,i])==F & ranking[1,i]<=0.1) { #unterste 10% returns addieren in holdper
          closers <- closers+prod(1+cdax.mret[(mon.index+r):(mon.index+r+h-1),i+1])-1
          nclosers <- nclosers + 1
        }
      }
      cmom[mon.index,c] <- cwinners/ncwinners - closers/nclosers #Momentum return ausrechnen
      #tmom
      tmom[mon.index,c] <- sum(na.omit(prod.ret.hp[prod.ret.rp>0]))/length(na.omit(prod.ret.hp[prod.ret.rp>0])) - sum(na.omit(prod.ret.hp[prod.ret.rp<0]))/length(na.omit(prod.ret.hp[prod.ret.rp>0]))
    }
    c <- c + 1 #eine Spalte nach rechts wandern zur n??chsten Kombi rper und hper
  }
}

prodret <- function(p,m){ #calculates accumulated monthly returns for period p starting from month m
  result <- matrix(1, 1, nconst)
  for(i in m:(m+p-1)) result <- result * (1 + cdax.mret[i,2:(nconst+1)])
  return(result)
}

par(mfrow=c(3,3))
excess <- c(1,6,12,6,11,17,12,17,23)
for(i in 1:9) {
  plot(cmom[1:(nrow(cmom)-excess[i]),i],type="l",ylab="Excess returns", xlab="Time")
  if(i==1) mtext("1 month", outer=T)
  if(i==2) mtext("6 months", outer=T)
  if(i==3) mtext("12 months", outer=T)
  if(i==1) mtext("1 month", side=2, outer=T)
  if(i==4) mtext("6 months", side=2, outer=T)
  if(i==7) mtext("12 months", side=2, outer=T)
}
mtext("Cross-sectional Momentum returns in time", line=3, outer=T)
par(mfrow=c(3,3))
for(i in 2:10) {
  plot(cmom_cdax[1:125,i],type="l",ylab="Excess returns", xlab="Time")
  if(i==1) mtext("1 month", outer=T)
  if(i==2) mtext("6 months", outer=T)
  if(i==3) mtext("12 months", outer=T)
  if(i==1) mtext("1 month", side=2, outer=T)
  if(i==4) mtext("6 months", side=2, outer=T)
  if(i==7) mtext("12 months", side=2, outer=T)
}
title(main="Time series Momentum returns in time", line=3, outer=T)


mom <- function(x, scaled=F){
  cmom <- array(0,c(nrow(x), 9))
  tmom <- array(0,c(nrow(x), 9))
  c <- 1 #Laufindex for columns
  x <- monexret(x)
  for(r in rankper){
    for(h in holdper){
      for(mon.index in 1:(nrow(x)-r-h+1)){
        #returns
        prod.ret.rp <- prodret(x,r,mon.index) #speichert f??r jedes Unternehmen das Produkt der Renditen in der Ranking Period
        prod.ret.hp <- prodret(x,h,mon.index+r) #speichert f??r jedes Unternehmen das Produkt der Renditen in der Holding Period
        #cmom
        cwinners <- 0 #highest 10%
        ncwinners <- 0 #number winners
        closers <- 0 #lowest 10%
        nclosers <- 0 #number losers
        ranking <- prod.ret.rp #Ranking zw 0 und 1 der nicht NA Werte
        nna <- 0 #Zahl der bisher begegneten NAs 
        for(j in 1:ncol(prod.ret.rp)){
          if(is.na(prod.ret.rp[1,j])==F) ranking[1,j] <- rank(prod.ret.rp[1,is.na(prod.ret.rp)==F])[j-nna]/length(prod.ret.rp[1,is.na(prod.ret.rp)==F])
          else nna <- nna + 1
        }
        for(i in 1:ncol(x)){
          if(is.na(prod.ret.rp[1,i])==F & is.na(ranking[1,i])==F & ranking[1,i]>=0.9) { #oberste 10% returns addieren in holdper
            cwinners <- cwinners+prod(1+x[(mon.index+r):(mon.index+r+h-1),i+1])-1
            ncwinners <- ncwinners + 1
          }
          if(is.na(prod.ret.rp[1,i])==F & is.na(prod.ret.hp[1,i])==F & ranking[1,i]<=0.1) { #unterste 10% returns addieren in holdper
            closers <- closers+prod(1+cdax.mret[(mon.index+r):(mon.index+r+h-1),i+1])-1
            nclosers <- nclosers + 1
          }
        }
        cmom[mon.index,c] <- cwinners/ncwinners - closers/nclosers #Momentum return ausrechnen
        #tmom
        tmom[mon.index,c] <- sum(na.omit(prod.ret.hp[prod.ret.rp>0]))/length(na.omit(prod.ret.hp[prod.ret.rp>0])) - sum(na.omit(prod.ret.hp[prod.ret.rp<0]))/length(na.omit(prod.ret.hp[prod.ret.rp>0]))
      }
      c <- c + 1 #eine Spalte nach rechts wandern zur n??chsten Kombi rper und hper
    }
  }
}

tmom_scaled <- function(data){
  #Input: data - data.frame of daily prices
  #Output: data.frame with time series of tmom returns in columns
  x <- monexret(removena(data)) #removes rows with over 20% NAs
  rankper <- c(1,6,12) #ranking periods 1,6,12
  holdper <- c(1,6,12) #holding periods 1,6,12
  tmom <- data.frame(Date=x[,1]) #saves in each row the time series momentum of the strategy
  #the first 3 columns have rankper 1, etc.
  x.vola <- vola(x)
  c <- 2 #running index for columns, runs if holdper changes
  for(r in rankper){
    for(h in holdper){
      for(mon.index in 1:(nrow(x)-r-h+1)){ #mon.index is the start period of the rankper, only runs to that
        #period such that one whole strategy is still possible
        prod.ret.rp <- prodret(x,r,mon.index) #saves the product of each company's returns in the rankper WITHOUT DATE
        prod.ret.hp <- prodret(x,h,mon.index+r)/x.vola[match(x[1,mon.index+r-1],x.vola[,1])-1,2:ncol(x.vola)] #saves the product of each company's returns in the holdper WITHOUT DATE divided by ex ante volatility
        tmom[mon.index,c] <- 2/(ncol(x)-1)*(sum(na.omit(prod.ret.hp[prod.ret.rp>=0]))-sum(na.omit(prod.ret.hp[prod.ret.rp<0])))
      }
      c <- c + 1 #runs as holdper changes
    }
  }  
  return(tmom)
}
