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
 c <- 1 #Laufindex f??r Spalte
 for(r in rankper){
   for(h in holdper){
       for(mon.index in 1:(nrow(cdax.mret)-r-h)){
           winners <- 0 #h??chste 10%
           nwinners <- 0 #Anzhal Winners
           losers <- 0 #niedrigste 10%
           nlosers <- 0 #Anzahl Losers
           prod.ret.rp <- matrix(1, 1, nconst) #speichert f??r jedes Unternehmen das Produkt der Renditen in der Ranking Period
           for(i in 1:r) prod.ret.rp <- prod.ret.rp * (1 + cdax.mret[mon.index+i-1,2:(nconst+1)])
           ranking <- prod.ret.rp #Ranking zw 0 und 1 der nicht NA Werte
           nna <- 0 #Zahl der bisher begegneten NAs 
           for(j in 1:ncol(prod.ret.rp)){
               if(is.na(prod.ret.rp[1,j])==F) ranking[1,j] <- rank(prod.ret.rp[1,is.na(prod.ret.rp)==F])[j-nna]/length(prod.ret.rp[1,is.na(prod.ret.rp)==F])
               else nna <- nna + 1
             }
           paste0("Periode",mon.index,"ranking =", ranking[1,])
           for(i in 1:nconst){
               if(is.na(ranking[1,i])==F & ranking[1,i]>=0.9) { #oberste 10% returns addieren in holdper
                   winners <- winners+prod(1+cdax.mret[mon.index:(mon.index+r-1),i+1])-1
                   nwinners <- nwinners + 1
                 }
               paste0("Periode",mon.index,"winners =", winners, "nwinners=",nwinners)
               if(is.na(ranking[1,i])==F & ranking[1,i]<=0.1) { #unterste 10% returns addieren in holdper
                   losers <- losers+prod(1+cdax.mret[mon.index:(mon.index+r-1),i+1])-1
                   nlosers <- nlosers + 1
                 }
               paste0("Periode", mon.index,"losers =", losers, "nlosers =",nlosers)
             }
           cmom[mon.index,c] <- winners/nwinners - losers/nlosers #Momentum return ausrechnen
         }
       c <- c + 1 #eine Spalte nach rechts wandern zur n??chsten Kombi rper und hper
     }
  }

par(mfrow=c(3,3))
for(i in 1:9) {
  plot(cmom[,i],type="l",ylab="Excess returns", xlab="Time")
  if(i==1) mtext("1 month", outer=T)
  if(i==2) mtext("6 months", outer=T)
  if(i==3) mtext("12 months", outer=T)
  if(i==1) mtext("1 month", side=2, outer=T)
  if(i==4) mtext("6 months", side=2, outer=T)
  if(i==7) mtext("12 months", side=2, outer=T)
}
title(main="Cross-sectional Momentum returns in time", line=3, outer=T)
