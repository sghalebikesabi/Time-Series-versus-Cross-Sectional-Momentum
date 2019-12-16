#-------------------------------------working directory
setwd("C:\\Users\\Hiwi\\Desktop\\Empirie\\Project")
files <- list.files()
files <- files[files!="script.R"]
for(i in 1:length(files)) source(paste0("C:\\Users\\Hiwi\\Desktop\\Empirie\\Project\\",files[i]))
setwd("/Users/sahra/Google Drive/Notability/FMT Seminar/Empirie/Data")

#-------------------------------------packages
require(xtable)
require(PerformanceAnalytics)

########################################################
########################################################
########################################################

#-------------------------------------data
bonds <- read("bondx.csv")
for(i in 2:ncol(bonds)){ 
  nas <- is.na(bonds[,i])==F
  bonds[nas,i] <- as.numeric(bonds[nas,i])
}
bonds <- bonds[,c(1:4,6,7,9:25,31:ncol(bonds))]

msci <- read("msci.csv")[,c(1,2)]

ps <- read2("Primeshare.csv")
ps.btm <- ps[,c(1,seq(5,ncol(ps),4))] #wrong datatype
ps.btm[,2:ncol(ps.btm)] <- 1/ps.btm[,2:ncol(ps.btm)]
ps.mv <- ps[,c(1,seq(2,ncol(ps),4))] 
ps <- ps[,c(1,seq(3,ncol(ps),4))] 

rf <- read2("dooofusshortdummi.csv",".")
rf <- rf[,c(1,2)] 
rf[,2] <- (1+rf[,2]/100)^(1/250)-1
rf.mon <- read2("bmk.csv")
rf.mon <- rf.mon[,c(1,2)]
rf.mon[,2] <- (1+rf.mon[,2]/100)^(1/12)-1

ps.years <- format(as.Date(ps[,1]), "%Y")
ps9400 <- ps[(ps.years>=1994)*(ps.years<2000)*(1:nrow(ps)),]
ps0002 <- ps[(ps.years>=2000)*(ps.years<2002)*(1:nrow(ps)),]
ps.short <- ps[(ps.years>=2002)*(ps.years<2005)*(1:nrow(ps)),]
ps.short.mon <- monexret(ps.short)
ps.short.vola <- vola(ps.short)

new <- cbind(bonds[(nrow(bonds)-nrow(ps)+1):nrow(bonds),], ps[,2:ncol(ps)])
new.mon <- monexret(new)
new.vola <- vola(new)

#-------------------------------------summary statistics
companies <- colnames(ps)[2:ncol(ps)]
data.start.date <- companies
for(i in 2:ncol(ps)){
  save <- is.na(ps[,i-1])*(1:nrow(ps))
  data.start.date[i-1] <- as.character(ps[which.min(save),1])
}
daily.ps <- dayexret(ps)
annualized.mean <- numeric(ncol(ps)-1)
for(i in 2:ncol(ps)){
  annualized.mean[i-1] <- mean(daily.ps[,i][is.na(daily.ps[,i])==F])
}
annualized.mean <- (1+annualized.mean)^250-1
ps.vola <- vola(ps) 
annualized.volatility <- companies
for(i in 2:ncol(ps)){
  annualized.volatility[i-1] <- mean(ps.vola[,i][is.na(ps.vola[,i])==F])
}
annualized.volatility <- round(as.numeric(annualized.volatility),4)
annualized.volatility[is.nan(annualized.volatility)==T] <- "Not enough values"
sum.stats <- data.frame(Name=companies, StartDate = data.start.date, AnnualizedReturn = round(as.numeric(annualized.mean),4), AnnualizedVolatility = annualized.volatility)
ps.mon <- monexret(ps)


#-------------------------------------summary statistics bonds
companies <- colnames(bonds)[2:ncol(bonds)]
data.start.date <- companies
for(i in 2:ncol(bonds)){
  save <- is.na(bonds[,i-1])*(1:nrow(bonds))
  data.start.date[i-1] <- as.character(bonds[which.min(save),1])
}
daily.bonds <- dayexret(bonds)
annualized.mean <- numeric(ncol(bonds)-1)
for(i in 2:ncol(bonds)){
  annualized.mean[i-1] <- mean(daily.bonds[,i][is.na(daily.bonds[,i])==F])
}
annualized.mean <- (1+annualized.mean)^250-1
bonds.vola <- vola(bonds) 
annualized.volatility <- companies
for(i in 2:ncol(bonds)){
  annualized.volatility[i-1] <- mean(bonds.vola[,i][is.na(bonds.vola[,i])==F])
}
annualized.volatility <- round(as.numeric(annualized.volatility),4)
annualized.volatility[is.nan(annualized.volatility)==T] <- "Not enough values"
sum.stats <- data.frame(Name=companies, StartDate = data.start.date, AnnualizedReturn = round(as.numeric(annualized.mean),4), AnnualizedVolatility = annualized.volatility)

########################################################
########################################################
########################################################
#-------------------------------------cmom
ps.cmom <- cmom(ps) #
#cmom netlong
ps.cmom.tvm <- cmom(ps,netlong=1)
#
save2 <- cmom(ps,netlong=2)
ps.cmom.netlong2 <- save2[[1]]
ps.cmom.netlongpos <- save2[[2]]
ps.cmom.long <- save2[[3]]
#
ps.cmom.netlong3 <- cmom(netlong=3)
#
save3 <- cmom(netlong=4)
ps.cmom.netlong4 <- save3[[1]]
#cmom scaled
ps.cmom.scaled <- cmom(ps,scaled=T)
#cmom scaled netlong
ps.cmom.scaled.tvm <- cmom(ps,netlong=1,scaled=T)
#
save4 <- cmom(ps,netlong=2,scaled=T)
ps.cmom.scaled.netlong2 <- save4[[1]]
ps.cmom.scaled.netlongpos2 <- save4[[2]]
ps.cmom.scaled.long2 <- save4[[3]]
#
ps.cmom.scaled.netlong3 <- cmom(ps,netlong=3,scaled=T)
#
save1 <- cmom(ps,netlong=4,scaled=T)
ps.cmom.scaled.netlong4 <- save1[[1]]
tvm <- save1[[2]]
#-------------------------------------tmom
ps.tmom <- tmom(ps,netlong=0)
#tmom netlong
save6 <- tmom(ps,netlong=2)
ps.tmom.netlong <- save6[[1]]
ps.tmom.netlongpos <- save6[[2]]
ps.tmom.long <- save6[[3]]
#tmom scaled
ps.tmom.scaled <- tmom(ps,scaled=T,netlong=0)
#tmom scaled netlong
save8 <- tmom(ps,netlong=2,scaled=T)
ps.tmom.scaled.netlong <- save8[[1]]
ps.tmom.scaled.netlongpos <- save8[[2]]
ps.tmom.scaled.long <- save8[[3]]
#-------------------------------------bah
ps.bah <- bah(ps)
#bah scaled
ps.bah.scaled <- bah(ps,scaled=T)

ts.tvm <- tmom(netlong=1)
####################################################
###################NEW##############################
####################################################
new.cmom <- cmom(new) #
#cmom netlong
new.cmom.tvm <- cmom(new,netlong=1)
#
#new.cmom.netlong3 <- cmom(netlong=3)
#
#cmom scaled
new.cmom.scaled <- cmom(new,scaled=T)
#cmom scaled netlong
new.cmom.scaled.tvm <- cmom(new,netlong=1,scaled=T)
#
#new.cmom.scaled.netlong3 <- cmom(new,netlong=3,scaled=T)
#
#-------------------------------------tmom
new.tmom <- tmom(new,netlong=0)
#tmom netlong
save6 <- tmom(new,netlong=2)
new.tmom.netlong <- save6[[1]]
new.tmom.netlongpos <- save6[[2]]
new.tmom.long <- save6[[3]]
#tmom scaled
new.tmom.scaled <- tmom(new,scaled=T,netlong=0)
new.tmom.scaled.netlong <- tmom(new,scaled=T,netlong=2)
#-------------------------------------bah
new.bah <- bah(new)
#bah scaled
new.bah.scaled <- bah(new,scaled=T)

#save <- list(new.cmom,new.cmom.scaled,new.cmom.tvm,new.cmom.scaled.tvm,new.cmom.scaled.netlong2,new.cmom.scaled.netlong) 
save <- list(new.cmom, new.cmom.scaled, new.cmom.tvm, new.cmom.scaled.tvm, new.tmom, new.tmom.scaled)
#cmom.strats <- list(new.cmom,new.cmom.scaled,new.cmom.tvm,new.cmom.scaled.tvm) 
#tmom.strats <- list(new.tmom,new.tmom.scaled,new.tmom.netlong,new.tmom.scaled.netlong)
#bah.strats <- list(new.bah, new.bah.scaled)

cmom.stats <- stats(cmom.strats)
tmom.stats <- stats(tmom.strats)
bah.stats <- stats(bah.strats)
#
tvm <- ps.bah
netlong <- ps.tmom.netlongpos
reg.st(tvm,netlong)

cum <- cumulate(ps.cmom.netlong2) #CAR diagram
cum2 <- cumulate(ps.tmom.scaled.netlong)
par(mfrow=c(1,1))
for(i in 2:10){
  plot(cum[,c(1,i)], type="l", ylab="Accumulated returns", main=paste0(i), ylim=c(0.94, max(cum2[,i])))
  lines(cum2[,c(1,i)], col="red")
}

plot(cum[,c(1,5)], type="l", ylab="Accumulated returns",ylim=c(0.94, max(cum2[,i])))
lines(cum2[,c(1,5)], col="grey")
legend("topright", col=c("black", "grey"), legend=c("CS", "TS"), inset = c(0,0), bty = "n", lty = c(1, 1), lwd=2, y.intersp=1.5)
View(tmom.scaled[[2]])
#plotstrat(cum)
########################################################
########################################################
########################################################

ts.levered <- cmom(th=0,netlong=2)
cs.levered <- cmom()
bah.levered <- cmom(th=-10000000000,netlong=2)
levered <- list(ts.levered, cs.levered, bah.levered)
stats3(levered)
#-------------------------------------position sizes
#Specifically, we regress CS excess returns against TS excess returns and vice versa, and test whether the alphas in these regressions are different from zero.

#-------------------------------------turnover
ps.cmom.turnover <- cmom(calc.turnover = T);
ps.tmom.turnover <- tmom(calc.turnover = T);
ps.bah.turnover <- bah(calc.turnover = T);
ps.cmom.scaled.turnover <- cmom(calc.turnover = T, scaled=T)
ps.tmom.scaled.turnover <- tmom(calc.turnover = T, scaled=T)
ps.bah.scaled.turnover <- bah(calc.turnover = T, scaled=T)
ps.tvm.turnover <- cmom(calc.turnover = T, netlong=1)
ps.tvm.scaled.turnover <- cmom(calc.turnover = T, scaled=T, netlong=1)
save <- list(ps.cmom.turnover,ps.cmom.scaled.turnover,ps.tvm.turnover,ps.tvm.scaled.turnover,ps.tmom.turnover,ps.tmom.scaled.turnover,ps.bah.scaled.turnover)

cmom.scaled <- cmom(scaled=T)
tmom.scaled <- tmom(scaled=T)
bah.scaled <- bah(scaled=T, netlong=2)
tvm.scaled <- cmom(scaled=T, netlong=1)
cmom.ts.scaled <- cmom(scaled=T, netlong=2)
tmom.cs.scaled <- tmom(scaled=T, netlong=0)
cmom.we.scaled <- cmom(scaled=T, netlong=3)

plot(threshold, type="l")

#regress CS profits against TS profits and find that the intercept is insignificant
#goy17: regress CS excess returns against TS excess returns and vice versa
#when they regress TS profits against CS profits, the intercept is significantly positive
#test wether alphas of TSMOM CSMOM BAH are different for each individual contract and in a joint hypothesis
#This table reports the annualized excess returns of long minus short portfolios.
#We regress the excess return rst for instrument s in month t on its return lagged h months, where both returns are scaled by their ex ante volatilities
#r/sigma=alpha+beta*sign(preceding return)
#sign on sign, marketexcess on sign
#relationship btw sum(1/sigma)=sigma mor16
#exclude crashes
#little relation between lagged volatility and average returns but there is a strong relationship between lagged volatility and current volatility
#correlation and squared diffence between true and optimal threshold