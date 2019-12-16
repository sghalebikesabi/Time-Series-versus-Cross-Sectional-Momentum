#-------------------------------------working directory
setwd("/Users/sahra/Google Drive/Notability/FMT Seminar/Empirie/Project")
files <- list.files()
files <- files[files!="script.R"]
for(i in 1:length(files)) source(paste0("/Users/sahra/Google Drive/Notability/FMT Seminar/Empirie/Project/",files[i]))
setwd("/Users/sahra/Google Drive/Notability/FMT Seminar/Empirie/Data")

#-------------------------------------packages
require(xtable)
require(PerformanceAnalytics)

########################################################
########################################################
########################################################

#-------------------------------------data
msci <- read("msci.csv")[,c(1,2)]

rf <- read2("dooofusshortdummi.csv",".")
rf <- rf[,c(1,2)] 
rf[,2] <- (1+rf[,2]/100)^(1/250)-1
rf.mon <- read2("bmk.csv")
rf.mon <- rf.mon[,c(1,2)]
rf.mon[,2] <- (1+rf.mon[,2]/100)^(1/12)-1

ps <- read2("Primeshare.csv")
ps.btm <- ps[,c(1,seq(5,ncol(ps),4))] #wrong datatype
ps.btm[,2:ncol(ps.btm)] <- 1/ps.btm[,2:ncol(ps.btm)]
ps.mv <- ps[,c(1,seq(2,ncol(ps),4))] 
ps <- ps[,c(1,seq(3,ncol(ps),4))] 

ps.years <- format(as.Date(ps[,1]), "%Y")
ps9400 <- ps[(ps.years>=1994)*(ps.years<2000)*(1:nrow(ps)),]
ps0002 <- ps[(ps.years>=2000)*(ps.years<2002)*(1:nrow(ps)),]
ps.short <- ps[(ps.years>=2002)*(ps.years<2005)*(1:nrow(ps)),]
ps.short.mon <- monexret(ps.short)
ps.short.vola <- vola(ps.short)


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
sd(annualized.volatility[is.na(annualized.volatility)==F])
mean(annualized.volatility[is.na(annualized.volatility)==F])
annualized.volatility[is.nan(annualized.volatility)==T] <- "Not enough values"
sum.stats <- data.frame(Name=companies, StartDate = data.start.date, AnnualizedReturn = round(as.numeric(annualized.mean),4), AnnualizedVolatility = annualized.volatility)
ps.mon <- monexret(ps)

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
#-------------------------------------optimal threshold
ps.opt.th <- optimal.th(ps)

cmom.strats <- list(ps.cmom,ps.cmom.scaled,ps.cmom.tvm,ps.cmom.netlong2,ps.cmom.netlong3,ps.cmom.netlong4,ps.cmom.scaled.tvm,ps.cmom.scaled.netlong2,ps.cmom.scaled.netlong3,ps.cmom.scaled.netlong4) 
tmom.strats <- list(ps.tmom,ps.tmom.scaled,ps.tmom.netlong,ps.tmom.scaled.netlong)
bah.strats <- list(ps.bah, ps.bah.scaled)

cmom.stats <- stats(cmom.strats)
tmom.stats <- stats(tmom.strats)
bah.stats <- stats(bah.strats)
#
tvm <- ps.bah
netlong <- ps.tmom.netlongpos
reg.st(tvm,netlong)

cum <- cumulate(ps.cmom) #CAR diagram
#plotstrat(cum)
########################################################
########################################################
########################################################

#-------------------------------------position sizes
#Specifically, we regress CS excess returns against TS excess returns and vice versa, and test whether the alphas in these regressions are different from zero.

#-------------------------------------turnover
ps.cmom.turnover <- cmom(calc.turnover = T)

#-------------------------------------mops vol
mop <- read.table("/Users/sahra/Google Drive/Notability/FMT Seminar/Empirie/Data/mop.csv", sep=";", dec=".")
mop <- as.numeric(mop)[1:(length(mop)-1)]/100
sd(mop)
mean(mop)
#autocorrelation between returns lags:1,3,36
#regress on dummy threshold
#period 36 to consider wether both times mean reversion
#return aufsplitten in TVM, risk premium market timing return
#Gewinne annualisieren
#regressions
#regress CS profits against TS profits and find that the intercept is insignificant
#goy17: regress CS excess returns against TS excess returns and vice versa
#in different periods (exclude year of crashes, analyse crashes)
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
#one month gap btw rp and hp
#correlation of turnover and excess returns (explained by transaction costs)
#leverage!!!! unleverage, CORRECT TVM STRATEGIES
#differently classified assets 0<r<th
#regress turnover-diff on difference in results
#
#
