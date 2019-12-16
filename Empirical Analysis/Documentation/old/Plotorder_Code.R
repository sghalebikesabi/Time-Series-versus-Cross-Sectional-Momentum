#plots kreiren und nach jedem Plot mtext
#plots werden von links nach rechts im grid ansortiert
#Plot links oben
plot(runif(10), xlab = '', ylab = '1', las = 1, ylim = c(0, 1))
mtext("12", side=2, line = 3, outer = F, adj=.5)  

#Plot mitte oben (ohne Beschriftung)
plot(runif(10), xlab = '', ylab = '', ylim = c(0, 1))

#Plot rechts oben (ohne Beschriftung)
plot(runif(10), xlab = '', ylab = '', ylim = c(0, 1))

#Plot links mitte
plot(runif(10), xlab = '', ylab = '1', las = 1, ylim = c(0, 1))
mtext("6", side=2, line = 3, outer = F, adj=.5)

#Plot mitte mitte (ohne Beschriftung)
plot(runif(10), xlab = '', ylab = '1', las = 1, ylim = c(0, 1))

#Plot rechts mitte (ohne Beschriftung)
plot(runif(10), xlab = '', ylab = '1', las = 1, ylim = c(0, 1))

#Plot links unten
plot(runif(10), xlab = '', ylab = '', las = 1, ylim = c(0, 1))
mtext("1", side=2, line = 3, outer = F, adj=.5) 
mtext("1", side=1, line = 3, outer = F, adj=.5)

#Plot mitte unten
plot(runif(10), xlab = '', ylab = '', ylim = c(0, 1))
mtext("6", side=1, line = 3, outer = F, adj=.5)

#Plot rechts unten
plot(runif(10), xlab = '', ylab = '', ylim = c(0, 1))
mtext("12", side=1, line = 3, outer = F, adj=.5)

#plotgrid erstellen
par(mfrow = c(3, 3)) 
par(oma = c(4, 4, 0, 0)) 
par(mar = c(2, 2, 1, 1))

#overall labels (ganz auﬂenstehende Labels)
mtext('Holding Period', side = 1, outer = TRUE, line = 3)
mtext('Ranking Period', side = 2, outer = TRUE, line = 3)
