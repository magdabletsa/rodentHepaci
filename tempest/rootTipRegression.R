#require(MASS)
require(graphics)

data.bo<-read.table("bovine.cut.ed.redDel.dates.fas.tempEst.txt", sep="\t", header=T)
reg.bo <- lm(data.bo$distance~data.bo$date)
data.eq<-read.table("equine.cut.ed.recDel.dates.fas.tempEst.txt", sep="\t", header=T)
reg.eq <- lm(data.eq$distance~data.eq$date)
data.ro1<-read.table("rodent1.cut.ed.recDel.dates.fas.tempEst.txt", sep="\t", header=T)
reg.ro1 <- lm(data.ro1$distance~data.ro1$date)
data.ro2<-read.table("rodent2.cut.ed.recDel.dates.fas.tempEst.txt", sep="\t", header=T)
reg.ro2 <- lm(data.ro2$distance~data.ro2$date)
data.h1a<-read.table("HCV1a.sub35.cut.fas.tempEst.txt", sep="\t", header=T)
reg.h1a <- lm(data.h1a$distance~data.h1a$date)
data.h1b<-read.table("HCV1b.sub34.cut.fas.tempEst.txt", sep="\t", header=T)
reg.h1b <- lm(data.h1b$distance~data.h1b$date)
data.h3a<-read.table("HCV3a.cut.sub.recDel.fas.tempEst.txt", sep="\t", header=T)
reg.h3a <- lm(data.h3a$distance~data.h3a$date)

reg.bo$coefficients[2]
reg.eq$coefficients[2]
reg.ro1$coefficients[2]
reg.ro2$coefficients[2]
reg.h1a$coefficients[2]
reg.h1b$coefficients[2]
reg.h3a$coefficients[2]

#colors according to slope
  Acol <- "#d53e4f"
  Ccol <- "#f46d43"
  Bcol <- "#fdae61"
  Gcol <- "#fee08b"
  Dcol <- "#e6f598"
  Fcol <- "#abdda4"
  Jcol <- "#66c2a5"
  Hcol <- "#3288bd"
  Mcol <- "#000000"

par(mfrow=c(4,2))

plot(data.bo$date, data.bo$distance, pch=16,col=Acol, xlab="time",ylab="RtT divergence (subst./site)", cex=1.5)
#axis(4)
abline(reg.bo,lwd=3, col="grey")
title(main = "bovine")
legend("topleft", bty="n", legend=c(paste("R2 =", format(summary(reg.bo)$adj.r.squared, digits=2)), paste("r =", format(reg.bo$coefficients[2], digits=2))))

plot(data.eq$date, data.eq$distance, pch=16, col=Bcol, xlab="time",ylab="RtT divergence (subst./site)", cex=1.5)
#axis(4)
abline(reg.eq,lwd=3, col="grey")
title(main = "equine")
legend("topleft", bty="n", legend=c(paste("R2 =", format(summary(reg.eq)$adj.r.squared, digits=2)), paste("r =", format(reg.eq$coefficients[2], digits=2))))

plot(data.ro1$date, data.ro1$distance, pch=16, col=Ccol, xlab="time",ylab="RtT divergence (subst./site)", cex=1.5)
#axis(4)
abline(reg.ro1,lwd=3, col="grey")
title(main = "rodent I")
legend("topleft", bty="n", legend=c(paste("R2 =", format(summary(reg.ro1)$adj.r.squared, digits=2)), paste("r =", format(reg.ro1$coefficients[2], digits=2))))

plot(data.ro2$date, data.ro2$distance, pch=16, col=Dcol, xlab="time",ylab="RtT divergence (subst./site)", cex=1.5)
#axis(4)
abline(reg.ro2,lwd=3, col="grey")
title(main = "rodent II")
legend("topleft", bty="n", legend=c(paste("R2 =", format(summary(reg.ro2)$adj.r.squared, digits=2)), paste("r =", format(reg.ro2$coefficients[2], digits=2))))

plot(data.h1a$date, data.h1a$distance, pch=16, col=Fcol, xlab="time",ylab="RtT divergence (subst./site)", cex=1.5)
#axis(4)
abline(reg.h1a,lwd=3, col="grey")
title(main = "HCV 1a")
legend("topleft", bty="n", legend=c(paste("R2 =", format(summary(reg.h1a)$adj.r.squared, digits=2)), paste("r =", format(reg.h1a$coefficients[2], digits=2))))

plot(data.h1b$date, data.h1b$distance, pch=16, col=Jcol, xlab="time",ylab="RtT divergence (subst./site)", cex=1.5)
#axis(4)
abline(reg.h1b,lwd=3, col="grey")
title(main = "HCV 1b")
legend("topleft", bty="n", legend=c(paste("R2 =", format(summary(reg.h1b)$adj.r.squared, digits=2)), paste("r =", format(reg.h1b$coefficients[2], digits=2))))

plot(data.h3a$date, data.h3a$distance, pch=16, col=Hcol, xlab="time",ylab="RtT divergence (subst./site)", cex=1.5)
#axis(4)
abline(reg.h3a,lwd=3, col="grey")
title(main = "HCV 3a")
legend("topleft", bty="n", legend=c(paste("R2 =", format(summary(reg.h3a)$adj.r.squared, digits=2)), paste("r =", format(reg.h3a$coefficients[2], digits=2))))


