## ----------------------------------------------------------------------
## Time Series: Trends der Elementar-und Feuerschaeden
## 
## Mirco Heidemann
## GVZ Gebaeudeversicherung Kanton Zuerich, Bereich Naturgefahren
##
## Januar 2015
## ----------------------------------------------------------------------

## Arbeitsverzeichnis waehlen: Statistik bis 2014
setwd('J:/Naturgefahren/Datenanalysen/Jahresschaden Statistik/gvz Schadentrend/2015/')
t.schaden.name <- 'gvz jahreschadenstatistik 2015'
## csv file einlesen
t.schaden.file <- paste(t.schaden.name,'.csv', sep = '')
d.schaden <- read.csv(t.schaden.file, header=T, sep=";")
names(d.schaden) <- c("jahr","vs","Fschad","Eschad")
d.schaden$vs <- as.numeric(as.character(d.schaden$vs))
d.schaden$Fschad <- as.numeric(as.character(d.schaden$Fschad))
d.schaden$Eschad <- as.numeric(as.character(d.schaden$Eschad))

## Nur Werte ab 1960 (falls aeltere Werte ueberhaupt vorhanden)
ind <- which(d.schaden$jahr > 1959)
if (length(ind) > 0) d.schaden <- d.schaden[ind,]

## Feuer- und Elementarschaeden in promille der VersSumme
pm.es <- d.schaden$Eschad / d.schaden$vs
pm.fs <- d.schaden$Fschad / d.schaden$vs
d.schaden <- cbind(d.schaden, pm.fs = pm.fs, pm.es=pm.es)

## Distribution of fs and es (in promille of the vs)
par(mfrow=c(4,2))
## Feuerschaeden
hist(d.schaden$pm.fs, col="tomato", main="Histogram of Feuerschaeden")
qqnorm(d.schaden$pm.fs, pch=20, main="Normal QQ Plot of Feuerschaeden")
qqline(d.schaden$pm.fs, col="red")
## log - Feuerschaeden
hist(log(d.schaden$pm.fs), col="tomato", main="Histogram of log-Feuerschaeden")
qqnorm(log(d.schaden$pm.fs), pch=20, main="Normal QQ Plot of Feuerschaeden")
qqline(log(d.schaden$pm.fs), col="red")
## Elementarschaeden
hist(d.schaden$pm.es, col="lightblue", main="Histogram of Elementarschaeden")
qqnorm(d.schaden$pm.es, pch=20, main="Normal QQ Plot of Elementarschaeden")
qqline(d.schaden$pm.es, col="blue")
## log - Elementarschaeden
hist(log(d.schaden$pm.es), col="lightblue", main="Histogram of log-Elementarschaeden")
qqnorm(log(d.schaden$pm.es), pch=20, main="Normal QQ Plot of log-Elementarschaeden")
qqline(log(d.schaden$pm.es), col="blue")
par(mfrow=c(1,1))

# ## Define as (multiple-) time series of class ts
# ## frequency:
# ## monthly: frequency=12, deltat=1/12
# ## yearly:  frequency=1, deltat=1
# ## daily:   frequency=365, deltat=1/365
# schad.ts <- ts(d.schaden[,-1],start=1960,frequency=1)

## Definde fs as time series
fs.ts <- ts(d.schaden$pm.fs, start=1960, frequency=1)
## Definde fs as time series
es.ts <- ts(d.schaden$pm.es, start=1960, frequency=1)

plot(d.schaden$jahr, es.ts, ylab="...", xlab="Time", type="l",
     main="...")

## Compute and display Log Returns
rt.es <- diff(log(d.schaden$pm.es))
plot(d.schaden$jahr[-1], rt.es, ylab="Log Return", xlab="Time", type="l",
     main="... Log Returns")
abline(h=0, col="gray")

## Analyze Log Returns
par(mfrow=c(2,2))
acf(rt.es, main="ACF of Log Returns")
acf(rt.es^2, main="ACF of Squared Log Returns")
## Check assumption of normal-distribution:
qqnorm(rt.es, pch=20, main="Normal QQ Plot of Log Returns")
qqline(rt.es)
hist(rt.es, col="lightblue", freq=F, main=NA)
title("Histogram of Log Returns"); box()
xx <- seq(-4, 4, length=300)
yy <- dnorm(xx, mean(rt.es), sd(rt.es))
zz <- dnorm(xx, median(rt.es), mad(rt.es))
lines(xx, yy, col="red")
lines(xx, zz, col="blue")
legend("topright", lty=1, col=c("red", "blue"), legend=c("plain", "robust"))
par(mfrow=c(1,1))


## Determine three reasonable guesses each for a pure ARCH
## model, as well as for a GARCH model --> Analyze squared Log Returns
par(mfrow=c(1,2))
acf(rt.es^2, ylim=c(-1,1), main="ACF of Squared Log Returns")
pacf(rt.es^2, ylim=c(-1,1),main="PACF of Squared Log Returns")
par(mfrow=c(1,1))

## Fit a suitable GARCH(p,q) model
## --> Find the order (p,q) with minimal AIC
## Choose p and q from 0 to 2. Both cannot be zero at the same time
require(tseries)
mAIC <- matrix(rep(NA, 9), nrow=3)
colnames(mAIC) <- c("ARCH0","ARCH1","ARCH2")
rownames(mAIC) <- c("GARCH0","GARCH1","GARCH2")
for (i in 0:2){ for (j in 0:2){
  if(i!=0 |j!=0){
    fit <- garch(log(fs.ts),order=c(i,j), trace=F)
    mAIC[i+1,j+1] <- AIC(fit)
  }}}
mAIC
min(mAIC, na.rm=T)
## The AIC suggests a GARCH(0,2) for es.ts and fs.ts


## Fitting a GARCH(1,1) Modell with function fGarch
## ------------------------------------------------
## gaussion distribution
require(fGarch)
gfit <- garchFit(~garch(2,0), rt.es, cond.dis="norm", trace=F)
summary(gfit)

## Residual Analysis GARCH model, Autocorrelation of residuals
res <- residuals(gfit)
par(mfrow=c(2,2))
plot(res, type="l", main="Time Series of Residuals")
acf(res, na.action=na.pass, ylim=c(-1,1), main="ACF of squared Residuals")
pacf(res, na.action=na.pass, ylim=c(-1,1), main="PACF of squared Residuals")
#'QQ-Plot
qqnorm(as.numeric(res, ylim=c(-10, 10), pch=20, main='Residual normal plot'))
qqline(as.numeric(res), col="grey", lty=3)
par(mfrow=c(1,1))

## Fit GARCH model that is based on a heavy tailed
## innovation distribution (t-distribution)
## -----------------------------------------------
## Fitting a GARCH(1,1) Modell with a generalized t-distribution
require(fGarch)
gfit.h <- garchFit(~garch(1,1), rt.es, cond.dis="std", trace=F)
summary(gfit.h)

## Fitting a mean mean required? --> if p-Value of mu signif
## diff from 0, than fittting a mean is required.
## --> mittelwert sollte modelliert werden!

## Box-Ljung test does not reject the H0, than
## --> No Autocorrelation in squared resids to lag1
## --> Volatility (not good)

## Jarque Bera Test does reject the H0, than
## --> Residuals are NOT normal distributed

## Some consideration about the quality of different risk management methods
## Insample proportion of days when the 1-day 95% VaR was exceeded for...
## ... GARCH with heavy tailed innovation
## conditional standard deviation
csd.h <- gfit.h@sigma.t
## transform for insample estimate of the conditional scale parameter
csp.h <- csd.h*sqrt((coef(gfit.h)[5]-2)/coef(gfit.h)[5])
mean(rt.es < coef(gfit.h)[1]+csp.h*qt(0.05, coef(gfit.h)[5]))
