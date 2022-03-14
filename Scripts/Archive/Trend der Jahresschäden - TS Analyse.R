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
d.schaden <- read.csv2(t.schaden.file)
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

## Define as (multiple-) time series of class ts
## frequency:
## monthly: frequency=12, deltat=1/12
## yearly:  frequency=1, deltat=1
## daily:   frequency=365, deltat=1/365
schad.ts <- ts(d.schaden[,-1],start=1960,frequency=1)

## Definde fs as time series
fs.ts <- ts(d.schaden$pm.fs, start=1960, frequency=1)
## Definde fs as time series
es.ts <- ts(d.schaden$pm.es, start=1960, frequency=1)

# ## Trend Estimation with Running Mean
# trend.est <- filter(fs.ts, filter=c(1,1,1)/3)
# plot(fs.ts, main="Jährliche Feuerschäden")
# lines(trend.est, col="red")

## Is this a stationary time series?
## Draw a time series plot. If need be, transform the original data. Than do diff()
source('ftp://stat.ethz.ch/WBL/Source-WBL-2/R/f.acf.R')
## fs
f.acf(fs.ts) # Peaks, stationary? --> no!
f.acf(diff(fs.ts)) # stationary now? --> yes, trend was removed by taking differences
## es
f.acf(es.ts) # Peaks?
f.acf(log(es.ts)) # stationary?
f.acf(diff(log(es.ts))) # stationary now?

## MODEL FIT
## Fit a suitable ARIMA(p,d,q) model --> correlogram
## p: Auto-Regressive model --> look at a partial autocorrelation graph of the data
## q: Moving Average model --> look at an autocorrelation graph of the data
## d: Differencing --> original ts, d=1, diff-data, d=0
 
## Alternative:
## try Akaike's Information Criterion (AIC) on a set of models and investigate the models 
## with the lowest AIC values
## try the Schwartz Bayesian Information Criterion (BIC) and investigate the models with 
## the lowest BIC values
## Exemple: require(forecast); auto.arima(x, ic = "aic")

## Write down the model with its estimated coefficients:
## ARIMA(6,1,1): Yt=Xt-Xt-1 mit 
## Yt = alpha1.hat*Yt-1 +Et - beta1.hat*Et-1; sigma^2(Et)
## Et follows the standard normal distribution N(0;1)
ar.fit.fs <- arima(fs.ts, order=c(6,1,1)) ## or order=c(0,1,1) mit aic
ar.fit.es <- arima(log(es.ts), order=c(4,1,1)) ## or order=c(2,1,0) mit aic
# Coefficients alpha1.hat, beta1.hat and sigma:
ar.fit.fs$coef[1]; ar.fit.fs$coef[2]; ar.fit.fs$sigma2 # or ar.fit
ar.fit.es$coef[1]; ar.fit.es$coef[2]; ar.fit.es$sigma2 # or ar.fit

## Check Residulas, must look like white noise:
ts.resid.fs <- ts(resid(ar.fit.fs))
f.acf(ts.resid.fs)
ts.resid.es <- ts(resid(ar.fit.es))
f.acf(ts.resid.es)
## Check assumption of normal-distribution:
par(mfrow=c(1,2))
qqnorm(ts.resid.fs, main="Normal QQ Plot of Resids-Feuerschaeden")
qqline(ts.resid.fs)
qqnorm(ts.resid.es, main="Normal QQ Plot of Resids-log Elementarschäden")
qqline(ts.resid.es)
par(mfrow=c(1,1))

# ## Exponential smoothing for predicting
# ## no seasonality, simple exponential smoothing to make short-term forecasts
# ## fs
# ex.fit.fs <- HoltWinters(fs.ts, beta=FALSE, gamma=FALSE)
# ex.fit.fs ## coef = alpha
# plot(ex.fit.fs)
# ## es
# ex.fit.es <- HoltWinters(es.ts, beta=FALSE, gamma=FALSE)
# ex.fit.es ## coef = alpha
# plot(ex.fit.es)

# ## Fit a suitable GARCH(p,q) model
# ## --> Find the order (p,q) with minimal AIC
# ## Choose p and q from 0 to 2. Both cannot be zero at the same time
# require(tseries)
# mAIC <- matrix(rep(NA, 9), nrow=3)
# colnames(mAIC) <- c("ARCH0","ARCH1","ARCH2")
# rownames(mAIC) <- c("GARCH0","GARCH1","GARCH2")
# for (i in 0:2){ for (j in 0:2){
#   if(i!=0 |j!=0){
#     fit <- garch(log(es.ts),order=c(i,j), trace=F)
#     mAIC[i+1,j+1] <- AIC(fit)
#   }}}
# mAIC
# min(mAIC, na.rm=T)
# 
# ## The AIC suggests a GARCH(0,2) for fs.ts
# gar.fit.fs <- garch(fs.ts,order=c(0,2), trace=F)
# round(coef(gar.fit.fs),3)
# ## The AIC suggests a GARCH(0,2) for log(es.ts)
# gar.fit.es <- garch(log(es.ts),order=c(0,2), trace=F)
# round(coef(gar.fit.es),3)
# 
# ## mit garchFit from fGarch-packages
# require(fGarch)
# gfit.fs <- garchFit(data=fs.ts, cond.dist="std")

