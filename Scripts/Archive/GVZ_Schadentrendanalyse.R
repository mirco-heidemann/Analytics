####################################################################################################################
## Eine Trend Analyse der Schadenentwicklung von 1960 - 2011
## Elementarschäden und Feuerschäden
##
## Gebäudeversicherung Kanton Zürich, Bereich Naturgefahren
## Mirco Heidemann, 10/2012
####################################################################################################################
library(XLConnect)
library(ggplot2)
library(gridExtra)
library(Kendall)
setwd('D:/USERS/mheidemann/Statistical_Computing_R/ES_und_Feuerjahresschaeden_Entwicklung/')

# Einlesen der Jahresschäden und speichern in einem Data Frame
schadenentwicklung.excel <- 'Schadenentwicklung_1960_2011.xlsx'
elementarschaden <- readWorksheet(loadWorkbook(schadenentwicklung.excel),sheet=1)
feuerschaden <- readWorksheet(loadWorkbook(schadenentwicklung.excel),sheet=2)
#**********************************************************************************************************
# Indem nur die Anzahl der beschädigten Gebäude im Verhältnis zum Gesamtgebäudebestand betrachtet wird,
# lässt sich die Schadenentwicklung vollständig von Wert- bzw. Preisentwicklungen abkoppeln.
#**********************************************************************************************************
feuerschaden.anteil<-feuerschaden[,4]/feuerschaden[,3]
elementarschaden.anteil<-elementarschaden[,4]/elementarschaden[,3]
# Time Series
ts.elementar<-ts(elementarschaden.anteil, start=1950, freq=1)
ts.feuer<-ts(feuerschaden.anteil, start=1950, freq=1)
# Trend Estimation with Running Mean
trend.elementar<-filter(ts.elementar, filter=c(1,1,1)/3)

# Ordinary Least Squares Regression Model
# Daten vorbereiten:
# 1. Elementar
num.schaden.es<-as.numeric(ts.elementar)
num.time<-as.numeric(time(ts.elementar))
dat.elementar<-data.frame(schaden=num.schaden.es, time=num.time)
lm.elementar<-lm(schaden ~ time, data=dat.elementar)
# Residual Diagnostic
summary(lm.elementar)
par(mfrow=c(2,2))
plot(lm.elementar)
# 2. Feuer
num.schaden.fs<-as.numeric(ts.feuer)
num.time<-as.numeric(time(ts.feuer))
dat.feuer<-data.frame(schaden=num.schaden.fs, time=num.time)
lm.feuer<-lm(schaden ~ time, data=dat.feuer)

# Residual Diagnostic
summary(lm.feuer)
par(mfrow=c(2,2))
plot(lm.feuer)

# Residuals oft he lm() Function
par(mfrow=c(1,1))
plot(dat.feuer$time, resid(lm.feuer), type="l")
plot(dat.elementar$time, resid(lm.elementar), type="l")
# ACF and PACF of Residuals
# 1. Elementar
par(mfrow=c(1,2))
acf(resid(lm.elementar), main="ACF of Residuals")
pacf(resid(lm.elementar), main="PACF of Residuals")
# 2. Feuer
par(mfrow=c(1,2))
acf(resid(lm.feuer), main="ACF of Residuals")
pacf(resid(lm.feuer), main="PACF of Residuals")

# Durbin-Watson Test
library(lmtest)
# 1. Elementar
dwtest(lm.elementar)
# 2. Feuer
dwtest(lm.feuer)

# Generalized Least Squares Regression Model
library(nlme)
corStruct<-corARMA(form=~time, p=2)
# 1. Feuer
fit.feuer.gls<-gls(schaden~time, data=dat.feuer, corr=corStruct)
# 2. Elementar
fit.elementar.gls<-gls(schaden~time, data=dat.elementar, corr=corStruct)
# ACF and PACF of Residuals
# 1. Feuer
par(mfrow=c(1,2))
acf(resid(fit.feuer.gls), main="Feuer: ACF of GLS-Residuals")
pacf(resid(fit.feuer.gls), main="Feuer: PACF of GLS-Residuals")
# 2. Elementar
acf(resid(fit.elementar.gls), main="Elem: ACF of GLS-Residuals")
pacf(resid(fit.elementar.gls), main="Elem: PACF of GLS-Residuals")

# Vergleich OLS und GLS
# OLS
coef(lm.feuer)["time"]
coef(lm.elementar)["time"]
confint(lm.feuer, "time")
confint(lm.elementar, "time")
# GLS
coef(fit.feuer.gls)["time"]
coef(fit.elementar.gls)["time"]
confint(fit.feuer.gls, "time")
confint(fit.elementar.gls, "time")

# ********************************************************************************
# ARIMA Model
# ********************************************************************************
# Daten vorbereiten - log Transformation
log.elementar<-log(ts.elementar)
plot(log.elementar, ylab="log(Schadenanteil)")
title ("Logarithmierter Jahresschadenanteil Elementarschäden")
log.feuer<-log(ts.feuer)
plot(log.feuer, ylab="log(Schadenanteil)")
title ("Logarithmierter Jahresschadenanteil Feuerschäden")
# Trend sichtbar (?) somit nicht stationär.
# a) try first-order differencing and then check wheter the result is stationary
# 1. Elementar
dlog.elementar<-diff(log.elementar)
plot(dlog.elementar, ylab="Differenzen")
title("Differenzen der log. Jahres Elementarschäden")
# 2. Feuer
dlog.feuer<-diff(log.feuer)
plot(dlog.feuer, ylab="Differenzen")
title("Differenzen der log. Jahres Feuerschäden")
# ACF and PACF of Residuals of the differences
# 1. Elementar
par(mfrow=c(1,2))
acf(dlog.elementar, main="Elem: ACF of logged Diff.", ylim=c(-1,1), lag.max=15)
pacf(dlog.elementar, main="Elem: PACF of logged Diff.", ylim=c(-1,1), lag.max=15)
# 2. Feuer
par(mfrow=c(1,2))
acf(dlog.feuer, main="Feuer: ACF of logged Diff.", ylim=c(-1,1), lag.max=15)
pacf(dlog.feuer, main="Feuer: PACF of logged Diff.", ylim=c(-1,1), lag.max=15)


# Find the right model with the auto.arima function (forecast package)
library(forecast)
auto.fit.elementar<-auto.arima(ts.elementar)
auto.fit.feuer<-auto.arima(ts.feuer)

# Plot the Log-Transformation
arima(log.feuer, order=c(1,1,1))
# Plot the Model with the "Arima function" in the "forecast" Packages
# Note: The arima() function is different from the Arima() function. The former is contained in the 
# basis {stats} package, the lattercomes from the package {forecast} and includes the fitted() function 
# to predict over the observed values.

par(mfrow=c(1,2))
# 1. Feuer
fit.log.feuer <- Arima(log.feuer,order=c(1,1,1))
# Original series in black...
plot(fit.log.feuer$x,col="black")
# ..., the fitted series in red
lines(fitted(fit.log.feuer),col="red")
# 2. Elementar
fit.log.elementar <- Arima(log.elementar,order=c(1,1,1))
# Original series in black...
plot(fit.log.elementar$x,col="black")
# ..., the fitted series in red
lines(fitted(fit.log.elementar),col="red")

# Plot the Time Series
arima(ts.feuer, order=c(0,1,1))
# Plot the Model with the "Arima function" in the "forecast" Packages
# Note: The arima() function is different from the Arima() function. The former is contained in the 
# basis {stats} package, the lattercomes from the package {forecast} and includes the fitted() function 
# to predict over the observed values.

par(mfrow=c(1,1))
# 1. Feuer
fit.feuer <- Arima(ts.feuer,order=c(1,1,1))
# Original series in black...
# plot(fit.feuer$x,col="black", main="Feuerschäden und ARIMA(0,1,1) Modell (rot)", ylab="Schadenanteil")
# ..., the fitted series in red
# lines(fitted(fit.feuer),col="red", lty=3)
# Modell Kurve geglättet
# lines(smooth.spline(fitted(fit.feuer), spar = 1), col="red")
plot(forecast(fit.feuer, h=10), ylim=c(0.00, 0.25))
lines(fitted(fit.feuer), col="red", lty=3)

# 2. Elementar
fit.elementar <- Arima(ts.elementar,order=c(1,1,1))
# Original series in black...
# plot(fit.elementar$x,col="black", main="Elementarschäden und ARIMA(0,1,1) Modell (rot)", ylab="Schadenanteil")
# ..., the fitted series in red
# lines(fitted(fit.elementar),col="red", lty=3)
# Modell Kurve geglättet
# lines(smooth.spline(fitted(fit.elementar), spar = 1), col="red")
plot(forecast(fit.elementar, h=10), ylim=c(0.00, 0.25))
lines(fitted(fit.elementar), col="red", lty=3)

# Forecasting
# fs.predict<-predict(fit.feuer,n.ahead=10)
# es.predict<-predict(fit.elementar,n.ahead=10)
# Plot forecasting
# 1. Feuer
# plot(ts.feuer,xlim=c(1950,2020))
# lines(fs.predict$pred,col="red")
# # Add the confidence interval (95%) of the prediction --> "prediction +- 2*SE"
# lines(fs.predict$pred+2*fs.predict$se,col="red",lty=3)
# lines(fs.predict$pred-2*fs.predict$se,col="red",lty=3)

# 2. Elementar
# plot(ts.elementar,xlim=c(1950,2020))
# lines(es.predict$pred,col="red")
# # Add the confidence interval (95%) of the prediction --> "prediction +- 2*SE"
# lines(es.predict$pred+2*es.predict$se,col="red",lty=3)
# lines(es.predict$pred-2*es.predict$se,col="red",lty=3)

# Diagnostic plot of the fitted time series ARIMA model
tsdiag(fit.feuer)
tsdiag(fit.elementar)

# ********************************************************************************
# Exponential Smoothing and Prediction of Time Series
# ********************************************************************************
# The prediction intervals are, by default, computed for 80% and 95% coverage.

# Holt-Winters
# ************
# The ts - library in R contains the function HoltWinters(x,alpha,beta,gamma), which 
# lets one perform the Holt-Winters procedure on a time series x. One can specify 
# the three smoothing parameters with the options alpha, beta and gamma. 
# Particular components can be excluded by setting the value of the corresponding 
# parameter to zero, e.g. one can exclude the seasonal component by using gamma=0. 
# In case one does not specify smoothing parameters, these are determined 
# "automatically" (i.e. by minimizing the mean squared prediction error from 
# one-step forecasts).

# 1. Feuer
HW.feuer<-HoltWinters(ts.feuer, gamma=FALSE)
# Plot Fitted Model and forecast
# plot(ts.feuer,xlim=c(1950, 2016))
plot(forecast(HW.feuer, h=9, fan=TRUE), ylim=c(0.00, 0.25))
lines(fitted(HW.feuer)[,1], col="red", lty=3)
# HW.predict.feuer<-predict(HW.feuer, n.ahead=5)
# plot(ts.feuer,xlim=c(1950, 2016))
# lines(HW.predict.feuer, col="red")

# 2. Elementar
HW.elementar<-HoltWinters(ts.elementar, gamma=FALSE)
# Plot Fitted Model
# plot(ts.elementar,xlim=c(1950, 2016))
plot(forecast(HW.elementar, h=9, fan=TRUE), ylim=c(0.00, 0.25))
lines(fitted(HW.elementar)[,1], col="red", lty=3)
# HW.predict.elementar<-predict(HW.elementar, n.ahead=5)
# plot(ts.elementar,xlim=c(1950, 2016))
# lines(HW.predict.elementar, col="red")

# ETS (Error, Trend, Seasonal)
# ****************************
# 1. Feuer
ets.feuer<-ets(ts.feuer)
plot(forecast(ets.feuer, h=9, fan=TRUE), ylim=c(0.00, 0.25))
lines(fitted(ets.feuer), col="red", lty=3)


# 2. Elementar
ets.elementar<-ets(ts.elementar)
plot(forecast(ets.elementar, h=9, fan=TRUE), ylim=c(0.00, 0.25))
lines(fitted(ets.elementar), col="red", lty=3)
