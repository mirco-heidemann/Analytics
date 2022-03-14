#'----------------------------------------------------------------------
#'Verlauf der GVZ Jahresschaeden seit 1960
#' 
#'Mirco Heidemann
#'GVZ Gebaeudeversicherung Kanton Zuerich, Bereich Naturgefahren
#'
#'Januar 2014
#'----------------------------------------------------------------------
## Arbeitsverzeichnis waehlen
setwd('J:/MHE/08 Jahresschaden Statistik/gvz Schadentrend/2014/')
t.schaden.name <- 'gvz jahreschadenstatistik 2014'
## csv file einlesen
t.schaden.file <- paste(t.schaden.name,'.csv', sep = '')
d.schaden <- read.csv(t.schaden.file, header=T, sep=";")
names(d.schaden) <- c("jahr","vs","fs","es")

#'Nur Werte ab 1960 (falls aeltere Werte ueberhaupt vorhanden)
ind <- which(d.schaden$jahr > 1959)
if (length(ind) > 0) d.schaden <- d.schaden[ind,]

#'Schaden als Anteil der Versicherungssumme:
#'Anzahl der beschädigten Gebäude im Verhältnis zum Gesamtgebäudebestand. Somit laesst 
#'sich die Schadenentwicklung von Wert- bzw. Preisentwicklungen entkoppeln
d.schaden$pm.fs <- as.numeric(d.schaden$fs) / as.numeric(d.schaden$vs)
d.schaden$pm.es <- as.numeric(d.schaden$es) / as.numeric(d.schaden$vs)

#'Schadenkurve schaetzen mit linearer Regression
lm.lin.fs <- lm(pm.fs ~ jahr, data=d.schaden)
lm.log.fs <- lm(log10(pm.fs) ~ jahr, data=d.schaden)
#'Wurzeltransformation da Zaehldaten
lm.sqrt.fs <- lm(sqrt(pm.fs) ~ jahr, data=d.schaden)
lm.lin.es <- lm(pm.es ~ jahr, data=d.schaden)
lm.log.es <- lm(log10(pm.es) ~ jahr, data=d.schaden)
lm.sqrt.es <- lm(sqrt(pm.es) ~ jahr, data=d.schaden)
#'ArcSin Transformation, da (Schaden-) Anteile in Promille
lm.arcsin.fs <- lm(asin(sqrt(pm.fs)) ~ jahr, data=d.schaden)
lm.arcsin.es <- lm(asin(sqrt(pm.es)) ~ jahr, data=d.schaden)

d.schaden<-cbind(d.schaden,
                 fit.lin.fs=fitted(lm.lin.fs),
                 fit.log.fs=10^fitted(lm.log.fs),
                 fit.sqrt.fs=(fitted(lm.sqrt.fs)^2),
                 fit.lin.es = fitted(lm.lin.es), 
                 fit.log.es = 10^fitted(lm.log.es),
                 fit.sqrt.es = (fitted(lm.sqrt.es)^2),
                 fit.arcsin.fs = sin(fitted(lm.arcsin.fs))^2,
                 fit.arcsin.es = sin(fitted(lm.arcsin.es))^2)

#'Trend Grafik und Tabelle für GVZ Schaeden
# file.pdf <- paste(t.filepath.out,t.schaden.name,'_fit.pdf',sep="")
# pdf(file=file.pdf, width=20, heigh=10, paper= "a4r")
par(mfrow = c(3, 2))
plot(d.schaden$jahr, d.schaden$pm.fs, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Feuerschadenentwicklung mit linearem Trend")
lines(d.schaden$jahr, d.schaden$fit.lin.fs, col = 'red', lwd = 1)
plot(d.schaden$jahr, d.schaden$pm.fs, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Feuerschadenentwicklung mit logarithmischem Trend")
lines(d.schaden$jahr, d.schaden$fit.log.fs, col = 'red', lwd = 1)
plot(d.schaden$jahr, d.schaden$pm.fs, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Feuerschadenentwicklung nach Wurzeltransformation")
lines(d.schaden$jahr, d.schaden$fit.sqrt.fs, col = 'red', lwd = 1)
plot(d.schaden$jahr, d.schaden$pm.fs, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Feuerschadenentwicklung mit ArcSin Wurzeltransformation")
lines(d.schaden$jahr, d.schaden$fit.arcsin.fs, col = 'red', lwd = 1)

plot(d.schaden$jahr, d.schaden$pm.es, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Elementarschadenentwicklung mit linearem Trend")
lines(d.schaden$jahr, d.schaden$fit.lin.es, col = 'red', lwd = 1)
plot(d.schaden$jahr, d.schaden$pm.es, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Elementarschadenentwicklung mit logarithmischem Trend")
lines(d.schaden$jahr, d.schaden$fit.log.es, col = 'red', lwd = 1)
plot(d.schaden$jahr, d.schaden$pm.es, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Elementarschadenentwicklung nach Wurzeltransformation")
lines(d.schaden$jahr, d.schaden$fit.sqrt.es, col = 'red', lwd = 1)
plot(d.schaden$jahr, d.schaden$pm.es, col = 'blue', lwd = 1,xlab="Jahr",
     ylab="Schaden als Anteil der VS",
     main="Elementarschadenentwicklung mit ArcSin Wurzeltransformation")
lines(d.schaden$jahr, d.schaden$fit.arcsin.es, col = 'red', lwd = 1)
par(mfrow = c(1, 1))
#dev.off()


summary(lm.sqrt.es)
summary(lm.arcsin.es)
summary(lm.log.es)

summary(lm.sqrt.fs)
summary(lm.arcsin.fs)
summary(lm.log.fs)

#'Residuen-Analyse (Residuen vs Fitted-Values, Normal-QQplot, ...)
# -----------------------------------------------------------------
## es sqrt
par(mfrow=c(2,2))
plot(lm.sqrt.es)
par(mfrow=c(1,1))
## es arc-sqrt
par(mfrow=c(2,2))
plot(lm.arcsin.es)
par(mfrow=c(1,1))
## es log
par(mfrow=c(2,2))
plot(lm.log.es)
par(mfrow=c(1,1))

## es vs zeit
par(mfrow=c(2,2))
## es sqrt
plot(resid(lm.sqrt.es)~d.schaden$jahr,
     main="Residuen (Elementar) gegen die Zeit",
     xlab="Jahr",ylab="Residuals")
abline(0,0, col = "red",lty=2)
## es arcsin
plot(resid(lm.arcsin.es)~d.schaden$jahr,
     main="Residuen (Elementar, arcsin) gegen die Zeit",
     xlab="Jahr",ylab="Residuals")
abline(0,0, col = "red",lty=2)
## es log
plot(resid(lm.log.es)~d.schaden$jahr,
     main="Residuen (Elementar, log) gegen die Zeit",
     xlab="Jahr",ylab="Residuals")
abline(0,0, col = "red",lty=2)
par(mfrow=c(1,1))

## fs sqrt
par(mfrow=c(2,2))
plot(lm.sqrt.fs)
par(mfrow=c(1,1))
## fs arsin
par(mfrow=c(2,2))
plot(lm.arcsin.fs)
par(mfrow=c(1,1))
## fs log
par(mfrow=c(2,2))
plot(lm.log.fs)
par(mfrow=c(1,1))

## fs vs zeit
par(mfrow=c(2,2))
## fs sqrt
plot(resid(lm.sqrt.fs)~d.schaden$jahr,
     main="Residuen (Feuer, sqrt) gegen die Zeit",
     xlab="Jahr",ylab="Residuals")
abline(0,0, col = "red",lty=2)
## fs arcsin
plot(resid(lm.arcsin.fs)~d.schaden$jahr,
     main="Residuen (Feuer, arcsin) gegen die Zeit",
     xlab="Jahr",ylab="Residuals")
abline(0,0, col = "red",lty=2)
## fs log
plot(resid(lm.log.fs)~d.schaden$jahr,
     main="Residuen (Feuer, log) gegen die Zeit",
     xlab="Jahr",ylab="Residuals")
abline(0,0, col = "red",lty=2)
par(mfrow=c(1,1))


# # Vorwaerts Verfahren (Keine NA's! --> na.omit)
# # --------------------------------------------
# source("http://stat.ethz.ch/~stahel/regression/regr.R")
# # r.modell <- regr(pm.es ~ jahr, data=d.schaden)
# r.modell <- regr(log10(pm.fs) ~ jahr, data=d.schaden)
# r.start <- regr(log10(pm.fs) ~ 1, data=d.schaden)
# r.fw <- step(r.start, scope=formula(r.modell), direction="forward")
# regr(r.fw, data=d.schaden)
# # Vorwaerts Verfahren mit regr --> lohnt sich ein Hinzufuegen von quadratischen Termen
# # oder Wechselwirkung? Nur, wenn hinzugefuegte Terme signifikant
# add1.regr(r.modell)
