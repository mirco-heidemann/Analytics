## ----------------------------------------------------------------------
## Schadentrend Elementar-und Feuerschaeden
## 
## Mirco Heidemann
## GVZ Gebaeudeversicherung Kanton Zuerich, Bereich Naturgefahren
##
## Feuer-Jahresschaeden als Exponentielles (log) Regressions Modell
## Elementar-Jahresschaeden als Gamma Regressions Modell
##
## Januar 2017
## ----------------------------------------------------------------------
require(fitdistrplus)

## Arbeitsverzeichnis waehlen: Statistik bis 2016
setwd('J:/Naturgefahren/FG2_Datenanalysen/Jahresschaden Statistik/gvz Schadentrend/2017')

t.schaden.name <- 'gvz jahreschadenstatistik 2017'

## csv file einlesen
t.schaden.file <- paste(t.schaden.name,'.csv', sep = '')
jschad <- read.csv2(t.schaden.file)
names(jschad) <- c("jahr","vs","Fschad","Eschad")
jschad$vs <- as.numeric(as.character(jschad$vs))
jschad$Fschad <- as.numeric(as.character(jschad$Fschad))
jschad$Eschad <- as.numeric(as.character(jschad$Eschad))

## Nur Werte ab 1960 (falls aeltere Werte ueberhaupt vorhanden)
ind <- which(jschad$jahr > 1959)
if (length(ind) > 0) jschad <- jschad[ind,]

## Feuer- und Elementarschaeden in promille der VersSumme
jschad$pm.fs <- as.numeric(jschad$Fschad) / as.numeric(jschad$vs)
jschad$pm.es <- as.numeric(jschad$Eschad) / as.numeric(jschad$vs)

## Lineare exponentielle Regression fuer jaehrliche Feuerschaeden
exp.model.fs <- lm(log(jschad$pm.fs)~ jschad$jahr)
# summary(exp.model.fs) ## das modell erklaert rund 40% der varianz

fit.exp.fs = exp(fitted(exp.model.fs))
## Welches Modell fuer die jaehrlichen Elementarschaeden?
# ## Lineares Regressions Modell
# ln.model <-lm(jschad$pm.es ~ jschad$jahr)
# summary(ln.model) ## das modell erklaert nur knapp 4% der varianz
# 
# ## Lineare exponentielle Regression
exp.model.es <- lm(log(jschad$pm.es)~ jschad$jahr)
# summary(exp.model) ## das modell erklaert rund 10% der varianz
# 
## Grafik zeichnen
yearvalues <- seq(min(jschad$jahr), max(jschad$jahr), 1)
counts.exponential <- exp(predict(exp.model.es,list(jahr=yearvalues)))
plot(jschad$jahr, jschad$pm.es,pch=16)
lines(yearvalues, counts.exponential,lwd=2, col = "blue",
      xlab = "Jahr", ylab = "Elementarschad")

## Gamma GLM mit einem log link
gam.model.es <- glm(jschad$pm.es ~ jschad$jahr, family=Gamma(link="log"))
summary(gam.model.es)

## Grafik zeichnen
yearvalues <- seq(min(jschad$jahr), max(jschad$jahr), 1)
counts.gamma <- predict(gam.model.es, newdata=list(jahr=yearvalues), type="response")
## ist dasselbe wie: counts.gamma <- fitted(gam.model.es, newdata=list(jahr=yearvalues))
plot(jschad$jahr, jschad$pm.es,pch=16)
lines(yearvalues, counts.gamma,lwd=2, col = "lightblue",
      xlab = "Jahr", ylab = "Elementarschad")
lines(yearvalues, counts.exponential,lwd=2, col = "blue",
      xlab = "Jahr", ylab = "Elementarschad")

fit.gam.es <- fitted(gam.model.es, newdata=list(jahr=yearvalues), type="response")

## Datensatz mit Trenddaten aufbereiten
jschad.out <- cbind(jschad, fit.exp.fs, fit.gam.es)

## Daten als csv file ausgeben
file.csv <- paste(t.schaden.name,'.glm.fit.csv',sep="")
write.csv(jschad.out, file = file.csv, row.names = FALSE)
