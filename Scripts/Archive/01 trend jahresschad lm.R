## ----------------------------------------------------------------------
## Schadentrend fuer Elementar-und Feuerschaeden Grafik
## 
## Mirco Heidemann
## GVZ Gebaeudeversicherung Kanton Zuerich, Bereich Naturgefahren
##
## Oktober 2012
## Updated Januar 2017
## ----------------------------------------------------------------------
## Arbeitsverzeichnis waehlen: Statistik bis 2016
setwd('J:/Naturgefahren/FG2_Datenanalysen/Jahresschaden Statistik/gvz Schadentrend/2016')
t.schaden.name <- 'gvz jahreschadenstatistik 2016'
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
pm.es <- as.numeric(d.schaden$Eschad) / as.numeric(d.schaden$vs)
pm.fs <- as.numeric(d.schaden$Fschad) / as.numeric(d.schaden$vs)
d.schaden <- cbind(d.schaden, pm.fs = pm.fs, pm.es=pm.es)

## Trendschadenkurve schaetzen mit linearer Regression
## ----------------------------------------------------
## Ohne Transformation
lm.lin.fs <- lm(d.schaden$pm.fs ~ d.schaden$jahr)
## Log-Transformation
lm.log.fs <- lm(log10(d.schaden$pm.fs) ~ d.schaden$jahr)
## Wurzeltransformation
lm.sqrt.fs <- lm(sqrt(d.schaden$pm.fs) ~ d.schaden$jahr)
## ArcSin Transformation, da (Schaden-) Anteile in Promille
lm.arcsin.fs <- lm(asin(sqrt(pm.fs)) ~ jahr, data=d.schaden)

## Ohne Transformation
lm.lin.es <- lm(d.schaden$pm.es ~ d.schaden$jahr)
## Log-Transformation
lm.log.es <- lm(log10(d.schaden$pm.es) ~ d.schaden$jahr)
## Wurzeltransformation
lm.sqrt.es <- lm(sqrt(d.schaden$pm.es) ~ d.schaden$jahr)
## ArcSin Transformation, da (Schaden-) Anteile in Promille
lm.arcsin.es <- lm(asin(sqrt(pm.es)) ~ jahr, data=d.schaden)

## Alle Trendschadenkurven in einen Dataframe packen
d.schaden<-cbind(d.schaden,
                 fit.lin.fs = fitted(lm.lin.fs),
                 fit.log.fs = 10^fitted(lm.log.fs),
                 fit.sqrt.fs = fitted(lm.sqrt.fs)^2,
                 fit.arcsin.sqrt.fs = sin(fitted(lm.sqrt.fs))^2,
                 fit.lin.es = fitted(lm.lin.es),
                 fit.log.es = 10^fitted(lm.log.es),
                 fit.sqrt.es = fitted(lm.sqrt.es)^2,
                 fit.arcsin.sqrt.es = sin(fitted(lm.sqrt.es))^2)

## Plot der Trendgrafiken und Tabelle
file.pdf <- paste(t.schaden.name,'fit.pdf',sep=" ")
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
     main="Feuerschadenentwicklung mit quadratischem Trend")
lines(d.schaden$jahr, d.schaden$fit.sqrt.fs, col = 'red', lwd = 1)
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
     main="Elementarschadenentwicklung mit quadratischem Trend")
lines(d.schaden$jahr, d.schaden$fit.sqrt.es, col = 'red', lwd = 1)

par(mfrow = c(1, 1))
# dev.off()

file.csv <- paste(t.schaden.name,'.fit.csv',sep="")
# write.csv(d.schaden, file = file.csv, row.names = FALSE)

## quit R
##q('no')


## Extrapolation in die Zukunft. 
## ACHTUNG nur interne Spielereien!!
## Bevor nicht richtig modelliert wird, kann eine solche Rechnung
## nicht verwendet werden
t.neuejahre <- 2015:2040
t.all.jahre <- c(d.schaden$jahr,t.neuejahre)
## Prognose Feuerschaedenentwicklung
t.pred.schad.fs <- 10^(as.numeric(lm.log.fs$coef[1])+
  as.numeric(lm.log.fs$coef[2])*t.neuejahre)
t.alle.schad.fs <- c(d.schaden$pm.fs,t.pred.schad.fs)
t.alle.fit.fs <- c(d.schaden$fit.log.fs,t.pred.schad.fs)
## Prognose Elementarschadenentwicklung
t.pred.schad.es <- 10^(as.numeric(lm.log.es$coef[1])+
                         as.numeric(lm.log.es$coef[2])*t.neuejahre)
t.alle.schad.es <- c(d.schaden$pm.es,t.pred.schad.es)
t.alle.fit.es <- c(d.schaden$fit.log.es,t.pred.schad.es)

## Plot Prognose Feuerschaedenentwicklung
plot(t.all.jahre, t.alle.schad.fs, col = 'blue', lwd = 1,
     xlab="Jahr", ylab="Schaden in Promille der Versicherungssumme",
     main="Feuerschadenentwicklung mit logarithmischem Trend")
lines(t.all.jahre, t.alle.fit.fs, col = 'red', lwd = 1)
preds.fs <- predict(lm.log.fs, newdata = data.frame(x=d.schaden$jahr),
                    interval = 'confidence')
polygon(c(rev(d.schaden$jahr), d.schaden$jahr),
        c(rev(10^preds.fs[ ,3]),10^preds.fs[ ,2]),
        col=adjustcolor("gray80",alpha.f=0.5), border = NA)
lines(d.schaden$jahr, 10^preds.fs[ ,3], lty = 'dashed', col = 'gray80')
lines(d.schaden$jahr, 10^preds.fs[ ,2], lty = 'dashed', col = 'gray80')

## Plot Prognose Elementarschadenentwicklung
plot(t.all.jahre, t.alle.schad.es, col = 'blue', lwd = 1,
     xlab="Jahr", ylab="Schaden in Promille der Versicherungssumme",
     main="Elementarschadenentwicklung mit logarithmischem Trend")
lines(t.all.jahre, t.alle.fit.es, col = 'red', lwd = 1)
## Vertrauensinterval berechnen und ploten
preds.es <- predict(lm.log.es, newdata = data.frame(x=d.schaden$jahr),
                    interval = 'confidence')
polygon(c(rev(d.schaden$jahr), d.schaden$jahr), c(rev(10^preds.es[ ,3]),
                                                  10^preds.es[ ,2]), 
        col=adjustcolor("gray80",alpha.f=0.5), border = NA)
lines(d.schaden$jahr, 10^preds.es[ ,3], lty = 'dashed', col = 'gray80')
lines(d.schaden$jahr, 10^preds.es[ ,2], lty = 'dashed', col = 'gray80')

## Plot Prognose Feuer- und Elementarschadenentwicklung
plot(t.all.jahre, t.alle.schad.es, ylim=c(0,max(t.alle.schad.fs)),
     col=adjustcolor("blue",alpha.f=0.5),lwd=1, pch=20,
     xlab=NA,ylab="Schaden in Promille der Versicherungssumme", cex.lab=0.75,
     tck=-.01, cex.axis=0.75)
title(main="Geschätzte Feuer- und Elementarschadenentwicklung der GVZ",
      cex.main=0.75)
points(t.all.jahre, t.alle.schad.fs,
       col =adjustcolor("red", alpha.f=0.5),lwd=1,pch=20)
lines(t.all.jahre, t.alle.fit.fs, col = 'red', lwd = 1)
polygon(c(rev(d.schaden$jahr), d.schaden$jahr),
        c(rev(10^preds.fs[ ,3]),10^preds.fs[ ,2]), 
        col=adjustcolor("gray80",alpha.f=0.5), border = NA)
lines(d.schaden$jahr, 10^preds.fs[ ,3], lty = 'dashed', col = 'gray80')
lines(d.schaden$jahr, 10^preds.fs[ ,2], lty = 'dashed', col = 'gray80')
lines(t.all.jahre, t.alle.fit.es, col = 'blue', lwd = 1)
polygon(c(rev(d.schaden$jahr), d.schaden$jahr),
        c(rev(10^preds.es[ ,3]),10^preds.es[ ,2]),
        col=adjustcolor("gray80",alpha.f=0.5), border = NA)
lines(d.schaden$jahr, 10^preds.es[ ,3], lty = 'dashed', col = 'gray80')
lines(d.schaden$jahr, 10^preds.es[ ,2], lty = 'dashed', col = 'gray80')
legend(2020, 0.23,
# legend(2.02e+03, 2.3e-01,
       title = "Lineare Regression (Stand: Jan 2016)",
       c("log-fit Feuerschaden","log-fit Elementarschaden"),
       lty=c(1,1),
       lwd=c(2.5,2.5),col=c("red","blue"),
       bty='n', cex=.7)

## Grafik "Trend der Feuer- und Elementarschaeden"
## Soll die Grafik im Excel/Powerpoint ersetzen
require(ggplot2)
require(reshape2)
require (grid)
## Preparing the barplot

## Show only years from 1975
ind <- which(d.schaden$jahr>=1975)
df <- d.schaden[ind,]

dfm <- melt(df[,c('jahr','pm.fs','pm.es')],id.vars = 1)

barplot <- ggplot(dfm,aes(x = jahr,y = value)) + 
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("salmon2", "lightsteelblue")) +
  geom_line(mapping=aes(x = df$jahr, y = df$fit.log.fs),
            colour = "red2", size = 2) + 
  geom_line(aes(x = df$jahr, y = df$fit.log.es),
            colour = "royalblue3", size = 2) +
  scale_x_continuous(expand = c(0.005, 0.005), # Reducing the whitespace
                     breaks = seq(from=min(df$jahr), to=max(df$jahr), by=5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25)) +
  ylab("Schaden in Promille der Versicherungssumme") +
  ggtitle('Trends der Feuer- und Elementarschäden\n(in Relation zur Versicherungssumme)') +
  
  ## theme_bw() will get rid of the background
  ## further themes e.g. theme_minimal(), theme_classic() or with library(ggthemes)
  theme_bw(base_size = 12) + #, base_family = "Helvetica") +
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust=0),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, vjust=1.1),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour="gray"),
        panel.grid.major.y=element_line(colour="gray"),
        ## Hide all the vertical gridlines
        panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(),
        legend.position="none", # No legend
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        plot.margin=unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")
barplot

