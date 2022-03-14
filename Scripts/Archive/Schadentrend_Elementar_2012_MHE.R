##----------------------------------------------------------------------
## Schadentrend fuer Elementar-Schaeden
##----------------------------------------------------------------------
library(XLConnect)
setwd('D:/USERS/mheidemann/Statistical_Computing_R/ES_und_Feuerjahresschaeden_Entwicklung/')
# Einlesen der Jahresschäden und speichern in einem Data Frame
schadenentwicklung.excel <- 'Schadenentwicklung_1960_2012.xlsx'
elementarschaden <- readWorksheet(loadWorkbook(schadenentwicklung.excel),sheet=1)
feuerschaden <- readWorksheet(loadWorkbook(schadenentwicklung.excel),sheet=2)
names(elementarschaden)<-c('Jahr', 'Index', 'Verswert', 'ESchaeden', 'ESchaeden_index', 'ESchaeden_anteil')
names(feuerschaden)<-c('Jahr', 'Index', 'Verswert', 'FSchaeden', 'FSchaeden_index', 'FSchaeden_anteil')

#**********************************************************************************************************
# Indem nur die Anzahl der beschädigten Gebäude im Verhältnis zum Gesamtgebäudebestand betrachtet wird,
# lässt sich die Schadenentwicklung vollständig von Wert- bzw. Preisentwicklungen abkoppeln.
#**********************************************************************************************************
# feuerschaden.anteil<-feuerschaden[,4]/feuerschaden[,3]
# elementarschaden.anteil<-elementarschaden[,4]/elementarschaden[,3]

# 1. Elementarschäden
#**********************************************************************************************************
## reduce to data since 1960
ind <- which(elementarschaden$Jahr > 1959)
if (length(ind) > 0) elementarschaden <- elementarschaden[ind,]

pm <- as.numeric(elementarschaden$ESchaeden) / as.numeric(elementarschaden$Verswert)
elementarschaden <- cbind(elementarschaden, pm = pm)
lm.lin <- lm(elementarschaden$pm ~ elementarschaden$Jahr)
lm.log <- lm(log10(elementarschaden$pm) ~ elementarschaden$Jahr)
lm.sqrt <- lm(sqrt(elementarschaden$pm) ~ elementarschaden$Jahr)

elementarschaden <- cbind(elementarschaden, fit.lin = fitted(lm.lin),
                  fit.log = 10^fitted(lm.log), fit.sqrt = (fitted(lm.sqrt)^2))

save.path <- 
  paste('D:/USERS/mheidemann/Statistical_Computing_R/ES_und_Feuerjahresschaeden_Entwicklung/R_Ausgabe_Trendanalyse/', 
                   sep = "", collapse = NULL)
filename.pdf <- paste('Eschaeden_lin.pdf', sep = '')
pdf(file=paste(save.path, filename.pdf), paper = 'a4r')
par(mfrow = c(2, 2))
plot(lm.lin)
par(mfrow = c(1, 1))
plot(elementarschaden$Jahr, elementarschaden$pm, col = 'blue', lwd = 3)
lines(elementarschaden$Jahr, elementarschaden$fit.lin, col = 'red', lwd = 2)
dev.off()

filename.pdf <- paste('Eschaeden_log.pdf', sep = '')
pdf(file=paste(save.path, filename.pdf), paper = 'a4r')
par(mfrow = c(2, 2))
plot(lm.log)
par(mfrow = c(1, 1))
plot(elementarschaden$Jahr, elementarschaden$pm, col = 'blue', lwd = 3)
lines(elementarschaden$Jahr, elementarschaden$fit.log, col = 'red', lwd = 2)
dev.off()

filename.pdf <- paste('Eschaeden_sqrt.pdf', sep = '')
pdf(file=paste(save.path, filename.pdf), paper = 'a4r')
par(mfrow = c(2, 2))
plot(lm.sqrt)
par(mfrow = c(1, 1))
plot(elementarschaden$Jahr, elementarschaden$pm, col = 'blue', lwd = 3)
lines(elementarschaden$Jahr, elementarschaden$fit.sqrt, col = 'red', lwd = 2)
dev.off()

file.out <- 'R_Ausgabe_Trendanalyse/ESchaeden_1960-2012_fit.csv'
write.csv(elementarschaden, file = file.out, row.names = FALSE)

# 1. Feuerschäden
#**********************************************************************************************************
## reduce to data since 1960
ind <- which(feuerschaden$Jahr > 1959)
if (length(ind) > 0) feuerschaden <- feuerschaden[ind,]

pm <- as.numeric(feuerschaden$FSchaeden) / as.numeric(feuerschaden$Verswert)
feuerschaden <- cbind(feuerschaden, pm = pm)
lm.lin <- lm(feuerschaden$pm ~ feuerschaden$Jahr)
lm.log <- lm(log10(feuerschaden$pm) ~ feuerschaden$Jahr)
lm.sqrt <- lm(sqrt(feuerschaden$pm) ~ feuerschaden$Jahr)

feuerschaden <- cbind(feuerschaden, fit.lin = fitted(lm.lin),
                          fit.log = 10^fitted(lm.log), fit.sqrt = (fitted(lm.sqrt)^2))

save.path <- 
  paste('D:/USERS/mheidemann/Statistical_Computing_R/ES_und_Feuerjahresschaeden_Entwicklung/R_Ausgabe_Trendanalyse/', 
        sep = "", collapse = NULL)
filename.pdf <- paste('Fschaeden_lin.pdf', sep = '')
pdf(file=paste(save.path, filename.pdf), paper = 'a4r')
par(mfrow = c(2, 2))
plot(lm.lin)
par(mfrow = c(1, 1))
plot(feuerschaden$Jahr, feuerschaden$pm, col = 'blue', lwd = 3)
lines(feuerschaden$Jahr, feuerschaden$fit.lin, col = 'red', lwd = 2)
dev.off()

# filename.pdf <- paste('Fschaeden_log.pdf', sep = '')
# pdf(file=paste(save.path, filename.pdf), paper = 'a4r')
# par(mfrow = c(2, 2))
# plot(lm.log)
# par(mfrow = c(1, 1))
# plot(feuerschaden$Jahr, feuerschaden$pm, col = 'blue', lwd = 3)
# lines(feuerschaden$Jahr, feuerschaden$fit.log, col = 'red', lwd = 2)
# dev.off()
# 
# filename.pdf <- paste('Fschaeden_sqrt.pdf', sep = '')
# pdf(file=paste(save.path, filename.pdf), paper = 'a4r')
# par(mfrow = c(2, 2))
# plot(lm.sqrt)
# par(mfrow = c(1, 1))
# plot(feuerschaden$Jahr, feuerschaden$pm, col = 'blue', lwd = 3)
# lines(feuerschaden$Jahr, feuerschaden$fit.sqrt, col = 'red', lwd = 2)
# dev.off()

file.out <- 'R_Ausgabe_Trendanalyse/FSchaeden_1960-2012_fit.csv'
write.csv(feuerschaden, file = file.out, row.names = FALSE)
