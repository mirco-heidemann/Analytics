##----------------------------------------------------------------------
## Schadentrend fuer Feuer-Schaeden
##----------------------------------------------------------------------

setwd('C:/data/private/doerte/Schadentrend_2012')

data.path <- 'data'
data.file <- paste(data.path, '/FSchaeden_1950-2012.csv', sep = '')
schaeden <- read.csv2(data.file, stringsAsFactor = FALSE)

## reduce to data since 1960
ind <- which(schaeden$Jahr > 1959)
if (length(ind) > 0) schaeden <- schaeden[ind,]

pm <- as.numeric(schaeden$FSchaeden) / as.numeric(schaeden$Verswert)
schaeden <- cbind(schaeden, pm = pm)
lm.lin <- lm(schaeden$pm ~ schaeden$Jahr)

schaeden <- cbind(schaeden, pm.fit = fitted(lm.lin))

ps.file <- paste('pdf/FSchaeden_lin.ps', sep = '')

postscript(ps.file, paper = 'a4', horizontal = TRUE)
par(mfrow = c(2, 2))
plot(lm.lin)
par(mfrow = c(1, 1))
plot(schaeden$Jahr, schaeden$pm, col = 'blue', lwd = 3)
lines(schaeden$Jahr, schaeden$pm.fit, col = 'red', lwd = 2)

dev.off()

## convert PostScript to PDF
system(paste(Sys.getenv("COMSPEC"),"/c ps2pdf14", ps.file))
file.remove(ps.file)

file.out <- 'data/FSchaeden_1960-2012_fit.csv'
write.csv(schaeden, file = file.out, row.names = FALSE)

## quit R
q('no')
