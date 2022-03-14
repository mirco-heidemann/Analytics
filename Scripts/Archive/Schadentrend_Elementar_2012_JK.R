##----------------------------------------------------------------------
## Schadentrend fuer Elementar-Schaeden
##----------------------------------------------------------------------

setwd('C:/data/private/doerte/Schadentrend_2012')

data.path <- 'data'
data.file <- paste(data.path, '/ESchaeden_1950-2012.csv', sep = '')
schaeden <- read.csv2(data.file, stringsAsFactor = FALSE)

## reduce to data since 1960
ind <- which(schaeden$Jahr > 1959)
if (length(ind) > 0) schaeden <- schaeden[ind,]

pm <- as.numeric(schaeden$ESchaeden) / as.numeric(schaeden$Verswert)
schaeden <- cbind(schaeden, pm = pm)
lm.lin <- lm(schaeden$pm ~ schaeden$Jahr)
lm.log <- lm(log10(schaeden$pm) ~ schaeden$Jahr)
lm.sqrt <- lm(sqrt(schaeden$pm) ~ schaeden$Jahr)

schaeden <- cbind(schaeden, fit.lin = fitted(lm.lin),
                  fit.log = 10^fitted(lm.log), fit.sqrt = (fitted(lm.sqrt)^2))

ps.file <- paste('pdf/Eschaeden_lin.ps', sep = '')
postscript(ps.file, paper = 'a4', horizontal = TRUE)
par(mfrow = c(2, 2))
plot(lm.lin)
par(mfrow = c(1, 1))
plot(schaeden$Jahr, schaeden$pm, col = 'blue', lwd = 3)
lines(schaeden$Jahr, schaeden$fit.lin, col = 'red', lwd = 2)
dev.off()
## convert PostScript to PDF
system(paste(Sys.getenv("COMSPEC"),"/c ps2pdf", ps.file))
file.remove(ps.file)

ps.file <- paste('pdf/Eschaeden_log.ps', sep = '')
postscript(ps.file, paper = 'a4', horizontal = TRUE)
par(mfrow = c(2, 2))
plot(lm.log)
par(mfrow = c(1, 1))
plot(schaeden$Jahr, schaeden$pm, col = 'blue', lwd = 3)
lines(schaeden$Jahr, schaeden$fit.log, col = 'red', lwd = 2)
dev.off()
## convert PostScript to PDF
system(paste(Sys.getenv("COMSPEC"),"/c ps2pdf", ps.file))
file.remove(ps.file)

ps.file <- paste('pdf/Eschaeden_sqrt.ps', sep = '')
postscript(ps.file, paper = 'a4', horizontal = TRUE)
par(mfrow = c(2, 2))
plot(lm.sqrt)
par(mfrow = c(1, 1))
plot(schaeden$Jahr, schaeden$pm, col = 'blue', lwd = 3)
lines(schaeden$Jahr, schaeden$fit.sqrt, col = 'red', lwd = 2)
dev.off()
## convert PostScript to PDF
system(paste(Sys.getenv("COMSPEC"),"/c ps2pdf", ps.file))
file.remove(ps.file)

file.out <- 'data/ESchaeden_1960-2012_fit.csv'
write.csv(schaeden, file = file.out, row.names = FALSE)

## quit R
##q('no')
