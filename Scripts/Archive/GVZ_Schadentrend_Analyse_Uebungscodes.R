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
library(wq)
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
df.schaden <- data.frame(feuerschaden[,1], feuerschaden.anteil, elementarschaden.anteil)
names(df.schaden) <- c('Jahr', 'Feuerschaeden', 'Elementarschaeden')
df.schaden$Jahr <- as.numeric(df.schaden$Jahr)
df.schaden$Feuerschaeden <- as.numeric(df.schaden$Feuerschaeden)
df.schaden$Elementarschaeden <- as.numeric(df.schaden$Elementarschaeden)

# #**********************************************************************************************************
# # Analyse der Daten (auf Normalverteilung): Histogramm, Quantil-Quantil Plot und Shapiro-Wilk Test
# # 1. Feuer
# hist(df.schaden$Feuerschaeden,prob=T,ylim=c(0,12),xlim=c(0,0.3),col="red")
# lines(density(df.schaden$Feuerschaeden),lwd=2)
# mu.f<-mean(df.schaden$Feuerschaeden)
# sigma.f<-sd(df.schaden$Feuerschaeden)
# x<-seq(0,0.3,length=1000)
# y<-dnorm(x,mu.f,sigma.f)
# lines(x,y,lwd=2,col="blue")
# 
# # Quantil-Quantil Plot und Shapiro-Wilk Normality Test
# # Der Shapiro-Wilk-Test ist ein statistischer Signifikanztest, der die Hypothese überprüft, 
# # dass die zugrunde liegende Grundgesamtheit einer Stichprobe normalverteilt ist (Null-Hypothese).
# # - Je näher die Teststatistik (Wert "W") an 1 liegt, desto "normalverteilter" sind die Daten.
# # - Ist der p-Wert (Wahrscheinlichkeit) grösser als das Signifikanzniveau (eg. 0.05 bei 5%), 
# #   sind die Daten Normalverteilt --> Die Null-Hypothese kann nicht verworfen werden
# qqnorm(df.schaden$Feuerschaeden)
# abline(mu.f,sigma.f)
# shapiro.test(df.schaden$Feuerschaeden)
#             
# # 2. Elementar
# hist(df.schaden$Elementarschaeden,prob=T,ylim=c(0,30),xlim=c(-0.05,0.25),col="red")
# lines(density(df.schaden$Elementarschaeden),lwd=2)
# mu.e<-mean(df.schaden$Elementarschaeden)
# sigma.e<-sd(df.schaden$Elementarschaeden)
# x<-seq(-0.1,0.3,length=1000)
# y<-dnorm(x,mu.e,sigma.e)
# lines(x,y,lwd=2,col="blue")
# 
# qqnorm(df.schaden$Elementarschaeden)
# abline(mu.f,sigma.f)
# shapiro.test(df.schaden$Elementarschaeden)
# #**********************************************************************************************************

#**********************************************************************************************************
# Nonparametric test: Mann-Kendall --> Elementarschaeden
# -------------------------------------------------------
# Since the data Elementarschaeden do not conform to a normal distribution, the Mann-Kendall test can be
# applied. This test evaluates whether "Elementarschaeden" values tend to increase or decrease over time
# through what is essentially a nonparametric form of monontonic trend regression analysis.
# 
# The Mann-Kendall test analyzes the sign of the difference between later-measured data
# and earlier-measured data. Each later-measured value is compared to all values measured
# earlier, resulting in a total of n(n-1)/2 possible pairs of data, where n is the total number
# of observations (In 2012 --> 62 for Elementarschaeden and Feuerschaeden). Missing values are allowed and 
# the data do not need to conform to any particular distribution. The Mann-Kendall test assumes that a 
# value can always be declared less than, greater than, or equal to another value; that data are independent; 
# and that the distribution of data remain constant in either the original units or transformed
# units (Helsel and Hirsch 1992). Because the Mann-Kendall test statistics are invariant to
# transformations such as logs (i.e., the test statistics will be the same value for both raw and
# log-transformed data), the Mann-Kendall test is applicable in many situations.
# 
# To perform a Mann-Kendall test, compute the difference between the later-measured
# value and all earlier-measured values, (yj-yi), where j>i, and assign the integer value of 1,
# 0, or -1 to positive differences, no differences, and negative differences, respectively. The
# test statistic, S, is then computed as the sum of the integers.  
# When the test statistic S is a large positive number, later-measured values tend to be larger than earlier
# values and an upward trend is indicated. When S is a large negative number, later values
# tend to be smaller than earlier values and a downward trend is indicated. When the
# absolute value of S is small, no trend is indicated.
# 
# The test statistic tau can be computed as: tau = S/n(n - 1)/2
# which has a range of -1 to +1 and is analogous to the correlation coefficient in regression
# analysis.
# The null hypothesis of no trend is rejected when S and tau are significantly different from zero 
# (and: the smaller the p-Value, the more speak against the null hypothesis)
# If a significant trend is found, the rate of change can be calculated using the 
# Sen slope estimator (Helsel and Hirsch 1992): ??1 = median (yj - yi)/(xj - xi)
# 
# for all i < j and i = 1, 2, ., n-1 and j = 2, 3,., n; in other words, computing the slope for
# all pairs of data that were used to compute S. The median of those slopes is the Sen slope
# estimator --> trend slope estimator.
#**********************************************************************************************************

# Mann-Kendall Test mit dem Package "kendall"
# ----------------------------------------------------------------------------------------------------------
# tau:  Kendall's tau statistic, tau=S/D => tau=S/n(n-1)/2 => estimated slope of the trend
#       Stärke eines monotonen Trends in der Datenreihe (1: monoton steigender Trend, 
#       -1: monoton fallender Trend, 0: kein Trend)
# sl:   two-sided p-value => Signifikanzniveau (p-Wert) für den jeweiligen tau-Wert
# S:    Kendall Score 
# D:    denominator 
# varS: variance of S

stat.value.vers1<-MannKendall(df.schaden$Elementarschaeden)

# Mann-Kendall Test mit dem Package "wq". inklusive "Sen Slope estimator"
# ----------------------------------------------------------------------------------------------------------
# Applies Kendall's tau test for the significance of a monotonic time series trend (Mann 1945). Also
# calculates the Sen slope as an estimate of this trend.

# Usage:
# mannKen(x, plot = FALSE, type = c("slope", "pct", "tau"), order = FALSE)

# Arguments:
# x:      A time series vector or matrix
# plot:   Should the trends be plotted when x is of class "mts"?
# type:   Type of trend to be plotted
# order:  Should the plotted trends be ordered by size?

# Details:
# The Sen slope (alternately, Theil or Theil-Sen slope)-the median slope joining all pairs of observations-
# is expressed both by quantity per unit time and percent of the mean quantity per unit time. The
# fraction of missing slopes involving the first and last fifths of the data are provided so that the appropriateness
# of the slope estimate can be assessed and results flagged. Other results are used for
# further analysis by other functions.
# If plot = TRUE, then either the Sen slope, the Sen slope as a percent of the mean, or Kendall's tau
# are plotted, along with an indication of p-value and fraction of missing slopes joining the first and
# last fifths of the data. Only the last two types make sense when the variables in x have different
# units.

# Value:
# sen.slope:      Sen slope
# sen.slope.pct:  Sen slope as percent of mean
# p.value:        Significance of slope
# S:              Kendall's S
# varS:           Variance of S
# miss:           Fraction of missing slopes connecting first and last fifths of x

# The varS calculation is based on kensen in the USGS library for S-PLUS (Slack et al. 2003).
# ----------------------------------------------------------------------------------------------------------

ts.elementarschaeden<-as.ts(df.schaden$Elementarschaeden)
stat.value.vers2<-mannKen(ts.elementarschaeden)

ts.feuerschaeden<-as.ts(df.schaden$Feuerschaeden)
stat.value.vers2.feuer<-mannKen(ts.feuerschaeden)

# Determine seasonal trends with the function "seasonTrend" ("wq-Package")
# ----------------------------------------------------------------------------------------------------------
# Finds the trend for each season of a time series (matrix or vector) and indicates statistical significance.

# Usage:
# seasonTrend(x, first, last, type = c("slope", "slope.pct"),
#             method = c("mk", "lin"), plot = FALSE, xlab = NULL,
#             ylab = NULL, miss = FALSE, legend = FALSE, ...)

# Arguments:
# x:      Time series vector, or time series matrix with column names.
# first:  First year of desired time interval. Will be adjusted if less than start of series.
# last:   Last year of desired time interval. Will be adjusted if more than end of series.
# type:   Type of trend, either in original units per year, or percent per year.
# method: 'mk' for Mann-Kendall (Theil-Sen slope), and 'lin' for linear regression (linear slope).
# plot:   If TRUE, a plot is generated; otherwise the results are listed.
# xlab:   Optional x-axis label.
# ylab:   Optional y-axis label.
# miss:   If TRUE, trends with insufficient data.
# legend: If TRUE, a legend is included.

# Details:
# The slope estimate and its significance are calculated for each season and time series. If type = "slope.pct",
# the slopes are multiplied by 100 and divided by the overall mean (not the median, which can be zero
# even in cases where a trend estimate is useful). 
# If method = 'mk'. the Theil-Sen slope is calculated with the Mann-Kendall test of significance. 
# Otherwise, linear regression is used to determine the slope and significance.
# If plot = TRUE, each time series is represented by a box plot showing the trend for each season. The
# fill colour of the box indicates whether the trend is significant or not (the legend is optional). When
# method = 'mk', the proportion of slopes joining the first and last fifths of the data is calculated. If
# this value is 0.5 or more, the corresponding trends can be omitted by setting miss = TRUE; the trend
# results may not be a good representation of the entire period and a different time window should be
# considered.
# Parameters can be passed to the plotting function, in particular, to facet_wrap in ggplot2. The
# most useful parameters here are ncol (or nrow), which determines the number of columns (or rows)
# of plots, and scales, which can be set to "free_x" to allow the x-axis to change for each time
# series.

# Value:
# A data frame with the following fields:
# trend:    Theil-Sen slope in original units per year, or percent per year.
# p:        p-value for the trend according to the Mann-Kendall test.
# missing:  Proportion of slopes joining first and last fifths of the data that are missing.
# season:   Season number.
# tsname:   Name of time series.
# ----------------------------------------------------------------------------------------------------------

trend.elementar<-seasonTrend(ts.elementarschaeden, type="slope", method="mk", plot=TRUE, legend=TRUE)



# Manuelle Berechnung der Mann-Kendall Statistik, inklusive des paarweisen Kendall-Theil Slopes
# ---------------------------------------------------------------------------------------------

# 1. Version
kt.mat <- 
  function(x,y,z){ 
    for(i in 1:length(x)){for(j in 1:length(y)){z[i,j]<-(y[j]-y[i])/(x[j]-x[i])}} 
    return(z)} 


kt.slope <- 
  function(x,y,z,s){ 
    count<-0 
    for(i in 1:length(x)){for(j in 1:length(y)){ 
      if(j >= i+1) { 
        count<-count+1 
        s[count]<-z[i,j]} 
    }} 
    print(count) 
    return(s)} 

x<- df.schaden$Jahr
y<- df.schaden$Elementarschaeden

z<-matrix(0:0,length(x),length(y)) 
z<-kt.mat(x,y,z) 
z 

s<-c(1:(length(x)*(length(x)-1)/2)) 
s<-kt.slope(x,y,z,s) 
s 
slope=median(s) 
intercept=median(y)-slope*median(x) 
cbind(slope,intercept) 
plot(x,y) 
abline(intercept,slope) 

# 2. Version, inkl. Vergleich mit dem "Fitting Linear Model" (lm)
#----------------------------------------------------------------
x <- df.schaden$Jahr[1]:df.schaden$Jahr[as.numeric(length(df.schaden$Jahr))]
y <- df.schaden$Elementarschaeden

# Fitting Linear Model
mod <- lm(y ~ x) 

# Mann-Kendall
kt <- function(x, y){ 
  yy <- outer(y, y, "-") 
  xx <- outer(x, x, "-") 
  z  <- yy / xx 
  s  <- z[lower.tri(z)] 
  slope <- median(s) 
  intercept <- median(y) - slope * median(x) 
  list(intercept = intercept, slope = slope) 
} 

ind1 <- which.min(x) 
ind2 <- which.max(x) 

w <- kt(x, y) 
a <- w[["intercept"]] 
b <- w[["slope"]] 

x.ends <- c(x[ind1], x[ind2]) 
y.ends.kt <- a + b * x.ends 

a <- coef(mod)[1] 
b <- coef(mod)[2] 
y.ends.lm <- a + b * x.ends 

plot(y ~ x) 
lines(x.ends, y.ends.kt, col = "blue") 
lines(x.ends, y.ends.lm, col = "red")

