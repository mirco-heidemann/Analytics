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
# df.schaden <- data.frame(feuerschaden[,1], feuerschaden[,6], elementarschaden[,6])
df.schaden <- data.frame(feuerschaden[,1], feuerschaden.anteil, elementarschaden.anteil)
names(df.schaden) <- c('Jahr', 'Feuerschaeden', 'Elementarschaeden')
df.schaden$Jahr <- as.numeric(df.schaden$Jahr)
df.schaden$Feuerschaeden <- as.numeric(df.schaden$Feuerschaeden)
df.schaden$Elementarschaeden <- as.numeric(df.schaden$Elementarschaeden)

#**********************************************************************************************************
# Analyse der Daten (auf Normalverteilung): Histogramm, Quantil-Quantil Plot und Shapiro-Wilk Test
# 1. Feuer
hist(df.schaden$Feuerschaeden,prob=T,ylim=c(0,12),xlim=c(0,0.3),col="red")
lines(density(df.schaden$Feuerschaeden),lwd=2)
mu.f<-mean(df.schaden$Feuerschaeden)
sigma.f<-sd(df.schaden$Feuerschaeden)
x<-seq(0,0.3,length=1000)
y<-dnorm(x,mu.f,sigma.f)
lines(x,y,lwd=2,col="blue")

# Quantil-Quantil Plot und Shapiro-Wilk Normality Test
# Der Shapiro-Wilk-Test ist ein statistischer Signifikanztest, der die Hypothese überprüft, 
# dass die zugrunde liegende Grundgesamtheit einer Stichprobe normalverteilt ist (Null-Hypothese).
# - Je näher die Teststatistik (Wert "W") an 1 liegt, desto "normalverteilter" sind die Daten.
# - Ist der p-Wert (Wahrscheinlichkeit) grösser als das Signifikanzniveau (eg. 0.05 bei 5%), 
#   sind die Daten Normalverteilt --> Die Null-Hypothese kann nicht verworfen werden
qqnorm(df.schaden$Feuerschaeden)
abline(mu.f,sigma.f)
shapiro.test(df.schaden$Feuerschaeden)
            
# 2. Elementar
hist(df.schaden$Elementarschaeden,prob=T,ylim=c(0,30),xlim=c(-0.05,0.25),col="red")
lines(density(df.schaden$Elementarschaeden),lwd=2)
mu.e<-mean(df.schaden$Elementarschaeden)
sigma.e<-sd(df.schaden$Elementarschaeden)
x<-seq(-0.1,0.3,length=1000)
y<-dnorm(x,mu.e,sigma.e)
lines(x,y,lwd=2,col="blue")

qqnorm(df.schaden$Elementarschaeden)
abline(mu.f,sigma.f)
shapiro.test(df.schaden$Elementarschaeden)
#**********************************************************************************************************

# Plot Jahresschäden Feuer und Elementar
schaden.plot <- ggplot(df.schaden, aes(df.schaden$Jahr)) + 
             geom_point(aes(y = df.schaden$Feuerschaeden),colour = "red4", size = 1) +
             geom_point(aes(y = df.schaden$Elementarschaeden),colour = "royalblue", size = 1) +
             geom_line(aes(y = df.schaden$Feuerschaeden, colour = "Feuerschaeden"), size = 0.6) +
             geom_line(aes(y = df.schaden$Elementarschaeden, colour = "Elementarschaeden"), size = 0.6) +
             scale_color_manual(values=c("cornflowerblue", "#CC6666"),labels=c("Elementar","Feuer"),
                                name  = "Schäden in Promille\nVersicherungssumme") +

# Berechnen der Trendlinien mit einer LOESS-Regression (lokale polynomiale Regression):
#**********************************************************************************************************
# --> 'span':   Bestimmt den Grad der Glättung. Die Spannweite als Anteil der Punkte, welche für die 
#               Berechnung des neuen (lokalen) Trendpunktes verwendet werden. 
#               Je grösser der Wert, desto geglätteter wird die Kurve (glattere Interpolation).
#               Daraus ergibt sich ein Trade-Off bei der Wahl des span-Wertes: 
#               Kleiner Wert = genauerer Verlauf des Trendes aber grosse Variabilität.
#               Grosser Wert = Fehler des Trendverlaufes wird grösser, dafür ist die Variabilität kleiner.
#               
#               Der 'span' Parameter soll so gewählt, dass die LOESS-Kurve dem Verlauf des gleitenden
#               Mittelwertes angepasst ist, ohne jedoch den jährlichen Schwankungen zu folgen.
#               Beste Wahl durch "trial and error" ermitteln...
#
# --> 'degree': Grad des lokalen angepassten Polynoms zur Trendschätzung (Grad der lokalen Polynome).
#**********************************************************************************************************

             stat_smooth(aes(y = df.schaden$Feuerschaeden), 
                         method = "loess", formula = y ~ (x), span =0.9, degree=1, se=FALSE, level=0.95,
                         na.rm = FALSE, color="red4", size=1, fullrange=TRUE) +
             stat_smooth(aes(y = df.schaden$Elementarschaeden),
                         method = "loess", formula = y ~ (x), span =0.9, degree=1, se=FALSE, level = 0.95,
                         na.rm = FALSE, color="royalblue4", size=1, fullrange=TRUE) +
            
#             geom_text(aes(y = df.schaden$Elementarschaeden, label = df.schaden$Elementarschaeden), 
#                       size = 3, col = "grey40") +
#             geom_text(aes(y = df.schaden$Feuerschaeden, label = df.schaden$Feuerschaeden), 
#                         size = 3, col = "grey40") +
            scale_x_continuous(breaks=seq(min(df.schaden$Jahr), max(df.schaden$Jahr), 10)) +           
            opts(title = paste('Trend der Feuer- und Elementarschäden im Kanton Zürich'), 
                 plot.title=theme_text(colour='black',angle =0,size = 14,hjust=0.5,vjust=2,face ='bold')) +
            ylab("") + # ylab("Schäden in Promille\nVersicherungssumme") + 
            xlab("") + # xlab("Jahre") +                                                                            
            opts(axis.text.x = theme_text(colour = 'grey40', angle = 45, size = 10, #hjust = -3, vjust = 7,
                                          face = 'plain')) +
            opts(axis.text.y = theme_text(colour = 'grey40', size = 10, face = 'plain')) +
            opts(axis.title.x = theme_text(colour = 'black', size = 10,hjust = 0.5, vjust = 0, face = 'bold')) +
            opts(axis.title.y = theme_text(colour = 'black', angle = 0, size = 10, hjust = 0, vjust = 0.5, 
                                            face = 'bold')) +
            opts(legend.text = theme_text(colour = 'black', angle = 0, size = 10, hjust = 3, vjust = 3, 
                                          face = 'plain')) +
            opts(legend.key.size = unit(0.8, "cm")) +
            opts(legend.key.width = unit(1.2, "cm")) +
            opts(legend.title = theme_text(colour = 'black', angle = 0, size = 10, hjust = 0,
                                           vjust = 0, face = 'bold')) +
            opts(legend.position = 'left')

# Fussnote / Text hinzufügen
final.plot <- arrangeGrob(schaden.plot, sub = textGrob("GVZ, Bereich Naturgefahren\n  Mirco Heidemann", x = 0, 
                                                hjust = -0.1, vjust = 0.1, 
                                                gp = gpar(fontface = "italic", fontsize = 8)))