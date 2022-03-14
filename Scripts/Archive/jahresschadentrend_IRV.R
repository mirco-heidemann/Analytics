## ----
## Schadentrend Elementar-und Feuerschaeden aller 18 KGV's, ohne Bern, mit ZH
## Darstellung der Feuer- und Elementarjahresschaeden
##
## Bemerkung: Die Schadenzahlen entsprechen dem SCHADENHAHR, nicht dem
##            dem Berichtsjahr (Lothar ist im Jahre 1999 sichtbar)
##
## April 2018, Mirco Heidemann
## ----

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- ("../data/IRV/")
pth_funct <- ("../R_functions/")
pth_out <- ("./out/")

library(fitdistrplus)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

## csv file irv einlesen
df <- read.csv2(paste0(pth_data, 'irv_jahreschadenstatistik_2017.csv'),
                stringsAsFactors = FALSE) %>%
  mutate(jahr = Jahr,
         vs = as.numeric(versSum),
         vs_trans = as.numeric(vs/5), # secondary axis
         f_schad = as.numeric(feuerschad),
         e_schad = as.numeric(elementarschad)) %>% 
  filter(Jahr > 1959) %>% # gleiche Zeitspanne wie in GVZ Grafik
  dplyr::select(jahr:e_schad)

## Prepare the df data for plotting and analysis
df <- df %>% 
  mutate(loess_fs = predict(loess(f_schad ~ jahr, data = df, span = 0.4)),
         loess_es = predict(loess(e_schad ~ jahr, data = df, span = 0.4)),
         loess_es = ifelse(loess_es < 0, 0, loess_es),
         lgnorm_fs = as.numeric(exp(fitted(lm(log(f_schad)~ jahr)))),
         lgnorm_es = as.numeric(exp(fitted(lm(log(e_schad)~ jahr)))),
         gamma_fs = as.numeric(predict(glm(f_schad ~ jahr,
                                           family = Gamma(link = "log")),
                                       newdata = list(jahr = seq(min(jahr),
                                                                 max(jahr), 1)),
                                       type = "response")),
         gamma_es = as.numeric(predict(glm(e_schad ~ jahr,
                                           family = Gamma(link = "log")),
                                       newdata = list(jahr = seq(min(jahr),
                                                                 max(jahr), 1)),
                                       type = "response")),
         pm_fs = f_schad / vs, # in promille der VersSumme
         pm_es = e_schad /vs, # in promille der VersSumme
         loess_pm_fs = predict(loess(pm_fs ~ jahr, data = df, span = 0.4)),
         loess_pm_es = predict(loess(pm_es ~ jahr, data = df, span = 0.4)),
         lgnorm_pm_fs = as.numeric(exp(fitted(lm(log(pm_fs)~ jahr)))),
         lgnorm_pm_es = as.numeric(exp(fitted(lm(log(pm_es)~ jahr)))),
         gamma_pm_fs = as.numeric(predict(glm(pm_fs ~ jahr,
                                              family = Gamma(link = "log")),
                                          newdata = list(jahr = seq(min(jahr),
                                                                    max(jahr), 1)),
                                          type = "response")),
         gamma_pm_es = as.numeric(predict(glm(pm_es ~ jahr,
                                              family = Gamma(link = "log")),
                                          newdata = list(jahr = seq(min(jahr),
                                                                    max(jahr), 1)),
                                          type = "response")))

## Melt df for ggplot
df_melt <- melt(df, id.vars = "jahr")

## --- Draw a bar plot of vs entwicklung
## set plot theme, theme_bw() will get rid of the background
## further themes e.g. theme_minimal(), theme_classic() or with library(ggthemes)
theme_set(theme_bw(base_size = 12)) #, base_family = "Helvetica") +

bar0 <- ggplot() +
  geom_bar(data = filter(df_melt, variable %in% c("vs")),
           aes(x = jahr, y = value, fill = variable) , stat = "identity",
           position = "dodge",  width = 0.6)+
  theme(axis.text.x = element_text(angle = 45),legend.position = "bottom") +
  scale_fill_manual(guide=FALSE, 
                    values = c("#bcbddc")) +
  scale_x_continuous(expand = c(0.005, 0.005), # Reducing the whitespace
                     breaks = seq(from = min(df$jahr), to = max(df$jahr), by = 5)) +
  ylab("Versicherungskapital [Mrd. CHF]") +
  labs(title = "Entwicklung des Versicherungskapitals aller 18 KGV (ohne Bern)",
       subtitle = "", 
       caption = "Quelle: IRV") + 
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 65, vjust = 0.6),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "gray"),
        panel.grid.major.y = element_line(colour = "gray"),
        ## Hide all the vertical gridlines
        panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", # "none" for no legend
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        aspect.ratio = 6/19,
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")


## --- Draw a bar plot of vs, es, fs
# bar1 <- ggplot() +
#   geom_bar(data = filter(df_melt, variable %in% c("vs_trans", "f_schad", "e_schad")),
#            aes(x = jahr, y = value, fill = variable) , stat = "identity", position = "dodge")+
#   # geom_point(data = filter(df_melt, variable %in% c("variable1", "variable2")),
#   #            aes(x = jahr, y = value,colour = variable)) +
#   # geom_line(data = filter(df_melt, variable %in% c("loess_fs", "loess_es")),
#   #           aes(x = jahr, y = value, colour = variable, group = variable), size = 0.8) +
#   theme(axis.text.x = element_text(angle = 45),legend.position = "bottom") +
#   scale_fill_manual(name = "", 
#                     labels = c("Versicherungskapital",
#                                "Jahresschäden Feuer",
#                                "Jahresschäden Elementar"), 
#                     values = c("#dadaeb", "#fcae91", "#9ecae1"), 
#                     guide = guide_legend(order = 1)) + # set first legend first)) +
#   # legend for loess smoother
#   scale_colour_manual(name = "", 
#                       labels = c("Gleitendes Mittel Feuerschäden",
#                                  "Gleitendes Mittel Elementarschäden"),
#                       values = c("#de2d26", "#2171b5")) +
#   scale_x_continuous(expand = c(0.005, 0.005), # Reducing the whitespace
#                      breaks = seq(from = min(df$jahr), to = max(df$jahr), by = 5)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 750),
#                      sec.axis = sec_axis(~ . *5, name = "Versicherungskapital [Mrd. CHF]")) +
#   ylab("Jahresschäden [Mio.CHF]") +
#   labs(title = "Versicherungskapital und Jahresschäden aller 18 KGV (ohne Bern)", 
#        subtitle = "", 
#        caption = "Quelle: IRV") + 
#   theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 65, vjust = 0.6),
#         axis.title.y = element_text(size = 12, vjust = 1.1),
#         axis.ticks = element_blank(),
#         panel.border = element_rect(colour = "gray"),
#         panel.grid.major.y = element_line(colour = "gray"),
#         ## Hide all the vertical gridlines
#         panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
#         legend.position = "bottom", # "none" for no legend
#         # Removing/Reducing the whitespace surrounding the plot
#         # (requires the grid library)
#         aspect.ratio = 6/19,
#         plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")

## --- Draw a bar plot of es, fs only
bar1 <- ggplot() +
  geom_bar(data = filter(df_melt, variable %in% c("f_schad", "e_schad")),
           aes(x = jahr, y = value, fill = variable) , stat = "identity", position = "dodge")+
  # geom_point(data = filter(df_melt, variable %in% c("variable1", "variable2")),
  #            aes(x = jahr, y = value,colour = variable)) +
  # geom_line(data = filter(df_melt, variable %in% c("loess_fs", "loess_es")),
  #           aes(x = jahr, y = value, colour = variable, group = variable), size = 0.8) +
  theme(axis.text.x = element_text(angle = 45),legend.position = "bottom") +
  scale_fill_manual(name = "", 
                    labels = c("Jahresschäden Feuer",
                               "Jahresschäden Elementar"), 
                    values = c("#fcae91", "#9ecae1")) + 
  # legend for loess smoother
  scale_colour_manual(name = "", 
                      labels = c("Gleitendes Mittel Feuerschäden",
                                 "Gleitendes Mittel Elementarschäden"),
                      values = c("#de2d26", "#2171b5")) +
  scale_x_continuous(expand = c(0.005, 0.005), # Reducing the whitespace
                     breaks = seq(from = min(df$jahr), to = max(df$jahr), by = 5)) +
  ylab("Jahresschäden [Mio.CHF]") +
  labs(title = "Jahresschäden aller 18 KGV (ohne Bern)", 
       subtitle = "", 
       caption = "Quelle: IRV, bearbeitet durch die GVZ") + 
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 65, vjust = 0.6),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "gray"),
        panel.grid.major.y = element_line(colour = "gray"),
        ## Hide all the vertical gridlines
        panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", # "none" for no legend
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        aspect.ratio = 6/19,
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")


## --- Draw a bar plot for elementar- und feuerschaeden  with trend in relation to vs
bar2 <- ggplot() +
  geom_bar(data = filter(df_melt, variable %in% c("pm_fs", "pm_es")),
           aes(x = jahr, y = value, fill = variable) , stat = "identity", position = "dodge") +
  geom_line(data = filter(df_melt, variable %in% c("lgnorm_pm_fs", "gamma_pm_es")),
            aes(x = jahr, y = value, colour = variable, group = variable), size = 1) +
  theme(axis.text.x = element_text(angle = 45),legend.position = "bottom") +
  scale_fill_manual(name = "",
                    labels = c("Jahresschäden Feuer",
                               "Jahresschäden Elementar"),
                    values = c("#fcae91", "#9ecae1"),
                    guide = guide_legend(order = 1)) + # set first legend first) +
  # legend for trend
  scale_colour_manual(name = "", 
                      labels = c("Trendkurve Feuerschäden",
                                 "Trendkurve Elementarschäden"),
                      values = c("#de2d26", "#2171b5")) +
  
  scale_x_continuous(expand = c(0.005, 0.005), # Reducing the whitespace
                     breaks = seq(from = min(df$jahr), to = max(df$jahr), by = 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.6)) +
  ylab("Schaden in Promille des Versicherungskapitals") +
  labs(title = "Trend der Feuer- und Elementarschäden aller 18 KGV nach Schadenjahr",
       subtitle = "(in Relation zum Versicherungskapital)",
       caption = "Quelle: IRV, bearbeitet durch die GVZ") +
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 65, vjust = 0.6),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "gray"),
        panel.grid.major.y = element_line(colour = "gray"),
        ## Hide all the vertical gridlines
        panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", # "none" for no legend
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        aspect.ratio = 6/19,
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")


## --- Diverging barplots
## Standardisierung der jahresschaeden: Transformation der Jahresschaeden auf
## den Erwartungswert Null und die Varianz Eins. Dadurch lassen sich die
## Jahresschäden in Bezug auf ihre Lage charakterisieren.

## Data Preparation
df <- df %>% mutate(f_schad_Z = round((f_schad - mean(f_schad))/sd(f_schad), 2),
                    e_schad_Z = round((e_schad - mean(e_schad))/sd(e_schad), 2),
                    pm_fs_Z = round((pm_fs - mean(pm_fs))/sd(pm_fs), 2),
                    pm_es_Z = round((pm_es - mean(pm_es))/sd(pm_es), 2),
                    ## above / below avg flag
                    f_schadType = ifelse(f_schad_Z < 0, "below", "above"),
                    e_schadType = ifelse(e_schad_Z < 0, "below", "above"),
                    pm_fsType = ifelse(pm_fs_Z < 0, "below", "above"),
                    pm_esType = ifelse(pm_es_Z < 0, "below", "above"),
                    ## LOESS Smoother der standardisierten Jahresschaeden
                    loess_pm_fs_Z = predict(loess(pm_fs_Z ~ jahr, data = df, span = 0.20)),
                    loess_pm_es_Z = predict(loess(pm_es_Z ~ jahr, data = df, span = 0.20)))

## Set base theme
theme_set(theme_bw(base_size = 12))

## Jahresschaeden Feuer in Promille des Versicherungskapitals
bar3 <- ggplot(df, aes(x = jahr, y = pm_fs_Z, label = pm_fs_Z)) + 
  geom_bar(stat = 'identity', aes(fill = pm_fsType), width = .5)  +
  geom_smooth(aes(x = jahr, y = pm_fs_Z, color = 'Gleitendes Mittel'), method = "loess",
              se = FALSE, span =0.75) +
  
  # geom_smooth(method = "lm", se = FALSE, colour = "#67000d") +
  # geom_smooth(colour = "red", se = F,
  #             method = "glm",
  #             formula = y~ns(x,4), # library(splines)
  #             family = gaussian(link = "log"),
  #             lwd = 0.7) +
  
  scale_fill_manual(name = "", 
                    labels = c("Über dem langjährigen Schaden Mittel",
                               "Unter dem langjährigen Schaden Mittel"), 
                    values = c("above" = "#de2d26", "below" = "#fcae91"),
                    guide = guide_legend(order = 1))+
  # legend for geom_smooth
  scale_colour_manual(name = "", values = c('Gleitendes Mittel' = "#a50f15")) + 
  scale_x_continuous(expand = c(0.005, 0.005), # Reducing the whitespace
                     breaks = seq(from = min(df$jahr), to = max(df$jahr), by = 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(floor(min(df$pm_fs_Z)),
                                                  ceiling(max(df$pm_fs_Z)))) +
  ylab("") +
  labs(title = "Abweichung vom Jahresschadenmittel Feuer aller 18 KGV (ohne Bern)",
       subtitle = "Standardisierte Schäden im Verhältnis zum Versicherungskapital",
       caption = "Quelle: IRV, bearbeitet durch die GVZ") +
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 65, vjust = 0.6),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "gray"),
        panel.grid.major.y = element_line(colour = "gray"),
        ## Hide all the vertical gridlines
        panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", # "none" for no legend
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        aspect.ratio = 6/19,
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")


## Jahresschaeden Elementar in Promille des Versicherungskapitals
bar4 <- ggplot(df, aes(x = jahr, y = pm_es_Z, label = pm_es_Z)) + 
  geom_bar(stat = 'identity', aes(fill = pm_esType), width = .5)  +
  geom_smooth(aes(x = jahr, y = pm_es_Z, color = 'Gleitendes Mittel'), method = "loess",
              se = FALSE, span= 0.75) +
  
  # geom_smooth(method = "lm", se = FALSE, colour = "#08306b") +
  # geom_smooth(colour = "blue", se = F,
  #             method = "glm",
  #             formula = y~ns(x,2),
  #             family = gaussian(link = "log"),
  #             show.legend = FALSE,lwd = 0.7) +
  
  scale_fill_manual(name = "", 
                    labels = c("Über dem langjährigen Schaden Mittel",
                               "Unter dem langjährigen Schaden Mittel"), 
                    values = c("above" = "#2171b5", "below" = "#9ecae1"),
                    guide = guide_legend(order = 1)) + # set first legend first
  # legend for geom_smooth
  scale_colour_manual(name = "", values = c('Gleitendes Mittel' = "#08519c")) + 
  scale_x_continuous(expand = c(0.005, 0.005), # Reducing the whitespace
                     breaks = seq(from = min(df$jahr), to = max(df$jahr), by = 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(floor(min(df$pm_es_Z)),
                                                  ceiling(max(df$pm_es_Z)))) +
  ylab("") +
  labs(title ="Abweichung vom Jahresschadenmittel Elementar aller 18 KGV (ohne Bern)",
       subtitle = "Standardisierte Schäden im Verhältnis zum Versicherungskapital",
       caption = "Quelle: IRV, bearbeitet durch die GVZ") +
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 65, vjust = 0.6),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "gray"),
        panel.grid.major.y = element_line(colour = "gray"),
        ## Hide all the vertical gridlines
        panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", # "none" for no legend
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        aspect.ratio = 6/19,
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")


## Sortierte Jahresschaeden Feuer in Promille des Versicherungskapitals
## Data Preparation
df.Fs <- df %>% arrange(desc(pm_fs_Z)) %>% 
  ## convert to factor to retain sorted order in plot
  mutate(jahr = factor(jahr, levels = jahr))

bar5 <- ggplot(df.Fs, aes(x = jahr, y = pm_fs_Z, label = pm_fs_Z)) + 
  geom_bar(stat = 'identity', aes(fill = pm_fsType), width = .5)  +
  scale_fill_manual(name = "Jahresschaden", 
                    labels = c("Über Mittel",
                               "Unter Mittel"), 
                    values = c("above" = "#de2d26", "below" = "#fcae91")) + 
  ylab("") + xlab("") +
  labs(title = "Sortierte Abweichung vom Jahresschadenmittel Feuer aller 18 KGV (ohne Bern)",
       subtitle = "Standardisierte Schäden im Verhältnis zum Versicherungskapital",
       caption = "Quelle: IRV") +
  coord_flip() +
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x=element_blank(),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        axis.ticks = element_blank(),
        #panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(colour = "#d9d9d9", size = 0.1),
        panel.border = element_blank(),
        legend.position = c(0.9, 0.2),
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        aspect.ratio = sqrt(2)/1, # a4
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")


## Sortierte Jahresschaeden Elementar in Promille des Versicherungskapitals
## Data Preparation
df.Es <- df %>% arrange(desc(pm_es_Z)) %>% 
  ## convert to factor to retain sorted order in plot
  mutate(jahr = factor(jahr, levels = jahr))

bar6 <- ggplot(df.Es, aes(x = jahr, y = pm_es_Z, label = pm_es_Z)) + 
  geom_bar(stat = 'identity', aes(fill = pm_esType), width = .5)  +
  scale_fill_manual(name = "Jahresschaden", 
                    labels = c("Über Mittel",
                               "Unter Mittel"), 
                    values = c("above" = "#2171b5", "below" = "#9ecae1")) + 
  ylab("") + xlab("") +
  labs(title = "Sortierte Abweichung vom Jahresschadenmittel Elementar aller 18 KGV (ohne Bern)",
       subtitle = "Standardisierte Schäden im Verhältnis zum Versicherungskapital",
       caption = "Quelle: IRV, bearbeitet durch die GVZ") +
  coord_flip() +
  theme(plot.title = element_text(size = 14, colour = "black", vjust = 1, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x=element_blank(),
        axis.title.y = element_text(size = 12, vjust = 1.1),
        axis.ticks = element_blank(),
        #panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(colour = "#d9d9d9", size = 0.1),
        panel.border = element_blank(),
        legend.position = c(0.9, 0.2),
        # Removing/Reducing the whitespace surrounding the plot
        # (requires the grid library)
        aspect.ratio = sqrt(2)/1, # a4
        plot.margin = unit(c(1,1,1,1),"mm")) # ("left", "right", "bottom", "top")


## --- Save the ggplot's
width.plot = 12
height.plot = (9/16) * width.plot

# # Save the plot as a PDF with ggsave and Cairo
# # R will want to autocomplete cairo_pdf to cairo_pdf() (note the parentheses)
# # This will not work with the parentheses; ensure there aren't any
# 
# # Save the plot as a high resolution PNG using Cairo
# # Note the difference here; instead of using device = cairo_pdf, you use
# # type = "cairo". It's confusing and weird and that's just life.
# 
# ## -- bar1
# ggsave(bar1, filename = "irv.vs.schad.pdf", device = cairo_pdf,
#        width = width.plot, height = height.plot, units = "in")
## save a png
# ggsave(bar1, filename = "irv.schad.png", dpi = 1000, type = "cairo",
#        width = width.plot, height = height.plot, units = "in")
# 
# # -- bar2
# ggsave(bar2, filename = "irv.trend.pdf", device = cairo_pdf,
#        width = width.plot, height = height.plot, units = "in")
# save a png
# ggsave(bar2, filename = "irv_trend.png", dpi = 1000, type = "cairo",
#        width = width.plot, height = height.plot, units = "in")
# 
# ## -- bar3
# ggsave(bar3, filename = "standard.feuer.pdf", device = cairo_pdf,
#        width = width.plot, height = height.plot, units = "in")
# ## save a png
# ggsave(bar3, filename = "irv.standard.feuer.png", dpi = 1000, type = "cairo",
#        width = width.plot, height = height.plot, units = "in")
# 
# ## -- bar4
# ggsave(bar4, filename = "irv.standard.elementar.pdf", device = cairo_pdf,
#        width = width.plot, height = height.plot, units = "in")
# ## save a png
# ggsave(bar4, filename = "irv.standard.elementar.png", dpi = 1000, type = "cairo",
#        width = width.plot, height = height.plot, units = "in")
# 
# ## -- bar5
# width.plot = 8
# height.plot = (sqrt(2)/1) * width.plot
# 
# ggsave(bar5, filename = "irv.sort.standard.Feuer.pdf", device = cairo_pdf,
#        width = width.plot, height = height.plot, units = "in")
# ## save a png
# ggsave(bar5, filename = "irv.sort.standard.Feuer.png", dpi = 1000, type = "cairo",
#        width = width.plot, height = height.plot, units = "in")
# 
# ## -- bar6
# ggsave(bar6, filename = "irv.sort.standard.elementar.pdf", device = cairo_pdf,
#        width = width.plot, height = height.plot, units = "in")
# ## save a png
# ggsave(bar6, filename = "irv.sort.standard.elementar.png", dpi = 1000, type = "cairo",
#        width = width.plot, height = height.plot, units = "in")


## --- Welche Modelle fuer den Schadentrend?

## Lineare exponentielle Regression fuer jaehrliche Feuerschaeden
## lognormal, log-transformed LM
m.lgnorm_fs <- lm(log(df$pm_fs)~ df$jahr)
# summary(m.lgnorm_fs) ## das modell erklaert rund 40% der varianz
fit.lgnorm_fs = exp(fitted(m.lgnorm_fs))

## --- Welches Modell fuer die jaehrlichen Elementarschaeden?

# ## Lineares Regressions Modell
# ln.model <-lm(df$pm_es ~ df$jahr)
# summary(ln.model) ## das modell erklaert nur knapp 4% der varianz

## Lineare exponentielle Regression, fuer jaehrlichen Elementarschaeden
## lognormal, log-transformed LM
m.lgnorm_es <- lm(log(df$pm_es)~ df$jahr)
# summary(m.lgnorm_es) ## das modell erklaert rund 10% der varianz

## --- log-linked GLM Gamma
## A Gamma error distribution with a log link works well for positive-only
## data with positively-skewed errors. The Gamma distribution is flexible
## and can mimic, among other shapes, a log-normal shape. The log link can
## represent an underlying multiplicate process.
## Although we call it a "log link", if we're working with the Gamma
## distribution directly, we exponentiate the linear predictor instead
## of logging the data. This ensures that we don't propose negative mean
## values to the Gamma distribution.

m.gamma_es <- glm(df$pm_es ~ df$jahr, family = Gamma(link = "log"))
summary(m.gamma_es)
yearvalues <- seq(min(df$jahr), max(df$jahr), 1)
fit.gamma_es = predict(m.gamma_es, newdata = list(jahr = seq(min(df$jahr),
                                                       max(df$jahr), 1)),
                      type = "response")

