
## load libraries
library(data.table)
library(ggplot2)
library(gridExtra)

### load data 
allDf <- fread("output/groups-herd-year.csv")

## number of surveys per year
allDf[, .N, by = c("HERD")]

## rename herds
allDf$HERD[allDf$HERD == "Cape_Shore"] <- "Cape Shore"
allDf$HERD[allDf$HERD == "Mt_Peyton"] <-"Mt. Peyton"
allDf$HERD[allDf$HERD == "Grey_River"] <- "Grey River"
allDf$HERD[allDf$HERD == "Gaff_Topsails"] <- "Gaff Topsails"
allDf$HERD[allDf$HERD == "Pothill"] <- "Pot Hill"
allDf$HERD[allDf$HERD == "St_Anthony"] <- "St. Anthony"
allDf$HERD[allDf$HERD == "Lapoile"] <- "La Poile"
allDf$HERD[allDf$HERD == "Middle_Ridge"] <- "Middle Ridge"



png("graphics/FigS2.png", width = 5000, height = 3500, units = "px", res = 600)
ggplot(allDf) +
  geom_point(aes(year, y = factor(HERD, level = 
                                    c( "St. Anthony",
                                       "Pot Hill",
                                       "Mt. Peyton",
                                       "Middle Ridge",
                                       "La Poile",
                                       "Grey River",
                                       "Gaff Topsails",
                                       "Cape Shore",
                                       "Avalon")),
                 group = season, color = season), 
             position = position_dodge(width = 0.75),
             alpha = 0.5) +
  scale_color_manual(values=c("#5aae61", "#e08214", "#b2abd2")) +
  geom_vline(xintercept = 2000) +
  geom_hline(yintercept = 1.5, lty = 2) +
  geom_hline(yintercept = 2.5, lty = 2) +
  geom_hline(yintercept = 3.5, lty = 2) +
  geom_hline(yintercept = 4.5, lty = 2) +
  geom_hline(yintercept = 5.5, lty = 2) +
  geom_hline(yintercept = 6.5, lty = 2) +
  geom_hline(yintercept = 7.5, lty = 2) +
  geom_hline(yintercept = 8.5, lty = 2) +
  ylab("Herd") + xlab("Year") +
  labs(color = "Season") +
  theme(#legend.position = 'none',
    legend.key = element_blank(),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))

dev.off()