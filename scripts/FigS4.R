
## load libraries
library(data.table)
library(lme4)
library(ggplot2)
library(gridExtra)

## load data
allDf <- fread("output/groups-herd-year.csv")


## convert herds to small, medium, large
allDf$HSize[allDf$HERD == "Avalon"] <- "A) Small"
allDf$HSize[allDf$HERD == "Cape_Shore"] <- "A) Small"
allDf$HSize[allDf$HERD == "Mt_Peyton"] <-"A) Small"
allDf$HSize[allDf$HERD == "Grey_River"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Gaff_Topsails"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Pothill"] <- "B) Medium"
allDf$HSize[allDf$HERD == "St_Anthony"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Lapoile"] <- "C) Large"
allDf$HSize[allDf$HERD == "Middle_Ridge"] <- "C) Large"


#### MEAN VS TYPICAL GROUP SIZE ####
a1 <- glmmTMB(round(meanGS,0) ~ log(size) + typicalGS + (1|Year),
              family = "gaussian", 
              data = allDf[HSize == "A) Small"])
summary(a1)
b1 <- glmmTMB(round(meanGS,0) ~ log(size) + typicalGS + (1|Year),
              family = "gaussian", 
              data = allDf[HSize == "B) Medium"])
summary(b1)
c1 <- glmmTMB(round(meanGS,0) ~ log(size) + typicalGS + (1|Year),
              family = "gaussian", 
              data = allDf[HSize == "C) Large"])
summary(c1)


## generate dummy dataframe to plot 1:1 relationship
df <- data.table(x = 1:125, y = 1:125)


## generate figure S3
png("graphics/FigS4.png", width = 8000, height = 4000, res = 600)
aa <- ggplot(allDf[HSize == "A) Small"]) +
  geom_point(aes(typicalGS, meanGS, color = HERD), 
             alpha = 0.5,
             size = 3) +
  geom_smooth(aes(typicalGS, meanGS), 
              method = "lm", 
              se = F, 
              color = "black") +
  geom_smooth(data = df, aes(x, y), color = "black", lty = 2) +
  ylab("Mean group size") +
  xlab("Typical group size") +
  ggtitle('A) Small herds') +
  scale_color_manual(values = c("#e41a1c", # Avalon (red)  
                                "#ffff33", #Cape Shore (yellow)
                                #"#f781bf", #Fogo (pink)
                                #"#984ea3", #Grey River (purple)
                                #"#4daf4a", #La Poile (green)
                                #"#ff7f00", #Middle Ridge (orange)
                                "#a65628", #Mt Peyton (brown)
                                #"#4d4d4d", #Pothill (dark green)
                                #"#377eb8", # St. Anthony (blue)
                                #"#a6cee3"), #Topsails (light blue)
                                name = "Herd")) + 
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=12, color = "black"),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

bb <- ggplot(allDf[HSize == "B) Medium"]) +
  geom_point(aes(typicalGS, meanGS, color = HERD), 
             alpha = 0.5,
             size = 3) +
  geom_smooth(aes(typicalGS, meanGS), 
              method = "lm", 
              se = F, 
              color = "black") +
  geom_smooth(data = df, aes(x, y), color = "black", lty = 2) +
  ylab("Mean group size") +
  xlab("Typical group size") +
  ggtitle('B) Medium herds') +
  scale_color_manual(values = c(#"#e41a1c", # Avalon (red)  
    #"#ffff33", #Cape Shore (yellow)
    #"#f781bf", #Fogo (pink)
    "#984ea3", #Grey River (purple)
    #"#4daf4a", #La Poile (green)
    #"#ff7f00", #Middle Ridge (orange)
    #"#a65628", #Mt Peyton (brown)
    "#4d4d4d", #Pothill (dark green)
    "#377eb8", # St. Anthony (blue)
    "#a6cee3"), #Topsails (light blue)
    name = "Herd") + 
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=12, color = "black"),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

cc <- ggplot(allDf[HSize == "C) Large"]) +
  geom_point(aes(typicalGS, meanGS, color = HERD), 
             alpha = 0.5,
             size = 3) +
  geom_smooth(aes(typicalGS, meanGS), 
              method = "lm", 
              se = F, 
              color = "black") +
  geom_smooth(data = df, aes(x, y), color = "black", lty = 2) +
  ylab("Mean group size") +
  xlab("Typical group size") +
  ggtitle('C) Large herds') +
  scale_color_manual(values = c(#"#e41a1c", # Avalon (red)  
    #"#ffff33", #Cape Shore (yellow)
    #"#f781bf", #Fogo (pink)
    #"#984ea3", #Grey River (purple)
    "#4daf4a", #La Poile (green)
    "#ff7f00"), #Middle Ridge (orange)
    #"#a65628", #Mt Peyton (brown)
    #"#4d4d4d", #Pothill (dark green)
    #"#377eb8", # St. Anthony (blue)
    #"#a6cee3"), #Topsails (light blue)
    name = "Herd") + 
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=12, color = "black"),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

grid.arrange(aa,bb,cc,nrow = 1)

dev.off()