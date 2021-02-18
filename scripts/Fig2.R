
## load libraries
library(data.table)
library(ggplot2)
library(gridExtra)

#### load data
allDf <- fread("output/groups-herd-year.csv")

## rename herds by size
allDf$HSize[allDf$HERD == "Avalon"] <- "A) Small"
allDf$HSize[allDf$HERD == "Cape_Shore"] <- "A) Small"
allDf$HSize[allDf$HERD == "Mt_Peyton"] <-"A) Small"
allDf$HSize[allDf$HERD == "Grey_River"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Gaff_Topsails"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Pothill"] <- "B) Medium"
allDf$HSize[allDf$HERD == "St_Anthony"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Lapoile"] <- "C) Large"
allDf$HSize[allDf$HERD == "Middle_Ridge"] <- "C) Large"

allDf$HSize2[allDf$HERD == "Avalon"] <- "D) Small"
allDf$HSize2[allDf$HERD == "Cape_Shore"] <- "D) Small"
allDf$HSize2[allDf$HERD == "Mt_Peyton"] <-"D) Small"
allDf$HSize2[allDf$HERD == "Grey_River"] <- "E) Medium"
allDf$HSize2[allDf$HERD == "Gaff_Topsails"] <- "E) Medium"
allDf$HSize2[allDf$HERD == "Pothill"] <- "E) Medium"
allDf$HSize2[allDf$HERD == "St_Anthony"] <- "E) Medium"
allDf$HSize2[allDf$HERD == "Lapoile"] <- "F) Large"
allDf$HSize2[allDf$HERD == "Middle_Ridge"] <- "F) Large"


scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#### Figure 2 ####
png("graphics/Fig2.png", width=6000, height=4500, res = 600)
aa <- ggplot(allDf[HSize == "A) Small"], aes(size, meanGS)) +
  geom_jitter(aes(color = factor(phase)), alpha = 0.5, size = 3) +
  geom_errorbar(aes(ymin=meanGS-seGR, ymax=meanGS+seGR), 
                colour="black",  alpha = 0.3) + 
  ylab("Mean group size (+/- se)") +
  geom_smooth(method = "lm", color = "black", se = F) +
  xlab("Density") +
  #ylim(0,30) +
  ggtitle('A)') +
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_x_log10() +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
bb <- ggplot(allDf[HSize == "B) Medium"], aes(size, meanGS)) +
  geom_jitter(aes(color = factor(phase)), alpha = 0.5, size = 3) +
  geom_errorbar(aes(ymin=meanGS-seGR, ymax=meanGS+seGR), 
                colour="black",  alpha = 0.3) + 
  ylab("Mean group size (+/- se)") +
  geom_smooth(method = "lm", color = "black", se = F) +
  xlab("Density") +
  #ylim(0,50) +
  ggtitle('B)') + 
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_x_log10() +
  theme(legend.position = 'none',
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
cc <- ggplot(allDf[HSize == "C) Large"], aes(size, meanGS)) +
  geom_jitter(aes(color = factor(phase)), alpha = 0.5, size = 3) +
  geom_errorbar(aes(ymin=meanGS-seGR, ymax=meanGS+seGR), 
                colour="black",  alpha = 0.3) + 
  ylab("Mean group size (+/- se)") +
  geom_smooth(method = "lm", color = "black", se = F) +
  xlab("Density") +
  #ylim(0,100) +
  ggtitle('C)') + 
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_x_log10() +
  theme(legend.position = 'none',
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
dd <- ggplot(allDf[HSize2 == "D) Small"], aes(size, typicalGS)) +
  geom_jitter(aes(color = factor(phase)), alpha = 0.5, size = 3) +
  #geom_errorbar(aes(ymin=meanGS-seGR, ymax=meanGS+seGR), 
  #              colour="black",  alpha = 0.3) + 
  ylab("Typical group size") +
  geom_smooth(method = "lm", color = "black", se = F) +
  xlab("Density") +
  #ylim(0,120) +
  ggtitle('D)') +
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_x_log10() +
  theme(legend.position = 'none',
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ee <- ggplot(allDf[HSize2 == "E) Medium"], aes(size, typicalGS)) +
  geom_jitter(aes(color = factor(phase)), alpha = 0.5, size = 3) +
  #geom_errorbar(aes(ymin=meanGS-seGR, ymax=meanGS+seGR), 
  #              colour="black",  alpha = 0.3) + 
  ylab("Typical group size") +
  geom_smooth(method = "lm", color = "black", se = F) +
  xlab("Density") +
  #ylim(0,50) +
  ggtitle('E)') + 
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_x_log10() +
  theme(legend.position = 'none',
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ff <- ggplot(allDf[HSize2 == "F) Large"], aes(size, typicalGS)) +
  geom_jitter(aes(color = factor(phase)), alpha = 0.5, size = 3) +
  #geom_errorbar(aes(ymin=meanGS-seGR, ymax=meanGS+seGR), 
  #              colour="black",  alpha = 0.3) + 
  ylab("Typical group size") +
  geom_smooth(method = "lm", color = "black", se = F) +
  xlab("Density") +
  #ylim(0,100) +
  ggtitle('F)') + 
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_x_log10() +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
grid.arrange(aa,bb,cc,
             dd,ee,ff, nrow = 2)
dev.off()