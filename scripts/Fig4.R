

## load libraries
library(data.table)
library(glmmTMB)
library(ggplot2)
library(gridExtra)
library(ggeffects)

## load data
fogo <- fread("output/fogo-group-locs.csv")

q5 <- glmmTMB(group_size ~ propOpen + season + composition +
                (1|Year), family = "nbinom1", 
              data = fogo)

summary(q5)

png("graphics/Fig4.png",width = 6000, height = 3000, res = 500)
aa <- ggplot(fogo, aes(propOpen, group_size)) +
  geom_jitter(aes(color = season), alpha = 0.5, 
              size = 3, height = 0.2) +
  geom_smooth(method = "lm", color = "black") +
  #geom_line(data = testgTMB, 
  #          aes(x,predicted), size = 1) +
  #geom_ribbon(data = testgTMB, 
  #            aes(x = x, y = predicted, 
  #                ymin=conf.low, ymax=conf.high), alpha=0.2) +
              #position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.2)) +
  
  #ylim(0,40) +
  scale_fill_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  scale_color_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  #scale_x_discrete(limits=c("Calving","Summer","Early winter")) +
  xlab('Habitat openness') +
  ylab('Group size') +
  ggtitle('A)') +
  theme(legend.position = c(0.15,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size = 18, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


bb <- ggplot(fogo, aes(season, group_size)) +
  geom_jitter(aes(color = season), alpha = 0.5, 
              size = 3, height = 0.2, width = 0.35) +
  geom_boxplot(aes(fill = season), outlier.color = NA, alpha = 0.4, notch = T, 
              position = position_dodge()) +
  scale_fill_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  scale_color_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  xlab('Season') +
  ylab('Group size') +
  ggtitle('B)') +
  theme( legend.position = 'none', #c(0.15,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size = 18, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

grid.arrange(aa,bb, nrow = 1)

dev.off()

## Samuel et al. 1987
B0 <- 1.22 
B1 <- 1.55 
veg <- 0.05  

## Unsworth et al. 1990
B0 <- 2.24
B1 <- 1.57
veg <- 0.86

## ZIP model
B0 <- 0.51
B1 <- 0.81
#veg <- 

## run correction factor (note, the final value in the equation is "% vegetation cover", i.e. proportion closed)
df01 <- data.table(sight = c(B0 + (B1 * log(df2$propOpen + 0.0125)) - 1), ## 90-100% cover
                   group = rep("Yes", 834))
df02 <- data.table(sight = c(B0 + (B1 * log(df2$propOpen + 0.0125)) - 0), ## 80-90% cover
                   group = rep("No", 834))


df03 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*8), ## 70-80% cover
                   propOpen = rep("70-80%", 466))
df04 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*7), ## 60-70% cover
                   propOpen = rep("60-70%", 466))
df05 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*6), ## 50-60% cover
                   propOpen = rep("50-60%", 466))
df06 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*5), ## 40-50% cover
                   propOpen = rep("40-50%", 466))
df07 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*4), ## 30-40% cover
                   propOpen = rep("30-40%", 466))
df08 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*3), ## 20-30% cover
                   propOpen = rep("20-30%", 466))
df09 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*2), ## 10-20% cover
                   propOpen = rep("10-20%", 466))
df10 <- data.table(sight = c(B0 + (B1 * log(fogo$group_size)) - veg*1), ## 0-10% cover
                   propOpen = rep("0-10%", 466))

dfAll <- rbind(df01, df02) #, df03, df04, df05, df06, df07, df08, df09, df10)

dfAll$propOpen <- rep(df2$propOpen, 2)
dfAll$propOpen <- rep(df2$propOpen, 2)


dfAll$probSight <- exp(dfAll$sight)/(1 + exp(dfAll$sight))

ggplot(dfAll) +
  geom_line(aes(propOpen, probSight, color = factor(group))) +
  scale_color_viridis_d() +
  ylab("Sightability") +
  xlab("Habitat openness") +
  #ylim(0.5,1) +
  labs(color = "Observed group") +
  theme(legend.position = "right", #c(0.8,0.4),
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.text = element_text(size = 12),
         axis.title.y = element_text(size=18),
         axis.title.x = element_text(size = 18, color = "black"),
         axis.text = element_text(size = 12, color = "black"),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, size=1))
