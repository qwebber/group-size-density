

## load libraries
library(data.table)
library(glmmTMB)
library(ggplot2)
library(gridExtra)

## load data
fogo <- fread("output/fogo-group-locs.csv")

q5 <- glmmTMB(group_size ~ I(propOpen^2) + season + composition +
                (1|Year), family = "nbinom1", 
              data = fogo)

summary(q5)

testgTMB<- ggeffect(q5, terms = c("propOpen"))
setDT(testgTMB)

png("graphics/Fig4.png",width = 3000, height = 3000, res = 500)
ggplot() +
  geom_jitter(data = fogo, aes(propOpen, group_size, color = season), 
              alpha = 0.5, 
              size = 3, height = 0.2) +
  geom_line(data = testgTMB, 
            aes(x,predicted), size = 1) +
  geom_ribbon(data = testgTMB, 
              aes(x = x, y = predicted, 
                  ymin=conf.low, ymax=conf.high), alpha=0.2) +
              #position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0.2)) +
  #geom_boxplot(outlier.color = NA, alpha = 0.4, notch = T, 
  #             position = position_dodge()) +
  ylim(0,40) +
  scale_fill_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  scale_color_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  #scale_x_discrete(limits=c("Calving","Summer","Early winter")) +
  xlab('Habitat openness') +
  ylab('Group size') +
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
dev.off()
