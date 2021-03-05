


## load libraries
library(data.table)
library(ggplot2)
library(gridExtra)

## load data
fogo <- fread("output/fogo-group-locs.csv")


png("graphics/FigS6.png",width = 4500, height = 3000, res = 500)
ggplot(fogo, 
       aes(composition, group_size, fill = season)) +
  geom_jitter(aes(color = season), alpha = 0.5, size = 3, 
              position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.2)) +
  geom_boxplot(outlier.color = NA, alpha = 0.4, 
               position = position_dodge()) +
  ylim(0,40) +
  scale_fill_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  scale_color_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  #scale_x_discrete(limits=c("Female groups","Male groups","Mixed groups")) +
  xlab('') +
  ylab('Group size') +
  theme(legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size = 9, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()
