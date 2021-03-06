

library(ggplot2)
library(gridExtra)
library(data.table)

out2 <- fread("output/zip-coefficients.csv")

out2$V1 <- round(out2$V1, 10)

png("graphics/FigS7.png", height = 3000, width = 6000, units = "px", res = 600)
aa <- ggplot(out2[coef == "count_habitatopen"]) +
  geom_histogram(aes(V1), 
                 fill = "darkgrey", 
                 color = "black", 
                 alpha = 0.5, 
                 bins = 50) +
  ggtitle("A)") +
  ylab("Frequency") +
  xlab("Coefficient (Poisson log link model)") +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black", hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

bb <- ggplot(out2[coef == "zero_habitatopen"]) +
  geom_histogram(aes(V1), 
                 fill = "darkgrey", 
                 color = "black", 
                 alpha = 0.5,
                 bins = 50) +
  ggtitle("B)") +
  ylab("Frequency") +
  xlab("Coefficient (Binomial logit link model)") +
  xlim(-1.3, 0.1) +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black", hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
grid.arrange(aa,bb,nrow = 1)
dev.off()
