

## load data
fogo <- fread("output/fogo-group-locs.csv")

fogo[, rowDay := seq_along(group_size), by = c("JDate", "Year")]
fogo[rowDay == 1]



png("graphics/FigS3.png", width = 5000, height = 3500, units = "px", res = 600)
ggplot(fogo[rowDay == 1]) +
  geom_jitter(aes(x = JDate, y = factor(Year, level = 
                                          c( "2019",
                                             "2018",
                                             "2017",
                                             "2016")),
                  group = season, color = season), 
              height = 0.1, alpha = 0.5) +
  scale_color_manual(values=c("#e08214", "#b2abd2", "#5aae61")) +
  scale_x_continuous(breaks = c(121, 181, 244, 305),
                     labels = c("May 1", "July 1", "September 1", "November 1")) +
  geom_vline(xintercept = 181, lty = 2) +
  geom_vline(xintercept = 305, lty = 2) +
  geom_hline(yintercept = 1.5, lty = 2) +
  geom_hline(yintercept = 2.5, lty = 2) +
  geom_hline(yintercept = 3.5, lty = 2) +
  ylab("Year") + xlab('') +
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