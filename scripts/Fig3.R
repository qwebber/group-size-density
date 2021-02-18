

## load libraries
library(data.table)
library(ggplot2)
library(gridExtra)

## load data
habitat <- fread("output/herd-survey-locs-data.csv")
habitat$HerdYear <- as.factor(paste(habitat$HERD, 
                                    habitat$year, sep = "_"))
## load data
allDf <- fread("output/groups-herd-year.csv")
allDf$HerdYear <- as.factor(paste(allDf$HERD, allDf$Year, sep = "_"))
allDf <- allDf[,c("HerdYear", "size")]

## merge files
habitat2 <- merge(allDf, habitat, by.EACHI = "HerdYear", allow.cartesian = TRUE)

##  change name of habitat types
habitat2$habitatType[habitat2$habitatType == "closed"] <- "Closed"
habitat2$habitatType[habitat2$habitatType == "open"] <- "Open"


png("graphics/Fig3.png",width = 6000, height = 4500, res = 600)
aa = ggplot(habitat2[HSize == "Small" & habitatType != "Rut"], 
            aes(size, Total_Caribou)) +
  geom_jitter(aes(color = habitatType), alpha = 0.5, width = 0.04, size = 3) +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_log10() +
  xlab('Density') +
  ylab('Group size') +
  ggtitle('A)') +
  theme(legend.position = 'none',
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),        
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

bb = ggplot(habitat2[HSize == "Med" & Total_Caribou < 100 & habitatType != "Rut"], 
            aes(size, Total_Caribou)) +
  geom_jitter(aes(color = habitatType), alpha = 0.5, width = 0.04, size = 3) +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_log10() +
  xlab('Density') +
  ylab('Group size') +
  ggtitle('B)') +
  theme(legend.position = 'none',
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),        
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
cc = ggplot(habitat2[HSize == "Large" & Total_Caribou < 100 & habitatType != "Rut"], 
            aes(size, Total_Caribou)) +
  geom_jitter(aes(color = habitatType), alpha = 0.5, width = 0.04, size = 3) +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_log10() +
  xlab('Density') +
  ylab('Group size') +
  ggtitle('C)') +
  theme(legend.position = c(0.75,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

dd = ggplot(habitat2[HSize == "Small" & season != "Rut"], 
            aes(size, Total_Caribou)) +
  geom_jitter(aes(color = season), alpha = 0.5, width = 0.04, size = 3) +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_fill_manual(values=c("#7fbf7b", "#af8dc3")) +
  scale_color_manual(values=c("#7fbf7b", "#af8dc3")) +
  scale_x_log10() +
  xlab('Density') +
  ylab('Group size') +
  ggtitle('D)') +
  theme(legend.position = 'none',
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),       
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ee = ggplot(habitat2[HSize == "Med" & Total_Caribou < 100 & season != "Rut"], 
            aes(size, Total_Caribou)) +
  geom_jitter(aes(color = season), alpha = 0.5, width = 0.04, size = 3) +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_fill_manual(values=c("#7fbf7b", "#af8dc3")) +
  scale_color_manual(values=c("#7fbf7b", "#af8dc3")) +
  scale_x_log10() +
  xlab('Density') +
  ylab('Group size') +
  ggtitle('E)') +
  theme(legend.position = 'none',
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),      
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ff = ggplot(habitat2[HSize == "Large" & Total_Caribou < 100 & season != "Rut"], 
            aes(size, Total_Caribou)) +
  geom_jitter(aes(color = season), alpha = 0.5, width = 0.04, size = 3) +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_fill_manual(values=c("#7fbf7b", "#af8dc3")) +
  scale_color_manual(values=c("#7fbf7b", "#af8dc3")) +
  scale_x_log10() + 
  xlab('Density') +
  ylab('Group size') +
  ggtitle('F)') +
  theme(legend.position = c(0.75,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, angle = 45 , color = "black", hjust = 1),      
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

grid.arrange(aa,bb,cc,dd,ee,ff, ncol = 3, nrow = 2)

dev.off()