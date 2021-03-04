
## load libraries
library(data.table)
library(Rmisc)
library(glmmTMB)

habitat <- fread("output/herd-survey-locs-data.csv")
habitat$HerdYear <- as.factor(paste(habitat$HERD, 
                                    habitat$year, sep = "_"))
## load data
allDf <- fread("output/groups-herd-year.csv")
allDf$HerdYear <- as.factor(paste(allDf$HERD, allDf$Year, sep = "_"))
allDf <- allDf[,c("HerdYear", "size")]

## only one density value per herd-year
allDf <- allDf[, mean(size), by = "HerdYear"]
setnames(allDf, "V1", "size")

## merge files
habitat2 <- merge(allDf, habitat, by = "HerdYear", allow.cartesian = TRUE)

## rename herds based on size
habitat2$HSize[habitat2$HERD == "Avalon"] <- "Small"
habitat2$HSize[habitat2$HERD == "Cape_Shore"] <- "Small"
habitat2$HSize[habitat2$HERD == "Mt_Peyton"] <-"Small"
habitat2$HSize[habitat2$HERD == "Grey_River"] <- "Med"
habitat2$HSize[habitat2$HERD == "Gaff_Topsails"] <- "Med"
habitat2$HSize[habitat2$HERD == "Pothill"] <- "Med"
habitat2$HSize[habitat2$HERD == "St_Anthony"] <- "Med"
habitat2$HSize[habitat2$HERD == "Lapoile"] <- "Large"
habitat2$HSize[habitat2$HERD == "Middle_Ridge"] <- "Large"

habitat2$habitatType[habitat2$habitatType == "open"] <- "Open"
habitat2$habitatType[habitat2$habitatType == "closed"] <- "Closed"

## remove outliers
habitat2 <- habitat2[Total_Caribou < 100]

## remove data from the rut
habitat2 <- habitat2[season != "Rut"]

## summary stats for season
habitat2[, mean(Total_Caribou), by = .(season, HSize)]
habitat2[, CI(Total_Caribou)[3], by = .(season, HSize)]
habitat2[, CI(Total_Caribou)[1], by = .(season, HSize)]

## summary stats for season
habitat2[, mean(Total_Caribou), by = .(habitatType, HSize)]
habitat2[, CI(Total_Caribou)[3], by = .(habitatType, HSize)]
habitat2[, CI(Total_Caribou)[1], by = .(habitatType, HSize)]

habitat2$year <- as.factor(habitat2$year)

## number of surveys per season
habitat2[season != "Rut"][, unique(season), by = c("season", "HERD", "year")]

## Small herds:
s1 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                    log(size) * season +
                    propOpen * season + 
                 + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Small"])

s2 <- glmmTMB(Total_Caribou ~ 
                    log(size) * season +
                    propOpen * season + 
                    + (1|year), family = "nbinom1", 
                  data = habitat2[HSize == "Small"])

s3 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                    propOpen * season + 
                    + (1|year), family = "nbinom1", 
                  data = habitat2[HSize == "Small"])

s4 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                    log(size) * season +
                    + (1|year), family = "nbinom1", 
                  data = habitat2[HSize == "Small"])

s5 <- glmmTMB(Total_Caribou ~ log(size) + 
                    propOpen  + 
                    season +
                    + (1|year), family = "nbinom1", 
                  data = habitat2[HSize == "Small"])

aic <- AIC(s1, s2, s3, s4, s5)
aic$deltaAIC <- aic$AIC - min(aic$AIC, na.rm = T)
aic
summary(s1)

## Medium herds:
m1 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                log(size) * season +
                propOpen * season + 
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Med"])

m2 <- glmmTMB(Total_Caribou ~ 
                log(size) * season +
                propOpen * season + 
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Med"])

m3 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                propOpen * season + 
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Med"])

m4 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                log(size) * season +
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Med"])

m5 <- glmmTMB(Total_Caribou ~ log(size) + 
                propOpen  + 
                season +
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Med"])

aicM <- AIC(m1, m2, m3, m4, m5)
aicM$deltaAIC <- aicM$AIC - min(aicM$AIC, na.rm = T)
aicM
summary(m2)

## Large herds:
l1 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                log(size) * season +
                propOpen * season + 
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Large"])

l2 <- glmmTMB(Total_Caribou ~ 
                log(size) * season +
                propOpen * season + 
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Large"])

l3 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                propOpen * season + 
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Large"])

l4 <- glmmTMB(Total_Caribou ~ log(size) * propOpen  + 
                log(size) * season +
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Large"])

l5 <- glmmTMB(Total_Caribou ~ log(size) + 
                propOpen  + 
                season +
                + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Large"])

aicL <- AIC(l1, l2, l3, l4, l5)
aicL$deltaAIC <- aicL$AIC - min(aicL$AIC, na.rm = T)
aicL
summary(l5)
