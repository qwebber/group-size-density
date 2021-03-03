
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

## merge files
habitat2 <- merge(allDf, habitat, by.EACHI = "HerdYear", allow.cartesian = TRUE)

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
small <- glmmTMB(Total_Caribou ~ log(size) * habitatType  + season * log(size)
                 + (1|year), family = "nbinom1", 
              data = habitat2[HSize == "Small" & season != "Rut"])
summary(small)

## Medium herds:
med <- glmmTMB(Total_Caribou ~ log(size) * habitatType  + season * log(size) + (1|year), family = "nbinom1", 
                 data = habitat2[HSize == "Med" & season != "Rut"])
summary(med)

## Large herds:
large <- glmmTMB(Total_Caribou ~ log(size) * habitatType  + season * log(size)  + (1|year), family = "nbinom1", 
               data = habitat2[HSize == "Large" & season != "Rut"])
summary(large)


