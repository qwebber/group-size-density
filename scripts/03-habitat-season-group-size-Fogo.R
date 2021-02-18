
## load libraries
library(data.table)
library(glmmTMB)

## load data
fogo <- fread("output/fogo-group-locs.csv")

## summary stats
fogo[, mean(group_size), by = .(habitat, season)]
fogo[, CI(group_size)[3], by = .(habitat, season)]
fogo[, CI(group_size)[1], by = .(habitat, season)]

fogo$Year <- as.factor(fogo$Year)

## number of sampling days per season
fogo[, uniqueN(JDate), by = c("season", "Year")]

## run model
q1 <- glmmTMB(group_size ~ habitat + composition*season +
                  (1|Year), family = "nbinom1", 
                 data = fogo)
summary(q1)

