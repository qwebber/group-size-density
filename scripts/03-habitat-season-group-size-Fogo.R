
## load libraries
library(data.table)
library(glmmTMB)
library(Rmisc)

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
## global model
q1 <- glmmTMB(group_size ~ 
                propOpen * season 
                propOpen * composition + 
                composition * season +
                  (1|Year), family = "nbinom1", 
                 data = fogo)

q2 <- glmmTMB(group_size ~ 
                propOpen * composition + 
                composition * season +
                (1|Year), family = "nbinom1", 
              data = fogo)

q3 <- glmmTMB(group_size ~ 
                propOpen * season +
                composition * season +
                (1|Year), family = "nbinom1", 
              data = fogo)

q4 <- glmmTMB(group_size ~ 
                propOpen * season +
                propOpen * composition +
                (1|Year), family = "nbinom1", 
              data = fogo)

q5 <- glmmTMB(group_size ~ propOpen + season + composition +
                (1|Year), family = "nbinom1", 
              data = fogo)

## run AIC model selection
aic <- AIC(q1,q2,q3,q4,q5) 
aic$deltaAIC <- aic$AIC - min(aic$AIC) ## Table S7

## top model is q5
summary(q5) ## Table 4 
