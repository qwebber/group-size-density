
## load libraries
library(data.table)
library(glmmTMB)
library(Rmisc)
library(lme4)

#### load data
allDf <- fread("output/groups-herd-year.csv")

## rename herds by size
allDf$HSize[allDf$HERD == "Avalon"] <- "A) Small"
allDf$HSize[allDf$HERD == "Cape_Shore"] <- "A) Small"
allDf$HSize[allDf$HERD == "Mt_Peyton"] <-"A) Small"
allDf$HSize[allDf$HERD == "Grey_River"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Gaff_Topsails"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Pothill"] <- "B) Medium"
allDf$HSize[allDf$HERD == "St_Anthony"] <- "B) Medium"
allDf$HSize[allDf$HERD == "Lapoile"] <- "C) Large"
allDf$HSize[allDf$HERD == "Middle_Ridge"] <- "C) Large"

## summary stats on herd size
b1 <- allDf[, mean(size), by = .(HERD,phase)]
b2 <- allDf[, CI(size)[3], by = .(HERD,phase)]
b3 <- allDf[, CI(size)[1], by = .(HERD,phase)]
cbind(b1, b2$V1, b3$V1)

## summary stats on typical group size
allDf[, mean(typicalGS, na.rm = TRUE), by = .(HERD,phase)]
#c2 <- allDf[, CI(typicalGS)[3], by = .(HERD,phase)]
#c3 <- allDf[, CI(typicalGS)[1], by = .(HERD,phase)]

allDf[, mean(typicalGS, na.rm = TRUE), by = .(phase)]

## summary stats on mean group size
d1 <- allDf[, mean(meanGS), by = .(HERD, phase)]
d2 <- allDf[, CI(meanGS)[3], by = .(HERD,phase)]
d3 <- allDf[, CI(meanGS)[1], by = .(HERD,phase)]
cbind(d1, d2$V1, d3$V1)
allDf$Year <- as.factor(allDf$Year)

### number of surveys per herd
aa <- allDf[, .N, by = c("HERD", "season", "year")]
aa[, sum(N), by = "HERD"]

## number of surveys per herd within decline phase
bb <- allDf[season != "Autumn"][phase == "2001-2013"][, .N, by = c("HERD", "season")]
bb[, sum(N), by = "season"]


### MEAN GROUP SIZE ###
## Small herds:
s1 <- lmer(round(meanGS,0) ~ log(size) +  (1|Year),
                 #family = "nbinom1", 
                 data = allDf[HSize == "A) Small"])
summary(s1)
car::Anova(s1)

## Medium herds:
m1 <- lmer(round(meanGS,0) ~ log(size) + (1|Year),
              #family = "nbinom1", 
              data = allDf[HSize == "B) Medium"])
summary(m1)
car::Anova(m1)

## large herds:
l1 <- lmer(round(meanGS,0) ~ log(size) + (1|Year),
              #family = "nbinom1", 
              data = allDf[HSize == "C) Large"])

summary(l1)
car::Anova(l1)


### TYPICAL GROUP SIZE ###
## Small herds:
s2 <- lmer(round(typicalGS,0) ~ log(size) +  (1|Year),
              #family = "nbinom1", 
              data = allDf[HSize == "A) Small"])
summary(s2)
car::Anova(s2)

## Medium herds:
m2 <- lmer(round(typicalGS,0) ~ log(size) + (1|Year),
              #family = "nbinom1", 
              data = allDf[HSize == "B) Medium"])
summary(m2)
car::Anova(m2)

## large herds:
l2 <- lmer(round(typicalGS,0) ~ log(size) + (1|Year),
              #family = "nbinom1", 
              data = allDf[HSize == "C) Large"])
summary(l2)
car::Anova(l2)
