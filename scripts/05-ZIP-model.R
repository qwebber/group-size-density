

### Packages ----
libs <- c('data.table', 'ggplot2', 'pscl')
lapply(libs, require, character.only = TRUE)

gs2 <- readRDS("output/group-size-rdm-for-map.RDS")


## run single ZIP Model
## select random points
df2 <- rbind(gs2[group.size > 0], 
             gs2[group.size == 0][sample(nrow(gs2[group.size == 0]), length(gs2[group.size > 0]$ID))])
summary(z1 <- zeroinfl(group.size ~ propOpen, data = df2))
summary(p1 <- glm(group.size ~ propOpen, family = poisson, data = df2))
AIC(z1, p1)

n <- 1000
out <- c()

for(i in 1:n){ 
  ## randomly selected 417 groups to be paired with the observed groups
  df1 <- rbind(gs2[group.size > 0], 
               gs2[group.size == 0][sample(nrow(gs2[group.size == 0]), length(gs2[group.size > 0]$ID))])
  
  ## run ZIP model and compare to GLM
  m1 <- zeroinfl(group.size ~ habitat, data = df1)
  
  out[[i]] <- data.table(coef(m1))
  
}

out2 <- rbindlist(out)
out2$coef <- rep(c("count_intercept", 
                   "count_habitatopen",
                   "zero_intercept",
                   "zero_habitatopen"), n)

hist(out2[coef == "zero_habitatopen"]$V1)

## model comparison
df2 <- rbind(gs2[group.size > 0], 
             gs2[group.size == 0][sample(nrow(gs2[group.size == 0]), length(gs2[group.size > 0]$ID))])
summary(z1 <- zeroinfl(group.size ~ habitat, data = df2))
summary(p1 <- glm(group.size ~ habitat, family = poisson, data = df2))
AIC(z1, p1)

fwrite(out2, "output/zip-coefficients.csv")
