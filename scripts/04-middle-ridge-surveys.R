

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'raster', 'dplyr', 'sf',
          'adehabitatHR')
lapply(libs, require, character.only = TRUE)

## load Fifield et al. data
gs <- readRDS("input/group-size-spatial.RDS")

utm21N <- '+proj=utm +zone=21 ellps=WGS84'
setDT(gs)[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(longitude, latitude), utm21N))]
gs$id <- rownames(gs)

## generate transects
df <- data.table(id = c(rep("-37", 1334),rep("-36", 1334),rep("-35", 1334),rep("-34", 1334),
                        rep("-33", 1334),rep("-32", 1334),rep("-31", 1334),rep("-30", 1334),
                        rep("-29", 1334),rep("-28", 1334),rep("-27", 1334),rep("-26", 1334),
                        rep("-25", 1334),rep("-24", 1334),rep("-23", 1334),rep("-22", 1334),
                        rep("-21", 1334),rep("-20", 1334),rep("-19", 1334),rep("-18", 1334),
                        rep("-17", 1334),rep("-16", 1334),rep("-15", 1334),rep("-14", 1334),
                        rep("-13", 1334),rep("-12", 1334),rep("-11", 1334),rep("-10", 1334),
                        rep("-9", 1334),rep("-8", 1334),rep("-7", 1334),rep("-6", 1334), 
                        rep("-5", 1334),rep("-4", 1334),rep("-3", 1334),rep("-2", 1334),rep("-1", 1334),
                        rep("1", 2167),rep("2", 2167),rep("3", 2167),rep("4", 2167), 
                        rep("5", 2167),rep("6", 2167),rep("7", 2167),rep("8", 2167), 
                        rep("9", 2167),rep("10", 2167),rep("11", 2167),rep("12", 2167), 
                        rep("13", 2167),rep("14", 2167),rep("15", 2167),rep("16", 2167), 
                        rep("17", 2167),rep("18", 2167),rep("19", 2167),rep("20", 2167),
                        rep("21", 2167),rep("22", 2167),rep("23", 2167),rep("24", 2167), 
                        rep("25", 2167),rep("26", 2167),rep("27", 2167),rep("28", 2167), 
                        rep("29", 2167),rep("30", 2167),rep("31", 2167),rep("32", 2167),
                        rep("33", 2167),rep("34", 2167),rep("35", 2167),rep("36", 2167)),
                 NORTHING = c(rep(5272000, 1334),rep(5273000, 1334),rep(5274000, 1334),
                              rep(5275000, 1334),rep(5276000, 1334),rep(5277000, 1334),
                              rep(5278000, 1334),rep(5279000, 1334),rep(5280000, 1334),
                              rep(5281000, 1334),rep(5282000, 1334),rep(5283000, 1334),
                              rep(5284000, 1334),rep(5285000, 1334),rep(5286000, 1334),
                              rep(5287000, 1334),rep(5288000, 1334),rep(5289000, 1334),
                              rep(5290000, 1334),rep(5291000, 1334),rep(5292000, 1334),
                              rep(5293000, 1334),rep(5294000, 1334),rep(5295000, 1334),
                              rep(5296000, 1334),rep(5297000, 1334),rep(5298000, 1334),
                              rep(5299000, 1334),rep(5300000, 1334),rep(5301000, 1334),
                              rep(5302000, 1334),rep(5303000, 1334),rep(5304000, 1334),
                              rep(5305000, 1334),rep(5306000, 1334),rep(5307000, 1334),
                              rep(5308000, 1334),rep(5310000, 2167),rep(5312000, 2167),
                              rep(5314000, 2167),rep(5316000, 2167),rep(5318000, 2167),
                              rep(5320000, 2167),rep(5322000, 2167),rep(5324000, 2167),
                              rep(5326000, 2167),rep(5328000, 2167),rep(5330000, 2167),
                              rep(5332000, 2167),rep(5334000, 2167),rep(5336000, 2167),
                              rep(5338000, 2167),rep(5340000, 2167),rep(5342000, 2167), 
                              rep(5344000, 2167),rep(5346000, 2167),rep(5348000, 2167),
                              rep(5350000, 2167),rep(5352000, 2167),rep(5354000, 2167),
                              rep(5356000, 2167),rep(5358000, 2167),rep(5360000, 2167),
                              rep(5362000, 2167),rep(5364000, 2167),rep(5366000, 2167), 
                              rep(5368000, 2167),rep(5370000, 2167),rep(5372000, 2167),
                              rep(5374000, 2167),rep(5376000, 2167), rep(5378000, 2167),
                              rep(5380000, 2167)),
                 EASTING   = c(rep(seq(625000, 665000, 30), 37),
                               rep(seq(600000, 665000, 30), 36)))

saveRDS(df, "output/transect-lines.RDS")

## randomly selected 474*2 points from along transects to assign zeroes
df2 <- df[sample(nrow(df), length(gs$ID)*100), ]
df2$group.size <- 0


gs2 <- rbind(gs, df2, fill = T)

### load raster
lc <- raster('input/NL landcover/Landcover/FINAL_RC.tif')
legend <- fread('input/landcover/Legend.csv')[Value != 10]

### # CRS
utm <- st_crs('+proj=utm +zone=21 ellps=WGS84')

bb <- c(
  xmin = -55.75,
  ymin = 48.75,
  xmax = -54.75,
  ymax = 47.5
)

dtbb <- data.table(x = c(bb[['xmin']], bb[['xmax']]),
                   y = c(bb[['ymin']], bb[['ymax']]))

# Project and buffer out for clarity
buf <- 3e4
utmBB <- data.table(dtbb[, project(cbind(x, y), utm$proj4string)])

# crop raster
lc <- crop(lc, utmBB)

# Focal rasters -----------------------------------------------------------
focals <- lapply(legend$Value, function(val) {
  subs(lc, legend[, .(Value, Value == val)])
})
names(focals) <- legend$Value


# Combine rasters ---------------------------------------------------------
# Combine land cover types using the Value numbers from the legend
open <- Reduce('+', focals[c(1, 6, 7, 8, 9)])
closed <- Reduce('+', focals[c(2, 3, 4, 5)])


# Proportion of habitat in buffer -----------------------------------------
# Set buffer size
buff <- 200

weight <- focalWeight(lc, d = buff, type = 'circle')

openFocal <- focal(open, weight, na.rm = TRUE, pad = TRUE, padValue = 0)
closedFocal <- focal(closed, weight, na.rm = TRUE, pad = TRUE, padValue = 0)

# Proportion of habitat at each relocation
gs2[, propOpen := extract(openFocal, matrix(c(EASTING, NORTHING), ncol = 2))]
gs2[, propClosed := extract(closedFocal, matrix(c(EASTING, NORTHING), ncol = 2))]

# Extract point land cover ------------------------------------------------
gs2[, Value := raster::extract(lc, matrix(c(EASTING, NORTHING), ncol = 2))]

## convert 1's to habitats 
gs2$Value[gs2$Value == "1"] <- "Wetland"
gs2$Value[gs2$Value == "2"] <- "BroadLeaf"
gs2$Value[gs2$Value == "3"] <- "Conifer Forest"
gs2$Value[gs2$Value == "4"] <- "Conifer Scrub"
gs2$Value[gs2$Value == "5"] <- "Mixed Wood"
gs2$Value[gs2$Value == "6"] <- "Rocky"
gs2$Value[gs2$Value == "7"] <- "Water"
gs2$Value[gs2$Value == "8"] <- "Lichen"
gs2$Value[gs2$Value == "9"] <- "Anthro"

gs2[Value == "Conifer Forest" | Value == "Conifer Scrub" | 
    Value == "Mixed Wood", 
    habitat := "closed"][Value == "Rocky" |  Value == "Wetland" | 
                               Value == "Lichen", habitat := "open"][Value == "Water" | Value == "Anthro" |
                                                                             Value == "BroadLeaf", habitat := NA]
## remove NAs
gs2 <- gs2[!is.na(habitat)]

saveRDS(gs2, "output/group-size-rdm-for-map.RDS")

