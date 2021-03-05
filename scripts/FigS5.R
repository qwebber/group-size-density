

## load libraries
library(data.table)
library(ggplot2)
library(sf)



df2 <- readRDS("output/group-size-rdm-for-map.RDS")



## load spatial data
utm <- '+proj=utm +zone=21 ellps=WGS84'
nlBounds <- rgdal::readOGR('../maps-in-gg/input/NL/NL-Bounds.shp') %>% 
  spTransform(CRSobj = utm)

nlBounds2 <- crop(nlBounds, utmBB)
nlBounds2 <- st_as_sf(nlBounds2)

# Colors
watercol <- '#c3e2ec'
islandcol <- '#d0c2a9'
coastcol <- '#82796a'
gridcol <- '#323232'

# Themes 
themeMap <- theme(legend.position = 'none',
                  plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), 
                  legend.key = element_blank(),
                  panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = watercol), 
                  panel.grid = element_line(color = gridcol, size = 0.2),
                  axis.text.y = element_text(size = 11, color = 'black'),
                  axis.text.x = element_text(angle = 45, hjust = 1, 
                                             size = 11, color = 'black'), 
                  axis.title = element_blank())



png("graphics/FigS5.png", width = 4000, height = 4000, res = 600, units = "px")
ggplot() +
  geom_sf(data = nlBounds2[1], fill = islandcol) +
  geom_point(data = df2[group.size > 0], aes(x = EASTING, y = NORTHING, size = group.size), alpha = 0.25) +
  geom_point(data = df2[group.size == 0], aes(x = EASTING, y = NORTHING), color = "blue", alpha = 0.25) +
  geom_line(data = df, aes(x = EASTING, y = NORTHING, group = id), color = "black", alpha = 0.5) +
  lims(y = c(min(df2$NORTHING)-1000, max(df2$NORTHING)+1000),
       x = c(min(df2$EASTING)-1000, max(df2$EASTING))) +
  themeMap
dev.off()
