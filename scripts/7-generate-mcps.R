

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 
          'spatsoc', 'igraph', 'gridExtra', 'adehabitatHR')
lapply(libs, require, character.only = TRUE)

habitat <- fread("output/herd-survey-locs-data.csv")

CS <- habitat[HERD == "Cape_Shore"]
AV <- habitat[HERD == "Avalon"]


utm21N <- '+proj=utm +zone=21 ellps=WGS84'

coords <- c('EASTING', 'NORTHING')

CS_MCP <- SpatialPointsDataFrame(CS[, ..coords],
                                     proj4string = CRS(utm21N),
                                     data = CS)
CS_MCP_poly <- mcp(CS_MCP, percent = 95)

#AV <- AV[NORTHING < 5200000]

AV_MCP <- SpatialPointsDataFrame(AV[, ..coords],
                                 proj4string = CRS(utm21N),
                                 data = AV)
AV_MCP_poly <- mcp(AV_MCP, percent = 95)


plot(AV_MCP_poly)

saveRDS(AV_MCP_poly, "input/AV.mcp.RDS")
saveRDS(CS_MCP_poly, "input/CS.mcp.RDS")
