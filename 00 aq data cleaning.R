
library(sf)
library(tmap)

no2 <- read.csv("data/aq model/NO2Grid.csv")
no2_shp <- st_read("data/shapefiles/NO2Grid(Polygons).shp")

no2_grid <- merge(no2_shp, no2, by = "grid_id")

tm_shape(no2_grid) +
  tm_polygons(col = "long.x", lwd = 0, style = "cont")


no2_test <- readRDS("/Users/brenna/Downloads/GridSite_UT_NO2.rds")

