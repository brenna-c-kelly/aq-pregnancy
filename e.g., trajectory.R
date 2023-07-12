
 #e.g., trajectory

quinn <- c("20180208", "1837785") #bdate, grid_id


coords <- st_point(c(40.39239587671283, -111.7893768113126))
coords$geom <- c("POINT (40.39239587671283, -111.7893768113126)")
names(coords) <- c("x", "y", "geom")
coords <- as.data.frame(coords)
coords <- st_as_sf(coords, coords = c("y", "x"), crs = projcrs, agr = "constant")
no2_grid <- st_transform(no2_grid, st_crs(projcrs))

af_grid <- st_join(coords, no2_grid)
af_grid

49049
library(tidycensus)
af_city <- get_decennial(geography = "county", variables = c("P001001"), 
                         year = 2010, state = "UT", geometry = TRUE) %>%
  filter(GEOID == "49049")

af_city <- st_transform(af_city, st_crs(projcrs))
city_grid <- st_intersection(no2_grid, af_city)
af_grid
city_grid

str(no2_2010)

af_grid_ids <- city_grid$grid_id
max(af_grid_ids)

## try a little merge > pregnancy to aq
city_grid # grid (just Utah County)
af_city # grid of pregnancy
pm25_t # PM2.5 2010
head(pm25_t)



head(no2_10_all_t)

rbind(no2_10_all_t[2:5]


