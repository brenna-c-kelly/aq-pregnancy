
library(sf)
library(tmap)
library(dplyr)
library(stringr)
library(tidycensus)


# grid cells to population

block_xwalk <- read.csv("data/census_block_xwalk/CensusBlock2010_ConUS_810m.csv")
names(block_xwalk) <- tolower(names(block_xwalk))
block_xwalk$blockid00 <- str_pad(block_xwalk$blockid00, side = "left", width = 15, pad = "0")

block_xwalk$state_fips <- str_sub(block_xwalk$blockid00, 0, 2)

block_xwalk_ut <- block_xwalk %>%
  filter(state_fips == "49")
rm(block_xwalk)
block_xwalk_ut
# note: grids can overlap with 1-3 blocks

# linking to census shapefiles

# summary file

pop <- get_decennial(geography = "block", variables = c("P1_001N"),#"P001001"), 
                     year = 2020, #2010, 
                     state = "UT", geometry = TRUE)
names(pop) <- tolower(names(pop))

pop_bb <- get_decennial(geography = "state", variables = c("P1_001N"),#"P001001"), 
                        year = 2020, #2010, 
                        state = "UT", geometry = TRUE)

# tm_shape(pop) +
#   tm_polygons(col = "value", lwd = 0, style = "cont")

# filtering only grids with population > 0
pop_ulated <- pop %>%
  filter(value > 0)

# linking to block_xwalk
pop_xwalk <- merge(pop_ulated, block_xwalk_ut, by.x = c("geoid"), by.y = c("blockid00"))
# one-to-many (pop has multiple block_xwalk matches)
# this just exclude the grids without any population overlap

1 - (nrow(pop_xwalk) / nrow(block_xwalk_ut)) # excludes 72% of the grids for 2010; 83% for 2020

# summary(block_xwalk_ut$x)
# summary(no2_dat$long.x)
# 
# summary(no2_dat$lat.x)
# summary(block_xwalk_ut$y)
# 
# head(no2_dat)
# head(block_xwalk_ut)

block_xwalk_ut$long <- paste0(substr(block_xwalk_ut$x, 1, nchar(block_xwalk_ut$x)-1),
                              ".", substr(block_xwalk_ut$x, 5, 5))
block_xwalk_ut$lat <- paste0(substr(block_xwalk_ut$y, 1, 2),
                             ".", substr(block_xwalk_ut$y, 3, 4))

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
block_xwalk_ut_sf <- st_as_sf(block_xwalk_ut, coords = c("long", "lat"), 
                              crs = projcrs, agr = "constant")
#aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
#block_xwalk_ut_sf <- st_transform(block_xwalk_ut_sf, st_crs(aea))

summary(block_xwalk_ut_sf)
# tm_shape(block_xwalk_ut_sf) +
#   tm_dots(col = "x", style = "cont")

#gc <- st_read("/Users/brenna/Downloads/poly_gc14_conus_810m_top.shp")
#st_crs(gc) <- projcrs
#head(gc)


#st_write(gc_crop, "data/shapefiles/gc_crop.shp")
gc <- st_read("data/shapefiles/gc_crop.shp")

# filter only grids in Utah
pop_bb <- st_transform(pop_bb, st_crs(gc))
gc_crop <- st_crop(gc, pop_bb)

# tm_shape(pop_bb) +
#   tm_polygons()
no2_shp <- st_read("data/shapefiles/NO2Grid(Polygons).shp")
no2_points <- st_drop_geometry(no2_shp)
no2_points <- st_as_sf(no2_points, coords = c("long", "lat"), # coodinates of centroid
                       crs = projcrs, agr = "constant")
pop_ulated <- st_transform(pop_ulated, st_crs(no2_points))

pts_in_gc <- st_join(no2_points, pop_ulated, join = st_within) %>%
  filter(!is.na(value))

tm_shape(pts_in_gc) +
  tm_dots(col = "grid_id", style = "cont") # works


populated_ids <- pts_in_gc$grid_id
# populated_ids <- st_drop_geometry(populated_ids)
write.csv(populated_ids, "populated_ids_20.csv")#"populated_ids_10.csv")

pop_10 <- read.csv("populated_ids.csv")
pop_20 <- read.csv("populated_ids_20.csv")

populated_ids <- rbind(pop_10, pop_20) %>%
  filter(!duplicated(x))
populated_ids <- populated_ids$x

write.csv(populated_ids, "populated_ids.csv")



# x-ref 2000 w/ 2020, get most inclusive list of grids

v_dec_20 <- load_variables(2020, "pl", cache = TRUE)

pop <- get_decennial(geography = "block", variables = c("P1_001N"), 
                        year = 2020, state = "UT", geometry = TRUE)
names(pop) <- tolower(names(pop))

pop_bb <- get_decennial(geography = "state", variables = c("P001001"), 
                        year = 2010, state = "UT", geometry = TRUE)
rm(block_xwalk)
pop_ulated <- pop %>%
  filter(value > 0)

# linking to block_xwalk
pop_xwalk <- merge(pop_ulated, block_xwalk_ut, by.x = c("geoid"), by.y = c("blockid00"))

# filter only grids in Utah
pop_bb <- st_transform(pop_bb, st_crs(gc))
gc_crop <- st_crop(gc, pop_bb)

no2_points <- st_drop_geometry(no2_shp)
no2_points <- st_as_sf(no2_points, coords = c("long", "lat"), # coodinates of centroid
                       crs = projcrs, agr = "constant")
pop_ulated <- st_transform(pop_ulated, st_crs(no2_points))

pts_in_gc <- st_join(no2_points, pop_ulated, join = st_within) %>%
  filter(!is.na(value))
populated_ids <- pts_in_gc$grid_id
populated_ids <- st_drop_geometry(populated_ids)
write.csv(populated_ids, "populated_ids_20.csv")



# points within grid cell
pts_in_gc <- st_join(no2_points, gc_crop, join = st_within)
head(pts_in_gc)

st_write(pts_in_gc, "data/shapefiles/pts_in_gc.shp")

pop_ulated

# try: merge gc, no2_dat; then merge with crosswalk points?
#test <- st_equals(gc, no2_dat)


# raster?
library(exactextractr)

pop <- get_decennial(geography = "block", variables = c("P1_001N"), 
                     year = 2020, state = "UT", geometry = TRUE)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
pop <- st_transform(pop, st_crs(projcrs))

pop.raster <- raster()
extent(pop.raster) <- extent(pop)

pop.r <- rasterize(pop, pop.raster)

st_rasterize(pop)
plot(pop.r)

str(pop.raster)
pop.raster <- raster(nrows = length(unique(temp_obs_ut$lon)), ncols = unique(temp_obs_ut$lat), 
          xmn = min(temp_obs_ut$lon), xmx = max(temp_obs_ut$lon), 
          ymn = min(temp_obs_ut$lat), ymx = max(temp_obs_ut$lat),
          crs = projcrs)

r <- rasterize(cbind(temp_day), r, temp_day$temp_k, fun = median)

temp_by_cell <- exact_extract(pop, no2_grid, "sum") # mean is weighted by coverage_fraction

temp_grid <- cbind(temp_grid, temp_by_cell)

pop.raster <- raster()
str(temp_grid)
extent(pop) <- extent(temp_grid)
res(r.raster) <- 2500 # set cell size to 2500 metres

# Make a raster of the wetlands:
coastline.r <- rasterize(wets, r.raster)





tm_shape()

st_crs(no2_dat)
st_crs(block_xwalk_ut_sf)



