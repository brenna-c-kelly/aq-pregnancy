
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

pop <- get_decennial(geography = "block", variables = c("P001001"), 
                     year = 2010, state = "UT", geometry = TRUE)
names(pop) <- tolower(names(pop))

pop_bb <- get_decennial(geography = "state", variables = c("P001001"), 
                        year = 2010, state = "UT", geometry = TRUE)

tm_shape(pop) +
  tm_polygons(col = "value", lwd = 0, style = "cont")

# filtering only grids with population > 0
pop_ulated <- pop %>%
  filter(value > 0)

# linking to block_xwalk
pop_xwalk <- merge(pop_ulated, block_xwalk_ut, by.x = c("geoid"), by.y = c("blockid00"))
# one-to-many (pop has multiple block_xwalk matches)
# this just exclude the grids without any population overlap

1 - (nrow(pop_xwalk) / nrow(block_xwalk_ut)) # excludes 72% of the grids

length(unique(pop_xwalk$ur))
head(pop_xwalk)
head(no2_dat)
summary(block_xwalk_ut$x)
summary(no2_dat$long.x)

summary(no2_dat$lat.x)
summary(block_xwalk_ut$y)

head(no2_dat)
head(block_xwalk_ut)

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
tm_shape(block_xwalk_ut_sf) +
  tm_dots(col = "x", style = "cont")

gc <- st_read("/Users/brenna/Downloads/poly_gc14_conus_810m_top.shp")
st_crs(gc) <- projcrs
head(gc)

# filter only grids in Utah
pop_bb <- st_transform(pop_bb, st_crs(gc))
gc_crop <- st_crop(gc, pop_bb)

st_write(gc_crop, "data/shapefiles/gc_crop.shp")
#gc_crop <- st_read("/data/shapefiles/gc_crop.shp")
tm_shape(pop_bb) +
  tm_polygons()

no2_points <- st_drop_geometry(no2_shp)
no2_points <- st_as_sf(no2_points, coords = c("long", "lat"), # coodinates of centroid
                       crs = projcrs, agr = "constant")
pop_ulated <- st_transform(pop_ulated, st_crs(no2_points))

tm_shape(pts_in_gc) +
  tm_dots(col = "grid_id", style = "cont") # works

pts_in_gc <- st_join(no2_points, no2_shp, join = st_within)
pts_in_gc <- st_join(no2_points, pop_ulated, join = st_within) %>%
  filter(!is.na(value))
populated_ids <- pts_in_gc$grid_id
populated_ids <- st_drop_geometry(populated_ids)
write.csv(populated_ids, "populated_ids.csv")

# points within grid cell
pts_in_gc <- st_join(no2_points, gc_crop, join = st_within)
head(pts_in_gc)

st_write(pts_in_gc, "data/shapefiles/pts_in_gc.shp")

# try: merge gc, no2_dat; then merge with crosswalk points?
#test <- st_equals(gc, no2_dat)



tm_shape()

st_crs(no2_dat)
st_crs(block_xwalk_ut_sf)




# prepare for merge with grid
gsub('a{2,}', '.', block_xwalk_ut$x)
block_xwalk_ut$x_test <- gsub('a{, 2}', '.', block_xwalk_ut$x)
head(block_xwalk_ut)
test_1 <- block_xwalk_ut
test_2 <- no2_dat
test_2
test_1$lat_long <- paste(test_1$x, test_1$y, sep = ", ")
test_2$long.x <- round(test_2$long.x, 1)
test_2$long.x <- as.character(test_2$long.x)
test_2$long.x <- gsub(test_2$long.x, ".", "")

test_2$lat.x <- round(test_2$lat.x, 2)
test_2$lat_long <- paste(test_2$long.x, test_2$lat.x, sep = ", ")

head(test_2$lat_long)
head(test_1$lat_long)

test <- merge(test_1, test_2, by = c("lat_long"))
head(test)
