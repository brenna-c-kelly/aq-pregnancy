
library(sf)
library(ncdf4)
library(raster)

# air temperature
tmp_mean <- nc_open("data/gridMET /tmmn_2014.nc")
tmp_max <- nc_open("data/gridMET /tmmx_2014.nc")

str(example_nc)

temp_coords <- data.frame("lon" = example_nc$dim$lon$vals,
                          "lat" = example_nc$dim$lat$vals,
                          "day" = example_nc$dim$day$vals)
head(example_nc$dim$day$group_id)
head(example_nc$dim$lat$vals)

lon <- ncvar_get(example_nc, "lon")
lat <- ncvar_get(example_nc, "lat", verbose = F)
t <- ncvar_get(example_nc, "day")
example_nc$dim$day$units
as.Date(as.POSIXct(example_nc$dim$day$vals, origin = "1900-01-01", tz = "America/Denver"))
range(example_nc$dim$day$vals)
class(example_nc$dim$day)
as.POSIXct(41638, origin = "1900-01-01", tz = "America/Denver")

temp <- ncvar_get(example_nc, "air_temperature")

fillvalue <- ncatt_get(example_nc, "air_temperature", "_FillValue")

nc_close(example_nc) # done with the file, have the data

# working with the data
temp[temp == fillvalue$value] <- NA
temp.slice <- temp[, , 365]
dim(temp.slice)

# create matrix
lon_lat_t <- as.matrix(expand.grid(lon, lat, t))
temp_long <- as.vector(temp)
length(temp_long)

temp_obs <- data.frame(cbind(lon_lat_t, temp_long))

temp_obs_ut <- temp_obs %>%
  filter(Var1 < -108) %>%
  filter(Var1 > -115) %>%
  filter(Var2 > 36) %>% 
  filter(Var2 < 43)


temp_obs_ut$day <- temp_obs_ut$Var3 - min(temp_obs_ut$Var3 - 1)
summary(temp_obs_ut$day)

length(unique(temp_obs_ut$Var2))

# only populated areas
populated_ids <- read.csv("/Users/brenna/Documents/School/Research/aqs-inequities/aqs_inequities/populated_ids.csv")
populated_ids <- populated_ids[, c("x")]

no2 <- read.csv("data/aq/NO2Grid.csv")
no2_shp <- st_read("data/shapefiles/NO2Grid(Polygons).shp")

no2_grid <- merge(no2_shp, no2, by = "grid_id")
no2_grid_pop <- no2_grid %>%
  filter(grid_id %in% populated_ids)

tm_shape(no2_grid_pop) +
  tm_polygons(col = NA)

names(temp_obs_ut) <- c("lon", "lat", "date", "temp_k", "day")

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp_ut <- st_as_sf(temp_obs_ut, coords = c("lon", "lat"),
                    crs = projcrs)

temp_ut_1 <- temp_ut %>%
  filter(day == 1)
tm_shape(temp_pts_in_grid) +
  tm_dots(col = NA)


# make temp points into grid
box <- st_buffer(temp_ut_1, 4000) %>%
  st_bbox() %>%
  st_as_sfc()

grid <- st_make_grid(temp_ut_1, cellsize = c(diff(st_bbox(temp_ut_1)[c(1, 3)]), 
                             diff(st_bbox(temp_ut_1)[c(2, 4)]))/n, 
             offset = st_bbox(temp_ut_1)[1:2], n = c(140, 109), 
             what = "centers")

# make a 10x10 grid of the bounding box
grid <- st_make_grid(box, n = c(560, 435))
ggplot() +
  geom_sf(data = grid, fill = NA, color = 'black')

# points within grid cell
temp_pts_in_grid <- st_join(temp_ut, no2_grid_pop, join = st_within) %>%
  filter(!is.na(grid_id))
head(temp_pts_in_grid)
table(is.na(temp_pts_in_grid$grid_id))

summary(temp_pts_in_grid)



length(unique(temp_obs_ut$Var3))
plot(aggregate(temp_obs_ut$temp_long, by = list(temp_obs_ut$Var3), FUN = mean))

plot(temp_obs_ut$Var1, temp_obs_ut$Var2, colour = temp_obs_ut$Var3)

names(temp_obs_ut)

test <- temp_obs_ut[which(temp_obs_ut$Var3 == 41990), ]

r <- raster(t(test),# xmn=min(test$Var1), xmx=max(test$Var1), 
            #ymn=min(test$Var2), ymx=max(test$Var2), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r)

summary(temp_obs_ut$Var3)
#plot
r <- raster(t(temp.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r)

# only get UT
temp_ut <- cbind(lon[1:1386], lat)

plot(lat[1:585], lon[1:1386])

library(RColorBrewer)
cols = brewer.pal(4, "Blues")
# Define colour pallete
coul <- brewer.pal(4, "PuOr") 

# Add more colors to this palette :
coul <- colorRampPalette(coul)(25)

coords <- expand.grid(lon[1:1386], lat[1:585])

plot(coords$Var1, coords$Var2, col = coul)

ggplot(temp_obs_ut, aes(Var1, Var2)) +
  geom_point(aes(color = Var3), size = 2)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
str(temp_ut)
temp_ut <- st_as_sf(temp_ut, coords = c())

block_xwalk_ut_sf <- st_as_sf(block_xwalk_ut, coords = c("long", "lat"), 
                              crs = projcrs, agr = "constant")

pop_bb


dim(temp)

example_nc$var$air_temperature
example_nc$var$air_temperature
example_nc$dim$lon$vals

save(res, file = "res.RData")
