
library(sf)
library(tmap)
library(purrr)
library(dplyr)
library(ncdf4)
library(tidyr)
library(raster)
library(tibble)
library(stringr)

# air temperature
tmp_mean <- nc_open("data/gridMET /tmmn_2014.nc")
tmp_max <- nc_open("data/gridMET /tmmx_2014.nc")

#temp_coords <- data.frame("lon" = tmp_mean$dim$lon$vals,
#                          "lat" = tmp_mean$dim$lat$vals,
#                          "day" = tmp_mean$dim$day$vals)
#head(example_nc$dim$day$group_id)
#head(example_nc$dim$lat$vals)

lon <- ncvar_get(tmp_mean, "lon")
lat <- ncvar_get(tmp_mean, "lat", verbose = F)
t <- ncvar_get(tmp_mean, "day")
#example_nc$dim$day$units
#as.Date(as.POSIXct(example_nc$dim$day$vals, origin = "1900-01-01", tz = "America/Denver"))
#range(example_nc$dim$day$vals)
#class(example_nc$dim$day)
#as.POSIXct(41638, origin = "1900-01-01", tz = "America/Denver")

temp <- ncvar_get(tmp_mean, "air_temperature")

fillvalue <- ncatt_get(tmp_mean, "air_temperature", "_FillValue")

#nc_close(example_nc) # done with the file, have the data

# working with the data
temp[temp == fillvalue$value] <- NA
temp.slice <- temp[, , 365]
#dim(temp.slice)

# create matrix
lon_lat_t <- as.matrix(expand.grid(lon, lat, t))
temp_long <- as.vector(temp)
#length(temp_long)

temp_obs <- data.frame(cbind(lon_lat_t, temp_long))

temp_obs_ut <- temp_obs %>%
  filter(Var1 < -108) %>%
  filter(Var1 > -115) %>%
  filter(Var2 > 36) %>% 
  filter(Var2 < 43)


temp_obs_ut$day <- temp_obs_ut$Var3 - min(temp_obs_ut$Var3 - 1)
#summary(temp_obs_ut$day)

#length(unique(temp_obs_ut$Var2))

# only populated areas
populated_ids <- read.csv("/Users/brenna/Documents/School/Research/aqs-inequities/aqs_inequities/populated_ids.csv")
populated_ids <- populated_ids[, c("x")]

no2 <- read.csv("data/aq/NO2Grid.csv")
no2_shp <- st_read("data/shapefiles/NO2Grid(Polygons).shp")

no2_grid <- merge(no2_shp, no2, by = "grid_id")
no2_grid_pop <- no2_grid %>%
  filter(grid_id %in% populated_ids)

#tm_shape(no2_grid_pop) +
#  tm_polygons(col = NA)

names(temp_obs_ut) <- c("lon", "lat", "date", "temp_k", "day")

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp_ut <- st_as_sf(temp_obs_ut, coords = c("lon", "lat"),
                    crs = projcrs)

temp_ut_1 <- temp_ut %>%
  filter(day == 1)
#tm_shape(temp_pts_in_grid) +
  #tm_dots(col = NA)

pts_grid <- st_nearest_feature(no2_grid_pop, temp_ut_1, pairwise = FALSE)

no2_grid_pop <- cbind(no2_grid_pop, pts_grid)
head(no2_grid_pop)

tm_shape(no2_grid_pop) +
  tm_polygons(col = "pts_grid", lwd = 0, style = "cont")

length(unique(no2_grid_pop$pts_grid))
str(temp_ut_1)

temp_ut_1$id <- rep(1:nrow(temp_ut_1))
table(temp_ut_1$id %in% no2_grid_pop$pts_grid)
summary(temp_ut_1$id)

temp_obs_ut$id <- rep(1:nrow(temp_ut_1),
                      length.out = nrow(temp_obs_ut))
summary(temp_obs_ut$id)
temp_obs_ut <- temp_obs_ut %>%
  filter(id %in% no2_grid_pop$pts_grid)
names(temp_obs_ut)
temp_dat <- merge(temp_obs_ut, no2_grid_pop, 
              by.y = "pts_grid", by.x = "id",
              all.x = TRUE)
length(unique(temp_obs_ut$grid_id))
summary(temp_dat)
day_1 <- temp_dat %>%
  filter(day == 1)

tm_shape(day_1) +
  tm_polygons(col = "temp_k", lwd = 0, style = "cont")

temp_dat <- temp_dat[order(temp_dat$day, decreasing = FALSE), ]

temp_dat$week <- rep(1:ceiling(nrow(temp_dat)/7), length.out = nrow(temp_dat), each = 459214) # ngrid * ndays

temp_data <- temp_dat[, c("grid_id", "temp_k", "week")]
temp_data <- st_drop_geometry(temp_data)
rownames(temp_data) <- as.character(temp_data[, 1])
temp_by_weeks <- split(temp_data, temp_dat$week)



fx <- function(x) {
  c(min = min(x), mean = mean(x), med = median(x), max = max(x))
}

str(temp_by_weeks$`1`)
temp_by_weeks$`1`$temp_k
temp_by_weeks[[1]]$temp_k
lapply(temp_by_weeks$`1`$temp_k, fx)

movies_lower <-unlist(lapply(movies,tolower))

tapply(temp_by_weeks$`1`$temp_k, temp_by_weeks$`1`$grid_id, FUN = fx)

temp_stats <- unlist(tapply(temp_by_weeks$`1`$temp_k, temp_by_weeks$`1`$grid_id, FUN = fx))#temp_by_weeks[[1]], fx))
temp_stats <- data.frame(t(sapply(temp_stats, function(x) x[1:max(lengths(temp_stats))])))
temp_stats <- t(temp_stats)
temp_stats$grid_fx <- tibble::rownames_to_column(temp_stats, "grid_fx")
names(temp_stats) <- c("grid_fx", "temp")

vex <- as.data.frame(str_split_fixed(temp_stats$grid_fx, "\\.", 2))
names(vex) <- c("grid", "stat")
vex$grid <- gsub("X", "", vex$grid)
temp <- as.data.frame(cbind(vex$grid,
                            vex$stat,
                            temp_stats$temp))

#test <- temp %>%
  #pivot_wider(names_from = stat, values_from = c(1:459214))
#length(unique(temp_dat$grid_id))
#r_test <- rbind(temp_stats, t(temp_dat$grid_id))
#head(temp_stats[c(1:5), c(1:10)])
#length(unique(temp_by_weeks$`1`$grid_id))


system.time(
  for(i in 2:53) {#53
    #t <- as.data.table(lapply(temp_by_weeks[[i]]$temp_k, fx))
    #t$stat <- c("min", "mean", "med", "max")
    t <- unlist(tapply(temp_by_weeks[[i]]$temp_k, temp_by_weeks[[i]]$grid_id, FUN = fx))#temp_by_weeks[[1]], fx))
    t <- data.frame(t(sapply(t, function(x) x[1:max(lengths(t))])))
    t <- as.data.frame(t(t))
    t <- tibble::rownames_to_column(t, "grid_fx")
    names(t) <- c("grid_fx", "temp")
    vex <- as.data.frame(str_split_fixed(t$grid_fx, "\\.", 2))
    names(vex) <- c("grid", "stat")
    vex$grid <- gsub("X", "", vex$grid)
    t <- cbind(vex$grid,
               vex$stat,
               temp_stats$temp)
    #t <- t %>%
    #  pivot_wider(names_from = stat, values_from = c(1:459214))
    test_x <- rbind(temp, t)
  }
)
pm_10_stats$week <- paste0("week_", rep(1:nrow(pm_10_stats)))

pm_10_all_t <- transpose(pm_10_stats)
rownames(pm_10_all_t) <- colnames(pm_10_stats)
colnames(pm_10_all_t) <- pm_10_stats$week
pm_10_all_t <- pm_10_all_t[-1, ]

pm_10_all_t$cell <- str_split_fixed(rownames(pm_10_all_t), "_", 2)[, 1]
pm_10_all_t$stat <- str_split_fixed(rownames(pm_10_all_t), "_", 2)[, 2]
write.csv(pm_10_all_t, "pm_10_all_t.csv")






head(test)
length(unique(temp_obs_ut$id))
nrow(temp_obs_ut)
length(unique(no2_grid_pop$pts_grid))
nrow(no2_grid_pop)

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

test <- st_nearest_feature(no2_grid_pop, temp_ut, pairwise = TRUE)

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
