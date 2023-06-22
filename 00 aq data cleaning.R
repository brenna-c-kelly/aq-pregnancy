
library(sf)
library(tmap)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

no2 <- read.csv("data/aq/NO2Grid.csv")
no2_shp <- st_read("data/shapefiles/NO2Grid(Polygons).shp")

no2_grid <- merge(no2_shp, no2, by = "grid_id")

#tm_shape(no2_grid) +
#  tm_polygons(col = "long.x", lwd = 0, style = "cont")


#no2_test <- readRDS("data/aq/GridSite_UT_NO2.rds")
#readRDS("data/aq/GridSite_UT_O3.rds")
#read.csv("/Users/brenna/Documents/School/Research/aq-pregnancy/data/aq/PM25Grid.csv")

#head(no2_test)

##      st_read("/Users/brenna/Downloads/NO2Grid_Edited.shp")


no2_100101 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100101.rds")
no2_100102 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100102.rds")
no2_100103 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100103.rds")
no2_100104 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100104.rds")
no2_100105 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100105.rds")
no2_100106 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100106.rds")
no2_100107 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100107.rds")
no2_100108 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100108.rds")
no2_100109 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100109.rds")
no2_100110 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100110.rds")
no2_100111 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100111.rds")
no2_100112 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100112.rds")
no2_100113 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100113.rds")
no2_100114 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/NO2/Daily/2010/20100114.rds")

test <- as.data.frame(rbind(no2_100101, 
                            no2_100102, 
                            no2_100103,
                            no2_100104, 
                            no2_100105, 
                            no2_100106,
                            no2_100107, 
                            no2_100108, 
                            no2_100109,
                            no2_100110, 
                            no2_100111, 
                            no2_100112,
                            no2_100113, 
                            no2_100114))


test_t <- transpose(test)
rownames(test_t) <- colnames(test)
colnames(test_t) <- rownames(test)


no2_dat <- cbind(no2_grid, test_t)

no2_dat
test <- no2_dat[c(1:10000), ]



tm_shape(test) +
  tm_polygons(col = "grid_id", lwd = 0, style = "cont")





no2_10_files <- list.files("data/Grid_1km_UT/NO2/Daily/2010/")

no2_10_all <- readRDS(paste0("data/Grid_1km_UT/NO2/Daily/2010/", no2_10_files[1]))

no2_10_all <- as.data.frame(no2_10_all)
#no2_10_all_t <- transpose(no2_10_all)
#rownames(no2_10_all_t) <- colnames(no2_10_all)
#colnames(no2_10_all_t) <- "X"

no2_10_all <- no2_10_all_t

system.time(
for(i in 1:14){#length(no2_10_files)) {
  no2_10 <- readRDS(paste0(getwd(), "/data/Grid_1km_UT/NO2/Daily/2010/", no2_10_files[i]))
  no2_10 <- as.data.frame(no2_10)
  # *avg across weeks here*
  #no2_10_t <- transpose(no2_10)
  #rownames(no2_10_t) <- colnames(no2_10)
  #colnames(no2_10_t) <- substr(no2_10_files[i], start = 0, stop = nchar(no2_10_files[i])-4) #colname = date
  #no2_10 <- no2_10_t
  #names(no2_10) <- tolower(names(no2_10))
  no2_10_all <- rbind(no2_10_all, no2_10)
}
)
rownames(no2_10_all)

no2_10_sub <- no2_10_all[, c(1:10)]

no2_10_sub <- no2_10_all

no2_10_sub <- no2_10_sub %>% #group_by(personid) %>% 
  group_by(x = ceiling(row_number()/7)) %>%
  summarise_all(list(mean = mean, max = max, median = median))
no2_10_sub_t <- transpose(no2_10_sub)
rownames(no2_10_sub_t) <- colnames(no2_10_sub)
colnames(no2_10_sub_t) <- c("wk_1", "wk_2", "wk_3")

no2_10_sub_t$cell <- str_split_fixed(rownames(no2_10_sub_t), "_", 2)[, 1]
no2_10_sub_t$stat <- str_split_fixed(rownames(no2_10_sub_t), "_", 2)[, 2]
no2_10_sub_t <- no2_10_sub_t[-1, ]

no2_10_sub_t <- no2_10_sub_t %>%
  group_by(cell) %>%
  pivot_wider(names_from = stat, values_from = c(wk_1, wk_2, wk_3))

no2_10_sub_t

