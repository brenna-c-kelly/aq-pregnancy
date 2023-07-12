
library(sf)
library(tmap)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

no2 <- read.csv("data/aq/NO2Grid.csv")
no2 <- read.csv("/Users/brenna/Downloads/NO2Grid_Edited.csv")
no2_shp <- st_read("data/shapefiles/NO2Grid(Polygons).shp")

no2_grid <- merge(no2_shp, no2, by = "grid_id")

#tm_shape(no2_grid) +
#  tm_polygons(col = "long.x", lwd = 0, style = "cont")


#no2_test <- readRDS("data/aq/GridSite_UT_NO2.rds")
#readRDS("data/aq/GridSite_UT_O3.rds")
#read.csv("/Users/brenna/Documents/School/Research/aq-pregnancy/data/aq/PM25Grid.csv")

#head(no2_test)

##      st_read("/Users/brenna/Downloads/NO2Grid_Edited.shp")


# tm_shape(no2_dat) +
#   tm_polygons(col = "grid_id", lwd = 0, style = "cont")

# test %>% 
#   # add date as date
#   #mutate(date = ymd(date)) %>%
#   # plot them
#   ggplot( aes(x = date, y = value, color = group, group = group)) +
#   geom_line() + geom_point() +   theme_test()
# 
# names(test)
# ggplot(test, aes(x = date, y = pop)) + 
#   geom_line(color = "#FC4E07", size = 2)



no2_10_files <- list.files("data/Grid_1km_UT/NO2/Daily/2010/") # no2
#no2_10_files <- list.files("data/Grid_1km_UT/o3/") # o3
#no2_10_files <- list.files("data/Grid_1km_UT/pm25/") # pm25

no2_10_all <- readRDS(paste0("data/Grid_1km_UT/NO2/Daily/2010/", no2_10_files[1]))
no2_10_all <- data.table(no2_10_all)
no2_10_all$date <- substr(no2_10_files[1], start = 0, stop = nchar(no2_10_files[1])-4) #colname = date

system.time(
for(i in 1:){#length(no2_10_files)) {
  no2_10 <- readRDS(paste0(getwd(), "/data/Grid_1km_UT/NO2/Daily/2010/", no2_10_files[i]))
  no2_10 <- data.table(no2_10)
  no2_10$date <- substr(no2_10_files[i], start = 0, stop = nchar(no2_10_files[i])-4) #colname = date colnames(no2_10)
  l = list(no2_10_all, no2_10)
  no2_10_all <- rbind(no2_10_all, no2_10, use.names = TRUE)
}
)

no2_10_all <- no2_10_all[-1, ] # duplicated jan 1

#x <- no2_10_all
str(no2_10_all)
no2_10_all[, c(1:5, 256475)]
rownames(no2_10_all)

no2_10_all$week <- rep(1:ceiling(nrow(no2_10_all)/7), length.out = nrow(no2_10_all), each = 7)

subset <- no2_10_all[, c(1:500, 256475)]
no2_10_stats
no2_10_stats <- subset %>% #group_by(personid) %>% 
  group_by(week) %>%
  summarise_all(list(mean = mean, max = max, median = median, min = min))

no2_10_all_t <- transpose(no2_10_stats)
rownames(no2_10_all_t) <- colnames(no2_10_stats)
colnames(no2_10_all_t) <- no2_10_stats$week
no2_10_all_t <- no2_10_all_t[-1, ]

no2_10_all_t$cell <- str_split_fixed(rownames(no2_10_all_t), "_", 2)[, 1]
no2_10_all_t$stat <- str_split_fixed(rownames(no2_10_all_t), "_", 2)[, 2]

no2_10_all_t <- no2_10_all_t %>% 
  pivot_longer(cols = c(1:3)) %>%
  group_by(cell, name) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  rename(week = name)


##



#####

no2_dat <- cbind(no2_grid[1:500, ], no2_10_all_t)

# tm_shape(no2_dat) +
#   tm_polygons(col = "X2_max", lwd = 0, style = "cont") +
#   tm_shape(af_city) +
#   tm_polygons()
# names(pop_bb)
tm_shape(pop_bb) +
  tm_polygons(col = "value") +
  tm_shape(no2_dat) +
  tm_polygons(col = "X2_max", lwd = 0, style = "cont")

# grid_sub <- no2_10_all %>%
#   filter()

#no2_10_sub <- no2_10_all[, c(1:10)]
#no2_10_sub <- no2_10_all[, c(1:1000)]
#no2_10_all[, week := ceiling(row_number(no2_10_all)/7)]

no2_10_all$week <- rep(1:ceiling(nrow(no2_10_all)/7), length.out = nrow(no2_10_all), each = 7)#(nrow(no2_10_all)/7), each = 7)
no2_10_all[, c(1:5, 256473:256475)]
fx <- function(x) {
  c(min = min(x), mean = mean(x), med = median(x), max = max(x))
}

no2_stats <- no2_10_all[, lapply(no2_10_all, FUN = fx), keyby = "week"] #only run once
no2_stats <- no2_10_all[, lapply(no2_10_all[, 1:256473], FUN = fx), keyby = "week"] #only run once
tapply(no2_10_all$V1, no2_10_all$week, median)

apply(no2_10_all[, 1:256473], 2, function(x) tapply(x, no2_10_all$week, c(sum, max)))

length(no2_10_all[, 1:20])
no2_stats[, c(1:5, 256474:256476)]

no2_stats$stat <- rep(c("min", "mean", "med", "max"), length.out = nrow(no2_stats))

no2_stats[, c(1:5, 256474:256477)]

no2_stats <- no2_stats[, -c(256475:256476)]

test <- no2_10_all[, 1:20]

no2_10_stats <- no2_10_all %>% #group_by(personid) %>% 
  group_by(week) %>%
  summarise(across(everything(), list(mean)))



# dplyr
system.time(
no2_10_stats <- no2_10_all %>% #group_by(personid) %>% 
  group_by(week) %>%
  summarise_all(list(mean = mean, max = max, median = median, min = min))
)

ceiling(row_number(no2_10_all)/7)

no2_stats_long <- no2_stats %>%
  group_by(cell) %>%
  pivot_wider(names_from = stat, values_from = c(wk_1, wk_2, wk_3))

no2_stats_long <- setNames(data.frame(t(no2_stats[, -1]), no2_stats[, 1]))


###ftable(xtabs(stat + week ~ ., no2_stats))

head(no2_stats_long)

no2_stats[, c(1:4, 256476:256477)]

statistic <- rep(c("min", "mean", "med", "max"), length.out = nrow(no2_stats))

no2_2010 <- cbind(statistic, no2_stats)
no2_2010 <- no2_2010[, -c(3)]

write.csv(no2_2010, "pm25_2010.csv")
#write.csv(no2_2010, "o3_2010.csv")
#write.csv(no2_2010, "no2_2010.csv")


pm25_t <- transpose(pm25)
names(pm25_t)
colnames(pm25_t) <- paste("wk", rownames(pm25), sep = "_")
rownames(pm25_t) <- colnames(pm25)
pm25_t$cell <- str_split_fixed(rownames(pm25_t), "_", 2)[, 1]
pm25_t$stat <- str_split_fixed(rownames(pm25_t), "_", 2)[, 2]
pm25_t <- pm25_t[-1, ]

str(no2_10_all_t)

no2_10_all_t <- no2_10_all_t %>%
  group_by(cell) %>%
  pivot_wider(names_from = stat, values_from = c(wk_1, wk_2, wk_3))




pm25 <- as.data.table(no2_2010)
pm25 <- read.csv("pm25_2010.csv") %>%
  as.data.table()
o3 <- read.csv("o3_2010.csv") %>%
  as.data.table()
no2 <- read.csv("no2_2010.csv") %>%
  as.data.table()


no2_10_all_t <- transpose(no2_10_all)



test[, c(1:4)]
no2_10_all[, c(1:4)]
no2_stats[, c(1:3, 256473)]
no2_10_all[, "week"]
str(no2_10_all)


ceiling(row_number(no2_10_all)/7)
row_number(no2_10_all)


Sys.time()
no2_10_stats <- no2_10_all %>% #group_by(personid) %>% 
  group_by(week = ceiling(row_number()/7)) %>%
  summarise_all(list(mean = mean, max = max, median = median, min = min))
Sys.time()
no2_10_all_t <- transpose(no2_10_all)
rownames(no2_10_all_t) <- colnames(no2_10_all)
colnames(no2_10_all_t) <- c("wk_1", "wk_2", "wk_3")

no2_10_all_t$cell <- str_split_fixed(rownames(no2_10_all_t), "_", 2)[, 1]
no2_10_all_t$stat <- str_split_fixed(rownames(no2_10_all_t), "_", 2)[, 2]
no2_10_all_t <- no2_10_all_t[-1, ]

no2_10_all_t <- no2_10_all_t %>%
  group_by(cell) %>%
  pivot_wider(names_from = stat, values_from = c(wk_1, wk_2, wk_3))

no2_10_all_t

