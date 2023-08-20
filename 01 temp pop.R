

# filtering down UT data to only populated grids
#     to be run after:
#       - 00 temp cleaning
#       - 01 population linking


temp_list <- list.files("data/gridMET", pattern = ".csv")

populated_ids <- read.csv("populated_ids.csv")

for(i in temp_list) {
  temp <- read.csv(paste0("data/gridMET/", i))
  temp <- temp[which(temp$grid_id %in% populated_ids), ]
  write.csv(temp, paste0("data/pop_", i))
}

x <- read.csv("no2_10_all_t.csv")
length(unique(x$cell))
head(x)

table(temp_cells == "1263347")

x_cells <- unique(x$cell)
temp_cells <- unique(as.character(tmmn_2013$grid_id))


extra_temp <- temp_cells[!temp_cells %in% x_cells] # extra temp cells
extra_x <- x_cells[!x_cells %in% temp_cells] # all temp cells are in x

head(x_cells)
head(temp_cells)


#test <- merge(temp_cells, x_cells)

# y <- read.csv("no2_2010.csv")
# head(y)

tmmn_2013 <- read.csv("data/pop_tmmn_2013.csv")
head(tmmn_2013)
head(x)
length(unique(tmmn_2013$grid_id))

v_1 <- names(no2_10_all)
v_2 <- names(test_pop)

extra_temp <- temp_cells[!temp_cells %in% v_2] # extra temp cells
extra_x <- v_2[!v_2 %in% temp_cells] # all temp cells are in x



test <- merge(x, tmmn_2013, by.x = c(""))


head(tmmn_2013)
nrow(tmmn_2013)

write.csv(populated_ids, "populated_ids.csv")

1 - (nrow(temp) / (256473 * 365))

