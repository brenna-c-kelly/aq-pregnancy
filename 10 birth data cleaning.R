
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

births_2013 <- read.csv("/Users/brenna/Downloads/STATA Ready Excel-selected/STATA Ready births 2013+.csv")
births_2016 <- read.csv("/Users/brenna/Downloads/STATA Ready Excel-selected/STATA Ready births 2016+.csv")

births <- rbind(births_2013, births_2016)

birth_dat <- births[, c("MomID", "StateFileNumber", "BirthCCYY",
                        "BirthDD", "BirthMM", "BirthWeightGrams",
                        "Gestation")]

names(birth_dat) <- c("mom_id", "sfn", "birth_year",
                      "birth_day", "birth_month",
                      "weight_g", "gestation")

summary(birth_dat)

birth_dat$birth_year <- str_pad(birth_dat$birth_year, 4, side = "left", "0")
birth_dat$birth_month <- str_pad(birth_dat$birth_month, 2, side = "left", "0")
birth_dat$birth_day <- str_pad(birth_dat$birth_day, 2, side = "left", "0")

birth_dat$dob <- paste0(str_pad(birth_dat$birth_year, 4, side = "left", "0"),#birth_dat$birth_year,
                        str_pad(birth_dat$birth_month, 2, side = "left", "0"),#birth_dat$birth_month,
                        str_pad(birth_dat$birth_day, 2, side = "left", "0")) #birth_dat$birth_day)

summary(birth_dat == "-9999")
summary(birth_dat$gestation)

birth_dat$mom_id <- ifelse(birth_dat$mom_id == "-9999", NA, birth_dat$mom_id)

table(is.na(birth_dat$mom_id), is.na(birth_dat$gestation))
# all missing gestation also have missing mom_id

head(birth_dat$dob)
  
