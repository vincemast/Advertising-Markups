###########################################################
#0: clear all and load data
############################################################
cat("\014")
rm(list = ls())
############################################################
#1: load api key, directory, and libraries
############################################################

library(fredr)
library(dplyr)
apikey <- "9fc98174c1127ca2629bab04f7f02951"
fredr_set_key(apikey)

#edit to include location of data:
d_folder <-
  "C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/data"

############################################################
#2: get data
############################################################

#relevant series
series_ids <- c("UNRATE")

data <- fredr_series_observations(series_ids[1],
                                  frequency = "a",
                                  aggregation_method = "avg")


head(data)

#
data <- data %>%
  select(date, value)

names(data) <- c("year", "unemp")
#clean names
data$year <- as.numeric(format(as.Date(data$year), "%Y"))

############################################################
#3: save data
############################################################

#save data
setwd(d_folder)

write.csv(data, "unemp.csv")