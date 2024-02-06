############################################################
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
series_ids <- c("A191RD3A086NBEA", "RIFSPFFNA", "FPCPITOTLZGUSA")

# Get the data for the first series ID
data <- fredr_series_observations(series_ids[1])
names(data)[3] <- series_ids[1]

data <- data %>%
  select(date, series_ids[1])

head(data)

# Loop over the remaining series IDs
for(id in series_ids[-1]) {
  # Get the data for the current series ID
  series_data <- fredr_series_observations(id)

  #clean
  names(series_data)[3] <- id
  series_data <- series_data %>%
    select(date, id)

  # Merge the data with the results dataframe by date
  data <- merge(data, series_data, by = "date", all = TRUE)
}
#clean names
data$date <- as.numeric(format(as.Date(data$date), "%Y"))
names(data) <- c("fyear", "GDP_def", "FEDFUNDS", "Ifl")

############################################################
#3: gen user cost
############################################################

#gen user cost
data$usercost <- (data$FEDFUNDS - data$Ifl + 12) * .01
#set user cost to NA if fed funds or inflation is missing
data$usercost[is.na(data$FEDFUNDS) | is.na(data$Ifl)] <- NA

# for years that i dont have fed funds or inflation, use DEU's #nolint
data <- data %>%
  mutate(usercost = ifelse(fyear == 1955, .1208544, usercost)) %>%
  mutate(usercost = ifelse(fyear == 1956, .1130887, usercost)) %>%
  mutate(usercost = ifelse(fyear == 1957, .1177995, usercost)) %>%
  mutate(usercost = ifelse(fyear == 1958, .1131686, usercost)) %>%
  mutate(usercost = ifelse(fyear == 1959, .1391089, usercost))

#trim data to 1950
data <- data %>%
  filter(fyear > 1950)

#drop fed funds and inflation
data <- data %>%
  select(fyear, GDP_def, usercost)


############################################################
#4: save data
############################################################

#save data
setwd(d_folder)

write.csv(data, "usercost.csv")
