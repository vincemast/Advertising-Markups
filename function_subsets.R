
require(dplyr)
############################################################
############################################################
##############   1: clean data  ############################
############################################################
############################################################

VariableGen <- function(data, Ucost) { #nolint

  ############################################################
  ###1: Convert table to panel data format. create entry/exit years, age
  tempdata <- data %>%
    group_by(GVKEY) %>% #nolint
    mutate(entry = min(fyear), #nolint
           exit = max(fyear),
           age = fyear - min(fyear),
           life = max(fyear) - fyear)

  ############################################################
  ###2: Merge and generate

  tempdata <- merge(tempdata, Ucost, by = "fyear", all = TRUE)

  #MU
  tempdata <- tempdata %>%
    mutate(MU = sale / (cogs + ppegt * usercost)) #nolint

  #MU -1
  tempdata <- tempdata %>%
    mutate(MU_1 = MU - 1) #nolint

  #Adr
  tempdata <- tempdata %>%
    mutate(Adr = xad / sale) #nolint

  #Adr MC
  tempdata <- tempdata %>%
    mutate(Adr_MC = xad / (cogs + ppegt * usercost)) #nolint

  #time untill 2022 (or newest year)
  #grab newest year
  maxyear <- max(tempdata$fyear, na.rm = TRUE)
  tempdata <- tempdata %>%
    mutate(time = fyear - maxyear) #nolint
  #make quadratic and cubic
  tempdata <- tempdata %>%
    mutate(time2 = -time * time)
  tempdata <- tempdata %>%
    mutate(time3 = time * time * time)

  print(tempdata)

}



Cleanadv <- function(data) {  #nolint

  ############################################################
  ###2: Merge and then filter
  ############################################################

  tempdata <- data %>%
    filter(MU >= 0 & !is.na(MU)) %>% #nolint
    filter(Adr >= 0 & !is.na(Adr)) %>% #nolint
    filter(Adr_MC >= 0 & !is.na(Adr_MC)) %>% #nolint
    filter(sale > 0 & !is.na(sale)) %>% #nolint
    filter(cogs >= 0 & !is.na(cogs)) %>% #nolint
    filter(xsga >= 0 & !is.na(xsga)) %>% #nolint
    filter(ppegt >= 0 & !is.na(ppegt)) %>% #nolint
    filter(usercost >= 0 & !is.na(usercost)) %>% #nolint
    filter(cogs + ppegt * usercost > 0)

  print(tempdata)
}



############################################################
############################################################
##############   2: NAiSC Codes  ###########################
############################################################
############################################################



industry_n_dig <- function(Clean_data, naics, n) {

  #keep only n level codes
  temp_naisc <- naics %>%
    filter(nchar(naics_n) == n) #nolint

  #grab n digit code level from data
  temp_data <- Clean_data %>%
    mutate(naics_n = ifelse(nchar(naics) >= n, substr(naics, 1, n), NA))

  temp_data <- merge(temp_data, temp_naisc, by = "naics_n", all = TRUE)

  temp_data <- temp_data %>%
    filter(!is.na(naics_n)) #nolint

  #get rid of annoying T's
  temp_data$industry <- paste(temp_data$industry, "")
  temp_data$industry <- gsub("T  ", "", temp_data$industry)
  temp_data$industry <- gsub("T ", "", temp_data$industry)
  temp_data$industry <- gsub("  ", "", temp_data$industry)
  #adding the space to end changes NA to string
  #replace so regression drops NA values
  temp_data$industry <- na_if(temp_data$industry, "NA ")

  print(temp_data)

}
