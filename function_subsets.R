


 
############################################################   
############################################################   
##############   1: clean data  ############################
############################################################   
############################################################  




Variable_Gen <- function(data,Ucost){
 
  ############################################################       
  ###1: Convert table to panel data format and create entry and exit years, and age
  ############################################################ 
  tempData <- data %>%
    group_by(GVKEY) %>%
    mutate(entry = min(fyear),
           exit = max(fyear),
           age = fyear - min(fyear),
           life = max(fyear) - fyear)
  
  ############################################################       
  ###2: Merge and generate
  ############################################################ 
  
  
  tempData <- merge(tempData, Ucost, by = "fyear", all = TRUE)
  
  #MU
  tempData <- tempData %>%
    mutate(MU = sale /(cogs+ppegt*usercost) )
  
  
  #MU -1
  tempData <- tempData %>%
    mutate(MU_1 = MU-1 )
  
  #Adr
  tempData <- tempData %>%
    mutate(Adr = xad /sale )
  
  
  #Adr MC
  tempData <- tempData %>%
    mutate(Adr_MC = xad /(cogs+ppegt*usercost) )
  
  
  #time untill 2022 (or newest year)
  #grab newest year
  Maxyear=max(tempData$fyear,na.rm = TRUE)
  tempData <- tempData %>%
    mutate(time = fyear-Maxyear )
  #make quadratic and cubic
  tempData <- tempData %>%
    mutate(time2 = -time*time)
  tempData <- tempData %>%
    mutate(time3 = time*time*time)
  
  
  
  
  print(tempData)
  
}



Clean_adv <- function(data){
  
  
  ############################################################       
  ###2: Merge & filter
  ############################################################ 
  
  tempData <- data %>%
    filter(MU >= 0 & !is.na(MU)) %>%
    filter(Adr >= 0 & !is.na(Adr)) %>%
    filter(Adr_MC >= 0 & !is.na(Adr_MC)) %>%
    filter(sale > 0 & !is.na(sale)) %>%
    filter(cogs >= 0 & !is.na(cogs)) %>%
    filter(xsga >= 0 & !is.na(xsga)) %>%
    filter(ppegt >= 0 & !is.na(ppegt)) %>%
    filter(usercost >= 0 & !is.na(usercost)) %>%
    filter(cogs+ppegt*usercost > 0)
  
  
  
  
  print(tempData)
  
}



############################################################   
############################################################   
##############   2: NAiSC Codes  ###########################
############################################################   
############################################################  



Industry_n_dig <- function(Clean_data,naics,n){
  
  #keep only n level codes
  temp_naisc <- naics %>%
    filter(nchar(naics_n) == n)
  
  #grab n digit code level from data
  temp_data <- Clean_data %>%
    mutate(naics_n = ifelse(nchar(naics) >= n, substr(naics, 1, n), NA))
  
  temp_data <- merge(temp_data, temp_naisc, by = "naics_n", all = TRUE)
  
  temp_data <- temp_data%>%
    filter(!is.na(naics_n))
  
  print(temp_data)
  
}






