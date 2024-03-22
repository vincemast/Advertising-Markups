############################################################
############################################################
#-1: Set working directories
############################################################
############################################################

#edit directories to match your computer

#edit to include location of fuctions:
f_folder <-
  "C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups" #nolint

#edit to include location of data:
d_folder <-
  "C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/data"

#edit to include location of where to save tables and plots:
p_folder <-
  "C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex"

#switch to save files or not
save_files <- FALSE

#directories
dircs <- c(f_folder, d_folder, p_folder)
keep_vars <- c("dircs", "save_files")

############################################################
############################################################
#0: clear all and load functions
############################################################
############################################################
cat("\014")
rm(list = setdiff(ls(), keep_vars))

library(ks)
library(robustHD)
library(dplyr)
library(plm)
library(ggplot2)
library(DescTools)
library(broom)
library(huxtable)
library(fixest)
library(modelsummary)
library(tibble)
library(stringr)
library(xtable)
library(tidyr)
library(scales)
library(sandwich)
library(multiwayvcov)
library(lubridate)


#navigate to and load data
setwd(dircs[2])
#compustat
compu <- read.csv("COMPUSTAT_CUSIP.csv") # nolint
# mergers and aquisitions
m_a <- read.csv("SDC_ma.csv") # nolint

############################################################
############################################################
#1: ogranize compustat data
############################################################
############################################################

#keep only observations with fyear and cuiscp
comp <- compu %>%
  filter(!is.na(fyear),
         !is.na(cusip),
         nchar(cusip) > 0)

#get 6 digit cusip (just first 6 digits)
comp <- comp %>%
  mutate(cusip6 = substr(cusip, 1, 6))
#check to make sure all length 6
summary(as.numeric(nchar(comp$cusip)))

#add last year availible
comp <- comp %>%
    group_by(gvkey) %>% #nolint
    mutate(exit = max(fyear))

#Get list of firms
firms <- comp %>% distinct(gvkey, .keep_all = TRUE)
#limit to just gvkey, cusip, exit, and company name for simplicity
firms <- firms %>% select(gvkey, cusip6, exit, conm)


############################################################
############################################################
#2: organize M&A data
############################################################
############################################################

#keep only observations with target in US or Canada
ma <- m_a %>%
  filter(TNATION %in% c("United States", "Canada"))

#generate year based on date annonunced
ma <- ma %>%
  mutate(year = as.integer(substr(DATEANN, 1, 4)))
#keep only mergers before 2023
ma <- ma %>%
  filter(year < 2023)
#get year of last recorded merger
ma <- ma %>%
  group_by(MASTER_CUSIP) %>% #nolint
  mutate(last = max(year))

#get list of firms who were target of merge/ aquisition
ma_l <- ma %>% distinct(MASTER_CUSIP, .keep_all = TRUE)
#limit to just cusip, last, and target name for simplicity
ma_l <- ma_l %>% select(MASTER_CUSIP, last, TMANAMES)
#clearly contains firms not publically traded, given length


############################################################
############################################################
#3: Merge and save firms who exited va M/A
############################################################
############################################################


#merge data sets
firms_ma <- merge(firms, ma_l, by.x = "cusip6", by.y = "MASTER_CUSIP")


#firms who exited same year as MA event
#only firms who exited the same rear as to a the merger/ aquisition
ma_exit <- firms_ma %>%
  filter(last == exit)

#alternatively firms who exited prior to a the merger/ aquisition
ma_exit2 <- firms_ma %>%
  filter(last <= exit)

#histogram
hist(ma_exit$exit,
     main = "Histogram of Firms Who Exited Via M/A",
     xlab = "Year", ylab = "Frequency")

#list of firms who exited via M/A
ma_exit_list <- ma_exit %>% select(gvkey, conm, exit)
ma_exit_list2 <- ma_exit2 %>% select(gvkey, conm, exit)

#navigate to data folder
setwd(dircs[2])
write.csv(ma_exit_list, "ma_exit.csv")
write.csv(ma_exit_list2, "ma_exit2.csv")