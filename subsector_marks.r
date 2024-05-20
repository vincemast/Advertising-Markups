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
library("gridExtra")
library(cowplot)

#navigate to folder with functions
setwd(dircs[1])
#functions
source("function_subsets.R")
source("function_plots.R")
source("function_regressions.R")
source("function_usefull.R")

############################################################
############################################################
#     1: Load and clean data
############################################################
############################################################

################### 1.a load data #########################
############################################################

#navigate to with data
setwd(dircs[2])

#compustat
dset <- read.csv("COMPUSTAT_wfoot.csv") # nolint
# naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")
colnames(naics) <- c("change", "naics_n", "industry")
#FRED DATA
usercost <- read.csv("usercost.csv") #nolint

################### 1.b clean data #########################
############################################################

#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"

#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

#apply GDP deflator and generate xad measures
dset <- vargen(dset, usercost)


#clean
data <- clean_deu(dset)
data <- data %>%
  filter(!is.na(MU))

#generate sales share
data <- data %>% # nolint
  group_by(fyear) %>% # nolint
  mutate(totsale = sum(sale, na.rm = TRUE)) # nolint
data <- data %>% # nolint
  mutate(sshare = sale *100/ totsale) #nolint

#make numeric indicator of year
#with fyear as index it operates weird when called, new var more convenient
data <- data %>%
  mutate(year = as.integer(fyear)) %>% #nolint
  filter(!is.na(fyear))

data <- data %>%
  mutate(decade = floor(year / 10) * 10)

#select only useful variables
data <- data %>%
  dplyr::select(MU, GVKEY, year, naics, sale,
                age, life, conm, decade, sshare) #nolint

#grab first 2 digits of naics
data$sec <- as.numeric(str_sub(data$naics, 1, 2))

datai <- industry_n_dig(data, naics, 6)

############################################################
############################################################
#     2: rank
############################################################
############################################################

#filter to post 1970
datai <- datai %>%
  filter(year >= 1980)


nyears <- max(datai$year) - min(datai$year) + 1



#keep only industries with atleast 1 obs per year
allyrs <- datai %>% # nolint
  group_by(industry) %>% # nolint
  filter(length(unique(year)) == nyears) %>%
  ungroup() # nolint

final <- allyrs %>%
    filter(year == max(datai$year))

top10 <- final %>%
  group_by(industry) %>%
  summarise(swmu = weighted.mean(MU - 1, w = sale, na.rm = TRUE)) %>%
  mutate(rank = rank(-swmu, ties.method = "min")) %>%
  filter(rank <= 10)

best <- allyrs %>%
  filter(industry %in% top10$industry)


sector_rank <- best %>%
  group_by(industry, year) %>%
  summarise(avg_mu  = mean(MU - 1, na.rm = TRUE),
    swmu = weighted.mean(MU - 1, w = sale, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank = rank(-swmu, ties.method = "min")) %>%
  select(industry, year, rank, swmu)


ggplot(sector_rank, aes(x = year, y = swmu, color = industry)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Rank") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))






hold <- allyrs %>% # nolint
  group_by(industry, year) %>% # nolint
  summarise(HHI = sum((sale / sum(sale))^2),
            ag_mu = weighted.mean(MU - 1, w = sale, na.rm = TRUE))



ggplot(hold, aes(x = ag_mu, y = HHI)) +
  geom_point(size = 2)


feols <- feols(log(ag_mu) ~ HHI | industry + year, data = hold)














sector_rank <- best %>%
  group_by(industry, year) %>%
  summarise(avg_mu  = mean(MU - 1, na.rm = TRUE),
    swmu = weighted.mean(MU - 1, w = sale, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank = rank(-swmu, ties.method = "min")) %>%
  select(industry, year, rank, swmu)

head(sector_rank)


ggplot(sector_rank, aes(x = year, y = swmu, color = industry)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Rank") +
  theme_minimal() +
  theme(legend.position = "right")




























sector_rank <- data %>%
  group_by(industry, year) %>%
  summarise(avg_mu  = mean(MU - 1, na.rm = TRUE),
    swmu = weighted.mean(MU - 1, w = sale, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank = rank(-swmu, ties.method = "min")) %>%
  select(industry, year, rank, swmu)


sector_rank5 <- sector_rank %>%
  filter(rank <= 5,
         !is.na(year))


unique(sector_rank5$industry)

head(sector_rank5)


ggplot(sector_rank5, aes(x = year, y = rank, color = industry)) +
  geom_line() +
  labs(x = "Year", y = "Rank") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_reverse()  # To make rank 1 at the top


view(sector_rank5)








top10 <- data %>%
  group_by(industry) %>%
  summarise(swmu = weighted.mean(MU - 1, w = sale, na.rm = TRUE)) %>%
  mutate(rank = rank(-swmu, ties.method = "min")) %>%
  filter(rank <= 10) %>%
select(industry)



best <- data %>%
  filter(industry %in% top10$industry)


sector_rank <- best %>%
  group_by(industry, year) %>%
  summarise(avg_mu  = mean(MU - 1, na.rm = TRUE),
    swmu = weighted.mean(MU - 1, w = sale, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank = rank(-swmu, ties.method = "min")) %>%
  select(industry, year, rank, swmu)

head(sector_rank)


ggplot(sector_rank, aes(x = year, y = rank, color = industry)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Rank") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_reverse()  # To make rank 1 at the top


view(sector_rank5)