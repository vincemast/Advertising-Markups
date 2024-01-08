############################################################
############################################################
#0: clear all and load functions
############################################################
############################################################
cat("\014")
rm(list = ls())

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

library(xtable)
library(tidyr)

library(scales)

#navigate to folder with functions
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint: line_length_linter.
#functions
source("function_subsets.R")
source("function_plots.R")
source("function_regressions.R")


############################################################
############################################################
#     1: Load and clean data
############################################################
############################################################

############# 1.a load data ##############
#navigate to with datra
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/data")

#compustat
Dset <- read.csv("COMPUSTAT_simp.csv") # nolint
#usercost
Ucost <- read.csv("usercost.csv") # nolint
# naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")

colnames(naics) <- c("change", "naics_n", "industry")



################## 1.b Clean data ##########################
#back to functions
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


Dset<-VariableGen(Dset, Ucost) # nolint

Data<- Cleanadv(Dset) # nolint
#clean and combine data



############################################################x
############################################################x
# 2: Density plots
############################################################x
############################################################x

#advertising density
xad_density <- xad_density(Data)
xad_density #print
#setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
#ggsave("xad_density.pdf",  xad_density, width = 10, height = 9, units = "in") # nolint
#setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#markup density
mu_density <- mu_density(Data, Dset)
mu_density #print
# setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
# ggsave("MU_density.pdf",  mu_density, width = 10, height = 9, units = "in") # nolint
# setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#zero_subset <- Data %>%
# filter(is.na(Adr_MC)) # nolint
#1149/82441= 1.39% report 0 advertising

############################################################
############################################################
#3: Aggregate Markup plots
############################################################
############################################################

agg_mu_plot <- agg_mu_plot(Dset, Data)
agg_mu_plot
    #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
    #ggsave("agg_mu_plot.pdf",  agg_mu_plot, width = 10, height = 9, units = "in") # nolint
    #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

############################################################
############################################################
#4: Scatter plots
############################################################
############################################################

two_d_data <- industry_n_dig(Data, naics, 2)
#add naics industries

plot_all <- MU_advert_plot(Data, "All", 1000)
# generate full sample scatter plot

#generate sub sample data sets and plots
retail <- two_d_data %>%
  filter(industry == "Retail Trade")
scatter_retail <- MU_advert_plot(retail, "Retail ", 1000)

unique(two_d_data$industry)

manuf <- two_d_data %>%
  filter(industry == "Manufacturing")
scatter_manuf <- MU_advert_plot(manuf, "Manufacturing", 1000)

fininc <- two_d_data %>% # nolint
  filter(industry == "Finance and Insurance")
scatter_finance <- MU_advert_plot(fininc, "Finance and Insurance", 1000)

#save images
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
#make sure to return wd
plot_all
# ggsave("scatter_all.pdf", plot_all, width = 9, height = 9, units = "in") # nolint
scatter_manuf
# ggsave("scatter_Manuf.pdf",scatter_manuf, width = 9, height = 9, units = "in") # nolint
scatter_retail
# ggsave("scatter_Retail.pdf",  scatter_retail, width = 9, height = 9, units = "in") # nolint
scatter_finance
# ggsave("scatter_Finance.pdf", scatter_finance, width = 9, height = 9, units = "in") # nolint
#return to correct wd
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint



############################################################
############################################################
#5: Test table
############################################################
############################################################

testtable <- test_table(Data, naics, 2)
testtable[5, 1] <- "Administrative and Support"
lign <- c("l", "c", "c", "c", "c", "c")

print(xtable(testtable, align = lign), include.rownames = FALSE)

############################################################
############################################################
#6: Main results; 2 digit naics
############################################################
############################################################

#run regressions and save output
Results_2Digit <- regression_output_n(Data, naics, 2) # nolint
#[1] clean table, [2] ugly table, [3] model 9 sector coefficients (well named)
#[4] model 9 year coefficients (well named), [5] all models
#[6] latex clean table

table_1 <- Results_2Digit[1]
table_1

sector_coef_2d <- data.frame(data.frame(Results_2Digit[3]))
year_coef_2d <- data.frame(data.frame(Results_2Digit[4]))

table_1_latex <- Results_2Digit[6]

#################### 6.a sector efficency plot ###########################
#collect needed data
#within sector aggregate mu and adr
agg_2_digit <- Sector_MU_Adr(Dset, naics, 2)
#merge with efficency coefficients
hold <- merge(agg_2_digit, sector_coef_2d)

industry_co_plot_2d <- Efficency_plot_stacked(hold)

industry_co_plot_2d
  #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
  #ggsave("industry_co_plot_2d.pdf",  industry_co_plot_2d, width = 15, height = 13.5, units = "in") # nolint
  #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


#################### 6.b time plots ###########################
#time trend plot
time_co_plot_2d <- time_plot(year_coef_2d, "Advertising Efficiency", "2")
time_co_plot_2d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_2d.pdf", time_co_plot_2d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint



############################################################
############################################################
#7: more digits naics
############################################################
############################################################

####################### 7.a 3 digit ####################
#run regressions and save output
Results_3Digit <- regression_output_n(Data, naics, 3) # nolint
sector_coef_3d <- data.frame(data.frame(Results_3Digit[3]))
year_coef_3d <- data.frame(data.frame(Results_3Digit[4]))
indcount3 <- Results_3Digit[7]

#time trend plot
time_co_plot_3d <- time_plot(year_coef_3d, "Advertising Efficiency", "3")
time_co_plot_3d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_3d.pdf", time_co_plot_3d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_3d_limit <-
  Efficency_plot(sector_coef_3d, indcount3, 100)
industry_co_plot_3d_limit
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("industry_co_plot_3d_limit.pdf", industry_co_plot_3d_limit, width = 9, height = 13.5, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


####################### 7.b 4 digit ####################
#run regressions and save output
Results_4Digit <- regression_output_n(Data, naics, 4) # nolint
sector_coef_4d <- data.frame(data.frame(Results_4Digit[3]))
year_coef_4d <- data.frame(data.frame(Results_4Digit[4]))
indcount4 <- Results_4Digit[7]

#time trend plot
time_co_plot_4d <- time_plot(year_coef_4d, "Advertising Efficiency", "4")
time_co_plot_4d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_4d.pdf", time_co_plot_4d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_4d_limit <-
  Efficency_plot(sector_coef_4d, indcount4, 100)
industry_co_plot_4d_limit
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("industry_co_plot_4d_limit.pdf", industry_co_plot_4d_limit, width = 9, height = 13.5, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


####################### 7.b 6 digit ####################
#run regressions and save output
Results_5Digit <- regression_output_n(Data, naics, 5) # nolint
sector_coef_5d <- data.frame(data.frame(Results_5Digit[3]))
year_coef_5d <- data.frame(data.frame(Results_5Digit[4]))
indcount5 <- Results_5Digit[7]

#time trend plot
time_co_plot_5d <- time_plot(year_coef_5d, "Advertising Efficiency", "5")
time_co_plot_5d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_5d.pdf", time_co_plot_5d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_5d_limit <-
  Efficency_plot(sector_coef_5d, indcount4, 50)
industry_co_plot_5d_limit
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("industry_co_plot_5d_limit.pdf", industry_co_plot_5d_limit, width = 9, height = 13.5, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint




############################################################
#5: Sector and time
############################################################
