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

library(stringr)
library(xtable)
library(tidyr)

library(scales)

library(sandwich)
library(multiwayvcov)

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
xaddensity <- xad_density(Data)
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("xad_density.pdf",  xaddensity, width = 10, height = 9, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#markup density
mudensity <- mu_density(Data, Dset)
      # setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      # ggsave("MU_density.pdf",  mudensity, width = 10, height = 9, units = "in") # nolint
      # setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#zero_subset <- Data %>%
# filter(is.na(Adr_MC)) # nolint
#1149/82441= 1.39% report 0 advertising

############################################################
############################################################
#3: Aggregate Markup plots
############################################################
############################################################

agg_muplot <- agg_mu_plot(Dset, Data)
    #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
    #ggsave("agg_mu_plot.pdf",  agg_muplot, width = 10, height = 9, units = "in") # nolint
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

manuf <- two_d_data %>%
  filter(industry == "Manufacturing")
scatter_manuf <- MU_advert_plot(manuf, "Manufacturing", 1000)

fininc <- two_d_data %>% # nolint
  filter(industry == "Finance and Insurance")
scatter_finance <- MU_advert_plot(fininc, "Finance and Insurance", 1000)

#save images
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #make sure to return wd
      # ggsave("scatter_all.pdf", plot_all, width = 9, height = 9, units = "in") # nolint
      # ggsave("scatter_Manuf.pdf",scatter_manuf, width = 9, height = 9, units = "in") # nolint
      # ggsave("scatter_Retail.pdf",  scatter_retail, width = 9, height = 9, units = "in") # nolint
      # ggsave("scatter_Finance.pdf", scatter_finance, width = 9, height = 9, units = "in") # nolint
      #return to correct wd
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


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
#[6] latex clean table #[7] industry count temp
#[8] Main Model (called Main_model)

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

labs <- c(C_MU = "Markup (Sales Weighted)", B_Adr = "xad (Sales Weighted)",
          A_Exad = "Advertising Efficency (2022)")

industry_co_plot_2d <- Efficency_plot_stacked(hold, labs)
  #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
  #ggsave("industry_co_plot_2d.pdf",  industry_co_plot_2d, width = 15, height = 13.5, units = "in") # nolint
  #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


#################### 6.b time plots ###########################
#time trend plot
time_co_plot_2d <- time_plot(year_coef_2d,
                             "Advertising Response Elasticity", "2")
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
time_co_plot_3d <- time_plot(year_coef_3d,
                             "Advertising Response Elasticity", "3")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_3d.pdf", time_co_plot_3d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_3d_limit <-
  Efficency_plot(sector_coef_3d, indcount3, 100)
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
time_co_plot_4d <- time_plot(year_coef_4d,
                             "Advertising Response Elasticity", "4")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_4d.pdf", time_co_plot_4d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_4d_limit <-
  Efficency_plot(sector_coef_4d, indcount4, 100)
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("industry_co_plot_4d_limit.pdf", industry_co_plot_4d_limit, width = 9, height = 13.5, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

####################### 7.b 5 digit ####################
#run regressions and save output
Results_5Digit <- regression_output_n(Data, naics, 5) # nolint
sector_coef_5d <- data.frame(data.frame(Results_5Digit[3]))
year_coef_5d <- data.frame(data.frame(Results_5Digit[4]))
indcount5 <- Results_5Digit[7]

#time trend plot
time_co_plot_5d <- time_plot(year_coef_5d,
                             "Advertising Response Elasticity", "5")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_5d.pdf", time_co_plot_5d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_5d_limit <-
  Efficency_plot(sector_coef_5d, indcount4, 50)
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("industry_co_plot_5d_limit.pdf", industry_co_plot_5d_limit, width = 9, height = 13.5, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


############################################################
############################################################
############# 8: Sector and time ###########################
############################################################
############################################################

############################################################
############# 8.a plot over time ##################
############################################################

#run regressions and save output
sector_time_estimates <- sector_time_n(Data, naics, 2)
sector_time_coefs <- sector_time_estimates %>% select(-intercept)

#plot sector coefficients over time

exad_ot_plot_ <- exad_ot_plot(sector_time_coefs, 10, "")

exad_ot_plot_smooth <- exad_ot_plot(sector_time_coefs, 10, "smooth")
#function checks if last input is set to "smooth" and if so plots a smooth line
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("exad_ot_plot.pdf", exad_ot_plot_, width = 9, height = 7, units = "in") # nolint
      #ggsave("exad_ot_plot_smooth.pdf", exad_ot_plot_smooth, width = 7, height = 9, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

############ interesting sectors ############
#fliter to interesting
fsector_time_coefs_interesting <- sector_time_coefs %>%
  filter(industry %in%
           c("Information", "Retail Trade", "Manufacturing", "Wholesale Trade",
             "Finance and\nInsurance", "Health Care and\nSocial Assistance"))
#plot interesting

exad_ot_interesting <-
  exad_ot_plot(fsector_time_coefs_interesting, 16, "")

exad_ot_interesting_smooth <-
  exad_ot_plot(fsector_time_coefs_interesting, 16, "smooth")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("exad_ot_interesting.pdf", exad_ot_interesting, width = 9, height = 7, units = "in") # nolint
      #ggsave("exad_ot_interesting_smooth.pdf", exad_ot_interesting_smooth, width = 9, height = 7, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

############################################################
############# 8.b regress against time ##################
############################################################

time_regression_table <- coef_regression(sector_time_coefs, Dset, naics, 2)
time_regression_table

coef_reg_plot <- coef_reg_plot_stacked(time_regression_table)
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("coef_reg_plot.pdf", coef_reg_plot, width = 15, height = 13.5, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

############################################################
############################################################
############# 9: Quantifying error #########################
############################################################
############################################################

############################################################
############# 9.a: intercept sector term ###################
############################################################

#rerun main model with lm to get standard errors for intercepts
intercepts_and_errors <- get_intercepts_and_errors(Data, naics, 2)

#save industry ints and names
industry_ints <- data.frame(intercepts_and_errors$industry)
industry_ints$industry <- gsub("industry", "", rownames(industry_ints))
#labels for plot
labs <- c(C_MU = "Markup (Sales Weighted)", B_Adr = "xad (Sales Weighted)",
          A_Exad = "Intercept (2022)")
#plot
intercept_plot <- intercept_plot_stacked(industry_ints, agg_2_digit, labs)
  #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
  #ggsave("intercept_plot.pdf",  intercept_plot, width = 15, height = 13.5, units = "in") # nolint
  #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

############################################################
############# 9.b: intercept time trend ###################
############################################################

#save year ints and clean names
year_ints <- data.frame(intercepts_and_errors$fyear)

time_int_plot_2d <- ints_timeplot(year_ints, "Intercept", "2")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_int_plot_2d.pdf", time_int_plot_2d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


############################################################
############# 9.c: intercept time trend more digits ########
############################################################

#3 digit
intercepts_and_errors_3 <- get_intercepts_and_errors(Data, naics, 3)
year_ints_3 <- data.frame(intercepts_and_errors_3$fyear)

time_int_plot_3d <- ints_timeplot(year_ints_3, "Intercept", "3")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_int_plot_3d.pdf", time_int_plot_3d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#4 digit
intercepts_and_errors_4 <- get_intercepts_and_errors(Data, naics, 4)
year_ints_4 <- data.frame(intercepts_and_errors_4$fyear)

time_int_plot_4d <- ints_timeplot(year_ints_4, "Intercept", "4")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_int_plot_4d.pdf", time_int_plot_4d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint

#5 digit
intercepts_and_errors_5 <- get_intercepts_and_errors(Data, naics, 5)
year_ints_5 <- data.frame(intercepts_and_errors_5$fyear)

time_int_plot_5d <- ints_timeplot(year_ints_5, "Intercept", "5")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_int_plot_5d.pdf", time_int_plot_5d, width = 4.5, height = 4, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


############################################################
############# 9.d: sector x time  ##########################
############################################################

#to do, not a priority rn

############################################################
############################################################
############# 10: Correcting error #########################
############################################################
############################################################


#try at 4 digit level

n <- 2

sector_time_estimates_n <- sector_time_n(Data, naics, n)

sector_time_estimates_n <- sector_time_estimates_n %>%
  mutate(correction = 1 - intercept / fit)

sector_time_correction <- sector_time_estimates_n %>%
  filter(industry != "Full Sample") %>%
  select(-fit, -se, -intercept)

head(sector_time_correction)

names(sector_time_correction) <- c("fyear", "industry", "correction")

use_data <- industry_n_dig(Data, naics, n) %>%
  select(-naics_n, -GVKEY, -xopr, -gind, -naics, -emp, -ppegt,
         -xad, -xlr, -cogs, -xsga, -entry, -age, -life, -X, -CPI,
         -Adr, -Adr_MC, -time, -time2, -time3, -exit, -change, -usercost)

head(use_data)

data_correction <- merge(use_data, sector_time_correction,
                         by = c("industry", "fyear"))

head(data_correction)

data_correction <- data_correction %>%
  mutate(MU_C = MU * correction)

data_correction <- data_correction %>%
  mutate(MU_C1 = MU_C - 1)

mudensity <- ggplot() +
  geom_density(data = data_correction, aes(x = MU_C1),
               color = "red") +
  geom_density(data = data_correction, aes(x = MU_1),
               color = "blue") +
  theme(text = element_text(size = 20)) +
  labs(x = "Markup (Log-scale)", y = "Density") +
  scale_x_continuous(trans = log10_trans(), # nolint
                     limits = c(.0001, 200), labels = comma) +
  theme(legend.position = "bottom")

mudensity

data_corrected <- data_correction %>%
  mutate(MU = MU_C)

agg_mu_all_2 <- agg_mu(data_correction)
agg_mu_c <- agg_mu(data_corrected)


agg_mu_c_plot <-  ggplot() +
  geom_line(data = agg_mu_all_2,
              aes(y = Ag_MU, x = year, color = "Full Sample")) + # nolint
  geom_line(data = agg_mu_c,
            aes(y = Ag_MU, x = year, color = "Corrected")) +
  theme(text = element_text(size = 20)) +
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(legend.position = "bottom")

agg_mu_c_plot

ggplot() +
  geom_smooth(data = agg_mu_all_2,
              aes(y = Ag_MU, x = year, color = "Full Sample")) + # nolint
  geom_smooth(data = agg_mu_c,
              aes(y = Ag_MU, x = year, color = "Corrected")) +
  theme(text = element_text(size = 20)) +
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(legend.position = "bottom")





























#zll ready generated all coefficients as sector_time_estimates
sector_time_estimates$correction <-
  (1 - sector_time_estimates$intercept / sector_time_estimates$fit)


sector_time_correction <- sector_time_estimates %>%
  filter(industry != "Full Sample") %>%
  select(-fit, -se, -intercept)

use_data <- two_d_data %>%
  select(-naics_n, -GVKEY, -xopr, -gind, -naics, -emp, -ppegt,
         -xad, -xlr, -cogs, -xsga, -entry, -age, -life, -X, -CPI,
         -Adr, -Adr_MC, -time, -time2, -time3, -exit, -change)

names(use_data) <- c("year", "usercost", "sale", "MU", "MU_1", "industry")

data_correction <- merge(use_data, sector_time_correction,
                         by = c("industry", "year"))

names(data_correction) <- c("industry", "fyear",
                            "usercost", "sale", "MU", "MU_1", "correction")

data_correction <- data_correction %>%
  mutate(MU_C = MU * correction)

data_correction <- data_correction %>%
  mutate(MU_C1 = MU_C - 1)

mudensity <- ggplot() +
  geom_density(data = data_correction, aes(x = MU_C1),
               color = "red") +
  geom_density(data = data_correction, aes(x = MU_1),
               color = "blue") +
  theme(text = element_text(size = 20)) +
  labs(x = "Markup (Log-scale)", y = "Density") +
  scale_x_continuous(trans = log10_trans(), # nolint
                     limits = c(.0001, 200), labels = comma) +
  theme(legend.position = "bottom")

mudensity

data_corrected <- data_correction %>%
  mutate(MU = MU_C)

agg_mu_all_2 <- agg_mu(data_correction)
agg_mu_c <- agg_mu(data_corrected)

agg_mu_c_plot <-  ggplot() +
  geom_line(data = agg_mu_all_2,
              aes(y = Ag_MU, x = year, color = "Full Sample")) + # nolint
  geom_line(data = agg_mu_c,
            aes(y = Ag_MU, x = year, color = "Corrected")) +
  theme(text = element_text(size = 20)) +
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(legend.position = "bottom")

agg_mu_c_plot



order(sector_time_estimates$correction, decreasing = TRUE)

plot(y = sector_time_estimates$correction, x = sector_time_estimates$year)

sector_time_estimates_all <- sector_time_estimates %>%
  filter(industry == "Full Sample")

plot(y = sector_time_estimates_all$correction, x = sector_time_estimates_all$year)

plot(density(sector_time_estimates$correction))

plot(density(sector_time_estimates_all$correction))

hold <- Winsorize(sector_time_estimates$correction, probs = c(0.05, 0.95))

plot(density(hold))

hold_all <-
  Winsorize(sector_time_estimates_all$correction, probs = c(0.05, 0.95))

plot(density(hold_all))

