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

############# 1.a load data ##############
#navigate to with data
setwd(dircs[2])

#compustat
Dset <- read.csv("COMPUSTAT.csv") # nolint
#rename gvkey to GVKEY
colnames(Dset)[colnames(Dset) == "gvkey"] <- "GVKEY"

#usercost
Ucost <- read.csv("usercost.csv") # nolint
# naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")

colnames(naics) <- c("change", "naics_n", "industry")

################## 1.b Clean data ##########################
#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

#clean and combine data
dset<-invisible(VariableGen(Dset, Ucost)) # nolint

# Remove rows with NA values in the GVKEY, fyear, cogs, sale and ppegt variables #nolint
dset <-
  dset[!is.na(Dset$GVKEY) & !is.na(dset$fyear) & !is.na(dset$ppegt) &
  !is.na(dset$sale) & !is.na(dset$cogs), ] #nolint

# trim at 1 and 99% of sale/cogs
dset <- alpha_trim(dset, 2)

# trim to xad reporting firms
Data<- invisible(Cleanadv(dset)) # nolint
#not trimming on adv at the moment
Data <- adv_trim(Data, 0)


#print summary statstable
sum_stat_table(dset, Data)

############################################################x
############################################################x
# 2: Density plots
############################################################x
############################################################x

#advertising density
xaddensity <- xad_density(Data)

#markup density
mudensity <- mu_density(Data, dset)
#zero_subset <- Data %>%
# filter(is.na(Adr_MC)) # nolint
#1149/82441= 1.39% report 0 advertising

#save files
save_f(xaddensity, "xaddensity.pdf", dircs, 10, 9, save_files)
save_f(mudensity, "MU_density.pdf", dircs, 10, 9, save_files)

############################################################
############################################################
#3: Aggregate Markup plots
############################################################
############################################################

agg_muplot <- agg_mu_plot(dset, Data)

#save files
save_f(agg_muplot, "agg_mu_plot.pdf", dircs, 10, 9, save_files)

############################################################
############################################################
#4: Scatter plots
############################################################
############################################################

allhold <- invisible(industry_n_dig(Dset, naics, 2))

unique(allhold$industry)

two_d_data <- invisible(industry_n_dig(Data, naics, 2))
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

info <- two_d_data %>% # nolint
  filter(industry == "Information")
scatter_info <- MU_advert_plot(info, "Information", 1000)

#save images
save_f(plot_all, "scatter_all.pdf", dircs, 9, 9, save_files)
save_f(scatter_retail, "scatter_Retail.pdf", dircs, 9, 9, save_files)
save_f(scatter_manuf, "scatter_Manuf.pdf", dircs, 9, 9, save_files)
save_f(scatter_finance, "scatter_Finance.pdf", dircs, 9, 9, save_files)

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
          A_Exad = "Advertising Response Elasticity \n (2020)")

industry_co_plot_2d <- Efficency_plot_stacked(hold, labs)
#save plot
save_f(industry_co_plot_2d,
       "industry_co_plot_2d.pdf", dircs, 15, 13.5, save_files)

#################### 6.b time plots ###########################
#time trend plot
time_co_plot_2d <- time_plot(year_coef_2d,
                             "Advertising Response Elasticity", "2")
#save plot
save_f(time_co_plot_2d, "time_co_plot_2d.pdf", dircs, 4.5, 4, save_files)

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

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_3d_limit <-
  Efficency_plot(sector_coef_3d, indcount3, 100)

#save plots
save_f(time_co_plot_3d, "time_co_plot_3d.pdf", dircs, 4.5, 4, save_files)
save_f(industry_co_plot_3d_limit,
       "industry_co_plot_3d_limit.pdf", dircs, 9, 13.5, save_files)
####################### 7.b 4 digit ####################
#run regressions and save output
Results_4Digit <- regression_output_n(Data, naics, 4) # nolint
sector_coef_4d <- data.frame(data.frame(Results_4Digit[3]))
year_coef_4d <- data.frame(data.frame(Results_4Digit[4]))
indcount4 <- Results_4Digit[7]

#time trend plot
time_co_plot_4d <- time_plot(year_coef_4d,
                             "Advertising Response Elasticity", "4")

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_4d_limit <-
  Efficency_plot(sector_coef_4d, indcount4, 100)

#save plots
save_f(time_co_plot_4d, "time_co_plot_4d.pdf", dircs, 4.5, 4, save_files)
save_f(industry_co_plot_4d_limit,
       "industry_co_plot_4d_limit.pdf", dircs, 9, 13.5, save_files)
####################### 7.b 5 digit ####################
#run regressions and save output
Results_5Digit <- regression_output_n(Data, naics, 5) # nolint
sector_coef_5d <- data.frame(data.frame(Results_5Digit[3]))
year_coef_5d <- data.frame(data.frame(Results_5Digit[4]))
indcount5 <- Results_5Digit[7]

#time trend plot
time_co_plot_5d <- time_plot(year_coef_5d,
                             "Advertising Response Elasticity", "5")

#sector plot (choose minimal number of obs needed to be included)
industry_co_plot_5d_limit <-
  Efficency_plot(sector_coef_5d, indcount4, 50)

#save plots
save_f(time_co_plot_5d, "time_co_plot_5d.pdf", dircs, 4.5, 4, save_files)
save_f(industry_co_plot_5d_limit,
       "industry_co_plot_5d_limit.pdf", dircs, 9, 13.5, save_files)

############################################################
############################################################
############# 8: Sector and time ###########################
############################################################
############################################################

############################################################
############# 8.a plot over time ##################
############################################################

#run regressions and save output
rolling_results <- rolling_window(Data, naics, 2, 5)
sector_time_coefs <- rolling_results$coefs
sector_time_coefs <- sector_time_coefs %>% select(-intercept)

#plot sector coefficients over time
#can make it plot smooth, leave last argument blank for non smoothed
exad_ot_plot_ <- exad_ot_plot(sector_time_coefs, 10, "")

############ interesting sectors ############
#fliter to interesting
fsector_time_coefs_interesting <- sector_time_coefs %>%
  filter(industry %in%
           c("Information", "Retail Trade", "Manufacturing", "Wholesale Trade",
             "Finance and Insurance", "Health Care and Social Assistance"))

fsector_time_coefs_inter_2 <- sector_time_coefs %>%
  filter(industry %in%
           c("Information", "Retail Trade", "Manufacturing", "Wholesale Trade",
             "Finance and Insurance", "Accommodation and Food Services"))

exad_ot_interesting <-
  exad_ot_plot(fsector_time_coefs_interesting, 16, "")

exad_ot_interesting2 <-
  exad_ot_plot(fsector_time_coefs_inter_2, 16, "")

#save plots
save_f(exad_ot_plot_, "exad_ot_plot.pdf", dircs, 9, 7, save_files)
save_f(exad_ot_interesting, "exad_ot_interesting.pdf", dircs, 9, 7, save_files)
save_f(exad_ot_interesting2,
       "exad_ot_interesting2.pdf", dircs, 9, 7, save_files)

############################################################
############# 8.b regress against time ##################
############################################################

time_regression_table <- coef_regression(sector_time_coefs, Data, naics, 2)

coef_reg_plot <- coef_reg_plot_stacked(time_regression_table)

#save plot
save_f(coef_reg_plot, "coef_reg_plot.pdf", dircs, 15, 13.5, save_files)

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
rownames(industry_ints) <- NULL
#labels for plot
labs <- c(C_MU = "Markup (Sales Weighted)", B_Adr = "xad (Sales Weighted)",
          A_Exad = "Intercept (2022)")
#plot
intercept_plot <- intercept_plot_stacked(industry_ints, agg_2_digit, labs)

#save plot
save_f(intercept_plot, "intercept_plot.pdf", dircs, 15, 13.5, save_files)

############################################################
############# 9.b: intercept time trend ###################
############################################################

#save year ints and clean names
year_ints <- data.frame(intercepts_and_errors$fyear)
year_ints$fyear <- as.numeric(gsub("fyear", "", rownames(year_ints)))
rownames(year_ints) <- NULL

time_int_plot_2d <- ints_timeplot(year_ints, "Intercept", "2")

#save plot
save_f(time_int_plot_2d, "time_int_plot_2d.pdf", dircs, 4.5, 4, save_files)

############################################################
############# 9.c: intercept time trend more digits ########
############################################################

#3 digit
intercepts_and_errors_3 <- get_intercepts_and_errors(Data, naics, 3)
year_ints_3 <- data.frame(intercepts_and_errors_3$fyear)
time_int_plot_3d <- ints_timeplot(year_ints_3, "Intercept", "3")

#4 digit
intercepts_and_errors_4 <- get_intercepts_and_errors(Data, naics, 4)
year_ints_4 <- data.frame(intercepts_and_errors_4$fyear)
time_int_plot_4d <- ints_timeplot(year_ints_4, "Intercept", "4")

#5 digit
intercepts_and_errors_5 <- get_intercepts_and_errors(Data, naics, 5)
year_ints_5 <- data.frame(intercepts_and_errors_5$fyear)
time_int_plot_5d <- ints_timeplot(year_ints_5, "Intercept", "5")


#save plotS
save_f(time_int_plot_3d, "time_int_plot_3d.pdf", dircs, 4.5, 4, save_files)
save_f(time_int_plot_4d, "time_int_plot_4d.pdf", dircs, 4.5, 4, save_files)
save_f(time_int_plot_5d, "time_int_plot_5e.pdf", dircs, 4.5, 4, save_files)

############################################################
############# 9.d: sector x time  ##########################
############################################################

#to do, not a priority rn

############################################################
############################################################
############# 10: Correcting error #########################
############################################################
############################################################

############################################################
################ 10.a mains pecification ###################
############################################################

mu_correct_main <- mu_correction(Data, naics, 2)

mu_c_density <- mu_c_plot(mu_correct_main)

agg_mu_c_plot <- agg_mu_c(mu_correct_main)

############################################################
#################### 10.b reverse ##########################
############################################################

mu_correct_main_r <- mu_correction_r(Data, naics, 2)

mu_c_density_r <- mu_c_plot(mu_correct_main_r)

agg_mu_c_plot_r <- agg_mu_c(mu_correct_main_r)

# save plots
save_f(mu_c_density, "mu_c_density.pdf", dircs, 9, 7, save_files)
save_f(mu_c_density_r, "mu_c_density_r.pdf", dircs, 9, 7, save_files)
save_f(agg_mu_c_plot, "agg_mu_c_plot.pdf", dircs, 9, 7, save_files)
save_f(agg_mu_c_plot_r, "agg_mu_c_plot_r.pdf", dircs, 9, 7, save_files)

############################################################
########## 10.c sector x time og specification #############
############################################################

rolling_results <- rolling_window(Data, naics, 2, 1)

mu_c_density_st <- mu_c_plot(rolling_results$corrections)

agg_mu_c_plot_st <- agg_mu_c(rolling_results$corrections)

############################################################
######### 10.d reverse specification sector x time #########
############################################################

corrected_mu_st_r <- mu_correction_st_reverse(Data, naics, 2)

mu_c_density_st_r <- mu_c_plot(corrected_mu_st_r)

agg_mu_c_plot_st_r <- agg_mu_c(corrected_mu_st_r)

#save plot
save_f(mu_c_density_st, "mu_c_density_st.pdf", dircs, 9, 7, save_files)
save_f(mu_c_density_st_r, "mu_c_density_st_r.pdf", dircs, 9, 7, save_files)
save_f(agg_mu_c_plot_st, "agg_mu_c_plot_st.pdf", dircs, 9, 7, save_files)
save_f(agg_mu_c_plot_st_r, "agg_mu_c_plot_st_r.pdf", dircs, 9, 7, save_files)

############################################################
############ 10.e Time rolling sample ######################
############################################################

#5 digit
rolling_results <- rolling_window(Data, naics, 2, 5)
mu_c_density_rolling <- mu_c_plot(rolling_results$corrections)
agg_mu_c_plot_rolling <- agg_mu_c(rolling_results$corrections)

#7 digit
rolling_results7 <- rolling_window(Data, naics, 2, 7)
mu_c_density_rolling7 <- mu_c_plot(rolling_results7$corrections)
agg_mu_c_plot_rolling7 <- agg_mu_c(rolling_results7$corrections)

#save plots
save_f(mu_c_density_rolling,
       "mu_c_density_rolling.pdf", dircs, 9, 7, save_files)
save_f(mu_c_density_rolling7,
       "mu_c_density_rolling7.pdf", dircs, 9, 7, save_files)
save_f(agg_mu_c_plot_rolling,
       "agg_mu_c_plot_rolling.pdf", dircs, 9, 7, save_files)
save_f(agg_mu_c_plot_rolling7,
       "agg_mu_c_plot_rolling7.pdf", dircs, 9, 7, save_files)

############################################################
################# 10.f more digits #########################
############################################################




#############################################################
#############################################################
#############################################################
#########################  Sandbox  #########################
#############################################################
#############################################################
#############################################################
