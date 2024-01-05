############################################################
############################################################
#0: clear all and load functions
############################################################
############################################################
cat("\014")
rm(lis = ls())

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


Dset<-Variable_Gen(Dset, Ucost) # nolint

Data<- Clean_adv(Dset) # nolint
#clean and combine data



############################################################x
############################################################x
# 2: Density plots
############################################################x
############################################################x

#advertising density
xad_density <- ggplot(Data, aes(x = Adr_MC)) +
  geom_density() +
  theme(text = element_text(size = 20)) +
  labs(x = "Advertising/Marginal Cost (Log-scale)", y = "Density") +
  scale_x_continuous(trans = log10_trans(),
                     limits = c(.00001, 30), labels = comma)
# plots xad / sales
xad_density #print
#setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
#ggsave("xad_density.pdf",  xad_density, width = 10, height = 9, units = "in") # nolint
#setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint



MU_density <- ggplot() + # nolint
  geom_density(data = Dset, aes(x = MU_1, color = "Full Sample")) +
  geom_density(data = Data, aes(x = MU_1, color = "XAD Reported")) +
  theme(text = element_text(size = 20)) +
  labs(x = "Markup (Log-scale)", y = "Density") +
  scale_x_continuous(trans = log10_trans(),
                     limits = c(.001, 50), labels = comma) +
  theme(legend.position = "bottom")

#markups
MU_density #print
# setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
# ggsave("MU_density.pdf",  MU_density, width = 10, height = 9, units = "in") # nolint
# setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


#zero_subset <- Data %>%
# filter(is.na(Adr_MC)) # nolint
#zero_subset_sectorknown <- zero_subset %>%
# filter(!is.na(naics)) # nolint
##1149/82441= 1.39% report 0 advertising
#plot(density(zero_subset_sectorknown$naics)) # nolint
#sector_known_subset <- Data %>%
# filter(!is.na(naics)) # nolint
#plot(density(sector_known_subset$naics)) # nolint


############################################################
############################################################
#3: Aggregate Markup plots
############################################################
############################################################

agg_mu_all <- agg_mu(Dset)
agg_mu_insamp <- agg_mu(Data)
agg_mu_rew <- agg_mu_reweight(Data, Dset)

agg_mu_plot <-  ggplot() +
  geom_line(data = agg_mu_all,
            aes(y = Ag_MU, x = year, color = "Full Sample")) +
  geom_line(data = agg_mu_insamp,
            aes(y = Ag_MU, x = year, color = "XAD Reported")) +
  geom_line(data = agg_mu_rew, aes(y = Ag_MU, x = year, color = "Reweighted")) +
  theme(text = element_text(size = 20)) +
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(legend.position = "bottom")

agg_mu_plot
    #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
    #ggsave("agg_mu_plot.pdf",  agg_mu_plot, width = 10, height = 9, units = "in") # nolint
    #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint


############################################################
############################################################
#4: Scatter plots
############################################################
############################################################

two_d_data <- Industry_n_dig(Data, naics, 2)
#add naics industries

plot_all <- MU_advert_plot(Data, "All", 1000)
# generate full sample scatter plot

#generate sub sample data sets and plots
Retail <- two_d_data %>% # nolint
  filter(industry == "Retail TradeT")
scatter_Retail <- MU_advert_plot(Retail, "Retail", 1000) # nolint

Manuf <- two_d_data %>% # nolint
  filter(industry == "ManufacturingT")
scatter_Manuf <- MU_advert_plot(Manuf, "Manufacturing", 1000) # nolint

Fininc <- two_d_data %>% # nolint
  filter(industry == "Finance and InsuranceT")
scatter_Finance <- MU_advert_plot(Fininc, "Finance and Insurance", 1000) # nolint

#save images
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
#make sure to return wd
plot_all
# ggsave("scatter_all.pdf", plot_all, width = 9, height = 9, units = "in") # nolint
scatter_Manuf
# ggsave("scatter_Manuf.pdf",scatter_Manuf, width = 9, height = 9, units = "in") # nolint
scatter_Retail
# ggsave("scatter_Retail.pdf",  scatter_Retail, width = 9, height = 9, units = "in") # nolint
scatter_Finance
# ggsave("scatter_Finance.pdf", scatter_Finance, width = 9, height = 9, units = "in") # nolint
#return to correct wd
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint



############################################################
############################################################
#5: Test table
############################################################
############################################################

testtable <- Test_table(Data, naics, 2)
testtable[5, 1] = "Administrative and Support"
lign = c("l", "c", "c", "c", "c", "c")

print(xtable(testtable, align = lign), include.rownames = FALSE)

############################################################
############################################################
#6: Main results; 2 digit naics
############################################################
############################################################

#run regressions and save output
Results_2Digit <- Regression_output_N(Data, naics, 2) # nolint
#[1] clean table, [2] ugly table, [3] model 9 sector coefficients (well named)
#[4] model 9 year coefficients (well named), [5] all models
#[6] latex clean table

table_1 <- Results_2Digit[1]
table_1

sector_coef_2d <- data.frame(data.frame(Results_2Digit[3]))
year_coef_2d <- data.frame(data.frame(Results_2Digit[4]))

table_1_latex <- Results_2Digit[6]
#################### 6.a efficency plot ###########################
#collect needed data
#within sector aggregate mu and adr
agg_2_digit <- Sector_MU_Adr(Dset, naics, 2)
#merge with efficency coefficients
hold <- merge(agg_2_digit, sector_coef_2d)

industry_co_plot_2d <- Efficency_plot_stacked(hold)

industry_co_plot_2d
setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
#ggsave("industry_co_plot_2d.pdf",  industry_co_plot_2d, width = 15, height = 13.5, units = "in") # nolint
#setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint



#################### 6.b time plots ###########################


#time trend plot
time_co_plot_2d <- 
  ggplot(year_coef_2d, aes(x = year, y = fit, group = year)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
                width = 0.2, color = "darkblue") +
  geom_hline(aes(yintercept = 0),
            colour = "black", linetype = "dashed", size = 1) +
  geom_text(aes(1985, 0, label = "2022 Reference", vjust = -1),
            size = 4, colour = "black") +
  labs(x = "Year", y = "Time Compoent of Advertising Efficency ") +
  ggtitle("Advertising Efficiency Over Time With 95% Confidence Intervals (2022 Reference year)")+ # nolint
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal()


time_co_plot_2d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex") # nolint
      #ggsave("time_co_plot_2d.pdf", time_co_plot_2d, width = 9, height = 7, units = "in") # nolint
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups") # nolint



############################################################
############################################################
#7: 3 digit naics
############################################################
############################################################

#run regressions and save output
Results_3Digit<-Regression_output_N(Data,naics,3)
#[1] clean table, [2] ugly table, [3] model 9 sector coefficients (well named), [4] model 9 year coefficients (well named), [5] all models,
      #[6] latex clean table #[7] industry count data
      
table_3dig<- Results_3Digit[1]
sector_coef_3d <- data.frame(data.frame(Results_3Digit[3]))
year_coef_3d <- data.frame(data.frame(Results_3Digit[4]))


table_3dig_latex<- Results_3Digit[6]

Ind_count3<-Results_3Digit[7]
####################3.a plots#######


#time trend plot
time_co_plot_3d<-
  ggplot(year_coef_3d, aes(x = year, y = fit, group=year)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se),
                width = 0.2, color = "darkblue") +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed", size = 1) + 
  geom_text(aes( 1985, 0, label = "2022 Reference", vjust = -1), size = 4, colour="black")+
  labs(x = "Year", y = "Time Compoent of Advertising Efficency ") +
  ggtitle("Advertising Efficiency Over Time With 95% Confidence Intervals (2022 Reference year)")+
  guides(fill = guide_legend(title = NULL))+
  theme_minimal()


time_co_plot_3d
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Latex")
      #ggsave("time_co_plot_3d.pdf", time_co_plot_3d, width = 9, height = 7, units = "in")
      #setwd("C:/Users/Vince/Documents/OneDrive - UCB-O365/advertising_markups/Advertising-Markups")

#sector plot

# coefficents plot
industry_co_plot_3d<-Efficency_plot_3_digit(sector_coef_3d)

industry_co_plot_3d

#limited coefficent plot
industry_co_plot_3d_limit<-Efficency_plot(sector_coef_3d,Ind_count3,100)


industry_co_plot_3d_limit




############################################################       
#4: 4 digit naics 
############################################################  

#run regressions and save output
Results_4Digit<-Regression_output_N(Data,naics,4)
#[1] clean table, [2] ugly table, [3] model 9 sector coefficients (well named), [4] model 9 year coefficients (well named), [5] all models

table_4dig<- Results_4Digit[1]
sector_coef_4d <- data.frame(data.frame(Results_4Digit[3]))
year_coef_4d <- data.frame(data.frame(Results_4Digit[4]))

####################4.a plots#######


#time trend plot
time_co_plot_4d<-
  ggplot(year_coef_4d, aes(x = year, y = fit, group=year)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se),
                width = 0.2, color = "darkblue") +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed", size = 1) + 
  geom_text(aes( 1985, 0, label = "2022 Reference", vjust = -1), size = 4, colour="black")+
  labs(x = "Year", y = "Time Compoent of Advertising Efficency ") +
  ggtitle("Advertising Efficiency Over Time With 95% Confidence Intervals (2022 Reference year)")+
  guides(fill = guide_legend(title = NULL))+
  theme_minimal()


time_co_plot_4d



############################################################
#5: Sector and time
############################################################

output_list<-Sector_time_coefs_N(Data,naics,4)

