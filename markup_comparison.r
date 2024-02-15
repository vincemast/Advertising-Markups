############################################################
############################################################
#0: Set working directories
############################################################
############################################################
cat("\014")
rm(list = setdiff(ls()))

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

deu_marks <- c("DEU_st.csv", "DEU_s.csv", "DEU_c.csv")
#select which version of DEU markups to use
use <- deu_marks[2]

#directories
dircs <- c(f_folder, d_folder, p_folder)

############################################################
############################################################
#1: clear all and load functions
############################################################
############################################################
cat("\014")
#variables to keep
keep_vars <- c("dircs", "save_files", "use")
rm(list = setdiff(ls(), keep_vars))

library(plm)
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
source("function_deu.R")
source("function_plots.R")
source("function_usefull.R")

############################################################
############################################################
#     2: Load and clean data
############################################################
############################################################

############# 1.a load data ##############
#navigate to with data
setwd(dircs[2])

#load accounting data with DEU markups
dset <- read.csv(use) # nolint

#load usercost
Ucost <- read.csv("usercost.csv") # nolint

#generate variables (mainly need)
data <- vargen(dset, Ucost)

#keep only relevant columns (dont drop full might use later)
data_mu <-
  data %>% dplyr::select(MU, MU_deu, GVKEY, fyear, naics, industry, sale)

############################################################
############################################################
#     2: Markup Comparison
############################################################
############################################################

cor(data_mu$MU, data_mu$MU_deu, use = "complete.obs")
#0.8100058 (constant elasticity)
# 0.7960846 (by year)

#navigate to folder with functions (incase want to reload)
setwd(dircs[1])
############################################################
################# 2.a: Density Comparison ##################
############################################################

#set limits for plots
xlimits_d <- c(0.4, 7)

#compare densities of MU to MU_deu
density_comp <- ggplot(data_mu) +
  geom_density(aes(x = MU, colour = "Cost Accounting Markup"), alpha = 0.5) +
  geom_density(aes(x = MU_deu, colour = "Production Function Markup"),
               alpha = 0.5) +
  scale_color_manual(values = c("Cost Accounting Markup" = "blue",
                                "Production Function Markup" = "red")) +
  scale_x_continuous(
                     trans = "log", breaks = c(0.5, 1, 5),
                     limits = xlimits_d) +
  guides(colour = guide_legend(title = NULL)) +
  labs(x = "Markup",
       y = "Density") +
  theme(text = element_text(size = 20), legend.position = "bottom")
#display plot
density_comp

#save plot
save_f(density_comp, "density_comp_s.pdf", dircs, 9, 9, TRUE)

####2.a.2: Density Comparison for specific years #######

#pick years to include
years <- c(1980, 1955, 2016, 2022)
# Filter data for specific years and create a new variable for the year order
data_selected_years <- data_mu %>%
  filter(fyear %in% years) %>%
  mutate(year_order = factor(fyear, levels = years))

# Plot densities for each year
density_comp_years <- ggplot(data_selected_years) +
  geom_density(aes(x = MU, colour = "Cost Accounting Markup"), alpha = 0.5) +
  geom_density(aes(x = MU_deu, colour = "Production Function Markup"),
               alpha = 0.5) +
  scale_color_manual(values = c("Cost Accounting Markup" = "blue",
                                "Production Function Markup" = "red")) +
  facet_wrap(~ year_order, ncol = 2) +
  scale_x_continuous(limits = xlimits_d, trans = "log10") +
  guides(colour = guide_legend(title = NULL)) +
  labs(x = "Markup",
       y = "Density") +
  theme(text = element_text(size = 20), legend.position = "bottom")

# Display plot
density_comp_years

#save plot

save_f(density_comp_years, "density_comp_years_s.pdf", dircs, 9, 9, TRUE)

############################################################
################# 2.b: Agg Comparison ######################
############################################################

#create data
agg_markups <- data_mu %>%
  group_by(fyear) %>% # nolint
  summarise(Agg_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            Agg_MU_DEU = weighted.mean(MU_deu, sale, na.rm = TRUE))


agg_mu_comp <- ggplot(data = agg_markups, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU, colour = "Cost Accounting Markup")) +
  geom_line(aes(y = Agg_MU_DEU, colour = "Production Function Markup")) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(text = element_text(size = 20), legend.position = "bottom")

agg_mu_comp

cor(agg_markups$Agg_MU, agg_markups$Agg_MU_DEU, use = "complete.obs")
#0.9648306 constant
#0.9653266 by sector


#save plot
save_f(agg_mu_comp, "agg_mu_comp_s.pdf", dircs, 9, 9, TRUE)


############################################################
################### 2.c: Scatterplot #######################
############################################################
set.seed(123)  # for reproducibility

# Sample data
data_sample <- data_mu %>% sample_n(500)

# Calculate percentiles
x_limits <- quantile(data_mu$MU, c(0.1, 0.9), na.rm = TRUE)
y_limits <- quantile(data_mu$MU_deu, c(0.1, 0.9), na.rm = TRUE)

# Plot
scatter_comp <- ggplot() +
  geom_point(data = data_sample, aes(x = MU, y = MU_deu), shape = 1) +
  geom_smooth(data = data_mu,
              aes(x = MU, y = MU_deu, color = "Regression line"),
              method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_blank(aes(color = "45-degree line")) +
  xlim(x_limits) +
  ylim(y_limits) +
  scale_color_manual("",
                     values = c("Regression line" = "blue",
                                "45-degree line" = "red")) +
  labs(x = "Cost Accounting Markup",
       y = "Production Function Markup") +
  theme(text = element_text(size = 20), legend.position = "bottom")

#disolay plot
scatter_comp

#save plot
save_f(scatter_comp, "scatter_comp_s.pdf", dircs, 9, 9, TRUE)
