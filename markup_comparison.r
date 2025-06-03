############################################################
############################################################
#0: Set working directories
############################################################
############################################################
cat("\014")
rm(list = ls())

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
use <- deu_marks[1]

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
library(grid)
library(gridExtra)

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
data <- laggen(data)

#keep only relevant columns (dont drop full might use later)
data_mu <-
  data %>% dplyr::select(MU, MU_deu, GVKEY, fyear, naics, industry, sale,
                         soc, weight, w_l, MU_l, MU_deu_l, theta)

############################################################
############################################################
#     2: Markup Comparison
############################################################
############################################################

cor(data_mu$MU, data_mu$MU_deu, use = "complete.obs")
# 0.8765214 sector time
#0.8100058 (constant elasticity)
# 0.7960846 (by sector)


#navigate to folder with functions (incase want to reload)
setwd(dircs[1])
############################################################
################# 2.a: Density Comparison ##################
############################################################

#set limits for plots
xlimits_d <- c(0.4, 7)

#compare densities of MU to MU_deu
density_comp <- ggplot(data_mu) +
  geom_density(aes(x = MU, colour = "Cost Accounting Markup",
                   linetype = "Cost Accounting Markup"),
              alpha = 0.5, size = 1.5) +
  geom_density(aes(x = MU_deu, colour = "Production Function Markup",
                   linetype = "Production Function Markup"),
               alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c("Cost Accounting Markup" = "blue",
                                "Production Function Markup" = "red")) +
  scale_linetype_manual(name = "",
                        values = c("Cost Accounting Markup" = "solid",
                                   "Production Function Markup" = "dashed")) +
  scale_x_continuous(
                     trans = "log",
                     breaks = c(0.5, 1, 2, 6),
                     limits = xlimits_d,
                     labels = function(x) x - 1) +
  guides(colour = guide_legend(title = NULL),
         linetype =  guide_legend(title = NULL)) +
  labs(x = "Markup ([p-mc]/mc, log scale)",
       y = "Density") +
  theme(text = element_text(size = 20), legend.position = "bottom")
#display plot
density_comp

#save plot
save_f(density_comp, "density_comp_st.pdf", dircs, 9, 9, TRUE)

save_f(density_comp, "density_comp_st_wide.pdf", dircs, 16, 9, TRUE)

####2.a.2: Density Comparison for specific years #######

#pick years to include
years <- c(1980, 1955, 2016, 2022)
# Filter data for specific years and create a new variable for the year order
data_selected_years <- data_mu %>%
  filter(fyear %in% years) %>%
  mutate(year_order = factor(fyear, levels = years))

# Plot densities for each year
density_comp_years <- ggplot(data_selected_years) +
  geom_density(aes(x = MU, colour = "Cost Accounting Markup",
                   linetype = "Cost Accounting Markup"),
              alpha = 0.5, size = 1.5) +
  geom_density(aes(x = MU_deu, colour = "Production Function Markup",
                   linetype = "Production Function Markup"),
               alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c("Cost Accounting Markup" = "blue",
                                "Production Function Markup" = "red")) +
  facet_wrap(~ year_order, ncol = 2) +
  scale_x_continuous(limits = xlimits_d,
                     trans = "log10",
                     breaks = c(0.5, 1, 2, 6),
                     labels = function(x) x - 1) +
  guides(colour = guide_legend(title = NULL),
         linetype =  guide_legend(title = NULL)) +
  labs(x = "Markup ([p-mc]/mc, log scale)",
       y = "Density") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL))


# Display plot
density_comp_years

#save plot

save_f(density_comp_years, "density_comp_years_st.pdf", dircs, 9, 9, TRUE)


save_f(density_comp_years, "density_comp_years_st_wide.pdf", dircs, 16, 9, TRUE)

############################################################
################# 2.b: Agg Comparison ######################
############################################################

#create data
agg_markups_nw <- data_mu %>%
  group_by(fyear) %>% # nolint
  summarise(Agg_MU = mean(MU, na.rm = TRUE), # nolint
            Agg_MU_DEU = mean(MU_deu, na.rm = TRUE))


agg_mu_comp_nw <- ggplot(data = agg_markups_nw, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup",
                linetype = "Cost Accounting Markup"), size = 1.5) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup",
                linetype = "Production Function Markup"), size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_linetype_manual(name = "",
                        values = c("Cost Accounting Markup" = "solid",
                                   "Production Function Markup" = "dashed")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Average Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL, keywidth = 4, keyheight = 1),
         linetype = guide_legend(title = NULL, keywidth = 4, keyheight = 1))

agg_mu_comp_nw

cor(agg_markups_nw$Agg_MU, agg_markups_nw$Agg_MU_DEU, use = "complete.obs")
# 0.9730549 sector time
#0.9754568 by sector

#save plot
save_f(agg_mu_comp_nw, "agg_mu_comp_nw_st.pdf", dircs, 9, 9, TRUE)

#save plot
save_f(agg_mu_comp_nw, "agg_mu_comp_nw_st_wide.pdf", dircs, 16, 9, TRUE)


#diffferent color for other paper

mu_endog_g <- ggplot(data = agg_markups_nw, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup",
                linetype = "Cost Accounting Markup"), size = 2.5) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup",
                linetype = "Production Function Markup"), size = 2.5) +
  theme(text = element_text(size = 30)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "black")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Average Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 40), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL))

mu_endog_g

#save plot
save_f(mu_endog_g, "mu_endog_g.pdf", dircs, 16, 10, TRUE)

############################################################
################# 2.b: Agg Comparison weighted ######################
############################################################

#create data
agg_markups <- data_mu %>%
  group_by(fyear) %>% # nolint
  summarise(Agg_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            Agg_MU_DEU = weighted.mean(MU_deu, sale, na.rm = TRUE))


agg_mu_comp <- ggplot(data = agg_markups, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup",
                linetype = "Cost Accounting Markup"), size = 1.5) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup",
                linetype = "Production Function Markup"), size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_linetype_manual(name = "",
                        values = c("Cost Accounting Markup" = "solid",
                                   "Production Function Markup" = "dashed")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Sales Weighted Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL, keywidth = 4, keyheight = 1),
         linetype = guide_legend(title = NULL, keywidth = 4, keyheight = 1))

agg_mu_comp

cor(agg_markups$Agg_MU, agg_markups$Agg_MU_DEU, use = "complete.obs")
# 0.8798914 sector time
#0.9648306 constant
#0.9653266 by sector


#save plot
save_f(agg_mu_comp, "agg_mu_comp_st.pdf", dircs, 9, 9, TRUE)

#save plot
save_f(agg_mu_comp, "agg_mu_comp_st_wide.pdf", dircs, 16, 9, TRUE)

############################################################
################### 2.c: Scatterplot #######################
############################################################
set.seed(123)  # for reproducibility

data_sample <- data_mu %>% ungroup() %>% sample_n(2000)

# Calculate percentiles
x_limits <- quantile(data_mu$MU - 1, c(0.1, 0.9), na.rm = TRUE)
y_limits <- quantile(data_mu$MU_deu - 1, c(0.1, 0.9), na.rm = TRUE)

# Plot
scatter_comp <- ggplot() +
  geom_point(data = data_sample, aes(x = MU - 1, y = MU_deu - 1),
             shape = 1, size = 1.25, alpha = .5) +
  stat_smooth(data = data_mu,
              aes(x = MU - 1, y = MU_deu - 1, color = "OLS line",
                  linetype = "OLS line"),
              method = "lm",
              se = FALSE,
              fullrange = TRUE, size = 1.5) +
  geom_line(aes(x = c(0, 2), y = c(0, 2), color = "45-degree line",
                linetype = "45-degree line"),
            size = 1.5, show.legend = TRUE) +
  scale_color_manual("",
                     values = c("OLS line" = "blue",
                                "45-degree line" = "red")) +
  scale_linetype_manual("",
                        values = c("OLS line" = "solid",
                                   "45-degree line" = "dashed")) +
  coord_cartesian(xlim = x_limits,
                  ylim = y_limits) +
  labs(x = "Cost Accounting Markup ([p-mc]/mc)",
       y = "Production Function Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL, keywidth = 4, keyheight = 1),
         linetype = guide_legend(title = NULL, keywidth = 4, keyheight = 1))

#display plot
scatter_comp

#save plot
save_f(scatter_comp, "scatter_comp_st.pdf", dircs, 9, 9, TRUE)


#save plot
save_f(scatter_comp, "scatter_comp_st_wide.pdf", dircs, 16, 9, TRUE)

############################################################
################### By sector: Scatterplot #######################
############################################################


#create data
agg_markups_s <- data_mu %>%
  group_by(industry) %>% # nolint
  summarise(MU = mean(MU, na.rm = TRUE), # nolint
            MU_deu = mean(MU_deu, na.rm = TRUE))

#create plot
scatter_comp_s <- ggplot() +
  geom_point(data = agg_markups_s, aes(x = MU - 1, y = MU_deu - 1),
             shape = 1, size = 3.5) +
  stat_smooth(data = agg_markups_s,
              aes(x = MU - 1, y = MU_deu - 1, color = "OLS line",
                  linetype = "OLS line"),
              method = "lm",
              se = FALSE,
              fullrange = TRUE, size = 1.5) +
  geom_line(aes(x = c(0, 1), y = c(0, 1), color = "45-degree line",
                linetype = "45-degree line"),
            size = 1.5, show.legend = TRUE) +
  scale_color_manual("",
                     values = c("OLS line" = "blue",
                                "45-degree line" = "red")) +
  scale_linetype_manual("",
                        values = c("OLS line" = "solid",
                                   "45-degree line" = "dashed")) +
  labs(x = "Sector Average Cost Accounting Markup ([p-mc]/mc)",
       y = "Sector Average Production Function Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL, keywidth = 4, keyheight = 1),
         linetype = guide_legend(title = NULL, keywidth = 4, keyheight = 1))

# Print the plot for debugging
print(scatter_comp_s)

#save plot
save_f(scatter_comp_s, "s_scatter_comp_st.pdf", dircs, 9, 9, TRUE)


#save plot
save_f(scatter_comp_s, "s_scatter_comp_st_wide.pdf", dircs, 16, 9, TRUE)

mean(agg_markups_s$MU/agg_markups_s$MU_deu)

cor(agg_markups_s$MU, agg_markups_s$MU_deu, use = "complete.obs")
# by sector corr = 0.6297631


###################################################
################# agg by firm #######################
###################################################

#create data
firm_markups <- data_mu %>%
  group_by(GVKEY) %>% # nolint
  summarise(MU = mean(MU, na.rm = TRUE), # nolint
            MU_deu = mean(MU_deu, na.rm = TRUE))

#create plot
firm_markups_comp <- ggplot() +
  geom_point(data = firm_markups, aes(x = MU - 1, y = MU_deu - 1), shape = 1) +
  stat_smooth(data = firm_markups,
              aes(x = MU - 1, y = MU_deu - 1, color = "OLS line"),
              method = "lm",
              se = FALSE,
              fullrange = TRUE) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_blank(aes(color = "45-degree line")) +
  coord_cartesian(xlim = x_limits,
                  ylim = y_limits) +
  scale_color_manual("",
                     values = c("OLS line" = "blue",
                                "45-degree line" = "red")) +
  labs(x = "Sector Average Cost Accounting Markup ([p-mc]/mc)",
       y = "Sector Average Production Function Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 20), legend.position = "bottom")

firm_markups_comp


#save plot
save_f(scatter_comp_s, "s_scatter_comp_s.pdf", dircs, 9, 9, TRUE)

mean(agg_markups_s$MU/agg_markups_s$MU_deu)

cor(agg_markups_s$MU, agg_markups_s$MU_deu, use = "complete.obs")



############################################################
################# 2.b: Agg Comparison by sector ############
############################################################

#retail averages (naisc 44,45)
agg_markups_retail <- data_mu %>%
    filter(industry %in% c(44, 45)) %>%
    group_by(fyear) %>% # nolint
    summarise(Agg_MU = mean(MU, na.rm = TRUE), # nolint
              Agg_MU_DEU = mean(MU_deu, na.rm = TRUE))

#manufacturing averages (naisc 31-33)
agg_markups_manu <- data_mu %>%
    filter(industry %in% c(31, 32, 33)) %>%
    group_by(fyear) %>% # nolint
    summarise(Agg_MU = mean(MU, na.rm = TRUE), # nolint
              Agg_MU_DEU = mean(MU_deu, na.rm = TRUE))

#whole sale averages (naisc 42)
agg_markups_whole <- data_mu %>%
    filter(industry == 42) %>%
    group_by(fyear) %>% # nolint
    summarise(Agg_MU = mean(MU, na.rm = TRUE), # nolint
              Agg_MU_DEU = mean(MU_deu, na.rm = TRUE))

#information averages (naisc 51)
agg_markups_info <- data_mu %>%
    filter(industry == 51) %>%
    group_by(fyear) %>% # nolint
    summarise(Agg_MU = mean(MU, na.rm = TRUE), # nolint
              Agg_MU_DEU = mean(MU_deu, na.rm = TRUE))


#plot retail
agg_mu_comp_retail <- ggplot(data = agg_markups_retail, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup"),
            size = 1.5) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup",
                linetype = "Production Function Markup"), size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_linetype_manual(name = "",
                        values = c("Cost Accounting Markup" = "solid",
                                   "Production Function Markup" = "dashed")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Average Markup ([p-mc]/mc)", title = "Retail") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL),
         linetype =  guide_legend(title = NULL))

agg_mu_comp_retail

#plot manufacturing
agg_mu_comp_manu <- ggplot(data = agg_markups_manu, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup"),
            size = 1.5) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup",
                linetype = "Production Function Markup"), size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_linetype_manual(name = "",
                        values = c("Cost Accounting Markup" = "solid",
                                   "Production Function Markup" = "dashed")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Average Markup ([p-mc]/mc)",
       title = "Manufactoring") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL),
         linetype =  guide_legend(title = NULL))

agg_mu_comp_manu

#plot whole sale
agg_mu_comp_whole <- ggplot(data = agg_markups_whole, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup"),
            size = 1.5) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup",
                linetype = "Production Function Markup"), size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_linetype_manual(name = "",
                        values = c("Cost Accounting Markup" = "solid",
                                   "Production Function Markup" = "dashed")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Average Markup ([p-mc]/mc)",
       title = "Whole Sale") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL),
         linetype =  guide_legend(title = NULL))

agg_mu_comp_whole

#plot information
agg_mu_comp_info <- ggplot(data = agg_markups_info, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup"),
            size = 1.5) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup",
                linetype = "Production Function Markup"), size = 1.5) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_linetype_manual(name = "",
                        values = c("Cost Accounting Markup" = "solid",
                                   "Production Function Markup" = "dashed")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Average Markup ([p-mc]/mc)",
       title = "Information Sector") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL),
         linetype =  guide_legend(title = NULL))

agg_mu_comp_info


#navigate to latex
setwd(dircs[3])
#ipo x year
pdf("agg_mu_comp_secs.pdf", width = 20, height = 20)
grid.arrange(agg_mu_comp_manu, agg_mu_comp_retail,
             agg_mu_comp_whole, agg_mu_comp_info,
             ncol = 2)
dev.off()



#navigate to latex
setwd(dircs[3])
#ipo x year
pdf("agg_mu_comp_secs_wide.pdf", width = 35, height = 20)
grid.arrange(agg_mu_comp_manu, agg_mu_comp_retail,
             agg_mu_comp_whole, agg_mu_comp_info,
             ncol = 2)
dev.off()





#####################################################


# Initialize an empty list to store the plots
plot_list <- list()

#loop over sectors and plot annnual averages within each sector (in seperate plots)
for (i in unique(data_mu$industry)) {
  #create data
  agg_markups_s <- data_mu %>%
    filter(industry == i) %>%
    group_by(fyear) %>% # nolint
    summarise(Agg_MU = mean(MU, na.rm = TRUE), # nolint
              Agg_MU_DEU = mean(MU_deu, na.rm = TRUE))

  #create plot
  agg_mu_comp_s <- ggplot(data = agg_markups_s, aes(x = fyear)) +
    geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup")) +
    geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup")) +
    theme(text = element_text(size = 20)) +
    scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                   "Production Function Markup" = "red")) +
    scale_x_continuous(limits = c(1955, 2022)) +
    labs(x = "Year", y = "Average Markup ([p-mc]/mc)") +
    theme(text = element_text(size = 20), legend.position = "bottom") +
    guides(colour = guide_legend(title = NULL))

  #display plot
  
  print(agg_mu_comp_s)

  #store plot with sector name (not as pdf but as ggplot object)
    # Store plot in the list with sector name
  plot_list[[paste0("agg_mu_comp_s_", i)]] <- agg_mu_comp_s
}



