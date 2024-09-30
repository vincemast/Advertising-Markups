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
library(tidyverse)
library(knitr)
library(kableExtra)
library(stargazer)
library(plm)
library(lmtest)
library(sandwich)
library(purrr)
library(grid)


#navigate to folder with functions
setwd(dircs[1])
#functions
source("function_subsets.R")
source("function_plots.R")
source("function_regressions.R")
source("function_usefull.R")

############################################################
############################################################
#     1: Load Data
############################################################
############################################################
#navigate to folder with markup data
setwd(dircs[2])

#import #compustat
dset <- read.csv("COMPUSTAT.csv") # nolint
#FRED DATA
usercost <- read.csv("usercost.csv") #nolint

#rename gvkey to GVKEY
dset <- dset %>% rename(GVKEY = gvkey)
#apply GDP deflator and generate xad measures
dset <- vargen(dset, usercost)
dset <- GDPdef(dset, usercost)


#grab gvkeys of faang and friends
faang_g <-
  c("160329", #Alphabet Inc (google)
    "1690", #Apple Inc
    "12141", #microsoft Corp
    "117768", #Nvidia Corp
    "64768", #Amazon.com Inc
    "170617", #Meta Platforms inc
    "147579", #Netflix INc
    "184996" #Tesla Inc
  )

faang <- dset[dset$GVKEY %in% faang_g, ]

names(faang)

#clean
data <- clean_deu(dset)
data <- data %>%
  filter(!is.na(MU))

# get full sample and sector averages
#tech are those with naisc that start with 52
avg <- data %>%
  group_by(fyear) %>%
  summarise(avg_markup = mean(MU_1, na.rm = TRUE),
            tech_avg = mean(MU_1[substr((dset$naics), 1 , 2 ) == 51], na.rm = TRUE)
            )

view(avg)

#trim average to start at 1980
avg <- avg[avg$fyear >= 1980, ]

############################################################
############################################################
#     2: Plot
############################################################
############################################################


# Generate a color palette using scales
color_palette <- hue_pal()(length(unique(faang$conm)))

# Plot markup by firm over time with names from conm
faang_plot <- ggplot(faang, aes(x = fyear, y = MU_1)) +
  geom_line(aes(color = conm), size = 1) +
  geom_line(data = avg, aes(x = fyear, y = avg_markup,
                            linetype = "Full Sample Average"),
            color = "black", size = 1) +
  geom_line(data = avg, aes(x = fyear, y = tech_avg,
            linetype = "Sector Average"),
            color = "grey", size = 1) +
  labs(x = "Year",
       y = "Markup ([p-mc]/mc)") +
  labs(color = "") +
  scale_color_manual(name = "", 
                     values = c(setNames(color_palette, unique(faang$conm)), 
                                "Full Sample Average" = "black", 
                                "Sector Average" = "grey")) +
  scale_linetype_manual(name = "",
                        values = c("Full Sample Average" = "dashed",
                                   "Sector Average" = "dashed")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(color = guide_legend(order = 1, nrow = 2),
         linetype = guide_legend(order = 3, nrow = 2,
                                 override.aes = list(color = c("black",
                                                               "grey"))))

# Print the plot for debugging
print(faang_plot)



save_f(faang_plot, "faang.pdf", dircs, 16, 12, TRUE)