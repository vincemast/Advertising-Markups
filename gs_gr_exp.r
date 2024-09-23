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
library(cowplot)
library(grid)
library(gridExtra)
library(fredr)
library(readxl)
library(pracma)
library(mFilter)
library(vars)
library(svars)
library(boot)
library(tseries)
library(AER)


#navigate to folder with functions
setwd(dircs[1])
#functions
source("function_subsets.R")
source("function_plots.R")
source("function_regressions.R")
source("function_usefull.R")

#load fred key
apikey <- "9fc98174c1127ca2629bab04f7f02951"
fredr_set_key(apikey)

################### 1.a load data #########################
############################################################

#navigate to with data
setwd(dircs[2])

#expected gov spending growth rate data
data <- read.csv("gs_gr_forecast.csv") # nolint

#keep only Q4 data
data <- data %>% filter(QUARTER == 4)

#from document drslgov{i}=100[ (\hat{X}_{t+i} / \hat{X}_{t+i-1})^4-1 ]
#(\hat{X}_{t+i} / \hat{X}_{t+i-1}) = [ .01 * drslgov{i} + 1 ] ^.25

#yoy growth is 100 *[ (\hat{X}_{t+4} / \hat{X}_{t}) - 1]

data$fcst_yoy <- 100 * (  (.01 * data$drslgov2+1)^(1/4) 
                 * (.01 * data$drslgov3 + 1)^(1/4)
                    * (.01 * data$drslgov4 + 1)^(1/4)
                    * (.01 * data$drslgov5 + 1)^(1/4) - 1)

data$fcstyear <- data$YEAR + 1

#keep only forecast and year it corresponds to
gvgr_exp <- data %>% dplyr::select(fcstyear, fcst_yoy)
names(gvgr_exp) <- c("year", "gvgr_exp")

#save data as csv
write.csv(gvgr_exp, "gvgr_exp.csv", row.names = FALSE)