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
#Real GDP (annual) from fred
rgdp <- fredr_series_observations("GDPCA")[, c(1, 3)]
names(rgdp) <- c("year", "RGDP")
#convernt date to year
rgdp$year <- as.numeric(format(as.Date(rgdp$year), "%Y"))
#user cost (from CSV)
usercost <- read.csv("usercost.csv") #nolint
#fernald series (from excel)
fernald <- read_excel(path = paste(dircs[2],
                                   "/fernald_series.xlsx", sep = "/"),
                      sheet = "annual")[, c(1, 14)]
names(fernald) <- c("year", "dtfp")

################### 1.b clean data #########################
############################################################

#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"

#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

#apply GDP deflator and generate xad measures
dset <- vargen(dset, usercost)
dset <- GDPdef(dset, usercost)

#clean
data <- clean_deu(dset)
data <- data %>%
  filter(!is.na(MU))






############################################################
############################################################
#     2: Aggregate Analysis
############################################################
############################################################


#####################################################
####### 2.1 gen agg data
#####################################################

#aggregate data
agg_markups <- data %>%
  group_by(fyear) %>% # nolint
  summarise(Agg_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            sw_MU = mean(MU, na.rm = TRUE))

names(agg_markups) <- c("year", "Agg_MU", "sw_MU")

#merge with RGDP and fernald
ag_data <- merge(agg_markups, rgdp, by = "year")
ag_data <- merge(ag_data, fernald, by = "year")

#create lags well need
l_data <- ag_data[,c(1:4)]
l_data$year <- ag_data$year + 1
names(l_data) <- c("year", "Agg_MU_l", "sw_MU_l", "RGDP_l")

#merge back
ag_data <- merge(ag_data, l_data, by = "year")

#generate GDP growth rate, dMU and dsw_MU
ag_data$GDP_g <- (ag_data$RGDP[] - ag_data$RGDP_l) / ag_data$RGDP_l * 100
ag_data$dMU <- ag_data$Agg_MU - ag_data$Agg_MU_l
ag_data$dsw_MU <- ag_data$sw_MU - ag_data$sw_MU_l

#generate cumulative dtfp
ag_data <- ag_data %>%
  arrange(year) %>%
  mutate(ctfp = cumsum(dtfp))


head(ag_data)


#####################################################
####### 2.2 plot agg data
#####################################################

###########################
# plot levels
###########################
# going to plot RGDP and ctfp as a percentage of 2024 value
#get relative ratio of ranges to scale plot by
range_mu <- range(c(ag_data$Agg_MU, ag_data$sw_MU), na.rm = TRUE)
range_gdp <- range(c(ag_data$RGDP / max(ag_data$RGDP),
                     ag_data$ctfp / max(ag_data$ctfp)), na.rm = TRUE)

# Calculate the transformation factor
sf1 <- diff(range_mu) / diff(range_gdp)
# Calculate the offset to shift the GDP and TFP values
o1 <- min(range_mu) - min(range_gdp * sf1)

# Create the plot
agg_levels <- 
ggplot(ag_data, aes(x = year)) +
  geom_line(aes(y = Agg_MU, color = "MU"), size = 1) +
  geom_line(aes(y = sw_MU, color = "sw MU"), size = 1) +
  geom_line(aes(y = (RGDP / max(RGDP)) * sf1 + offset,
                color = "GDP growth"), size = 1) +
  geom_line(aes(y = (ctfp / max(ctfp)) * sf1 + offset,
                color = "ctfp"), size = 1) +
  scale_y_continuous(
    name = "Markups (Average, Sales Weighted)",
    sec.axis = sec_axis(~ (. - o1) / sf1,
                        name = "GDP, TFP (as % of 2023 LeveL)",
                        labels = percent)
  ) +
  scale_color_manual(
    values = c("MU" = "black", "sw MU" = "blue",
               "GDP growth" = "red", "ctfp" = "purple"),
    labels = c("MU" = "Average Markup",
               "sw MU" = "Sales Weighted Markup",
               "GDP growth" = "GDP", "ctfp" = "TFP")
  ) +
  labs(
    x = "Year",
    color = "Legend"
  ) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  theme(legend.position = "bottom")

agg_levels




###########################
# plot change
###########################
# Calculate the range for each set of variables
range_dmu <- range(c(ag_data$dMU, ag_data$dsw_MU), na.rm = TRUE)
range_dgdp <- range(c(ag_data$GDP_g, ag_data$dtfp), na.rm = TRUE)

# Calculate the transformation factor
sf2 <- diff(range_dgdp) / diff(range_dmu)
# Calculate the offset to shift the GDP and TFP values
o2 <- min(range_dmu) - min(range_dgdp * sf2)

# Create the plot
agg_d <- ggplot(ag_data, aes(x = year)) +
  geom_line(aes(y = dMU, color = "dMU"), size = 1) +
  geom_line(aes(y = dsw_MU, color = "dsw MU"), size = 1) +
  geom_line(aes(y = GDP_g / scale_factor, color = "GDP growth"), size = 1) +
  geom_line(aes(y = dtfp / scale_factor, color = "ctfp"), size = 1) +
  scale_y_continuous(
    name = "Change (Average / Sales Weighted Markups)",
    limits = range_dMU_dsw_MU,
    sec.axis = sec_axis(~ (. - o2) / sf2,
                        name = "GDP growth, Change In Fernald Series TFP",
                        labels = percent)
  ) +
  scale_color_manual(
    values = c("dMU" = "black", "dsw MU" = "blue",
               "GDP growth" = "red", "ctfp" = "purple"),
    labels = c("dMU" = "Average Markup Change",
               "dsw MU" = "Sales Weighted Markup Change",
               "GDP growth" = "GDP", "ctfp" = "TFP")
  ) +
  labs(
    x = "Year",
    color = "Legend"
  ) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  theme(legend.position = "bottom")

# Print the plot
agg_d



#save

save_f(agg_levels, "cyc_l.pdf", dircs, 11.5, 12, TRUE)
save_f(agg_d, "cyc_d.pdf", dircs, 11.5, 12, TRUE)

#####################################################
####### Some regressions on agg data
#####################################################

#####################################################
#first in levels
#####################################################

#regressions
model_l1 <- feols(Agg_MU ~ Agg_MU_l + year + RGDP,
                  data = ag_data)
model_l2 <- feols(Agg_MU ~ Agg_MU_l + ctfp + year,
                  data = ag_data)
model_l3 <- feols(Agg_MU ~ Agg_MU_l + RGDP + ctfp + year,
                  data = ag_data)
model_l4 <- feols(sw_MU ~ sw_MU_l + RGDP + year,
                  data = ag_data)
model_l5 <- feols(sw_MU ~ sw_MU_l + ctfp + year,
                  data = ag_data)
model_l6 <- feols(sw_MU ~ sw_MU_l + RGDP + ctfp + year,
                  data = ag_data)


# Create a named character vector of new names
names_l <- c(
  "Agg_MU" = "Average Markups",
  "Agg_MU_l" = "Laggged Average Markups",
  "RGDP" = "Real GDP",
  "time" = "Year",
  "ctfp" = "TFP (Fernald Series, Cumulative)",
  "sw_MU" = "Sales Weighted Markups",
  "sw_MU_l" = "Laggged Sales Weighed Markups",
  "year" = "Year"
)


# Create a named list of models
modelsl <- list("Model 1" = model_l1,
                "Model 2" = model_l2,
                "Model 3" = model_l3,
                "Model 4" = model_l4,
                "Model 5" = model_l5,
                "Model 6" = model_l6)

# Create the summary table with the new names
summary_tablel <- etable(modelsl, dict = names_l)


summary_tablel

etable(modelsl, dict = names_l, tex = TRUE)





#####################################################
#changes
#####################################################

#regressions
model_c1 <- feols(dMU ~ + year + GDP_g,
                  data = ag_data)
model_c2 <- feols(dMU ~ + dtfp + year,
                  data = ag_data)
model_c3 <- feols(dMU ~ GDP_g + dtfp + year,
                  data = ag_data)
model_c4 <- feols(dsw_MU ~ GDP_g + year,
                  data = ag_data)
model_c5 <- feols(dsw_MU ~ dtfp + year,
                  data = ag_data)
model_c6 <- feols(dsw_MU ~ GDP_g + dtfp + year,
                  data = ag_data)

# Create a named character vector of new names
names_c <- c(
  "dMU" = "Change in Average Markups",
  "dsw_MU" = "Change in Sales Weighted Markups",
  "GDP_g" = "Real Growth Rate",
  "time" = "Year",
  "dtfp" = "Change in TFP (Fernald Series, Cumulative)",
  "year" = "Year"
)

# Create a named list of models
modelsc <- list("Model 1" = model_c1,
                "Model 2" = model_c2,
                "Model 3" = model_c3,
                "Model 4" = model_c4,
                "Model 5" = model_c5,
                "Model 6" = model_c6)

# Create the summary table with the new names
summary_tablec <- etable(modelsc, dict = names_c)


summary_tablel
summary_tablec

etable(modelsc, dict = names_c, tex = TRUE)