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

################### 1.c gen agg data #########################
############################################################

#aggregate data
agg_markups <- data %>%
  group_by(fyear) %>% # nolint
  summarise(Agg_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            sw_MU = mean(MU, na.rm = TRUE))

names(agg_markups) <- c("year", "Agg_MU", "sw_MU")

#merge with RGDP and fernald
ag_data <- merge(agg_markups, rgdp, by = "year")
ag_data <- merge(ag_data, fernald, by = "year")


#gen decade indicator by taking first 3 digits of year then adding 0
ag_data$decade <- as.numeric(substr(as.character(ag_data$year), 1, 3))*10



################### 1.d gen GDP growth rate #########################
############################################################
#create lag
l_data <- ag_data[, c("year", "RGDP")]
l_data$year <- l_data$year + 1
names(l_data) <- c("year", "RGDP_l")
#merge back
ag_data <- merge(ag_data, l_data, by = "year")
#generate GDP growth rate
ag_data$GDP_g <- (log(ag_data$RGDP) - log(ag_data$RGDP_l)) * 100
#convert to percentage points

################### 1.e detrend #########################
############################################################

#filter to precovid
data_f <- ag_data[ag_data$year < 2019, ]

# function to detrend by hp filter
#lambda 100 since annual
detrend_hp <- function(x, lambda = 100) {
  hp_result <- hpfilter(x, freq = lambda)
  # Calculate percentage deviation from trend
  percent_deviation <- (hp_result$cycle / hp_result$trend) * 100
  return(percent_deviation)
}
#gives output in units of % point deviation from trend


# take log first 
#since convert to % doesnt effect interpretation, but hp is linear so
detrend_hp_log <- function(x, lambda = 100) {
  hp_result <- hpfilter(log(x), freq = lambda)
  # Calculate percentage deviation from trend
  percent_deviation <- (hp_result$cycle / hp_result$trend) * 100
  return(percent_deviation)
}
#gives output in units of % point deviation from trend

#detrend Agg_MU and sw_MU
data_f$Agg_MU_dt <- detrend_hp_log(data_f$Agg_MU-1)
data_f$sw_MU_dt <- detrend_hp_log(data_f$sw_MU-1)

#detrend GDP
data_f$GDP_dt <- detrend_hp_log(data_f$RGDP)

################### 1.e gen lags #########################
############################################################

#create lags
to_lag <- c("Agg_MU", "sw_MU", "RGDP", "dtfp", "GDP_g",
            "Agg_MU_dt", "sw_MU_dt", "GDP_dt")

# Add "_l" to each element
lag_n <- paste0(to_lag, "_l")

l_data <- data_f[, c("year", to_lag)]
l_data$year <- l_data$year + 1
names(l_data) <- c("year", lag_n)
l_data <- l_data[, c("year", lag_n[c(1:2, 4:8)])]


#merge back
data_f <- merge(data_f, l_data, by = "year")

#create 2ndlags
l_data_2 <- data_f[, c("year", lag_n)]
l_data_2$year <- l_data_2$year + 1
lagn2 <- paste0(lag_n, "2")
names(l_data_2) <- c("year", lagn2)
#merge back
data_f <- merge(data_f, l_data_2, by = "year")


#create 3rdlags
l_data_3 <- data_f[, c("year", lagn2)]
l_data_3$year <- l_data_3$year + 1
lagn3 <- paste0(lag_n, "3")
names(l_data_3) <- c("year", lagn3)
#merge back
data_f <- merge(data_f, l_data_3, by = "year")

names(data_f)

############################################################
############################################################
#     2: Plot
############################################################
############################################################
#make seperate data frame for plot data
plotname <- c("year", "Agg_MU_dt", "sw_MU_dt", "GDP_g", "dtfp", "GDP_dt")
plot_data <- data_f[, plotname]


##############2.1 avg mu and detrend GDP ####################
#####################################################


mu_gdp_plot <-
  ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = Agg_MU_dt / 100, color = "MU"), size = 1) +
  geom_line(aes(y = (GDP_dt / 100), color = "GDPg"), size = 1) +
  scale_color_manual(
    values = c("MU" = "black",
               "GDPg" = "blue"),
    labels = c("MU" = "Average Markup",
               "GDPg" = "Real GDP Growth Rate")
  ) +
  scale_y_continuous(
    name = "Deviation From Trend",
    labels = percent
  ) + 
  labs(
    x = "Year",
    y = "Deviation From Trend",
    color = "Legend"
  ) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  theme(legend.position = "bottom")
mu_gdp_plot


##############2.2 sw mu and detrend GDP ####################
#####################################################

swmu_gdp_plot <-
  ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = sw_MU_dt / 100, color = "MU"), size = 1) +
  geom_line(aes(y = (GDP_dt / 100), color = "GDPg"), size = 1) +
  scale_color_manual(
    values = c("MU" = "black",
               "GDPg" = "blue"),
    labels = c("MU" = "Sales Weighted Markup",
               "GDPg" = "Real GDP Growth Rate")
  ) +
  scale_y_continuous(
    name = "Deviation From Trend",
    labels = percent
  ) + 
  labs(
    x = "Year",
    y = "Deviation From Trend",
    color = "Legend"
  ) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  theme(legend.position = "bottom")
swmu_gdp_plot


##############2.2 mu and GDP g ####################
#####################################################

range_mu <- range(c(plot_data$Agg_MU_dt / 100), na.rm = TRUE)
range_gdpg <- range(c(plot_data$GDP_g / 100), na.rm = TRUE)
range_swmu <- range(c(plot_data$sw_MU_dt / 100), na.rm = TRUE)
range_dtfp <- range(c(plot_data$dtfp / 100), na.rm = TRUE)


# Calculate the transformation factor
sf1 <- diff(range_mu) / diff(range_gdpg)
# Calculate the offset to shift the GDP and TFP values
o1 <- min(range_mu) - min(range_gdpg * sf1)

ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = Agg_MU_dt / 100, color = "MU"), size = 1) +
  geom_line(aes(y = ((GDP_g / 100) * sf1 + o1 ),
                color = "GDP growth"), size = 1) +
  scale_y_continuous(
    name = "Average Markups (deviation from trend)",
    labels = percent,
    sec.axis = sec_axis(~ (. - o1) / sf1,
                        name = "RGDP Growth Rate",
                        labels = percent)
  ) +
  scale_color_manual(
    values = c("MU" = "black",
               "GDP growth" = "blue"),
    labels = c("MU" = "Average Markup",
               "GDP growth" = "Lagged Real GDP Growth Rate")
  ) +
  labs(
    x = "Year",
    color = "Legend"
  ) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  theme(legend.position = "bottom")




##############2.2 mu and dtfp ####################
#####################################################

# Calculate the transformation factor
sf2 <- diff(range_mu) / diff(range_dtfp)
# Calculate the offset to shift the GDP and TFP values
o2 <- min(range_mu) - min(range_dtfp * sf1)

ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = Agg_MU_dt / 100, color = "MU"), size = 1) +
  geom_line(aes(y = ((dtfp / 100) * sf2 + o2 ),
                color = "GDP growth"), size = 1) +
  scale_y_continuous(
    name = "Average Markups (deviation from trend)",
    labels = percent,
    sec.axis = sec_axis(~ (. - o2) / sf2,
                        name = "Purified TFP Shocks",
                        labels = percent)
  ) +
  scale_color_manual(
    values = c("MU" = "black",
               "GDP growth" = "blue"),
    labels = c("MU" = "Average Markup",
               "GDP growth" = "Purified TFP Shocks")
  ) +
  labs(
    x = "Year",
    color = "Legend"
  ) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  theme(legend.position = "bottom")


#####################################################
#####################################################
#######    Regressions
#####################################################
#####################################################

names(data_f)

feols(Agg_MU_dt ~ GDP_g + GDP_g_l + GDP_g_l2 + GDP_g_l3,
      data = data_f)

feols(Agg_MU_dt ~ dtfp + dtfp_l + dtfp_l2 + dtfp_l3,
      data = data_f)



feols(sw_MU_dt ~ GDP_g + GDP_g_l + GDP_g_l2 + GDP_g_l3,
      data = data_f)

feols(sw_MU_dt ~ dtfp + dtfp_l + dtfp_l2 + dtfp_l3,
      data = data_f)



feols(Agg_MU_dt ~ GDP_dt,
                data = data_f)

feols(sw_MU_dt ~ GDP_dt,
                data = data_f)



head(data_f)





###old

#####################################################
#####################################################
#######    Regressions
#####################################################
#####################################################

#average
model1 <- feols(Agg_MU ~ year + dtfp_l,
                data = ag_data)
model2 <- feols(Agg_MU ~ year + dtfp_l + dtfp_l2,
                data = ag_data)
model3 <- feols(Agg_MU ~ + Agg_MU_l + year + dtfp_l + dtfp_l2,
                data = ag_data)
#sales weighted
model4 <- feols(sw_MU ~ year + dtfp_l,
                data = ag_data)
model5 <- feols(sw_MU ~ year + dtfp_l + dtfp_l2,
                data = ag_data)
model6 <- feols(sw_MU ~ + sw_MU_l + year + dtfp_l + dtfp_l2,
                data = ag_data)

# Create a named character vector of new names
names_l <- c(
  "Agg_MU" = "Average Markups",
  "Agg_MU_l" = "Average Markups t-1",
  "sw_MU" = "Sales Weighted Markups",
  "sw_MU_l" = "Sales Weighted Markups t-1",
  "dtfp_l" = "TFP Shocks t-1",
  "dtfp_l2" = "TFP Shocks t-2",
  "GDP_g_l" = "RGDP Growth t-1",
  "GDP_g_l2" = "RGDP Growth t-2",
  "year" = "Year"
)


# Create a named list of models
models <- list("Model 1" = model1,
               "Model 2" = model2,
               "Model 3" = model3,
               "Model 4" = model4,
               "Model 5" = model5,
               "Model 6" = model6)

# Create the summary table with the new names
summary_tablel <- etable(models, dict = names_l)

summary_tablel

etable(modelsl, dict = names_l, tex = TRUE)



#####################################################
#first with average
#####################################################

#regressions
#fernald
model_l1 <- feols(Agg_MU ~ year + dtfp_l,
                  data = ag_data)
model_l2 <- feols(Agg_MU ~ year + dtfp_l + dtfp_l2,
                  data = ag_data)
model_l3 <- feols(Agg_MU ~ + Agg_MU_l + year + dtfp_l + dtfp_l2,
                  data = ag_data)
#GDP
model_l4 <- feols(Agg_MU ~ year + GDP_g_l,
                  data = ag_data)
model_l5 <- feols(Agg_MU ~ year + GDP_g_l + GDP_g_l2,
                  data = ag_data)
model_l6 <- feols(Agg_MU ~ + Agg_MU_l + year + GDP_g_l + GDP_g_l2,
                  data = ag_data)
#both
model_l7 <- feols(Agg_MU ~ year + dtfp_l + GDP_g_l,
                  data = ag_data)
model_l8 <- feols(Agg_MU ~ year + dtfp_l + dtfp_l2 + GDP_g_l + GDP_g_l2,
                  data = ag_data)
model_l9 <- feols(Agg_MU ~ + Agg_MU_l + year +
                    dtfp_l + dtfp_l2 + GDP_g_l + GDP_g_l2,
                  data = ag_data)

# Create a named character vector of new names
names_l <- c(
  "Agg_MU" = "Average Markups",
  "Agg_MU_l" = "Average Markups t-1",
  "dtfp_l" = "TFP Shocks t-1",
  "dtfp_l2" = "TFP Shocks t-2",
  "GDP_g_l" = "RGDP Growth t-1",
  "GDP_g_l2" = "RGDP Growth t-2",
  "year" = "Year"
)


# Create a named list of models
modelsl <- list("Model 1" = model_l1,
                "Model 2" = model_l2,
                "Model 3" = model_l3,
                "Model 4" = model_l4,
                "Model 5" = model_l5,
                "Model 6" = model_l6,
                "Model 7" = model_l7,
                "Model 8" = model_l8,
                "Model 9" = model_l9)

# Create the summary table with the new names
summary_tablel <- etable(modelsl, dict = names_l)


summary_tablel

etable(modelsl, dict = names_l, tex = TRUE)


#####################################################
#sales weighted
#####################################################

#regressions
#fernald
model_l1_sw <- feols(sw_MU ~ year + dtfp_l,
                     data = ag_data)
model_l2_sw <- feols(sw_MU ~ year + dtfp_l + dtfp_l2,
                     data = ag_data)
model_l3_sw <- feols(sw_MU ~ + sw_MU_l + year + dtfp_l + dtfp_l2,
                     data = ag_data)

#GDP
model_l4_sw <- feols(sw_MU ~ year + GDP_g_l,
                     data = ag_data)
model_l5_sw <- feols(sw_MU ~ year + GDP_g_l + GDP_g_l2,
                     data = ag_data)
model_l6_sw <- feols(sw_MU ~ + sw_MU_l + year + GDP_g_l + GDP_g_l2,
                     data = ag_data)

#both
model_l7_sw <- feols(sw_MU ~ year + dtfp_l + GDP_g_l,
                     data = ag_data)
model_l8_sw <- feols(sw_MU ~ year + dtfp_l + dtfp_l2 + GDP_g_l + GDP_g_l2,
                     data = ag_data)
model_l9_sw <- feols(sw_MU ~ + sw_MU_l + year +
                       dtfp_l + dtfp_l2 + GDP_g_l + GDP_g_l2,
                     data = ag_data)

# Create a named character vector of new names
names_l <- c(
  "sw_MU" = "Sales Weighted Markups",
  "sw_MU_l" = "Sales Weighted Markups t-1",
  "dtfp_l" = "TFP Shocks t-1",
  "dtfp_l2" = "TFP Shocks t-2",
  "GDP_g_l" = "RGDP Growth t-1",
  "GDP_g_l2" = "RGDP Growth t-2",
  "year" = "Year"
)


# Create a named list of models
modelsl_sw <- list("Model 1" = model_l1_sw,
                   "Model 2" = model_l2_sw,
                   "Model 3" = model_l3_sw,
                   "Model 4" = model_l4_sw,
                   "Model 5" = model_l5_sw,
                   "Model 6" = model_l6_sw,
                   "Model 7" = model_l7_sw,
                   "Model 8" = model_l8_sw,
                   "Model 9" = model_l9_sw)

# Create the summary table with the new names
summary_table_sw <- etable(modelsl_sw, dict = names_l)


summary_table_sw

etable(modelsl_sw, dict = names_l, tex = TRUE)




##############################################################
#play ground
##############################################################

names(ag_data)


# next try with decade fixed effects instead
  #worse lmao

feols(Agg_MU ~ year + dtfp_l,
      data = ag_data)

feols(log(Agg_MU) ~ year + log(dtfp_l),
      data = ag_data)

feols(log(Agg_MU-1) ~ year + log(Agg_MU_l-1) + log(dtfp) + log(dtfp_l) + log(dtfp_l2),
      data = ag_data)

feols(log(Agg_MU-1) ~ log(dtfp_l) + log(dtfp_l2) +  log(dtfp) | decade,
      data = ag_data)

feols(log(sw_MU-1) ~ year + log(sw_MU_l-1) + log(GDP_g) + log(GDP_g_l) + log(GDP_g_l2),
      data = ag_data)


feols(log(Agg_MU-1) ~ year + log(dtfp) + log(dtfp_l) + log(dtfp_l2),
      data = ag_data[ag_data$year < 2019, ])

feols(log(Agg_MU-1) ~ year + log(dtfp) + log(dtfp_l) + log(dtfp_l2),
      data = ag_data[ag_data$year < 2019, ])

feols(log(Agg_MU-1) ~ year + log(GDP_g) + log(GDP_g_l) + log(GDP_g_l2),
      data = ag_data[ag_data$year < 2019, ])



#regressions
#fernald
model_l1 <- feols(Agg_MU ~ dtfp_l | decade,
                  data = ag_data)
model_l2 <- feols(Agg_MU ~ year + dtfp_l + dtfp_l2 | decade,
                  data = ag_data)
model_l3 <- feols(Agg_MU ~ + Agg_MU_l + dtfp_l + dtfp_l2 | decade,
                  data = ag_data)
#GDP
model_l4 <- feols(Agg_MU ~ GDP_g_l | decade,
                  data = ag_data)
model_l5 <- feols(Agg_MU ~ GDP_g_l + GDP_g_l2 | decade,
                  data = ag_data)
model_l6 <- feols(Agg_MU ~ + Agg_MU_l + GDP_g_l + GDP_g_l2 | decade,
                  data = ag_data)
#both
model_l7 <- feols(Agg_MU ~ dtfp_l + GDP_g_l | decade,
                  data = ag_data)
model_l8 <- feols(Agg_MU ~ dtfp_l + dtfp_l2 + GDP_g_l + GDP_g_l2 | decade,
                  data = ag_data)
model_l9 <- feols(Agg_MU ~ + Agg_MU_l +
                    dtfp_l + dtfp_l2 + GDP_g_l + GDP_g_l2 | decade,
                  data = ag_data)

# Create a named character vector of new names
names_l <- c(
  "Agg_MU" = "Average Markups",
  "Agg_MU_l" = "Average Markups t-1",
  "dtfp_l" = "TFP Shocks t-1",
  "dtfp_l2" = "TFP Shocks t-2",
  "GDP_g_l" = "RGDP Growth t-1",
  "GDP_g_l2" = "RGDP Growth t-2",
  "decade" = "Decadal"
)


# Create a named list of models
modelsl <- list("Model 1" = model_l1,
                "Model 2" = model_l2,
                "Model 3" = model_l3,
                "Model 4" = model_l4,
                "Model 5" = model_l5,
                "Model 6" = model_l6,
                "Model 7" = model_l7,
                "Model 8" = model_l8,
                "Model 9" = model_l9)

# Create the summary table with the new names
summary_tablel <- etable(modelsl, dict = names_l)


summary_tablel

etable(modelsl, dict = names_l, tex = TRUE)