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
#unemployment (annual) rate
unemp <- fredr_series_observations("UNRATE",
                                   frequency = "a",
                                   aggregation_method = "avg")[, c(1, 3)]
#NBER recession indicators
#going to take as 1 in any years where 1 quater indicates recession
rec <- fredr_series_observations("USRECQ",
                                 frequency = "a",
                                 observation_start = as.Date("1947-01-01"),
                                 aggregation_method = "sum")[, c(1, 3)]
# set to 1 if any quarter in year is recession
rec$value <- ifelse(rec$value > 0, 1, 0)

#unemployment (annual) rate
gdpdef <- fredr_series_observations("GDPDEF",
                                    frequency = "a",
                                    aggregation_method = "avg")[, c(1, 3)]

tbill <- fredr_series_observations("TB3MS",
                                   frequency = "a",
                                   aggregation_method = "avg")[, c(1, 3)]



names(rgdp) <- c("year", "RGDP")
names(unemp) <- c("year", "unemp")
names(rec) <- c("year", "rec")
names(gdpdef) <- c("year", "gdpdf")
names(tbill) <- c("year", "tbill")


#convernt date to year
rgdp$year <- as.numeric(format(as.Date(rgdp$year), "%Y"))
unemp$year <- as.numeric(format(as.Date(unemp$year), "%Y"))
rec$year <- as.numeric(format(as.Date(rec$year), "%Y"))
gdpdef$year <- as.numeric(format(as.Date(gdpdef$year), "%Y"))
tbill$year <- as.numeric(format(as.Date(tbill$year), "%Y"))

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

#merge with RGDP, unemp and fernald
ag_data <- merge(agg_markups, rgdp, by = "year")
ag_data <- merge(ag_data, unemp, by = "year")
ag_data <- merge(ag_data, fernald, by = "year")
ag_data <- merge(ag_data, gdpdef, by = "year")
ag_data <- merge(ag_data, tbill, by = "year")
ag_data <- merge(ag_data, rec, by = "year")

#gen decade indicator by taking first 3 digits of year then adding 0
ag_data$decade <- as.numeric(substr(as.character(ag_data$year), 1, 3)) * 10

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

# function detrended by hp filter with log trans
detrend_hp_log <- function(x, lambda = 100) {
  # Step 1: Apply log transformation
  log_x <- log(x)
  hp_result <- hpfilter(log_x, freq = lambda)
  exp_trend <- exp(hp_result$trend)
  percent_deviation <- ((x - exp_trend) / exp_trend) * 100
  return(percent_deviation)
}
#gives output in units of % point deviation from trend

# function to detrend by perserve units
  #not using atm but might want to do for ur later
  #easier interpretation of units?
detrend_hp_raw_units <- function(x, lambda = 100) {
  hp_result <- hpfilter(x, freq = lambda)
  # Calculate deviation from trend
  deviation <- hp_result$cycle
  return(deviation)
}



#detrend Agg_MU and sw_MU
#take log of markup ratio
data_f$Agg_MU_dt <- detrend_hp_log(data_f$Agg_MU - 1)
data_f$sw_MU_dt <- detrend_hp_log(data_f$sw_MU - 1)

#detrend GDP
data_f$GDP_dt <- detrend_hp_log(data_f$RGDP)
#detrend Unemp
data_f$unemp_dt <- detrend_hp_log(data_f$unemp)

################### 1.e gen lags #########################
############################################################

#create lags
to_lag <- c("Agg_MU", "sw_MU", "RGDP", "dtfp", "GDP_g",
            "Agg_MU_dt", "sw_MU_dt", "GDP_dt", "unemp_dt", "rec")

# Add "_l" to each element
lag_n <- paste0(to_lag, "_l")

l_data <- data_f[, c("year", to_lag)]
l_data$year <- l_data$year + 1
names(l_data) <- c("year", lag_n)
l_data <- l_data[, c("year", lag_n[c(1:2, 4:10)])]


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
plotname <- c("year", "Agg_MU_dt", "sw_MU_dt", "GDP_g",
              "dtfp", "GDP_dt", "unemp_dt", "rec")
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


##############2.1 avg mu and detrend unemp ####################
#####################################################


mu_ur_plot <-
  ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = Agg_MU_dt / 100, color = "MU"), size = 1) +
  geom_line(aes(y = (unemp_dt / 100), color = "GDPg"), size = 1) +
  scale_color_manual(
    values = c("MU" = "black",
               "GDPg" = "blue"),
    labels = c("MU" = "Average Markup",
               "GDPg" = "Unemployment Rate")
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
mu_ur_plot

#note here that untrended UR is dergivation FROM THE TREND
#so 40% means 40% higher than then trend not 40% unemp rate


##############2.1 avg, sw mu and recession indivators ####################
#####################################################

library(scales)

head(rec)

# Filter the recession data to get the start and end of recession periods
recession_periods <- plot_data %>%
  filter(rec == 1) %>%
  mutate(start = year, end = year)

# Add a factor variable for the fill aesthetic
recession_periods$fill <- "Recession"

mu_rec_plot <-
  ggplot(plot_data, aes(x = year)) +
  geom_rect(data = recession_periods,
            aes(xmin = start - 0.5, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf, fill = fill),
            alpha = 0.5) +
  geom_line(aes(y = Agg_MU_dt / 100, color = "MU"), size = 1) +
  geom_line(aes(y = (sw_MU_dt / 100), color = "swmu"), size = 1) +
  scale_color_manual(
    values = c("MU" = "black",
               "swmu" = "blue"),
    labels = c("MU" = "Average Markup",
               "swmu" = "Sales Weighted Markup")
  ) +
  scale_fill_manual(
    values = c("Recession" = "grey"),
    labels = c("Recession" = "Recession Period")
  ) +
  scale_y_continuous(
    name = "Deviation From Trend",
    labels = percent
  ) +
  labs(
    x = "Year",
    y = "Deviation From Trend",
    color = "",
    fill = ""
  ) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  theme(legend.position = "bottom")
mu_rec_plot



#####################################################
#####################################################
#######    3 Unconditional Cyclicality
#####################################################
#####################################################


#############3.1 average markups ################

# Add a dummy identifier to the data (required for NW st errors in feols)
data_f$id <- 1
# Run the regression model with Newey-West standard errors
model1 <- feols(Agg_MU_dt ~ GDP_g + GDP_g_l + GDP_g_l2 + GDP_g_l3,
                data = data_f,
                panel.id = ~id + year,
                vcov = NW(lag = 4))

model2 <- feols(Agg_MU_dt ~ dtfp + dtfp_l + dtfp_l2 + dtfp_l3,
                data = data_f,
                panel.id = ~id + year,
                vcov = NW(lag = 4))

model3 <- feols(Agg_MU_dt ~ unemp_dt + unemp_dt_l + unemp_dt_l2 + unemp_dt_l3,
                data = data_f,
                panel.id = ~id + year,
                vcov = NW(lag = 4))
#note units are weird here, need to think about a 1% increase in the
# deviation from trend of urate, range is like [-25:40]

model4 <- feols(Agg_MU_dt ~ rec + rec_l + rec_l2 + rec_l3,
                data = data_f,
                panel.id = ~id + year,
                vcov = NW(lag = 4))


names(data_f)

feols(Agg_MU_dt ~ GDP_dt - 1,
      data = data_f,
      panel.id = ~id + year,
      vcov = NW(lag = 4))

# Display the results
# Create a named character vector of new names
names_l <- c(
  "Agg_MU_dt" = "Average Markup (% point deviation from trend)",
  "sw_MU_dt" = "Sales Weighted Markup (% point deviation from trend)",
  "dtfp" = "Purified TFP Shock",
  "dtfp_l" = "Purified TFP Shock t-1",
  "dtfp_l2" = "Purified TFP Shock Shock t-2",
  "dtfp_l3" = "Purified TFP Shock Shock t-3",
  "GDP_g" = "RGDP Growth",
  "GDP_g_l" = "RGDP Growth t-1",
  "GDP_g_l2" = "RGDP Growth t-2",
  "GDP_g_l3" = "RGDP Growth t-3",
  "unemp_dt" = "Unemp. Rate (% deviation from trend)",
  "unemp_dt_l" = "Unemp. Rate t-1 (% deviation from trend)",
  "unemp_dt_l2" = "Unemp. Rate t-2 (% deviation from trend)",
  "unemp_dt_l3" = "Unemp. Rate t-3 (% deviation from trend)",
  "rec" = "Recession Indicator",
  "rec_l" = "Recession Indicator t-1",
  "rec_l2" = "Recession Indicator t-2",
  "rec_l3" = "Recession Indicator t-3",
  "year" = "Year"
)


# Create a named list of models
models <- list("Model 1" = model1,
               "Model 2" = model2,
               "Model 3" = model3,
               "Model 4" = model4)

# Create the summary table with the new names
summary_tablel <- etable(models, dict = names_l)


view(summary_tablel)


#############3.2 sales weighted markups ################

##### sales wighted
model1_sw <- feols(sw_MU_dt ~ GDP_g + GDP_g_l + GDP_g_l2 + GDP_g_l3,
                   data = data_f,
                   panel.id = ~id + year,
                   vcov = NW(lag = 4))

model2_sw <- feols(sw_MU_dt ~ dtfp + dtfp_l + dtfp_l2 + dtfp_l3,
                   data = data_f,
                   panel.id = ~id + year,
                   vcov = NW(lag = 4))

model3_sw <- feols(sw_MU_dt ~ unemp_dt + unemp_dt_l + unemp_dt_l2 + unemp_dt_l3,
                   data = data_f,
                   panel.id = ~id + year,
                   vcov = NW(lag = 4))
#note units are weird here, need to think about a 1% increase in the
# deviation from trend of urate, range is like [-25:40]

model4_sw <- feols(sw_MU_dt ~ rec + rec_l + rec_l2 + rec_l3,
                   data = data_f,
                   panel.id = ~id + year,
                   vcov = NW(lag = 4))



# Create a named list of models
models_sw <- list("Model 1" = model1_sw,
               "Model 2" = model2_sw,
               "Model 3" = model3_sw,
               "Model 4" = model4_sw)

# Create the summary table with the new names
summary_table_sw <- etable(models_sw, dict = names_l)


view(summary_table_sw)


#####################################################
#####################################################
#######    4 Conditional Cyclicality (sVAR)
#####################################################
#####################################################



########### 4.1 average

#log everything well use (except tbill rate and dtfp)
ag_data$ln_aMu <- log(ag_data$Agg_MU - 1)
ag_data$ln_swMu <- log(ag_data$sw_MU - 1)
ag_data$ln_gdp <- log(ag_data$RGDP)
ag_data$ln_gdpdf <- log(ag_data$gdpdf)

rec_c_order <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_aMu")

ts_data <- ts(ag_data[, rec_c_order], start = min(ag_data$year), frequency = 1)


# Estimate VAR model
var_model <- VAR(ts_data)

# Impose Cholesky Decomposition
x1 <- id.chol(var_model)

# Impulse response analysis
i1 <- irf(x1, n.ahead = 10)
plot(i1, scales = 'free_y')

# Bootstrap
n_ah = 30
n_boot = 10000
set.seed(123)
bb <- mb.boot(x1, n.ahead = n_ah, nboot = n_boot)
summary(bb)
#all plot
plot(bb, lowerq = .05, upperq = .95)

#get data for my plot
#point estimates
gdp_pe <- bb$true$irf$'epsilon[ dtfp ] %->% ln_gdp'
mu_pe <- bb$true$irf$'epsilon[ dtfp ] %->% ln_aMu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#loop over each bootstrap sample
gdp_boot <- matrix(NA, nrow = n_boot, ncol = n_ah)
mu_boot <- matrix(NA, nrow = n_boot, ncol = n_ah)

for (i in 1:n_boot) {
  # Extract the IRF for the i-th bootstrap sample
  irf_i <- bb$bootstrap[[i]]
  irf_i <- data.frame(unlist(irf_i))
  #irf_i is ordered: 1:n_ah is names
  #then its ordered with responses on y_1 first, then 2, so on and so forth
  #thus reponses on y_1 are n_ah + 1 : n_ah * 6
  #responses on y_2 are n_ah *(6) + 1 : n_ah * 11
  #responses on y_5 are n_ah *(21) + 1 : n_ah * 26
  #y_2 is ln_gdp, y_5 is ln_aMu
  gdp_boot[i, ] <- irf_i[(n_ah * 6 + 1):(n_ah * 7), 1]
  # Extract the IRF for dtfp -> ln_aMu
  mu_boot[i, ] <- irf_i[(n_ah * 21 + 1):(n_ah * 22), 1]
}

# Compute the 90% confidence intervals
gdp_lb <- apply(gdp_boot, 2, quantile, probs = 0.05)
gdp_ub <- apply(gdp_boot, 2, quantile, probs = 0.95)
mu_lb <- apply(mu_boot, 2, quantile, probs = 0.05)
mu_ub <- apply(mu_boot, 2, quantile, probs = 0.95)


# Plot the IRFs with 90% confidence intervals
gdp_irf_plot <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = gdp_lb, ymax = gdp_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1)) +
  labs(x = "Year", y = "",
       title = "Response of ln GDP to a TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "", values = c("90% Confidence Interval (Bootstrapped)" = "grey")) +
  theme_minimal() + 
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb, ymax = mu_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah-1)) +
  labs(x = "Year", y = "",
       title = "Response of ln MU to a TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "", values = c("90% Confidence Interval (Bootstrapped)" = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot


setwd(dircs[3])

#ipo x decade
pdf("IRF.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot, mu_irf_plot,
             ncol = 2)
dev.off()




##########################4.2 sales weighted markups ################

rec_c_order_sw <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_swMu")

ts_data_sw <- ts(ag_data[, rec_c_order_sw], start = min(ag_data$year), frequency = 1)


# Estimate VAR model
var_sw <- VAR(ts_data_sw)
# Impose Cholesky Decomposition
x1_sw <- id.chol(var_sw)
# Impulse response (no bs)
i_sw <- irf(x1_sw, n.ahead = 10)
plot(i_sw, scales = 'free_y')

# Bootstrap
n_ah = 11
n_boot = 10000
set.seed(123)
bb_sw <- mb.boot(x1_sw, n.ahead = n_ah, nboot = n_boot)

#get data for my plot
#point estimates
gdp_pe_sw <- bb_sw$true$irf$'epsilon[ dtfp ] %->% ln_gdp'
mu_pe_sw <- bb_sw$true$irf$'epsilon[ dtfp ] %->% ln_swMu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#loop over each bootstrap sample
gdp_boot_sw <- matrix(NA, nrow = n_boot, ncol = n_ah)
mu_boot_sw <- matrix(NA, nrow = n_boot, ncol = n_ah)

for (i in 1:n_boot) {
  # Extract the IRF for the i-th bootstrap sample
  irf_i <- bb_sw$bootstrap[[i]]
  irf_i <- data.frame(unlist(irf_i))
  #irf_i is ordered: 1:n_ah is names
  #then its ordered with responses on y_1 first, then 2, so on and so forth
  #thus reponses on y_1 are n_ah + 1 : n_ah * 6
  #responses on y_2 are n_ah *(6) + 1 : n_ah * 11
  #responses on y_5 are n_ah *(21) + 1 : n_ah * 26
  #y_2 is ln_gdp, y_5 is ln_aMu
  gdp_boot_sw[i, ] <- irf_i[(n_ah * 6 + 1):(n_ah * 7), 1]
  # Extract the IRF for dtfp -> ln_aMu
  mu_boot_sw[i, ] <- irf_i[(n_ah * 21 + 1):(n_ah * 22), 1]
}

# Compute the 90% confidence intervals
gdp_lb_sw <- apply(gdp_boot_sw, 2, quantile, probs = 0.05)
gdp_ub_sw <- apply(gdp_boot_sw, 2, quantile, probs = 0.95)
mu_lb_sw <- apply(mu_boot_sw, 2, quantile, probs = 0.05)
mu_ub_sw <- apply(mu_boot_sw, 2, quantile, probs = 0.95)


# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_sw <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = gdp_lb_sw, ymax = gdp_ub_sw,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1)) +
  labs(x = "Year", y = "",
       title = "Response of ln GDP to a TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() + 
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_sw


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_sw <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb_sw, ymax = mu_ub_sw,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah-1)) +
  labs(x = "Year", y = "",
       title = "Response of ln MU to a TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_sw


setwd(dircs[3])

#ipo x decade
pdf("IRF_sw.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_sw, mu_irf_plot_sw,
             ncol = 2)
dev.off()






















gdp_pe <- bb$true$irf$'epsilon[ dtfp ] %->% ln_gdp'



gdp_se <- bb$SE
bb$SE$'epsilon[ dtfp ] %->% ln_gdp'

mu_pe <- bb$true$irf$'epsilon[ dtfp ] %->% ln_aMu'



gdp_lb <- bb$Lower$'epsilon[ dtfp ] %->% ln_gdp'

hold$irf$

bb$


bb$SE









bb$

# Extract specific IRFs from bootstrap results
irf_dtfp_ln_gdp <- bb$irf$`epsilon[ dtfp ] %->% ln_gdp`
irf_dtfp_ln_aMu <- bb$irf$`epsilon[ dtfp ] %->% ln_aMu`

# Extract confidence intervals
ci_dtfp_ln_gdp_lower <- bb$Lower$`epsilon[ dtfp ] %->% ln_gdp`
ci_dtfp_ln_aMu_lower <- bb$Lower$`epsilon[ dtfp ] %->% ln_aMu`
ci_dtfp_ln_gdp_upper <- bb$Upper$`epsilon[ dtfp ] %->% ln_gdp`
ci_dtfp_ln_aMu_upper <- bb$Upper$`epsilon[ dtfp ] %->% ln_aMu`

# Plot the specific IRFs with confidence intervals
par(mfrow = c(2, 1))  # Set up a 2x1 plotting area

# Plot IRF for ln_gdp
plot(irf_dtfp_ln_gdp, type = "l", col = "blue", ylim = range(c(ci_dtfp_ln_gdp_lower, ci_dtfp_ln_gdp_upper)), ylab = "Response of ln_gdp", xlab = "Periods")
lines(ci_dtfp_ln_gdp_lower, col = "red", lty = 2)
lines(ci_dtfp_ln_gdp_upper, col = "red", lty = 2)
title("Response of ln_gdp to a Shock in dtfp")

# Plot IRF for ln_aMu
plot(irf_dtfp_ln_aMu, type = "l", col = "blue", ylim = range(c(ci_dtfp_ln_aMu_lower, ci_dtfp_ln_aMu_upper)), ylab = "Response of ln_aMu", xlab = "Periods")
lines(ci_dtfp_ln_aMu_lower, col = "red", lty = 2)
lines(ci_dtfp_ln_aMu_upper, col = "red", lty = 2)
title("Response of ln_aMu to a Shock in dtfp")











rec_c_order <- c("dtfp", "ln_gdp", "ln_gdpdf", "ln_aMu")

ts_data <- ts(ag_data[, rec_c_order], start = min(ag_data$year), frequency = 1)


# Estimate VAR model
var_model <- VAR(ts_data)

x1 <- id.chol(var_model)


# impulse response analysis
i1 <- irf(x1, n.ahead = 10)
plot(i1, scales = 'free_y')





# impulse response analysis
i1 <- irf(x1, n.ahead = 30)
i2 <- irf(x2, n.ahead = 30)
plot(i1, scales = 'free_y')
plot(i2, scales = 'free_y')





# Estimate VAR model
var_model <- VAR(ts_data, p = 1, type = "const") # Adjust 'p' based on AIC or BIC

# Perform SVAR with Choleski decomposition
svar_model <- SVAR(var_model, estmethod = "direct", Amat = diag(5))






# Plot impulse response functions
irf_result <- irf(svar_model, impulse = NULL, response = NULL, n.ahead = 20, boot = TRUE, ci = 0.90)

# Plot IRFs with 90% confidence intervals
plot(irf_result)



# Bootstrap procedure for 90% confidence intervals
boot_irf <- function(data, indices) {
  # Bootstrap sample
  boot_data <- data[indices, ]
  
  # Estimate VAR and SVAR
  boot_var <- VAR(boot_data, p = 1, type = "const")
  boot_svar <- SVAR(boot_var, estmethod = "direct", Amat = diag(5))
  
  # Compute IRFs
  boot_irf_result <- irf(boot_svar, impulse = NULL, response = NULL, n.ahead = 20)
  return(boot_irf_result$irf)
}

# Apply bootstrapping
set.seed(123) # For reproducibility
boot_results <- boot(ts_data, boot_irf, R = 1000)

# Extract and plot the bootstrapped confidence intervals
plot(irf_result, ci = boot_results)























# Define a function to compute IRFs
compute_irf <- function(data, p = 5, n.ahead = 10) {
  var_model <- VAR(data, p = p, type = "const")
  svar_model <- id.chol(var_model)
  irf_results <- irf(svar_model, n.ahead = n.ahead)
  return(irf_results)
}

# Compute the original IRFs
original_irf <- compute_irf(ts_data)

# Inspect the length of the IRFs for a single bootstrap sample
sample_irf <- compute_irf(ts_data)
length_irf_dtfp_ln_gdp <- length(sample_irf$irf$`epsilon[ dtfp ] %->% ln_gdp`)
length_irf_dtfp_ln_aMu <- length(sample_irf$irf$`epsilon[ dtfp ] %->% ln_aMu`)

# Number of bootstrap samples
n_boot <- 1000

# Initialize matrices to store bootstrapped IRFs with the correct number of columns
boot_irf_dtfp_ln_gdp <- matrix(NA, nrow = n_boot, ncol = length_irf_dtfp_ln_gdp)
boot_irf_dtfp_ln_aMu <- matrix(NA, nrow = n_boot, ncol = length_irf_dtfp_ln_aMu)
# Perform bootstrapping
set.seed(123)  # For reproducibility
for (i in 1:n_boot) {
  # Resample the data with replacement
  boot_data <- ts_data[sample(1:nrow(ts_data), replace = TRUE), ]
  
  # Compute IRFs for the bootstrap sample
  boot_irf <- compute_irf(boot_data)
  
  # Store the IRFs for dtfp -> ln_gdp and dtfp -> ln_aMu
  boot_irf_dtfp_ln_gdp[i, ] <- boot_irf$irf$`epsilon[ dtfp ] %->% ln_gdp`
  boot_irf_dtfp_ln_aMu[i, ] <- boot_irf$irf$`epsilon[ dtfp ] %->% ln_aMu`
}

# Compute the 90% confidence intervals
ci_lower_dtfp_ln_gdp <- apply(boot_irf_dtfp_ln_gdp, 2, quantile, probs = 0.1)
ci_upper_dtfp_ln_gdp <- apply(boot_irf_dtfp_ln_gdp, 2, quantile, probs = 0.9)
ci_lower_dtfp_ln_aMu <- apply(boot_irf_dtfp_ln_aMu, 2, quantile, probs = 0.1)
ci_upper_dtfp_ln_aMu <- apply(boot_irf_dtfp_ln_aMu, 2, quantile, probs = 0.9)

# Extract the original IRFs
irf_dtfp_ln_gdp <- original_irf$irf$`epsilon[ dtfp ] %->% ln_gdp`
irf_dtfp_ln_aMu <- original_irf$irf$`epsilon[ dtfp ] %->% ln_aMu`

# Plot the IRFs with confidence intervals
par(mfrow = c(2, 1))  # Set up a 2x1 plotting area


# Plot IRF for ln_gdp
plot(irf_dtfp_ln_gdp, type = "l", col = "blue", ylim = range(c(ci_lower_dtfp_ln_gdp, ci_upper_dtfp_ln_gdp)), ylab = "Response of ln_gdp", xlab = "Periods")
lines(ci_lower_dtfp_ln_gdp, col = "red", lty = 2)
lines(ci_upper_dtfp_ln_gdp, col = "red", lty = 2)
title("Response of ln_gdp to a Shock in dtfp")

# Plot IRF for ln_aMu
plot(irf_dtfp_ln_aMu, type = "l", col = "blue", ylim = range(c(ci_lower_dtfp_ln_aMu, ci_upper_dtfp_ln_aMu)), ylab = "Response of ln_aMu", xlab = "Periods")
lines(ci_lower_dtfp_ln_aMu, col = "red", lty = 2)
lines(ci_upper_dtfp_ln_aMu, col = "red", lty = 2)
title("Response of ln_aMu to a Shock in dtfp")
















# Estimate the VAR model
var_model <- VAR(ts_data, p = 5, type = "const")

# Impose Cholesky Decomposition using the svars package
svar_model <- id.chol(var_model)


# Compute impulse response functions (IRFs) with bootstrapped confidence intervals
irf_results <- irf(svar_model, n.ahead = 10, boot = TRUE, ci = 0.90)

 head(irf_results)


# Extract specific IRFs
irf_dtfp_ln_gdp <- irf_results$irf$`epsilon[ dtfp ] %->% ln_gdp`
irf_dtfp_ln_aMu <- irf_results$irf$`epsilon[ dtfp ] %->% ln_aMu`


# Inspect the structure of irf_results$Lower and irf_results$Upper
str(irf_results$Lower)
str(irf_results$Upper)


# Extract confidence intervals
ci_dtfp_ln_gdp <- irf_results$Lower$`epsilon[ dtfp ] %->% ln_gdp`
ci_dtfp_ln_aMu <- irf_results$Lower$`epsilon[ dtfp ] %->% ln_aMu`
ci_dtfp_ln_gdp_upper <- irf_results$Upper$`epsilon[ dtfp ] %->% ln_gdp`
ci_dtfp_ln_aMu_upper <- irf_results$Upper$`epsilon[ dtfp ] %->% ln_aMu`

# Plot the IRFs with confidence intervals
par(mfrow = c(2, 1))  # Set up a 2x1 plotting area

# Plot IRF for ln_gdp
plot(irf_dtfp_ln_gdp, type = "l", col = "blue", ylim = range(c(ci_dtfp_ln_gdp, ci_dtfp_ln_gdp_upper)), ylab = "Response of ln_gdp", xlab = "Periods")
lines(ci_dtfp_ln_gdp, col = "red", lty = 2)
lines(ci_dtfp_ln_gdp_upper, col = "red", lty = 2)
title("Response of ln_gdp to a Shock in dtfp")

# Plot IRF for ln_aMu
plot(irf_dtfp_ln_aMu, type = "l", col = "blue", ylim = range(c(ci_dtfp_ln_aMu, ci_dtfp_ln_aMu_upper)), ylab = "Response of ln_aMu", xlab = "Periods")
lines(ci_dtfp_ln_aMu, col = "red", lty = 2)
lines(ci_dtfp_ln_aMu_upper, col = "red", lty = 2)
title("Response of ln_aMu to a Shock in dtfp")




















# Estimate the VAR model
var_model <- VAR(ts_data, p = 5, type = "const")

# Impose Cholesky Decomposition
svar_model <- id.chol(var_model)


# Compute impulse response functions (IRFs) with bootstrapped confidence intervals
irf_results <- irf(svar_model, n.ahead = 10, boot = TRUE, ci = 0.90)


# Extract specific IRFs
irf_dtfp_ln_gdp <- irf_results$irf[,"ln_gdp"]
irf_dtfp_ln_aMu <- irf_results$irf[,"ln_aMu"]

# Extract confidence intervals
ci_dtfp_ln_gdp <- irf_results$Lower[,"ln_gdp"]
ci_dtfp_ln_aMu <- irf_results$Lower[,"ln_aMu"]
ci_dtfp_ln_gdp_upper <- irf_results$Upper[,"ln_gdp"]
ci_dtfp_ln_aMu_upper <- irf_results$Upper[,"ln_aMu"]

# Plot the IRFs with confidence intervals
par(mfrow = c(2, 1))  # Set up a 2x1 plotting area

# Plot IRF for ln_gdp
plot(irf_dtfp_ln_gdp, type = "l", col = "blue", ylim = range(c(ci_dtfp_ln_gdp, ci_dtfp_ln_gdp_upper)), ylab = "Response of ln_gdp", xlab = "Periods")
lines(ci_dtfp_ln_gdp, col = "red", lty = 2)
lines(ci_dtfp_ln_gdp_upper, col = "red", lty = 2)
title("Response of ln_gdp to a Shock in dtfp")

# Plot IRF for ln_aMu
plot(irf_dtfp_ln_aMu, type = "l", col = "blue", ylim = range(c(ci_dtfp_ln_aMu, ci_dtfp_ln_aMu_upper)), ylab = "Response of ln_aMu", xlab = "Periods")
lines(ci_dtfp_ln_aMu, col = "red", lty = 2)
lines(ci_dtfp_ln_aMu_upper, col = "red", lty = 2)
title("Response of ln_aMu to a Shock in dtfp")




































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
model



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