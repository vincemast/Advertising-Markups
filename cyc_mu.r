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

#user cost (from CSV)
usercost <- read.csv("usercost.csv") #nolint

#military news from CSV (ramey 2011 updated to 2017 via ramey and Nekarda 2020)
mnews <- read.csv("gs_shock.csv") #nolint
#create annual 
mnews <- mnews %>%
  mutate(year = as.numeric(sub("q[1-4]", "", qdate)))
mnews <- mnews %>%
  group_by(year) %>%
  summarize(avg_pdvmil_ngdp = mean(pdvmil_ngdp, na.rm = TRUE))
names(mnews) <- c("year", "mnews")
#4th q shocks
mnews4 <-  read.csv("gs_shock.csv") %>% filter(grepl("q4", qdate))
mnews4 <- mnews4 %>%
  mutate(qdate = as.numeric(sub("q[1-4]", "", qdate)))
names(mnews4) <- c("year", "mnews4")





#consumption of nondurables plus service (from n&r2020)
ppcendsv <- read.csv("n_r_2020_data.csv") #nolint
#create annual
ppcendsv <- ppcendsv %>%
  mutate(year = as.numeric(sub("q[1-4]", "", qdate)))
ppcendsv <- ppcendsv %>%
  group_by(year) %>%
  summarize(avg_ppcendsv = mean(ppcendsv, na.rm = TRUE))
names(ppcendsv) <- c("year", "ppcendsv")

#fernald series (from excel)
fernald <- read_excel(path = paste(dircs[2],
                                   "/fernald_series.xlsx", sep = "/"),
                      sheet = "annual")[, c(1, 14)]
names(fernald) <- c("year", "dtfp")



#fred data



#Real GDP percapita from fred
rgdp <- fredr_series_observations("A939RX0Q048SBEA",
                                  frequency = "a",
                                  aggregation_method = "avg")[, c(1, 3)]
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
#tbills
tbill <- fredr_series_observations("TB3MS",
                                   frequency = "a",
                                   aggregation_method = "avg")[, c(1, 3)]
#PPI (all commodities) from fred
ppi <- fredr_series_observations("PPIACO",
                                 frequency = "a",
                                 aggregation_method = "avg")[, c(1, 3)]
#fed funds rate from fred
fedfunds <- fredr_series_observations("FEDFUNDS",
                                      frequency = "a",
                                      aggregation_method = "avg")[, c(1, 3)]

#fixed equiptment investment price index from fred
pequip <- fredr_series_observations("Y032RG3A086NBEA",
                                    frequency = "a",
                                    aggregation_method = "avg")[, c(1, 3)]

#federal defense spending
fdspend <- fredr_series_observations("FDEFX",
                                     frequency = "a",
                                     aggregation_method = "avg")[, c(1, 3)]

#population
pop <- fredr_series_observations("B230RC0A052NBEA")[, c(1, 3)]


#government expenditures
gexp <- fredr_series_observations("W068RCQ027SBEA",
                                  frequency = "a",
                                  aggregation_method = "avg")[, c(1, 3)]


#set names
names(rgdp) <- c("year", "RGDP")
names(unemp) <- c("year", "unemp")
names(rec) <- c("year", "rec")
names(gdpdef) <- c("year", "gdpdf")
names(tbill) <- c("year", "tbill")
names(ppi) <- c("year", "ppi")
names(fedfunds) <- c("year", "ffunds")
names(pequip) <- c("year", "pequip")
names(fdspend) <- c("year", "fdspend")
names(pop) <- c("year", "pop")
names(gexp) <- c("year", "gexp")

#convernt date to year
rgdp$year <- as.numeric(format(as.Date(rgdp$year), "%Y"))
unemp$year <- as.numeric(format(as.Date(unemp$year), "%Y"))
rec$year <- as.numeric(format(as.Date(rec$year), "%Y"))
gdpdef$year <- as.numeric(format(as.Date(gdpdef$year), "%Y"))
tbill$year <- as.numeric(format(as.Date(tbill$year), "%Y"))
ppi$year <- as.numeric(format(as.Date(ppi$year), "%Y"))
fedfunds$year <- as.numeric(format(as.Date(fedfunds$year), "%Y"))
pequip$year <- as.numeric(format(as.Date(pequip$year), "%Y"))
fdspend$year <- as.numeric(format(as.Date(fdspend$year), "%Y"))
pop$year <- as.numeric(format(as.Date(pop$year), "%Y"))
gexp$year <- as.numeric(format(as.Date(gexp$year), "%Y"))


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
ag_data <- merge(ag_data, ppi, by = "year")
ag_data <- merge(ag_data, fedfunds, by = "year")
ag_data <- merge(ag_data, mnews, by = "year")
ag_data <- merge(ag_data, mnews4, by = "year")
ag_data <- merge(ag_data, ppcendsv, by = "year")
ag_data <- merge(ag_data, pequip, by = "year")
ag_data <- merge(ag_data, fdspend, by = "year")
ag_data <- merge(ag_data, pop, by = "year")
ag_data <- merge(ag_data, gexp, by = "year")


#gen decade indicator by taking first 3 digits of year then adding 0
ag_data$decade <- as.numeric(substr(as.character(ag_data$year), 1, 3)) * 10



################### 1.d log data #########################
#log everything well use (except tbill rate and dtfp)
ag_data$ln_aMu <- log(ag_data$Agg_MU - 1)
ag_data$ln_swMu <- log(ag_data$sw_MU - 1)
ag_data$ln_gdp <- log(ag_data$RGDP)
ag_data$ln_gdpdf <- log(ag_data$gdpdf)
ag_data$ln_ppi <- log(ag_data$ppi)
#mil spending per capita
ag_data$lnmilpc <- log(ag_data$fdspend / ag_data$pop)
#mil spending as % of gdp
ag_data$lnmilgdp <- log(ag_data$fdspend / ag_data$RGDP)
# gov spending as a % of gdp
ag_data$lngexp <- log(ag_data$gexp) - log(ag_data$pop) - log(ag_data$RGDP)

#switch sign so positive fed funds shock is expansionary
ag_data$ffunds <- -ag_data$ffunds

#create inflation
ag_data$inf <- c(NA, diff(ag_data$ln_gdpdf))
# create "log ratio price of PCE ND+SV to price of equipment investment"
#needed for fisher 2006
ag_data$lpe <- log(ag_data$ppcendsv) - log(ag_data$pequip)


#####################################################
#####################################################
#######    2 TFP SVAR
#####################################################
#####################################################

rec_c_order_sw <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_swMu")

ts_data_sw <- ts(ag_data[, rec_c_order_sw],
                 start = min(ag_data$year),
                 frequency = 1)


# Bootstrap
n_ah <- 16

# Estimate VAR model
var_sw <- VAR(ts_data_sw)
# Impose Cholesky Decomposition
x1_sw <- id.chol(var_sw)
# Impulse response (no bs)
i_sw <- irf(x1_sw, n.ahead = n_ah)
plot(i_sw, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(123)
bb_sw <- mb.boot(x1_sw, n.ahead = n_ah, nboot = n_boot)
plot(bb_sw, lowerq = .05, upperq = .95)


#get data for my plot
#point estimates
gdp_pe_sw <- i_sw$irf$'epsilon[ dtfp ] %->% ln_gdp'
mu_pe_sw <- i_sw$irf$'epsilon[ dtfp ] %->% ln_swMu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#done with function in function_useful
tfp_bs_ci_gdp <- be_extra(bb_sw, 1, 2, n_boot, n_ah, 5, 90)
tfp_gdp_lb <- tfp_bs_ci_gdp[, 2]
tfp_gdp_ub <- tfp_bs_ci_gdp[, 1]

mu_bs_ci_gdp <- be_extra(bb_sw, 1, 5, n_boot, n_ah, 5, 90)
mu_gdp_lb <- mu_bs_ci_gdp[, 2]
mu_gdp_ub <- mu_bs_ci_gdp[, 1]



year_max = 16

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_sw <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = tfp_gdp_lb, ymax = tfp_gdp_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.015, .02)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "TFP Shock") +
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
                  ymin = mu_gdp_lb, ymax = mu_gdp_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.015, .02)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "TFP Shock") +
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
pdf("IRF_tfp.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_sw, mu_irf_plot_sw,
             ncol = 2)
dev.off()








#####################################################
#####################################################
#######    3 MP SVAR
#####################################################
#####################################################

mp_order <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", "ln_swMu")

mp_data <- ts(ag_data[, mp_order], start = min(ag_data$year),
              end = 2007, frequency = 1)


# Estimate VAR model
var_mp <- VAR(mp_data)

# Impose Cholesky Decomposition
x1mp <- id.chol(var_mp)

n_ah <- 16

# Impulse response analysis
i1mp <- irf(x1mp, n.ahead = n_ah)
plot(i1mp, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(123)
bbmp <- mb.boot(x1mp, n.ahead = n_ah, nboot = n_boot)
summary(bbmp)
#all plot
plot(bbmp, lowerq = .05, upperq = .95)


#point estimates
gdp_pe_mp <- i1mp$irf$'epsilon[ ffunds ] %->% ln_gdp'
mu_pe_mp <- i1mp$irf$'epsilon[ ffunds ] %->% ln_swMu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#done with function in function_useful
#gdp is 1, mu is 5, shock is ffunds [4]
gdp_bs_ci_mp <- be_extra(bbmp, 4, 1, n_boot, n_ah, 5, 90)
gdp_mp_lb <- gdp_bs_ci_mp[, 1]
gdp_gdp_ub <- gdp_bs_ci_mp[, 2]

mu_bs_ci_mp <- be_extra(bbmp, 4, 5, n_boot, n_ah, 5, 90)
mu_mp_lb <- mu_bs_ci_mp[, 1]
mu_mp_ub <- mu_bs_ci_mp[, 2]

year_max <- 16

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_mp <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = gdp_mp_lb, ymax = gdp_gdp_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_mp, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.01, .0325)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_mp


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_mp <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_mp_lb, ymax = mu_mp_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_mp, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.01, .0325)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_mp


setwd(dircs[3])

#ipo x decade
pdf("IRF_mp.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_mp, mu_irf_plot_mp,
             ncol = 2)
dev.off()









#####################################################
#####################################################
#######    3 Gspen SVAR
#####################################################
#####################################################

gs_order <- c("mnews4", "ln_gdp", "ln_gdpdf", "tbill", "ln_swMu")

gs_data <- ts(ag_data[, gs_order], start = min(ag_data$year),
              end = 2017, frequency = 1)


# Estimate VAR model
var_gs <- VAR(gs_data)

# Impose Cholesky Decomposition
x1gs <- id.chol(var_gs)

n_ah <- 16

# Impulse response analysis
i1gs <- irf(x1gs, n.ahead = n_ah)
plot(i1gs, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(123)
bbgs <- mb.boot(x1gs, n.ahead = n_ah, nboot = n_boot)
#all plot
plot(bbgs, lowerq = .05, upperq = .95)


#point estimates
gdp_pe_gs <- i1gs$irf$'epsilon[ mnews4 ] %->% ln_gdp'
mu_pe_gs <- i1gs$irf$'epsilon[ mnews4 ] %->% ln_swMu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#done with function in function_useful
#gdp is 1, mu is 5, shock is ffunds [4]
gdp_bs_ci_gs <- be_extra(bbgs, 1, 2, n_boot, n_ah, 5, 90)
gdp_gs_lb <- gdp_bs_ci_gs[, 1]
gdp_gs_ub <- gdp_bs_ci_gs[, 2]

mu_bs_ci_gs <- be_extra(bbgs, 1, 5, n_boot, n_ah, 5, 90)
mu_gs_lb <- mu_bs_ci_gs[, 1]
mu_gs_ub <- mu_bs_ci_gs[, 2]

year_max <- 16

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_gs <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = gdp_gs_lb, ymax = gdp_gs_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_gs, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.0025, .02)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "Governent Spending Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_gs


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_gs <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_gs_lb, ymax = mu_gs_ub,
                  fill = "90% Confidence Interval"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_gs, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.0025, .02)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "Governent Spending Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_gs


setwd(dircs[3])

#ipo x decade
pdf("IRF_gs.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_gs, mu_irf_plot_gs,
             ncol = 2)
dev.off()







#####################################################
#####################################################
#######    4 ITS SVAR
#####################################################
#####################################################


#create lags that will be in SVAR
ag_data$D_lpe <- c(NA, diff(ag_data$lpe))
ag_data$D_ln_gdp <- c(NA, diff(ag_data$ln_gdp))


######## 4.1 generate ITS via fisher 2006 ##########

iv_vars <- c("year", "ln_gdp", "inf", "tbill", "lpe", "ln_swMu",
             "D_lpe", "D_ln_gdp")
data_iv <- ag_data[iv_vars]

#create lags needed for ivreg
data_iv$D_inf <- c(NA, diff(data_iv$inf))
data_iv$D_tbill <- c(NA, diff(data_iv$tbill))
data_iv$D_ln_swMu <- c(NA, diff(data_iv$ln_swMu))

# create the ivs (lags)
data_iv$L_D_ln_gdp <- lag(data_iv$D_ln_gdp, 1)
data_iv$L_inf <- lag(data_iv$inf, 1)
data_iv$L_tbill <- lag(data_iv$tbill, 1)
data_iv$L_mu <- lag(data_iv$ln_swMu, 1)

data_iv$L2_D_ln_gdp <- lag(data_iv$D_ln_gdp, 2)
data_iv$L2_inf <- lag(data_iv$inf, 2)
data_iv$L2_tbill <- lag(data_iv$tbill, 2)
data_iv$L2_mu <- lag(data_iv$ln_swMu, 2)

data_iv$L3_D_ln_gdp <- lag(data_iv$D_ln_gdp, 3)
data_iv$L3_inf <- lag(data_iv$inf, 3)
data_iv$L3_tbill <- lag(data_iv$tbill, 3)
data_iv$L3_mu <- lag(data_iv$ln_swMu, 3)

data_iv$L4_D_ln_gdp <- lag(data_iv$D_ln_gdp, 4)
data_iv$L4_inf <- lag(data_iv$inf, 4)
data_iv$L4_tbill <- lag(data_iv$tbill, 4)
data_iv$L4_mu <- lag(data_iv$ln_swMu, 4)

#time trends
data_iv$t <- 1:nrow(data_iv)
data_iv$t2 <- data_iv$t^2

# estimate the IV model
iv_model <- ivreg(
  D_lpe ~ D_ln_gdp + D_inf + D_tbill + D_ln_swMu + t + t2 |
    L_D_ln_gdp + L_inf + L_tbill + L_mu,
  data = data_iv
)

ist <- data.frame(cbind(c(NA , NA, resid(iv_model)), data_iv$year))
names(ist) <- c("ist", "year")

#merge with data
ag_data_ist <- merge(ag_data, ist, by = "year")


######## 4.2 SVAR ##########

its_order <- c("ist", "ln_gdp", "ln_gdpdf", "tbill", "ln_swMu")

its_data <- ts(ag_data_ist[, its_order], start = min(ag_data$year),
               end = 2017, frequency = 1)

#cut first year (since NA for diggs)
its_data <- window(its_data, start = 1962)


# Estimate VAR model
var_its <- VAR(its_data)

# Impose Cholesky Decomposition
x1its <- id.chol(var_its)

# Impulse response analysis
i1its <- irf(x1its, n.ahead = 10)
plot(i1its, scales = 'free_y')

# Bootstrap
n_ah <- 16
n_boot <- 1000
set.seed(123)
bbits <- mb.boot(x1its, n.ahead = n_ah, nboot = n_boot)
#all plot
plot(bbits, lowerq = .05, upperq = .95)





























#############################################
#############################################
# 5:aVERAGE instead of sw
#############################################
#############################################

########### TFP average

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
n_boot = 1000
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
  scale_fill_manual(name = "", 
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
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
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
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
