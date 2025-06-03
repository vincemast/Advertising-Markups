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
  summarize(avg_pdvmil_ngdp = sum(pdvmil_ngdp, na.rm = TRUE))
names(mnews) <- c("year", "mnews")
#4th q shocks
mnews4 <-  read.csv("gs_shock.csv") %>% filter(grepl("q4", qdate))
mnews4 <- mnews4 %>%
  mutate(qdate = as.numeric(sub("q[1-4]", "", qdate)))
names(mnews4) <- c("year", "mnews4")



#1st q shocks
mnews1 <-  read.csv("gs_shock.csv") %>% filter(grepl("q1", qdate))
mnews1 <- mnews1 %>%
  mutate(qdate = as.numeric(sub("q[1-4]", "", qdate)))
names(mnews1) <- c("year", "mnews1")

#shift down by 1 quater
mnews2 <- read.csv("gs_shock.csv")
mnews2 <- mnews2 %>%
  mutate(qdate = as.numeric(sub("q[1-4]", "", qdate)))
mnews2$qdate[2:length(mnews2$qdate)] <- mnews2$qdate[1:length(mnews2$qdate)-1] 
#create annual
mnews2 <- mnews2 %>%
  mutate(year = as.numeric(sub("q[1-4]", "", qdate)))
mnews2 <- mnews2 %>%
  group_by(year) %>%
  summarize(avg_pdvmil_ngdp = sum(pdvmil_ngdp, na.rm = TRUE))
names(mnews2) <- c("year", "mnews2")


# federal government spending growth rate expectations
gvgr_exp <- read.csv("gvgr_exp.csv") # nolint


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
                      sheet = "annual")[, c(1, 14, 21, 22)]
names(fernald) <- c("year", "dtfp", "dtfp_I", "dtfp_C")



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
#gdp deflator
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


#federal government expenditures (note nominal)
fedexp <- fredr_series_observations("FGCEA")[, c(1, 3)]

#growth in federal government expenditures (use to create gs_shock)
gs_gr <- fredr_series_observations("FGCEA", units = "pc1")[, c(1, 3)]

#gdp deflator gr (so we can convert gs_gr to real)
infl <- fredr_series_observations("GDPDEF",
                                    frequency = "a",
                                    aggregation_method = "avg",
                                    units = "pc1")[, c(1, 3)]

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
names(gs_gr) <- c("year", "gs_gr")
names(infl) <- c("year", "infl")
names(fedexp) <- c("year", "fedexp")

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
gs_gr$year <- as.numeric(format(as.Date(gs_gr$year), "%Y"))
infl$year <- as.numeric(format(as.Date(infl$year), "%Y"))
fedexp$year <- as.numeric(format(as.Date(fedexp$year), "%Y"))


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

#add 2 d industry codes
data <- invisible(industry_n_dig(data, naics, 2))

unique(data$industry)

names(data)

summary(data)


#aggregate data
agg_markups <- data %>%
  group_by(fyear) %>% # nolint
  summarise(sw_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            Agg_MU = mean(MU, na.rm = TRUE),# nolint
            med_MU = median(MU, na.rm = TRUE),
            cw_MU = weighted.mean(MU, cogs + ppegt * usercost.x,
                                  na.rm = TRUE),
            m_mu = mean(MU[industry == "Manufacturing"], na.rm = TRUE),
            r_mu = mean(MU[industry == "Retail Trade"], na.rm = TRUE),
            t_mu = mean(MU[industry == "Information"], na.rm = TRUE),
            w_mu = mean(MU[industry == "Wholesale Trade"], na.rm = TRUE),
            sw_m_mu = weighted.mean(MU[industry == "Manufacturing"],
                                    sale[industry == "Manufacturing"],
                                    na.rm = TRUE),
            sw_r_mu = weighted.mean(MU[industry == "Retail Trade"],
                                    sale[industry == "Retail Trade"],
                                    na.rm = TRUE),
            sw_t_mu = weighted.mean(MU[industry == "Information"],
                                    sale[industry == "Information"],
                                    na.rm = TRUE),
            sw_w_mu = weighted.mean(MU[industry == "Wholesale Trade"],
                                    sale[industry == "Wholesale Trade"],
                                    na.rm = TRUE)
  )


names(agg_markups) <- c("year", "sw_MU", "Agg_MU", "med_MU",
                        "cw_MU", "m_mu", "r_mu", "t_mu", "w_mu",
                        "sw_m_mu", "sw_r_mu", "sw_t_mu", "sw_w_mu")

#merge with RGDP, unemp, fernald, deflator, tbill
ag_data <- merge(agg_markups, rgdp, by = "year")
ag_data <- merge(ag_data, unemp, by = "year")
ag_data <- merge(ag_data, fernald, by = "year")
ag_data <- merge(ag_data, gdpdef, by = "year")
ag_data <- merge(ag_data, tbill, by = "year")
ag_data <- merge(ag_data, fedexp, by = "year")


################### 1.d log data #########################
#log everything well use (except tbill rate and dtfp)
ag_data$ln_aMu <- log(ag_data$Agg_MU)
ag_data$ln_swMu <- log(ag_data$sw_MU)
ag_data$ln_medMU <- log(ag_data$med_MU)
ag_data$ln_cwMu <- log(ag_data$cw_MU)
ag_data$ln_MMu <- log(ag_data$m_mu)
ag_data$ln_RMu <- log(ag_data$r_mu)
ag_data$ln_TMu <- log(ag_data$t_mu)
ag_data$ln_WMu <- log(ag_data$w_mu)

ag_data$ln_sw_m_mu <- log(ag_data$sw_m_mu)
ag_data$ln_sw_r_mu <- log(ag_data$sw_r_mu)
ag_data$ln_sw_t_mu <- log(ag_data$sw_t_mu)
ag_data$ln_sw_w_mu <- log(ag_data$sw_w_mu)

ag_data$ln_gdp <- log(ag_data$RGDP)
ag_data$ln_gdpdf <- log(ag_data$gdpdf)
ag_data$ln_fexp <- log(ag_data$fedexp)




#####################################################
#####################################################
#######    Elasticities
#####################################################
#####################################################

mu_list <- c("ln_aMu", "ln_swMu", "ln_medMU",
             "ln_cwMu", "sw_m_mu", "sw_r_mu", "sw_w_mu")

#set empty matrix to store results
elasts <- data.frame(matrix(nrow = 2, ncol = length(mu_list)))
names(elasts) <- mu_list
rownames(elasts) <- c("TFP", "MP")

########################################################
######################  TFP
for (i in mu_list) {
  #TFP
  # set variable order
  TFPvars <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", i)
  TFP_ts<- ts(ag_data[, TFPvars],
              start = min(ag_data$year),
              frequency = 1, end = 2017)
  # Estimate VAR model
  TFPvar <- VAR(TFP_ts, type = "both", p = 2)
  # Impose Cholesky Decomposition
  TFPx1 <- id.chol(TFPvar)
  # irs
  # Impulse response (no bs)
  i_TFP <- irf(TFPx1, n.ahead = 16)
  # collect elasticity 
  #area under IRF of gdp (first 5 years)
  tfp_gdp_ce <- sum(i_TFP$irf$'epsilon[ dtfp ] %->% ln_gdp'[1:6])
  #area under IRF of mu (first 5 years)
  shock_name <- paste("'","epsilon[ dtfp ] %->% " , i, "'", sep = "")
  ir_mu <- eval(parse(text = paste0("i_TFP$irf$", shock_name)))
  tfp_mu_ce <- sum(ir_mu[1:6])
  elas <- tfp_mu_ce / tfp_gdp_ce
  eval(parse(text = paste0("elasts$", i,"[1] <- elas")))
  }

########################################################
######################  MP

#need to add ppi and ffunds to data
mp_data <- merge(ag_data, ppi, by = "year")
mp_data <- merge(mp_data, fedfunds, by = "year")

#log ppi
mp_data$ln_ppi <- log(mp_data$ppi)

#switch sign so positive fed funds shock is expansionary
mp_data$ffunds <- -mp_data$ffunds


for (i in mu_list) {
  #MP
  # set variable order
  MPvars <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", i)
  MP_ts<- ts(mp_data[, MPvars], start = min(ag_data$year),
              end = 2007, frequency = 1)
  # Estimate VAR model
  MPvar <- VAR(MP_ts, type = "both", p = 2)
  # Impose Cholesky Decomposition
  MPx1 <- id.chol(MPvar)
  # irs
  # Impulse response (no bs)
  i_MP <- irf(MPx1, n.ahead = 16)
  # collect elasticity 
  #area under IRF of gdp (first 5 years)
  tfp_gdp_ce <- sum(i_MP$irf$'epsilon[ ffunds ] %->% ln_gdp'[1:6])
  #area under IRF of mu (first 5 years)
  shock_name <- paste("'","epsilon[ ffunds ] %->% " , i, "'", sep = "")
  ir_mu <- eval(parse(text = paste0("i_MP$irf$", shock_name)))
  tfp_mu_ce <- sum(ir_mu[1:6])
  elas <- tfp_mu_ce / tfp_gdp_ce
  eval(parse(text = paste0("elasts$", i,"[2] <- elas")))
  }

elasts





#####################################################
#####################################################
#######    2 TFP SVAR
#####################################################
#####################################################


rec_c_order_sw <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_swMu")

ts_data_sw <- ts(ag_data[, c(rec_c_order_sw)],
                 start = min(ag_data$year),
                 frequency = 1, end = 2018)


# Bootstrap
n_ah <- 16

# Estimate VAR model
var_sw <- VAR(ts_data_sw, type = "both", p = 2)
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
tfp_gdp_lb <- tfp_bs_ci_gdp[, 1]
tfp_gdp_ub <- tfp_bs_ci_gdp[, 2]

mu_bs_ci_gdp <- be_extra(bb_sw, 1, 5, n_boot, n_ah, 5, 90)
mu_gdp_lb <- mu_bs_ci_gdp[, 1]
mu_gdp_ub <- mu_bs_ci_gdp[, 2]

tfp_lb <- min(c(tfp_gdp_lb, mu_gdp_lb))
tfp_ub <- max(c(tfp_gdp_ub, mu_gdp_ub))


year_max <- 16

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_sw <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = tfp_gdp_lb, ymax = tfp_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_sw


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_sw <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_gdp_lb, ymax = mu_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_sw


setwd(dircs[3])

#
pdf("IRF_tfp.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_sw, mu_irf_plot_sw,
             ncol = 2)
dev.off()


#####################################################
#####################################################
#######    3 MP SVAR
#####################################################
#####################################################

#need to add ppi and ffunds to data
mp_data <- merge(ag_data, ppi, by = "year")
mp_data <- merge(mp_data, fedfunds, by = "year")

#log ppi
mp_data$ln_ppi <- log(mp_data$ppi)

#switch sign so positive fed funds shock is expansionary
mp_data$ffunds <- -mp_data$ffunds


mp_order <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", "ln_swMu")

mp_data_t <- ts(mp_data[, mp_order], start = min(ag_data$year),
              end = 2008, frequency = 1)


# Estimate VAR model
var_mp <- VAR(mp_data_t, type = "both", p = 2)

# Impose Cholesky Decomposition
x1mp <- id.chol(var_mp)

n_ah <- 16

# Impulse response analysis
i1mp <- irf(x1mp, n.ahead = n_ah)
plot(i1mp, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(1234)
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

mpllim <- min(c(gdp_mp_lb, mu_mp_lb))
mpulim <- max(c(gdp_gdp_ub, mu_mp_ub))

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_mp <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = gdp_mp_lb, ymax = gdp_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_mp, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(mpllim, mpulim)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_mp


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_mp <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_mp_lb, ymax = mu_mp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_mp, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits =  c(mpllim, mpulim)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_mp


setwd(dircs[3])

#
pdf("IRF_mp.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_mp, mu_irf_plot_mp,
             ncol = 2)
dev.off()








#####################################################
#####################################################
#######    2 TFP SVAR by industry
#####################################################
#####################################################

##### retail 
rec_c_order_r <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_sw_r_mu")

##### manuf
rec_c_order_m <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_sw_m_mu")

##### whole sale
rec_c_order_w <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_sw_w_mu")

ts_data_r <- ts(ag_data[, c(rec_c_order_r)],
                 start = min(ag_data$year),
                 frequency = 1, end = 2018)
                 

ts_data_m <- ts(ag_data[, c(rec_c_order_m)],
                 start = min(ag_data$year),
                 frequency = 1, end = 2018)

ts_data_w <- ts(ag_data[, c(rec_c_order_w)],
                  start = min(ag_data$year),
                  frequency = 1, end = 2018)



# Bootstrap
n_ah <- 16

# Estimate VAR models
var_r <- VAR(ts_data_r, type = "both", p = 2)
var_m <- VAR(ts_data_m, type = "both", p = 2)
var_w <- VAR(ts_data_w, type = "both", p = 2)
# Impose Cholesky DecompositionS
x1_r <- id.chol(var_r)
x1_m <- id.chol(var_m)
x1_w <- id.chol(var_w)
# Impulse response (no bs)
i_r <- irf(x1_r, n.ahead = n_ah)
i_m <- irf(x1_m, n.ahead = n_ah)
i_w <- irf(x1_w, n.ahead = n_ah)

# Bootstrap
n_boot <- 1000
set.seed(123)
bb_r <- mb.boot(x1_r, n.ahead = n_ah, nboot = n_boot)
bb_m <- mb.boot(x1_m, n.ahead = n_ah, nboot = n_boot)
bb_w <- mb.boot(x1_w, n.ahead = n_ah, nboot = n_boot)

#all plot
plot(bb_r, lowerq = .05, upperq = .95)
plot(bb_m, lowerq = .05, upperq = .95)
plot(bb_w, lowerq = .05, upperq = .95)


#point estimates
mu_pe_r <- i_r$irf$'epsilon[ dtfp ] %->% ln_sw_r_mu'
mu_pe_m <- i_m$irf$'epsilon[ dtfp ] %->% ln_sw_m_mu'
mu_pe_w <- i_w$irf$'epsilon[ dtfp ] %->% ln_sw_w_mu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#done with function in function_useful
mu_bs_ci_gdp_r <- be_extra(bb_r, 1, 5, n_boot, n_ah, 5, 90)
mu_lb_r <- mu_bs_ci_gdp_r[, 1]
mu_ub_r <- mu_bs_ci_gdp_r[, 2]

mu_bs_ci_gdp_m <- be_extra(bb_m, 1, 5, n_boot, n_ah, 5, 90)
mu_lb_m <- mu_bs_ci_gdp_m[, 1]
mu_ub_m <- mu_bs_ci_gdp_m[, 2]

mu_bs_ci_gdp_w <- be_extra(bb_w, 1, 5, n_boot, n_ah, 5, 90)
mu_lb_w <- mu_bs_ci_gdp_w[, 1]
mu_ub_w <- mu_bs_ci_gdp_w[, 2]

tfp_lb <- min(c(mu_lb_r, mu_lb_m, mu_lb_w))
tfp_ub <- max(c(mu_ub_r, mu_ub_m, mu_ub_w))


year_max <- 16

# Plot the IRFs with 90% confidence intervals

#retail
mu_irf_plot_r <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb_r, ymax = mu_ub_r,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_r, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Retail Markup",
       title = "TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_r

#manuf
mu_irf_plot_m <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb_m, ymax = mu_ub_m,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_m, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Manufacturing Markup",
       title = "TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_m

#whole sale
mu_irf_plot_w <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb_w, ymax = mu_ub_w,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_w, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Whole Sale Markup",
       title = "TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_w


setwd(dircs[3])
#
pdf("IRF_tfp_indu.pdf", width = 20, height = 10)
grid.arrange(mu_irf_plot_r, mu_irf_plot_m,mu_irf_plot_w,
             ncol = 3)
dev.off()


setwd(dircs[3])
#
pdf("IRF_tfp_indu_wide.pdf", width = 30, height = 7.5)
grid.arrange(mu_irf_plot_r, mu_irf_plot_m,mu_irf_plot_w,
             ncol = 3)
dev.off()



#####################################################
#####################################################
#######    2 MP SVAR by industry
#####################################################
#####################################################

#retail
##### retail 
mp_c_order_r <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", "ln_sw_r_mu")

##### manuf
mp_c_order_m <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", "ln_sw_m_mu")

##### whole sale
mp_c_order_w <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", "ln_sw_w_mu")

mp_ts_data_r <- ts(mp_data[, c(mp_c_order_r)],
                   start = min(mp_data$year),
                   frequency = 1, end = 2008)

mp_ts_data_m <- ts(mp_data[, c(mp_c_order_m)],
                   start = min(mp_data$year),
                   frequency = 1, end = 2008)

mp_ts_data_w <- ts(mp_data[, c(mp_c_order_w)],
                    start = min(mp_data$year),
                    frequency = 1, end = 2008)


# Estimate VAR models
var_r <- VAR(mp_ts_data_r, type = "both", p = 2)
var_m <- VAR(mp_ts_data_m, type = "both", p = 2)
var_w <- VAR(mp_ts_data_w, type = "both", p = 2)
# Impose Cholesky DecompositionS
x1_r <- id.chol(var_r)
x1_m <- id.chol(var_m)
x1_w <- id.chol(var_w)
# Impulse response (no bs)
i_r <- irf(x1_r, n.ahead = n_ah)
i_m <- irf(x1_m, n.ahead = n_ah)
i_w <- irf(x1_w, n.ahead = n_ah)
plot(i_r, scales = 'free_y')
plot(i_m, scales = 'free_y')
plot(i_w, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(123)
bb_r <- mb.boot(x1_r, n.ahead = n_ah, nboot = n_boot)
bb_m <- mb.boot(x1_m, n.ahead = n_ah, nboot = n_boot)
bb_w <- mb.boot(x1_w, n.ahead = n_ah, nboot = n_boot)

#all plot
plot(bb_r, lowerq = .05, upperq = .95)
plot(bb_m, lowerq = .05, upperq = .95)
plot(bb_w, lowerq = .05, upperq = .95)


#point estimates
mu_pe_r <- i_r$irf$'epsilon[ ffunds ] %->% ln_sw_r_mu'
mu_pe_m <- i_m$irf$'epsilon[ ffunds ] %->% ln_sw_m_mu'
mu_pe_w <- i_w$irf$'epsilon[ ffunds ] %->% ln_sw_w_mu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#done with function in function_useful
mu_bs_ci_gdp_r <- be_extra(bb_r, 4, 5, n_boot, n_ah, 5, 90)
mu_lb_r <- mu_bs_ci_gdp_r[, 1]
mu_ub_r <- mu_bs_ci_gdp_r[, 2]

mu_bs_ci_gdp_m <- be_extra(bb_m, 4, 5, n_boot, n_ah, 5, 90)
mu_lb_m <- mu_bs_ci_gdp_m[, 1]
mu_ub_m <- mu_bs_ci_gdp_m[, 2]

mu_bs_ci_gdp_w <- be_extra(bb_w, 4, 5, n_boot, n_ah, 5, 90)
mu_lb_w <- mu_bs_ci_gdp_w[, 1]
mu_ub_w <- mu_bs_ci_gdp_w[, 2]

tfp_lb <- min(c(mu_lb_r, mu_lb_m, mu_lb_w))
tfp_ub <- max(c(mu_ub_r, mu_ub_m, mu_ub_w))


year_max <- 16

# Plot the IRFs with 90% confidence intervals

#retail
mu_irf_plot_r <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb_r, ymax = mu_ub_r,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_r, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Retail Markup",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")

mu_irf_plot_r

#manuf

mu_irf_plot_m <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb_m, ymax = mu_ub_m,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_m, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Manufacturing Markup",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")

mu_irf_plot_m

#whole sale
mu_irf_plot_w <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_lb_w, ymax = mu_ub_w,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_w, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Whole Sale Markup",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_w

setwd(dircs[3])

#
pdf("IRF_mp_indu.pdf", width = 20, height = 10)
grid.arrange(mu_irf_plot_r, mu_irf_plot_m,mu_irf_plot_w,
             ncol = 3)
dev.off()


pdf("IRF_mp_indu_wide.pdf", width = 30, height = 7.5)
grid.arrange(mu_irf_plot_r, mu_irf_plot_m,mu_irf_plot_w,
             ncol = 3)
dev.off()































#####################################################
#####################################################
#######    OLD
#####################################################
#####################################################



#####################################################
#####################################################
#######    Elasticities
#####################################################
#####################################################

mu_list <- c("ln_aMu", "ln_swMu",
             "ln_cwMu", "sw_m_mu", "sw_r_mu")

#set empty matrix to store results
elasts <- data.frame(matrix(nrow = 3, ncol = length(mu_list)))
names(elasts) <- mu_list
rownames(elasts) <- c("TFP", "MP", "GS")

i = "ln_aMu"

eval(parse(text = paste0("elasts$", i)))

#######
# 1.1 TFP
for (i in mu_list) {
  #TFP
  # set variable order
  TFPvars <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", i)
  TFP_ts<- ts(ag_data[, TFPvars],
              start = min(ag_data$year),
              frequency = 1, end = 2017)
  # Estimate VAR model
  TFPvar <- VAR(TFP_ts, type = "both", p = 2)
  # Impose Cholesky Decomposition
  TFPx1 <- id.chol(TFPvar)
  # irs
  # Impulse response (no bs)
  i_TFP <- irf(TFPx1, n.ahead = 16)
  # collect elasticity 
  #area under IRF of gdp (first 5 years)
  tfp_gdp_ce <- sum(i_TFP$irf$'epsilon[ dtfp ] %->% ln_gdp'[1:6])
  #area under IRF of mu (first 5 years)
  shock_name <- paste("'","epsilon[ dtfp ] %->% " , i, "'", sep = "")
  ir_mu <- eval(parse(text = paste0("i_TFP$irf$", shock_name)))
  tfp_mu_ce <- sum(ir_mu[1:6])
  elas <- tfp_mu_ce / tfp_gdp_ce
  eval(parse(text = paste0("elasts$", i,"[1] <- elas")))
  }

#######
# 1.2 MP

#need to add ppi and ffunds to data
mp_data <- merge(ag_data, ppi, by = "year")
mp_data <- merge(mp_data, fedfunds, by = "year")

#log ppi
mp_data$ln_ppi <- log(mp_data$ppi)

#switch sign so positive fed funds shock is expansionary
mp_data$ffunds <- -mp_data$ffunds


for (i in mu_list) {
  #MP
  # set variable order
  MPvars <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", i)
  MP_ts<- ts(mp_data[, MPvars], start = min(ag_data$year),
              end = 2007, frequency = 1)
  # Estimate VAR model
  MPvar <- VAR(MP_ts, type = "both", p = 2)
  # Impose Cholesky Decomposition
  MPx1 <- id.chol(MPvar)
  # irs
  # Impulse response (no bs)
  i_MP <- irf(MPx1, n.ahead = 16)
  # collect elasticity 
  #area under IRF of gdp (first 5 years)
  tfp_gdp_ce <- sum(i_MP$irf$'epsilon[ ffunds ] %->% ln_gdp'[1:6])
  #area under IRF of mu (first 5 years)
  shock_name <- paste("'","epsilon[ ffunds ] %->% " , i, "'", sep = "")
  ir_mu <- eval(parse(text = paste0("i_MP$irf$", shock_name)))
  tfp_mu_ce <- sum(ir_mu[1:6])
  elas <- tfp_mu_ce / tfp_gdp_ce
  eval(parse(text = paste0("elasts$", i,"[2] <- elas")))
  }


### gov spending
#add needed data
gs_data_a <- merge(ag_data, pop, by = "year")
gs_data_a <- merge(gs_data_a, gs_gr, by = "year")
gs_data_a <- merge(gs_data_a, infl, by = "year")
gs_data_a <- merge(gs_data_a, gvgr_exp, by = "year")
gs_data_a <- merge(gs_data_a, fedexp, by = "year")
gs_data_a <- merge(gs_data_a, rec, by = "year")

## gs_shock is the gap between expected and realized gr in fed spending
# real fed spending growth rate
gs_data_a$rgsgr <- gs_data_a$gs_gr - gs_data_a$infl
#gs suprise
gs_data_a$gs_shock <- gs_data_a$rgsgr - gs_data_a$gvgr_exp
# fed gov spending as a % of gdp
gs_data_a$lngexp <- log(gs_data_a$fedexp)

for (i in mu_list) {
  #GS
  # set variable order
  GSvars <- c("gs_shock", "ln_gdp", "ln_gdpdf", "tbill", i)
  GS_ts<- ts(gs_data_a[, GSvars], start = min(gs_data_a$year),
              end = 2017, frequency = 1)
  # Estimate VAR model
  GSvar <- VAR(GS_ts, type = "both")
  # Impose Cholesky Decomposition
  GSx1 <- id.chol(GSvar)
  # irs
  # Impulse response (no bs)
  i_GS <- irf(GSx1, n.ahead = 16)
  plot(i_GS, scales = 'free_y')
  # collect elasticity 
  #area under IRF of gdp (first 5 years)
  tfp_gdp_ce <- sum(i_GS$irf$'epsilon[ gs_shock ] %->% ln_gdp'[1:6])
  #area under IRF of mu (first 5 years)
  shock_name <- paste("'","epsilon[ gs_shock ] %->% " , i, "'", sep = "")
  ir_mu <- eval(parse(text = paste0("i_GS$irf$", shock_name)))
  tfp_mu_ce <- sum(ir_mu[1:6])
  elas <- tfp_mu_ce / tfp_gdp_ce
  eval(parse(text = paste0("elasts$", i,"[3] <- elas")))
  }

elasts[1:2,]


i = "ln_medMU"

#####################################################
#####################################################
#######    2 TFP SVAR
#####################################################
#####################################################


rec_c_order_sw <- c("dtfp", "ln_gdp", "ln_gdpdf", "tbill", "ln_swMu")

ts_data_sw <- ts(ag_data[, c(rec_c_order_sw)],
                 start = min(ag_data$year),
                 frequency = 1, end = 2018)


# Bootstrap
n_ah <- 16

# Estimate VAR model
var_sw <- VAR(ts_data_sw, type = "both", p = 2)
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
tfp_gdp_lb <- tfp_bs_ci_gdp[, 1]
tfp_gdp_ub <- tfp_bs_ci_gdp[, 2]

mu_bs_ci_gdp <- be_extra(bb_sw, 1, 5, n_boot, n_ah, 5, 90)
mu_gdp_lb <- mu_bs_ci_gdp[, 1]
mu_gdp_ub <- mu_bs_ci_gdp[, 2]

tfp_lb <- min(c(tfp_gdp_lb, mu_gdp_lb))
tfp_ub <- max(c(tfp_gdp_ub, mu_gdp_ub))


year_max <- 16

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_sw <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = tfp_gdp_lb, ymax = tfp_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_sw


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_sw <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_gdp_lb, ymax = mu_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_sw, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(tfp_lb, tfp_ub)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "TFP Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_sw


setwd(dircs[3])

#
pdf("IRF_tfp.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_sw, mu_irf_plot_sw,
             ncol = 2)
dev.off()


############## 2.2 generate ratio of area under IRF to area under IRF of gdp

#area under IRF of gdp (first 5 years)
tfp_gdp_ce <- sum(i_sw$irf$'epsilon[ dtfp ] %->% ln_gdp'[1:5])
#area under IRF of mu (first 5 years)
tfp_mu_ce <- sum(i_sw$irf$'epsilon[ dtfp ] %->% ln_swMu'[1:5])
tfp_elas <- tfp_mu_ce / tfp_gdp_ce
tfp_elas










#####################################################
#####################################################
#######    3 ITS SVAR
#####################################################
#####################################################

rec_order_its <- c("dtfp_I", "ln_gdp", "ln_gdpdf", "tbill", "ln_aMu")

ts_data_its <- ts(ag_data[, rec_order_its],
                  start = min(ag_data$year),
                  frequency = 1, end = 2018)


# Bootstrap
n_ah <- 16

# Estimate VAR model
var_its <- VAR(ts_data_its, trend = "both", p = 2)
# Impose Cholesky Decomposition
x1_its <- id.chol(var_its)
# Impulse response (no bs)
i_its <- irf(x1_its, n.ahead = n_ah)
plot(i_its, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(123)
bb_its <- mb.boot(x1_its, n.ahead = n_ah, nboot = n_boot)
plot(bb_its, lowerq = .05, upperq = .95)


#get data for my plot
#point estimates
gdp_pe_its <- i_its$irf$'epsilon[ dtfp_I ] %->% ln_gdp'
mu_pe_its <- i_its$irf$'epsilon[ dtfp_I ] %->% ln_swMu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#done with function in function_useful
its_bs_ci_gdp <- be_extra(bb_its, 1, 2, n_boot, n_ah, 5, 90)
its_gdp_lb <- its_bs_ci_gdp[, 2]
its_gdp_ub <- its_bs_ci_gdp[, 1]

its_mu_bs_ci_gdp <- be_extra(bb_its, 1, 5, n_boot, n_ah, 5, 90)
its_mu_gdp_lb <- its_mu_bs_ci_gdp[, 2]
its_mu_gdp_ub <- its_mu_bs_ci_gdp[, 1]



year_max <- 16

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_its <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = its_gdp_lb, ymax = its_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_its, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.025, .015)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "IST Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_its


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_ist <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = its_mu_gdp_lb, ymax = its_mu_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_its, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(-.025, .015)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "IST Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_ist


setwd(dircs[3])

#
pdf("IRF_ist.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_its, mu_irf_plot_ist,
             ncol = 2)
dev.off()


############## 2.2 generate ratio of area under IRF to area under IRF of gdp

#area under IRF of gdp (first 5 years)
tfp_I_gdp_ce <- sum(i_its$irf$'epsilon[ dtfp_I ] %->% ln_gdp'[1:5])
#area under IRF of mu (first 5 years)
tfp_I_mu_ce <- sum(i_its$irf$'epsilon[ dtfp_I ] %->% ln_swMu'[1:5])
tfpI_elas <- tfp_I_mu_ce / tfp_I_gdp_ce
tfpI_elas

























#####################################################
#####################################################
#######    3 MP SVAR
#####################################################
#####################################################

#need to add ppi and ffunds to data
mp_data <- merge(ag_data, ppi, by = "year")
mp_data <- merge(mp_data, fedfunds, by = "year")

#log ppi
mp_data$ln_ppi <- log(mp_data$ppi)

#switch sign so positive fed funds shock is expansionary
mp_data$ffunds <- -mp_data$ffunds


mp_order <- c("ln_gdp", "ln_gdpdf", "ln_ppi", "ffunds", "ln_swMu")

mp_data <- ts(mp_data[, mp_order], start = min(ag_data$year),
              end = 2008, frequency = 1)


# Estimate VAR model
var_mp <- VAR(mp_data, type = "both", p = 2)

# Impose Cholesky Decomposition
x1mp <- id.chol(var_mp)

n_ah <- 16

# Impulse response analysis
i1mp <- irf(x1mp, n.ahead = n_ah)
plot(i1mp, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(1234)
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

mpllim <- min(c(gdp_mp_lb, mu_mp_lb))
mpulim <- max(c(gdp_gdp_ub, mu_mp_ub))

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_mp <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = gdp_mp_lb, ymax = gdp_gdp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_mp, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(mpllim, mpulim)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_mp


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_mp <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_mp_lb, ymax = mu_mp_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_mp, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits =  c(mpllim, mpulim)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "Monetary Policy Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_mp


setwd(dircs[3])

#
pdf("IRF_mp.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_mp, mu_irf_plot_mp,
             ncol = 2)
dev.off()


############## 3.2 generate ratio of area under IRF to area under IRF of gdp

#area under IRF of gdp (first 5 years)
mp_gdp_ce <- sum(i1mp$irf$'epsilon[ ffunds ] %->% ln_gdp'[1:6])
#area under IRF of mu (first 5 years)
mp_mu_ce <- sum(i1mp$irf$'epsilon[ ffunds ] %->% ln_swMu'[1:6])
mp_elas <- mp_mu_ce / mp_gdp_ce
mp_elas






#####################################################
#####################################################
#######    4 Gspen SVAR
#####################################################
#####################################################

#add needed data
gs_data_a <- merge(ag_data, pop, by = "year")
gs_data_a <- merge(gs_data_a, gs_gr, by = "year")
gs_data_a <- merge(gs_data_a, infl, by = "year")
gs_data_a <- merge(gs_data_a, gvgr_exp, by = "year")
gs_data_a <- merge(gs_data_a, fedexp, by = "year")
gs_data_a <- merge(gs_data_a, rec, by = "year")

## gs_shock is the gap between expected and realized gr in fed spending
# real fed spending growth rate
gs_data_a$rgsgr <- gs_data_a$gs_gr
#gs suprise
gs_data_a$gs_shock <- gs_data_a$rgsgr - gs_data_a$gvgr_exp
# fed gov spending as a % of gdp
gs_data_a$lngexp <- log(gs_data_a$fedexp)

gs_data_a <- merge(ag_data, mnews1, by = "year")


gs_order <- c("gs_shock", "ln_exp", "ln_gdp", "ln_gdpdf", "tbill", "ln_swMu")

gs_data <- ts(gs_data_a[, gs_order], start = min(gs_data_a$year),
              end = 2017, frequency = 1)


# Estimate VAR model
var_gs <- VAR(gs_data, type = "both", p = 2)

# Impose Cholesky Decomposition
x1gs <- id.chol(var_gs)

n_ah <- 16

# Impulse response analysis
i1gs <- irf(x1gs, n.ahead = n_ah)
plot(i1gs, scales = 'free_y')

# Bootstrap
n_boot <- 1000
set.seed(123)
bbgs <- mb.boot(x1gs, , design = "fixed", n.ahead = n_ah, nboot = n_boot)
#all plot
plot(bbgs, lowerq = .05, upperq = .95)


#point estimates
gdp_pe_gs <- i1gs$irf$'epsilon[ gs_shock ] %->% ln_gdp'
mu_pe_gs <- i1gs$irf$'epsilon[ gs_shock ] %->% ln_swMu'


#confidence intervals
#have to grab each bootstrapped estimate 1 by 1
#done with function in function_useful
#gdp is 1, mu is 5, shock is ffunds [4]
gdp_bs_ci_gs <- be_extra(bbgs, 1, 2, n_boot, n_ah, length(gs_order), 60)
gdp_gs_lb <- gdp_bs_ci_gs[, 1]
gdp_gs_ub <- gdp_bs_ci_gs[, 2]

mu_bs_ci_gs <- be_extra(bbgs, 1, 5, n_boot, n_ah, length(gs_order), 60)
mu_gs_lb <- mu_bs_ci_gs[, 1]
mu_gs_ub <- mu_bs_ci_gs[, 2]

gs_lb <- min(c(gdp_gs_lb, mu_gs_lb))
gs_ub <- max(c(gdp_gs_ub, mu_gs_ub))

year_max <- 16

# Plot the IRFs with 90% confidence intervals
gdp_irf_plot_gs <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = gdp_gs_lb, ymax = gdp_gs_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = gdp_pe_gs, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0: (n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(gs_lb, gs_ub)) +
  labs(x = "Year", y = "Real GDP Per Capita",
       title = "Governent Spending Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c(
                               "90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
gdp_irf_plot_gs


#plot IRF for ln_aMu wth 90% CI
mu_irf_plot_gs <- ggplot() +
  geom_ribbon(aes(x = 1:n_ah,
                  ymin = mu_gs_lb, ymax = mu_gs_ub,
                  fill = "90% Confidence Interval (Bootstrapped)"),
              alpha = 0.5) +
  geom_line(aes(x = 1:n_ah, y = mu_pe_gs, color = "Point Estimate")) +
  scale_x_continuous(breaks = 1:n_ah, labels = 0:(n_ah - 1),
                     limits = c(1, year_max)) +
  scale_y_continuous(labels = percent, limits = c(gs_lb, gs_ub)) +
  labs(x = "Year", y = "Sales Weighted Markup",
       title = "Governent Spending Shock") +
  scale_color_manual(name = "", values = c("Point Estimate" = "blue")) +
  scale_fill_manual(name = "",
                    values = c("90% Confidence Interval (Bootstrapped)"
                               = "grey")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed")
mu_irf_plot_gs


setwd(dircs[3])

#
pdf("IRF_gs.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot_gs, mu_irf_plot_gs,
             ncol = 2)
dev.off()


############## 4.2 generate ratio of area under IRF to area under IRF of gdp

#area under IRF of gdp (first 5 years)
gs_gdp_ce <- sum(i1gs$irf$'epsilon[ gs_shock ] %->% ln_gdp'[1:5])
#area under IRF of mu (first 5 years)
gs_mu_ce <- sum(i1gs$irf$'epsilon[ gs_shock ] %->% ln_swMu'[1:5])
gs_elas <- gs_mu_ce / gs_gdp_ce
gs_elas

































































#####################################################
#####################################################
#######    5 ITS SVAR
#####################################################
#####################################################

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

#
pdf("IRF.pdf", width = 20, height = 10)
grid.arrange(gdp_irf_plot, mu_irf_plot,
             ncol = 2)
dev.off()
