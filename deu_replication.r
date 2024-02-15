############################################################
############################################################
#0: Set working directories
############################################################
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
############################################################

############################################################
#switch to save files or not
save_files <- FALSE
############################################################

############################################################
#select which market shares to include in the analysis
############################################################
none <- c()
m_share_2_digit <- c("m")
m_share_2_3_digit <- c("m", "m3")
m_share_2_3_4_digit <- c("m", "m3", "m4")

#pick here
mvars <- m_share_2_digit

############################################################
############################################################
### DONT EDIT BELOW THIS LINE
#1: clear all and load functions
############################################################
############################################################
#directories
dircs <- c(f_folder, d_folder, p_folder)
keep_vars <- c("dircs", "save_files", "mvars")
cat("\014")
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

############################################################
############################################################
#     1: Load and clean data
############################################################
############################################################

############################################################
################### 1.a load data #########################
############################################################
#navigate to with data
setwd(dircs[2])

#compustat
dset <- read.csv("COMPUSTAT.csv") # nolint
# naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")
colnames(naics) <- c("change", "naics_n", "industry")
#FRED DATA
usercost <- read.csv("usercost.csv") #nolint

#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"

#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

############################################################
################## 1.b Set up year stuff  ##################
############################################################

#apply GDP deflator and generate K = ppegt*usercost
dset <- VariableGen(dset, usercost)

#make numeric indicator of year
#with fyear as index it operates weird when called, new var more convenient
dset <- dset %>%
  mutate(year = as.numeric(fyear)) %>% #nolint
  filter(!is.na(fyear))

############################################################
################## 1.c Set up industry stuff  ##############
############################################################

#add industry names following deu and literally using 2 digit naics
data <- invisible(industry_n_dig_2(dset, 2))
#remove sector 92
data <- data[data$industry != "92",]

#add market shares
data <- invisible(market_share(data))

############################################################
################## 1.d Clean  ##########################
############################################################

#clean and trim following DEU 2020
data <- clean_deu(data)

#keep only relevant columns
data <- data %>% dplyr::select(sale, cogs,
  ppegt, xsga, naics, industry, conm, #nolint
  year, GVKEY, fyear, m, m3, m4, xad, K) #nolint

############################################################
################## 1.e Check DEU subset  ###################
############################################################

#check if properly getting deu obs
pdata_deu <- data[data[["year"]] < 2017 & data[["year"]] > 1954, ]
summary(pdata_deu[, c("sale", "cogs", "ppegt", "xsga")])
#seems a little off
#cogs is quite a bit higher than DEU, others quite close, also i have extra obs
 #      Mean          Median     No.obs             #nolint
 # Sales 1,922,074    147,806    247,644
 # COGS  1,016,550    55,384
 # PPEGT 1,454,210    57,532
 # XSG&A 342,805      29,682
#maybe trim cogs different, but median also different... weird
#getting alot of NA for industry share. double check na rm

############################################################
################## 1.f Set up panel  #######################
############################################################

# Create the panel data
pdata <- pdata.frame(data, index = c("GVKEY", "fyear"))

#gen logs
pdata <- pdata %>% #nolint
  mutate(c = log(cogs), k = log(ppegt), y = log(sale), x = log(xsga) ) #nolint

#keep only relevant columns
pdata <- pdata %>% dplyr::select(y, c, k, x, naics, industry, #nolint
  year, GVKEY, fyear, m, m3, m4) #nolint


############################################################
############################################################
#     2: estimate
############################################################
############################################################

############################################################
###########         2.a set up data       #############
############################################################

xvars <- c("c", "k")
#zvars <- NULL #nolint for test
zvars <- c(all_of(mvars))
yvar <- "y"
orthogx <- c("c_l", "k")

#note item 3 is first stage controls, item 4 is second stage controls
setup <- set_up(yvar, xvars, zvars, zvars, orthogx, pdata)

temp <- setup$data
yvar <- setup$y
xvars <- setup$xvars
xvar_l <- setup$xvar_l
fs_rhs <- setup$fs_rhs
ss_con <- setup$ss_controls
ss_con_l <- setup$ss_controls_l
orthogs <- setup$orthogs

############################################################
###########         2.b by sector estimate       #############
############################################################

source("function_deu.R")
theta_s <-
  acf_bysector_exp(
                   temp, yvar, xvars, xvar_l,
                   fs_rhs, ss_con, ss_con_l, orthogs)

view(theta_s)

plot(density(theta_s$theta, na.rm = TRUE))

data_s <-
  merge(data, theta_s, by.x = c("industry"),
        by.y = c("industry"), all.x = TRUE)

#generate mu_deu = theta/alpha
data_s$MU_deu <- data_s$theta / data_s$alpha

#navigate to data folder
setwd(dircs[2])
write.csv(data_s, "DEU_s.csv")
write.csv(theta_s, "thetas_s.csv")

############################################################
###########         2.b rolling window       #############
############################################################

#set rolling window length and initial block
r <- 5
block <- 19
source("function_deu.R")
theta_st <-
  acf_rolling_window_exp(temp, yvar, xvars, xvar_l, fs_rhs,
                         ss_con, ss_con_l, orthogs, r, block)

view(theta_st)

plot(density(theta_st$theta, na.rm = TRUE))

############################################################
###########         2.c constant .85       #############
############################################################

#constant
theta_c <- .85

############################################################
############################################################
############     3: Compute DWL markups   ##################
############################################################
############################################################

#merge thetas with data
data_st <-
  merge(data, theta_st, by.x = c("industry", "fyear"),
        by.y = c("industry", "fyear"), all.x = TRUE)


data_c <- data
data_c$theta <- .85 #nolint

#generate mu_deu = theta/alpha
data_st$MU_deu <- data_st$theta / data_st$alpha
data_c$MU_deu <- data_c$theta / data_c$alpha

############################################################
############################################################
#################     4: Save Data   #######################
############################################################
############################################################

#navigate to data folder
setwd(dircs[2])

write.csv(data_s_yr, "DEU_st.csv")
write.csv(data_s, "DEU_s.csv")
write.csv(pdata_deu, "DEU_c.csv")

write.csv(theta_st, "theta_st.csv")
write.csv(thetas_s, "thetas_s.csv")







############################################################
############################################################
############     5: explore Markups /sandbox  ##################
############################################################
############################################################

plot(density(data_st$MU_deu, na.rm = TRUE))

# Calculate the 1st and 99th percentiles
q1 <- quantile(data_st$MU_deu, 0.05, na.rm = TRUE)
q99 <- quantile(data_st$MU_deu, 0.95, na.rm = TRUE)

# Create the density plot
ggplot(data_st, aes(x = MU_deu)) +
  geom_density() +
  scale_x_continuous(trans = 'log10', limits = c(q1, q99)) + #nolint
  theme_minimal()

# Create the agg plot
#create data
agg_data <- data_st %>%
  group_by(fyear) %>% # nolint
  summarise(weighted_mean = weighted.mean(MU_deu, sale, na.rm = TRUE)) # nolint

names(agg_data) <- c("year", "Ag_MU")

agg_data$year <- as.numeric(as.character(agg_data$year))

# Filter the data
agg_data_filtered <- agg_data %>% filter(year > 1955)

agg_data_filtered <- agg_data_filtered %>% filter(year < 2017)

#plot
agg_mu_plot <-  ggplot() +
  geom_line(data = agg_data,
              aes(y = Ag_MU - 1, x = year, color = "DWL/DEU")) + # nolint
  theme(text = element_text(size = 20)) +
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(legend.position = "bottom")

agg_mu_plot


hold <- thetas %>%
  filter(theta < 0)

View(hold)

hold2 <- thetas %>%
  filter(industry == "42")

summary(hold2)
view(hold2)
#low values at 1993 and 2016


ggplot(hold2, aes(x = fyear, y = theta)) +
  geom_line() +
  ylim(0.2, 1.1)

#merge thetas with data
datafinal <-
  merge(pdata, thetas,
        by.x = c("industry", "year"),
        by.y = c("industry", "fyear"), all.x = TRUE)

#generate mu_deu = theta/alpha
datafinal$mu_deu <- datafinal$theta / datafinal$alpha

density(datafinal$mu_deu, na.rm = TRUE)