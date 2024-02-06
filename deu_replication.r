############################################################
############################################################
#0: Set working directories
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
#1: clear all and load functions
############################################################
############################################################
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

############# 1.a load data ##############
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

################## 1.b Set up year stuff  ##########################

#apply GDP deflator
dset <- GDPdef(dset, usercost)

#make numeric indicator of year
#with fyear as index it operates weird when called, new var more convenient
dset <- dset %>%
  mutate(year = as.numeric(fyear)) %>% #nolint
  filter(!is.na(fyear))

################## 1.c Set up industry stuff  ##########################

#add industry names following deu and literally using 2 digit naics
data <- invisible(industry_n_dig_2(dset, 2))
#commented code maps to industry name
#data <- invisible(industry_n_dig(data, naics, 2)) #nolint

#get unique sectors
sectors <- unique(data$industry)

#add market shares
data <- invisible(market_share(data))

################## 1.d Set up panel  ##########################
#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

#clean and trim following DEU 2020
pdata <- clean_deu(data)

# Create the panel data
pdata <- pdata.frame(pdata, index = c("GVKEY", "fyear"))

#keep only relevant columns
pdata <- pdata %>% select(sale, cogs,
  ppegt, xsga, naics, industry, industry_share, alpha, conm, #nolint
  year, GVKEY, fyear, industry_share_3, industry_share_4, xad) #nolint

################## 1.c Check DEU subset  ##########################

#check if properly getting deu obs
pdata_deu <- pdata[pdata[["year"]] < 2017 & pdata[["year"]] > 1954, ]
summary(pdata_deu$fyear)
summary(pdata_deu)
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
############################################################
##################     2: ACF   ####################
############################################################
############################################################

#rolling window
#set length of rolling window
r <- 5
thetas <- acf_rolling_window(pdata, r)
view(thetas)
plot(density(thetas$theta, na.rm = TRUE),
     main = "Density of theta estimates", xlab = "theta", ylab = "Density")
thetas_s_yr <- thetas[, 1:3]

#by sector
thetas_s_deu <- acf_bysector(pdata_deu)
thetas_s <- thetas_s_deu[, 1:2]
names(thetas_s) <- c("industry", "theta")
view(thetas_s_deu)

#constant
theta_c <- .85

############################################################
############################################################
############     3: Compute DWL markups   ##################
############################################################
############################################################

#merge thetas with data
data_s_yr <-
  merge(pdata, thetas_s_yr, by.x = c("industry", "fyear"),
        by.y = c("industry", "fyear"), all.x = TRUE)

data_s <-
  merge(pdata, thetas_s, by.x = c("industry"),
        by.y = c("industry"), all.x = TRUE)

pdata_deu <- pdata
pdata_deu$theta <- .85 #nolint

#generate mu_deu = theta/alpha
data_s_yr$MU_deu <- data_s_yr$theta / data_s_yr$alpha
data_s$MU_deu <- data_s$theta / data_s$alpha
pdata_deu$MU_deu <- pdata_deu$theta / pdata_deu$alpha

############################################################
############################################################
#################     4: Save Data   #######################
############################################################
############################################################

#navigate to data folder
setwd(dircs[2])

write.csv(data_s_yr, "DEU_s_yr.csv")
write.csv(data_s, "DEU_s.csv")
write.csv(pdata_deu, "DEU_c.csv")








############################################################
############################################################
############     5: explore Markups   ##################
############################################################
############################################################

plot(density(datafinal$mu_deu, na.rm = TRUE))

# Calculate the 1st and 99th percentiles
q1 <- quantile(datafinal$mu_deu, 0.01, na.rm = TRUE)
q99 <- quantile(datafinal$mu_deu, 0.99, na.rm = TRUE)

# Create the density plot
ggplot(datafinal, aes(x = mu_deu)) +
  geom_density() +
  scale_x_continuous(trans = 'log10', limits = c(q1, q99)) + #nolint
  theme_minimal()

# Create the agg plot
#create data
agg_data <- datafinal %>%
  group_by(fyear) %>% # nolint
  summarise(weighted_mean = weighted.mean(mu_deu, sale, na.rm = TRUE)) # nolint

names(agg_data) <- c("year", "Ag_MU")

agg_data$year <- as.numeric(as.character(agg_data$year))

# Filter the data
agg_data_filtered <- agg_data %>% filter(year > 1955)

agg_data_filtered <- agg_data_filtered %>% filter(year < 2017)

#plot
agg_mu_plot <-  ggplot() +
  geom_line(data = agg_data_filtered,
              aes(y = Ag_MU - 1, x = year, color = "DWL/DEU")) + # nolint
  theme(text = element_text(size = 20)) +
  labs(x = "Year", y = "Sales Weighted Markup") +
  theme(legend.position = "bottom")

agg_mu_plot

############################################################
############################################################
############     999:sandbox   ##################
############################################################
############################################################
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
  ylim(0.2, 1)

#merge thetas with data
datafinal <-
  merge(pdata, thetas,
        by.x = c("industry", "year"),
        by.y = c("industry", "fyear"), all.x = TRUE)

#generate mu_deu = theta/alpha
datafinal$mu_deu <- datafinal$theta / datafinal$alpha

density(datafinal$mu_deu, na.rm = TRUE)