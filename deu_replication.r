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

#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"

#keep only relevant columns
dset <- dset %>% select(GVKEY, fyear, sale, cogs, ppegt, xsga, naics, conml) #nolint


################## 1.b Set up year stuff  ##########################

#make numeric indicator of year
#with fyear as index it operates weird when called, new var more convenient
dset <- dset %>%
  mutate(year = as.numeric(fyear)) %>% #nolint
  filter(!is.na(fyear))

#trim to 1950+
data <- dset[dset$year > 1950, ]
# Get the range of years
years <- range(data$year)

# Create a sequence of n-year windows
all_years <- seq(from = years[1], to = years[2])


################## 1.b Set up industry stuff  ##########################

#add industry names
data <- invisible(industry_n_dig(data, naics, 2))
data <- data[!is.na(data$industry), ]

#get unique sectors
sectors <- unique(data$industry)

#add market shares
data <- invisible(market_share(data))

################## 1.b Set up panel  ##########################
#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

# Remove rows with NA values in the sale or cogs for trim
data <-
  data[!is.na(data$sale) & !is.na(data$cogs), ] #nolint

#trim at 1%
data <- alpha_trim(data, 1)

# Create the panel data
pdata <- pdata.frame(data, index = c("GVKEY", "fyear"))

# Drop duplicates based on GVKEY and fyear
pdata <- pdata[!duplicated(pdata[, c("GVKEY", "fyear")]), ]

# Remove rows with NA values in the GVKEY, fyear, and ppegt variables #nolint
pdata <-
  pdata[!is.na(pdata$GVKEY) & !is.na(pdata$fyear) & !is.na(pdata$ppegt), ] #nolint

# Ensure that sale, cogs, and ppegt are positive
pdata <- pdata[pdata$sale > 0 & pdata$cogs > 0 & pdata$ppegt > 0, ]

############################################################
############################################################
##################     1: ACF   ####################
############################################################
############################################################

#set length of rolling window
r <- 5

#empty thetas
thetas <- NULL

#loop over sectors
for (i in 1 : length(sectors)) { #nolint
  # Subset the data to the sector
  pdata_sector <- pdata %>% filter(industry == sectors[i]) #nolint

  #loop over years
  for (current_year in  all_years) {

    # Initialize a flag for errors
    error_flag <- FALSE

    # Subset the data to the rolling windows
    pdata_window <- pdata_sector %>% filter(year >= current_year - (r - 1) / 2, #nolint
                                            year <= current_year + (r - 1) / 2) #nolint


    # Run the first stage and handle errors
    panelf <- tryCatch({
      first_stage(pdata_window)
    }, error = function(e) {
      error_flag <<- TRUE
      NA
    })

    # Run the second stage and handle errors
    theta_est <- tryCatch({
      coefs <- second_stage(panelf)
      coefs[2]
    }, error = function(e) {
      error_flag <<- TRUE
      NA
    })

    theta_temp <- c(
      sector = sectors[i],
      year = current_year,
      theta = theta_est
    )

    thetas <- rbind(thetas, theta_temp)

  }
  if (error_flag) {
    print(paste(sectors[i], "estimation complete WITH ERRORS."))
  } else {
    print(paste(sectors[i], "estimation complete without errors."))
  }
}

thetas <- data.frame(thetas)
rownames(thetas) <- NULL
names(thetas) <- c("industry", "fyear", "theta")
thetas$theta <- as.numeric(thetas$theta)

summary(thetas$theta)

plot(density(thetas$theta, na.rm = TRUE),
     main = "Density of theta estimates", xlab = "theta", ylab = "Density")


############################################################
############################################################
############     3: Compute DWL markups   ##################
############################################################
############################################################

#merge thetas with data
datafinal <-
  merge(pdata, thetas, by.x = c("industry", "year"),
        by.y = c("industry", "fyear"), all.x = TRUE)

#when uncommented looks like DEU, problem is with estimation of theta
#datafinal$theta <- .85 #nolint

#generate mu_deu = theta/alpha
datafinal$mu_deu <- datafinal$theta / datafinal$alpha

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
  summarise(weighted.mean(mu_deu, sale, na.rm = TRUE)) # nolint

names(agg_data) <- c("year", "Ag_MU")

#plot
agg_mu_plot <-  ggplot() +
  geom_line(data = agg_data,
              aes(y = Ag_MU - 1, x = as.numeric(as.character(year)), color = "DWL/DEU")) + # nolint
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
  filter(industry == "Retail Trade")

View(hold2)


plot(y=hold2$theta, x=hold2$fyear)

#merge thetas with data
datafinal <-
merge(pdata, thetas, by.x = c("industry", "year"), by.y = c("industry", "fyear"), all.x = TRUE)

#generate mu_deu = theta/alpha
datafinal$mu_deu <- datafinal$theta / datafinal$alpha

density(datafinal$mu_deu, na.rm = TRUE)