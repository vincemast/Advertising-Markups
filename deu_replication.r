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

############################################################
############################################################
#     1: Load and clean data
############################################################
############################################################

############# 1.a load data ##############
#navigate to with data
setwd(dircs[2])

#compustat
Dset <- read.csv("COMPUSTAT_simp.csv") # nolint
# naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")

colnames(naics) <- c("change", "naics_n", "industry")

#add industry names
data <- invisible(industry_n_dig(Dset, naics, 2))
data <- data[!is.na(data$industry), ]

#get unique sectors
sectors <- unique(data$industry)

#make numeric indicator of year
#with fyear as index it operates weird when called, new var more convenient
data <- data %>%
  mutate(year = as.numeric(fyear)) %>% #nolint
  filter(!is.na(fyear))

#trim to 1950+
data <- data[data$year > 1959, ]
# Get the range of years
years <- range(data$year)

# Create a sequence of n-year windows
all_years <- seq(from = years[1], to = years[2])

################## 1.b Set up panel  ##########################
#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

pdata <- pdata.frame(data, index = c("GVKEY", "fyear"))
# Drop duplicates based on GVKEY and fyear
pdata <- pdata[!duplicated(pdata[, c("GVKEY", "fyear")]), ]

# Ensure that sale, cogs, and ppegt are positive
pdata <- pdata[pdata$sale > 0 & pdata$cogs > 0 & pdata$ppegt > 0, ]
# Remove rows with NA values in the GVKEY, fyear, sale, cogs, and ppegt variables #nolint
pdata <-
  pdata[!is.na(pdata$GVKEY) & !is.na(pdata$fyear) & !is.na(pdata$sale) & !is.na(pdata$cogs) & !is.na(pdata$ppegt), ] #nolint

############################################################
############################################################
##################     1: DWL   ####################
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
thetas$theta.c <- as.numeric(thetas$theta.c)
rownames(thetas) <- NULL
names(thetas) <- c("industry", "fyear", "theta")

summary(thetas$theta)

plot(density(thetas$theta, na.rm = TRUE),
     main = "Density of theta estimates", xlab = "theta", ylab = "Density")

hold <- thetas %>%
  filter(theta < 0)

hold2 <- thetas %>%
  filter(industry == "Finance and Insurance")

View(hold2)