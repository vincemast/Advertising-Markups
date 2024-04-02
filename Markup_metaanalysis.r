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
library(tidyverse)
library(knitr)
library(kableExtra)
library(stargazer)
library(plm)
library(lmtest)
library(sandwich)

#navigate to folder with functions
setwd(dircs[1])
#functions
source("function_subsets.R")
source("function_plots.R")
source("function_regressions.R")
source("function_usefull.R")

############################################################
############################################################
#     1: Load Data
############################################################
############################################################

######################## 1.a ###############################
# markups

#navigate to folder with markup data
setwd(dircs[2])


#Either take compustat and clean (done here) or:
#use DEU data (includes both markups)
#comment out line 86 to use this method

#compustat
dset <- read.csv("COMPUSTAT.csv") # nolint
#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"
#with DEU markups generated
data <- read.csv("DEU_s.csv")

# comment out to clean differently
#wont allow deu markups
dset <- data


#FRED DATA
usercost <- read.csv("usercost.csv") #nolint
#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"
#apply GDP deflator and generate xad measures
dset <- VariableGen(dset, usercost)

#make a numericl variable of year (might be string by default)
dset <- dset %>%
  mutate(year = as.numeric(fyear)) %>% #nolint
  filter(!is.na(fyear)) #nolint

#make numeric indicator of year
#uncomment out this block if using Compustat directly
#dset$industry <- as.factor(dset$naics) #nolint
#dset <- clean(dset) #nolint

######################## 1.a ###############################
# blp data prep and function
#create data set for storage
blp_0 <- data.frame(industry = character(),
                    agg = character(),
                    measure = character(),
                    naics = numeric(),
                    ind_digits = numeric(),
                    start = numeric(),
                    end = numeric(),
                    paper = character(),
                    method = character(),
                    mu = numeric(),
                    ca_mu = numeric(),
                    deu_mu = numeric())

#create function to generate ac_mu
gen_ac_mu <- function(blp_c, data) {

  #get data for industry
  temp <- data %>%
    filter(substr(naics, 1, blp_c[5]) == blp_c[4]) %>% #nolint
    filter(year >= blp_c[6] & year <= blp_c[7]) #nolint

  # Check if MU_deu exists in the data (if not will just set to NA)
  if (!"MU_deu" %in% names(temp)) {
    temp$MU_deu <- NA
  }

  # change accounting markup to version in blp before aggregation
  # markup ratio := mu = p/c; markup := mu - 1;
  #learner: l = (p-c)/p = 1-1/(mu) nolint
  if (blp_c[3] == "markup") {
    temp <-  temp %>% mutate(mark = MU_1, mark2 = MU_deu-1) #nolint
  } else if (blp_c[3] == "ratio") {
     temp <-  temp %>% mutate(mark = MU, mark2 = MU_deu-1) #nolint
  } else if (blp_c[3] == "lerner") {
     temp <-  temp %>% mutate(mark = 1 - 1 / MU_1, mark2 = 1 - 1 / (MU_deu-1)) #nolint
  } else {
    stop("Invalid measure. It should be 'markup', 'ratio', or 'lerner'.")
  }

  # Remove infinite values from temp$mark
  temp <- temp %>% filter(is.finite(mark)) #nolint
  temp2 <- temp %>% filter(is.finite(mark2)) #nolint

  #get relevant summary of accounting data based on agg
  if (blp_c[2] == "avg") {
    agg_mu <- mean(temp$mark, na.rm = TRUE)
    agg_mudeu <- mean(temp2$mark2, na.rm = TRUE)
  } else if (blp_c[2] == "median") {
    agg_mu <- median(temp$mark, na.rm = TRUE)
    agg_mudeu <- median(temp2$mark2, na.rm = TRUE)
  } else if (blp_c[2] == "sw") {
    agg_mu <- weighted.mean(temp$mark, w = temp$sale, na.rm = TRUE)
    agg_mudeu <- weighted.mean(temp2$mark2, w = temp2$sale, na.rm = TRUE)
  } else {
    stop("Invalid value for agg. It should be 'avg', 'median', or 'sw'.")
  }

  #save as markup (mu-1)
  #also change their measure to markup
  # mu - 1 = 1/(1-l) - 1 nolint
  if (blp_c[3] == "markup") {
    blp_c[11] <- agg_mu
    blp_c[12] <- agg_mudeu
  } else if (blp_c[3] == "ratio") {
   blp_c[11] <- agg_mu - 1 #nolint
   blp_c[10] <- as.numeric(blp_c[10]) - 1
    blp_c[12] <- agg_mudeu - 1
  } else if (blp_c[3] == "lerner") {
   blp_c[11] <- 1 / (1 - agg_mu) #nolint
   blp_c[10] <- 1 / (1 - as.numeric(blp_c[10])) - 1
   blp_c[12] <- 1 / (1 - agg_mudeu)
  } else {
    stop("Invalid value for measure. It should be 'ratio' or 'lerner'.")
  }

  # give with correct column names
  data.frame(
    industry = blp_c[1],
    agg = blp_c[2],
    measure = blp_c[3],
    naics = blp_c[4],
    ind_digits = blp_c[5],
    start = blp_c[6],
    end = blp_c[7],
    paper = blp_c[8],
    method = blp_c[9],
    mu = blp_c[10],
    ca_mu = blp_c[11],
    deu_mu = blp_c[12]
  )
}


######################## 1.b ###############################
# BLP 1995 https://pages.stern.nyu.edu/~wgreene/Econometrics/BLP.pdf
# 1990 cars below table VIII: average lerner is 0.239

# naisc code: 3361 Motor Vehicle Manufacturing
cars_1990 <- c("Automobile", "avg", "lerner", 3361, 4, 1990, 1990,
               "BLP (1995)", "BLP",  .239)

blp  <- rbind(blp_0, gen_ac_mu(cars_1990, dset))

######################## 1.c ###############################
# Nevo (2001) https://pages.stern.nyu.edu/~acollard/NevoEcma.pdf
# ready to eat cereal
# 1988-1992 (median) lerner =  .422 (table VIII)
nevo <- c("Breakfast Cereal", "median", "lerner", 31123, 5, 1988, 1992,
          "Nevo (2001)", "BLP",  .422)

blp_nevo <- rbind(blp_0, gen_ac_mu(nevo, dset))

blp <- rbind(blp, blp_nevo)


######################## 1.d ###############################
# https://www.nber.org/system/files/working_papers/w31230/w31230.pdf
# uses lerner (P-mc)/P
# going to use their medians, perserved under transformation
#note is product level median, well use firm level median
# data in 2006 and 2018
# Soft Drinks- Carbonated: 2006: 0.15; 2018: 0.11
# Soft Drinks- Low Calorie : 0.18; 2018: 0.35
# Water-Bottled: 0.25; 2018: 0.40
# Cereal- Ready To Eat: 0.37; 2018: 0.35
# Light Beer (Low Calorie/Alcohol): 0.17; 2018: 0.16
# Toilet Tissue: 0.23; 2018: 0.39
# Fruit Drinks-Other Container: 0.22 ; 2018: 0.21
# Dairy-Milk-Refrigerated: 0.25 ; 2018: 0.36
# Yogurt-Refrigerated: 0.29 ; 2018: 0.37

#more here if want; all 75 product markets (page 33+)

#for soft drinks will take midpoint of reg and loc cal: 2006: 0.165; 2018: 0.23
# naisc code: 312111 Soft Drink Manufacturing
# lerner
soda_2006 <- c("Soft Drinks", "median", "lerner", 312111, 6, 2006, 2006,
               "Atalay et al. (WP)", "NL", .165)
soda_2018 <- c("Soft Drinks", "median", "lerner", 312111, 6, 2018, 2018,
               "Atalay et al. (WP)", "NL", .23)

# naisc code: 312112 Bottled Water Manufacturing
water_2006 <- c("Water-Bottled", "median", "lerner", 312111, 6, 2006, 2006,
                "Atalay et al. (WP)", "NL", .25)
water_2018 <- c("Water-Bottled", "median", "lerner", 312111, 6, 2018, 2018,
                "Atalay et al. (WP)", "NL", .4)

# naisc code: 31123 Breakfast Cereal Manufacturing
cereal_2006 <- c("Cereal- Ready To Eat", "median", 
                 "lerner", 31123, 5, 2006, 2006,
                 "Atalay et al. (WP)", "NL", .37)
cereal_2018 <- c("Cereal- Ready To Eat", "median", 
                 "lerner", 31123, 5, 2018, 2018,
                 "Atalay et al. (WP)", "NL", .35)

# naisc code: 322291 Sanitary Paper Product Manufacturing
toilet_2006 <- c("Toilet Tissue", "median", "lerner", 322291, 6, 2006, 2006,
                 "Atalay et al. (WP)", "NL", .23)
toilet_2018 <- c("Toilet Tissue", "median", "lerner", 322291, 6, 2018, 2018,
                 "Atalay et al. (WP)", "NL", .39)

# naisc code: 311411 Frozen Fruit, Juice, and Vegetable Manufacturing
juice_2006 <- c("Fruit Drinks", "median", "lerner", 311411, 6, 2006, 2006,
                "Atalay et al. (WP)", "NL", .22)
juice_2018 <- c("Fruit Drinks", "median", "lerner", 311411, 6, 2018, 2018,
                "Atalay et al. (WP)", "NL", .21)

# naisc code: 311511 Fluid Milk Manufacturing
milk_2006 <- c("Dairy-Milk-Refrigerated", "median", "lerner",
               311511, 6, 2006, 2006,
               "Atalay et al. (WP)", "NL", .25)
milk_2018 <- c("Dairy-Milk-Refrigerated", "median", "lerner",
               311511, 6, 2018, 2018,
               "Atalay et al. (WP)", "NL", .36)



blp_atalay <- rbind(blp_0,
  gen_ac_mu(soda_2006, dset), gen_ac_mu(soda_2018, dset),
  gen_ac_mu(water_2006, dset), gen_ac_mu(water_2018, dset),
  gen_ac_mu(cereal_2006, dset), gen_ac_mu(cereal_2018, dset),
  #gen_ac_mu(toilet_2006, dset), gen_ac_mu(toilet_2018, dset),
  gen_ac_mu(juice_2006, dset), gen_ac_mu(juice_2018, dset),
  gen_ac_mu(milk_2006, dset), gen_ac_mu(milk_2018, dset)
)

blp <- rbind(blp, blp_atalay)


######################## 1.e ###############################
# airline https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3417225
# LERNER: 1999:  0.63 (TABLE 8a); LERNER 2006: .60
#doent say agg method, use sales weighted

airline_1999 <- c("Airline", "sw", "lerner", 481111, 6, 1999, 1999,
                  "Berry, Barwick (WP)", "???",  .63)
airline_2006 <- c("Airline", "sw", "lerner", 481111, 6, 2006, 2006,
                  "Berry, Barwick (WP)", "???",  .6)

blp_bb <- rbind(blp_0,
                gen_ac_mu(airline_1999, dset), gen_ac_mu(airline_2006, dset))

#blp <- rbind(blp, blp_bb) #nolint
#not using, not a good anaology and fit is terrible

######################## 1.f ###############################
# https://web.stanford.edu/~ayurukog/CarMarkupsJuly2023.pdf
# sales weighted markup ratio US automobile indsutry (1980-2018)
# naisc 3361
#REPLICATION  Citation: Grieco, Paul L. E.; Murry, Charles; Yurukoglu, Ali, 2023, "Replication Data for: 'The Evolution of Market Power in the Automobile Industry'", https://doi.org/10.7910/DVN/CZGOKP, Harvard Dataverse, V1; GMY_Replication.tar.gz [fileName] #nolint
#1980: 1.65345769837934
#1990: 1.36329051479847
#2000: 1.26640829830938
#2010: 1.26993766489373
#2018: 1.25655168574207

auto_1980 <- c("Automobile", "sw", "ratio", 3361, 4, 1980, 1980,
               "Grieco et al. (2023)", "RCL", 1.65345769837934)
auto_1990 <- c("Automobile", "sw", "ratio", 3361, 4, 1990, 1990,
               "Grieco et al. (2023)", "RCL", 1.36329051479847)
auto_2000 <- c("Automobile", "sw", "ratio", 3361, 4, 2000, 2000,
               "Grieco et al. (2023)", "RCL", 1.26640829830938)
auto_2010 <- c("Automobile", "sw", "ratio", 3361, 4, 2010, 2010,
               "Grieco et al. (2023)", "RCL", 1.26993766489373)
auto_2018 <- c("Automobile", "sw", "ratio", 3361, 4, 2018, 2018,
               "Grieco et al. (2023)", "RCL", 1.25655168574207)

blp_grieco <- rbind(blp_0,
                    gen_ac_mu(auto_1980, dset), gen_ac_mu(auto_1990, dset),
                    gen_ac_mu(auto_2000, dset), gen_ac_mu(auto_2010, dset),
                    gen_ac_mu(auto_2018, dset))

blp <- rbind(blp, blp_grieco)

######################## 1.g ###############################
# http://www.nathanhmiller.org/mwbeer.pdf
# data from 2001-2008, average lerner (p-mc)/p=.34
#naisc code

beer <- c("Beer", "avg", "lerner", 312120, 6, 2001, 2008,
          "Miller, Weinberg (2017)", "RCNL", .34)

blp_miller <- rbind(blp_0, gen_ac_mu(beer, dset))

blp <- rbind(blp, blp_miller)

######################## 1.h ###############################
# Brand: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3712513
# full sample Median markup (FRAC) (ratio); 1.92 (2006); 2.32 (2017)
#use FRAC
#Median
#BLP Own-Price 2006 Markups 2006 Own-Price 2017 Markups 2017
#Fruit Drinks -2.26 1.82 -1.55 2.20
#Soup -4.00 1.97 -2.41 2.90
#Cookies -2.08 2.01 -1.59 2.55
#Pizza -2.31 1.88 -2.22 1.90
#Ice Cream -1.93 2.01 -1.87 2.28
#Entrees -1.92 2.09 -1.55 2.42
#Yogurt -2.28 1.89 -1.44 2.13
#Remaining Fruit -2.40 1.73 -2.04 2.18
#Light Beer -3.13 1.49 -2.23 1.67

# full sample: maybe food manufactoring naisc 311


#ice cream naisc 311520
ice_2006 <- c("Ice Cream", "median", "ratio", 311520, 6, 2006, 2006,
              "Brand (WP)", "FRAC", 2.01)
ice_2017 <- c("Ice Cream", "median", "ratio", 311520, 6, 2017, 2017,
              "Brand (WP)", "FRAC", 2.28)

# naisc code: 311411 Frozen Fruit, Juice, and Vegetable Manufacturing
juice_b_2006 <- c("Fruit Drinks", "median", "ratio", 311411, 6, 2006, 2006,
                  "Brand (WP)", "FRAC", 1.82)
juice_b_2017 <- c("Fruit Drinks", "median", "ratio", 311411, 6, 2017, 2017,
                  "Brand (WP)", "FRAC", 2.20)

#cookies naisc code: 311821 Cookie and Cracker Manufacturing
cookies_2006 <- c("Cookies", "median", "ratio",  311821, 6, 2006, 2006,
                  "Brand (WP)", "FRAC", 2.01)
cookies_2017 <- c("Cookies", "median", "ratio",  311821, 6, 2017, 2017,
                  "Brand (WP)", "FRAC", 2.55)

#Entrees naisc code: 311412 Frozen Specialty Food Manufacturing
entrees_2006 <- c("Entrees", "median", "ratio", 311412, 6, 2006, 2006,
                  "Brand (WP)", "FRAC", 2.09)
entrees_2017 <- c("Entrees", "median", "ratio", 311412, 6, 2017, 2017,
                  "Brand (WP)", "FRAC", 2.42)


blp_brand <- rbind(blp_0,
                   gen_ac_mu(ice_2006, dset), gen_ac_mu(ice_2017, dset),
                   gen_ac_mu(juice_b_2006, dset), gen_ac_mu(juice_b_2017, dset),
                   gen_ac_mu(cookies_2006, dset), gen_ac_mu(cookies_2017, dset),
                   gen_ac_mu(entrees_2006, dset), gen_ac_mu(entrees_2017, dset))

blp <- rbind(blp, blp_brand)

######################## 1.i ###############################
# https://www.nber.org/system/files/working_papers/w32036/w32036.pdf
#    - wholesale
#    -table 7 panel B: average markup ratio (p/c) (1997, 2002, 2007)
#    -3 data points
#    1997 2002 2007
# FullModelWithLocalMarketPower 1.256 1.283 1.303

wholesale_1997 <- c("Wholesale", "avg", "ratio", 42, 2, 1997, 1997,
                    "Ganapati (2024)", "NL",  1.256)
wholesale_2002 <- c("Wholesale", "avg", "ratio", 42, 2, 2002, 2002,
                    "Ganapati (2024)", "NL",  1.283)
wholesale_2007 <- c("Wholesale", "avg", "ratio", 42, 2, 2007, 2007,
                    "Ganapati (2024)", "NL",  1.303)

blp_ganapati <-
  rbind(blp_0,
        gen_ac_mu(wholesale_1997, dset), gen_ac_mu(wholesale_2002, dset),
        gen_ac_mu(wholesale_2007, dset))

blp <- rbind(blp, blp_ganapati)


######################## 1.j ###############################
# adam https://cadampfander.github.io/papers/Pfander_JMP.pdf
#mean transport p/mc-1: .035 (table 2) 2017 commercial transportation
#naisc code: 48
#representative so sales weighted

transport_2017 <- c("Transportation", "sw", "markup", 48, 2, 2017, 2017,
                    "Pfander (WP)", "NL",  .035)

blp_adam <- rbind(blp_0, gen_ac_mu(transport_2017, dset))

blp <- rbind(blp, blp_adam)


######################## 1.k ###############################
# https://www.stern.nyu.edu/sites/default/files/assets/documents/Rising_Markups_and_the_Role_of_Consumer_Preferences.pdf #nolint
#https://drive.google.com/file/d/15vpWCUa77W2wpFAjwJmxV-h4-sNbX5xK/view
# use food manufactoring naisc 311
# annual median lerner (from figure 3), cant find the exact values
# https://apps.automeris.io/wpd/:
#2006: .454
#2007: .492
#2008: .5
#2009: .537
#2010: .528
#2011: .509
#2012: .551
#2013: .567
#2014: .579
#2015: .609
#2016: .595
#2017: .611
#2018: .62
#2019: .6
 
#take first and last

food_2006 <- c("Neilson retail goods", "median", "lerner", 311, 3, 2006, 2006,
               "Dooper et al. (WP)", "RCL",  .454)
food_2019 <- c("Neilson retail goods", "median", "lerner", 311, 3, 2019, 2019,
               "Dooper et al. (WP)", "RCL",  .6)
blp_dooper <- rbind(blp_0,
                    gen_ac_mu(food_2006, dset), gen_ac_mu(food_2019, dset))

blp <- rbind(blp, blp_dooper)

############################################################
############################################################
#     2: Make Nice table
############################################################
############################################################

#clean up names for table
blp_r <- blp %>%
  mutate(
    agg = case_when(
      agg == "median" ~ "Median",
      agg == "avg" ~ "Average",
      agg == "sw" ~ "Sales-weighted average",
      TRUE ~ agg
    ),
    measure = case_when(
      measure == "lerner" ~ "lerner index",
      measure == "ratio" ~ "markup ratio",
      TRUE ~ measure
    )
  )


# Create the table
table = blp_r %>%
  mutate(
    "Markup Measure" = paste(agg, measure, sep = " "),
    "Original Level" = "Product Level",
    "Comparison Level" = "Firm Level",
    "Original Industry (Comparison NAICS code)"
    = paste(industry, " (", naics, ")", sep = ""),
    Paper = paper,
    Method = method,
    Range = ifelse(start == end, end, paste(start, end, sep = "-"))
  ) %>%
  dplyr::select(Paper, Method, "Markup Measure", "Original Level",
                "Comparison Level",
                "Original Industry (Comparison NAICS code)", Range) %>%
  group_by(Paper) %>%
  summarise_all(list(~paste(unique(.), collapse = ", ")))

# Sort the table by 'Paper'
table$Paper <- factor(table$Paper, levels = unique(blp_r$paper))
table <- table[order(table$Paper), ]

#change the two cases that are different
table <- table %>%
  mutate("Original Level" = case_when(
    Paper == "Pfander (WP)" ~ "Aggregate",
    Paper == "Ganapati (2024)" ~ "Firm Level",
    TRUE ~ `Original Level`
  ))
# change the range of grieco to reflect original range in paper
table <- table %>%
  mutate("Range" = case_when(
    Paper == "Grieco et al. (2023)" ~ "1980-2018",
    TRUE ~ `Range`
  ))


# Export the table to LaTeX
latex_table <- kable(table, "latex", booktabs = TRUE, col.names = names(table),
                     align = c(">{\\raggedright\\arraybackslash}p{4cm}",
                               ">{\\raggedright\\arraybackslash}p{3cm}",
                               ">{\\raggedright\\arraybackslash}p{4.2cm}",
                               ">{\\raggedright\\arraybackslash}p{3cm}",
                               ">{\\raggedright\\arraybackslash}p{3cm}",
                               ">{\\raggedright\\arraybackslash}p{8cm}",
                               ">{\\raggedright\\arraybackslash}p{3cm}"))

latex_table

# Convert the table to a character string
latex_table_str <- as.character(latex_table)

# Add \midrule after each row
latex_table_str <-
  gsub("\\\\", "\\\\ \\midrule ", latex_table_str, fixed = TRUE)

# Print the modified LaTeX table
cat(latex_table_str)

############################################################
############################################################
#     3: Plot Data
############################################################
############################################################
# Convert ac_mu and mu to numeric
blp$ca_mu <- as.numeric(blp$ca_mu)
blp$mu <- as.numeric(blp$mu)
blp$deu_mu <- as.numeric(blp$deu_mu)

############################################################
#cost accounting
############################################################


# Calculate ols line and correlation coef
model <- lm(mu ~ ca_mu, data = blp)
correlation <- cor(blp$ca_mu, blp$mu, use = "pairwise.complete.obs")
# Create the label for the legend
label <- paste0("OLS line (Slope = ", round(coef(model)[2], 2),
                ", Intercept = ", round(coef(model)[1], 2),
                ", Correlation Coef. = ", round(correlation, 2),
                ", N. obs =", nrow(blp), ")")

# Get the unique levels in the paper and method and save as shapes and colors
shapes <- c(15 + seq(0, length(unique(blp$method)) - 2), 1)
seq(0, length(unique(blp$method)) - 1) + 30
palette <- rainbow(length(unique(blp$paper)))

# Create a dummy data frame for the 45-degree line
df_line <- data.frame(ca_mu = range(blp$ca_mu), mu = range(blp$mu))

############## plot #####################
comp_ca <- ggplot(blp, aes(x = ca_mu, y = mu)) +
  geom_point(aes(color = paper, shape = method), size = 4) +
  geom_smooth(
    aes(linetype = label),  # Use the label for the OLS line
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  geom_line(data = df_line,
            aes(y = ca_mu, linetype = "45-degree line"), color = "black") +
  scale_linetype_manual(
    values = c("solid", "dashed"),
    labels = c(label, "45-degree line"),
    breaks = c(label, "45-degree line")
  ) +
  labs(
    color = "Paper",
    shape = "Method",
    linetype = ""
  ) +
  theme(
    text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  coord_cartesian(
    #xlim = c(min(blp$ca_mu, blp$mu), max(blp$ca_mu, blp$mu)),  #nolint
    #ylim = c(min(blp$ca_mu, blp$mu), max(blp$ca_mu, blp$mu))  #nolint
  ) +
  xlab("Accounting Markup") +
  ylab("Demand Estimation Markup") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE, order = 1),
    shape = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3)
  )

comp_ca

save_f(comp_ca, "BLP_comp_CA.pdf", dircs, 11, 11, TRUE)


############################################################
#   DEU
############################################################


# Calculate ols line and correlation coef
model_deu <- lm(mu ~ deu_mu, data = blp)
correlation_deu <- cor(blp$deu_mu, blp$mu, use = "pairwise.complete.obs")
# Create the label for the legend
label_deu <- paste0("OLS line (Slope = ", round(coef(model_deu)[2], 2),
                    ", Intercept = ", round(coef(model_deu)[1], 2),
                    ", Correlation Coef. = ", round(correlation_deu, 2),
                    ", N. obs =", nrow(blp), ")")

# Create a dummy data frame for the 45-degree line
df_line2 <- data.frame(deu_mu = range(blp$deu_mu), mu = range(blp$mu))

############## plot #####################
comp_deu <- ggplot(blp, aes(x = deu_mu, y = mu)) +
  geom_point(aes(color = paper, shape = method), size = 4) +
  geom_smooth(
    aes(linetype = label_deu),  # Use the label for the OLS line
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  geom_line(data = df_line2,
            aes(y = deu_mu, linetype = "45-degree line"), color = "black") +
  scale_linetype_manual(
    values = c("solid", "dashed"),
    labels = c(label_deu, "45-degree line"),
    breaks = c(label_deu, "45-degree line")
  ) +
  labs(
    color = "Paper",
    shape = "Method",
    linetype = ""
  ) +
  theme(
    text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  coord_cartesian(
    #xlim = c(min(blp$deu_mu, blp$mu), max(blp$deu_mu, blp$mu)),  #nolint
    #ylim = c(min(blp$deu_mu, blp$mu), max(blp$deu_mu, blp$mu))  #nolint
    xlim = c(min(blp$deu_mu), max(blp$deu_mu)),  #nolint
    ylim = c(min(blp$mu), max(blp$mu))  #nolint
  ) +
  xlab("DEU (2020) Markup") +
  ylab("Demand Estimation Markup") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE, order = 1),
    shape = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3)
  )

comp_deu


save_f(comp_deu, "BLP_comp_DEU.pdf", dircs, 11, 11, TRUE)


############################################################
############################################################
#     3: Plot Data
############################################################
############################################################

# Fit the models with clustered standard errors
model <- plm(mu ~ ca_mu, data = blp,
             index = "paper", model = "pooling")
model_deu <- plm(mu ~ deu_mu, data = blp,
                 index = "paper", model = "pooling")
model2 <- plm(mu ~ ca_mu + as.factor(method), data = blp,
              index = "paper", model = "pooling")
model2_deu <- plm(mu ~ deu_mu + as.factor(method), data = blp,
                  index = "paper", model = "pooling")

# Define a function that removes the 'method' prefix from the coefficient names
remove_prefix <- function(model) {
  # Compute the summary with the clustered standard errors
  model_summary <-
    coeftest(model, 
             vcov = vcovHC(model, type = "HC1", cluster = "group"))
  # Get the coefficient names
  coef_names <- rownames(model_summary)
  # Remove the 'method' prefix from the coefficient names
  new_coef_names <- gsub("as.factor\\(method\\)", "", coef_names)
  #also change names of markup coefficents
  new_coef_names <- gsub("ca_mu", "Cost Accouting Markup", new_coef_names)
  new_coef_names <- gsub("deu_mu", "DEU Markup", new_coef_names)
  # Rename the coefficients in the model summary
  rownames(model_summary) <- new_coef_names
  return(model_summary)
}

# Apply the function to each model
c_model <- remove_prefix(model)
c_model_deuc <- remove_prefix(model_deu)
c_model2 <- remove_prefix(model2)
c_model2_deu <- remove_prefix(model2_deu)

# Create a table with the model summaries
#put models into list
models <- list("Demand Estimate Markup" = c_model,
               "Demand Estimate Markup" = c_model_deuc,
               "Demand Estimate Markup" = c_model2,
               "Demand Estimate Markup" = c_model2_deu)

#select GOF measures
gm_temp <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "N", 0,
  "adj.r.squared", "Adj. R2", 3,
)

# Create a summary table of the models
ms <- msummary(models, gof_map = gm_temp, stars = TRUE, notes = c(
  "Standard errors in parentheses. Clustered by paper.",
  "BLP used as reference method."
))

# Print the model summary table with the notes
ms

# Export the summary table as a LaTeX file

msummary(models, gof_map = gm_temp, stars = TRUE,
         notes = c("Standard errors in parentheses. Clustered by paper.",
                   "BLP used as reference method."),
         output = "latex")