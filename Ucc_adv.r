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

#navigate to folder with functions
setwd(dircs[1])
#functions
source("function_subsets.R")
source("function_plots.R")
source("function_regressions.R")
source("function_usefull.R")
source("function_exit.r")

############################################################
############################################################
#     1: Load and clean data
############################################################
############################################################

#navigate to folder with data
setwd(dircs[2])

#############################################################
#1.a load data
###########################################################
#compustat
compu <- read.csv("COMPUSTAT_wfoot.csv") # nolint
#naics
naics <- read.csv("2022_NAICS_Structure.csv")
colnames(naics) <- c("change", "naics_n", "industry")
#macro data
macro <- read.csv("real_int_rate.csv")

#############################################################
#1.b clean data
###########################################################

#rename gvkey to GVKEY
colnames(compu)[colnames(compu) == "gvkey"] <- "GVKEY"

#add industry names following deu and literally using 2 digit naics
dset <- invisible(industry_n_dig_2(compu, 2))

#clean data (and trim to advertising)
dset <- clean_adv(dset)


# Create flags for not reporting and for "no advertising"
#count as no advertising if adv/sales in bottom 1% of sector/year
dset <- dset %>%
  mutate(
    Adv_share = ifelse(!is.na(xad), xad / sale, NA_real_),
    report_inc = ifelse(!is.na(xad), 1, 0)
  ) %>%
  group_by(industry, fyear) %>%
  mutate(
    bottom = quantile(Adv_share, 0.02, na.rm = TRUE),
    adv_inc = ifelse(Adv_share <= bottom, 0, 1)
  ) %>%
  ungroup() %>%
  mutate(
    adv_inc = replace_na(adv_inc, 0)
  )


#select only variables we need
data <- dset %>%
  select(GVKEY, fyear, industry,
         sale, cogs, xsga, ppegt, xopr, xad,
         Adv_share, adv_inc, report_inc)

#firms not advertising
summary(data$adv_inc)
summary(data$report_inc)
# about 1/3 of the set reports non 0, 1/3 of the set reports adv, most non-zero

#generate exit flags in words
data <- data %>%
  mutate(adv_incyn = case_when(
    adv_inc == 1 ~ "Reported (Non Negligible)",
    adv_inc == 0 & report_inc == 1 ~ "Reported (0 or Negligible)",
    report_inc == 0 ~ "Not Reported",
  ))


############################################################
############################################################
#     2: Exit analsysis start
############################################################
############################################################

#############################################################
#2.a histograms
###########################################################


#industry adv probability
adv_prob_sec <- data %>%
  filter(!is.na(adv_inc)) %>%
    group_by(industry) %>% #nolint
    summarise(
      prob_advp = mean(adv_inc, na.rm = TRUE),
      prob_adv = mean(report_inc, na.rm = TRUE),
      prob_diff = prob_adv - prob_advp
    )

# Reshape the data to long format
adv_prob_sec_long <- adv_prob_sec %>%
  pivot_longer(
               cols = c(prob_advp, prob_diff),
               names_to = "variable", values_to = "value")

# Create a stacked bar plot
adv_prob_industry <-
  ggplot(adv_prob_sec_long, aes(x = industry, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values =
    c("prob_advp" = "#AEC6CF", "prob_diff" = "steelblue"),
    labels =
    c("prob_advp" = "Reports Positive Advertising",
      "prob_diff" = "Reports Negligible Advertising")
  ) +
  labs(x = "Industry", y = "Probability", fill = "Advertising") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Advertising")) +
  ggtitle("Advertising Reporting by Industry") +
  theme(
    aspect.ratio = 3 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16),
    legend.position = "bottom"
  )

adv_prob_industry

#############################################################
#2.b No Adv condition
###########################################################

#A/QMC \leq varepsilon (P-MC)/MC
#with equality if A/QMC >0
#no advertising if P<MC<
#Sale<Cogs + R K
#(Sale - Cogs)/K > R
#R_{adv}  equiv (Sale - Cogs)/K
#advertise if R_{adv} <R


#only want firms with capital
data <- data %>%
  filter(ppegt > 0)

data <- data %>%
  mutate(R_idf = (sale - cogs) / ppegt)

summary(data$R_idf)


#but some elements of the user cost of capital vary over time
#user cost of capital = rf + delta + risk premium
#rf = real risk free return
#call adj risk premium: + delta + risk premium
#R_{adj adv}  = R_{adv} - rf


#get real interest rate
macro_vars <- macro %>%
  mutate(real_int = (FEDFUNDS - Ifl) * .01)
macro_vars <- macro_vars %>%
  select(fyear, real_int)

#merge with data by fyear
data_int <- merge(data, macro_vars, by = "fyear")

data_int <- data_int %>%
  mutate(R_adjidf = R_idf - real_int)

summary(data_int$R_adjidf)
summary(data_int$real_int)

############################################################
############################################################
#     3 Break even user cost of capital
############################################################
############################################################

#############################################################
#3.a density
###########################################################

#density of break even user cost of capital given not exit
lower_limit <- quantile(data$R_idf, 0.01)
upper_limit <- quantile(data$R_idf, 0.95)

advuccplot <-
  ggplot(data, aes(x = R_idf, fill = factor(adv_incyn))) +
  geom_density(alpha = .4) +
  labs(
       x = "R Indifferent",
       y = "Density",
       fill = "Advertising?") +
  scale_fill_manual(values = c("black", "blue", "red")) +
  theme_minimal() +
  xlim(lower_limit, upper_limit) +
  theme(
    aspect.ratio = 3 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16)
  )
#error message just from the limts, not a concern




# Create the plot
advucc_plot <- ggplot(data, aes(x = R_idf, fill = factor(adv_incyn))) +
  geom_density(alpha = 0.5) +
  labs(
    x = "R Indifferent",
    y = "Density",
    fill = "Advertising?"
  ) +
  scale_fill_manual(values = c("blue", "black", "red")) +
  theme_minimal() +
  coord_cartesian(xlim = c(lower_limit, upper_limit)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(base = 2)) +
  theme(
    aspect.ratio = 3 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16)
  )

advucc_plot

#############################################################
#3.b probability of advertising vs break even
###########################################################

#include rounding bc it wants to put into scientific notation sometimes
int_breaks <- c(round(seq(0, .6, by = .02), 2))
breaks <- c(-Inf, int_breaks, Inf)

# Create the buckets and calculate the averages
bucket_prob <- data %>%
  mutate(bucket = cut(R_idf, breaks, include.lowest = TRUE)) %>%
  group_by(bucket) %>%
  summarise(
    prob_advp = 1 - mean(adv_inc, na.rm = TRUE),
    prob_adv = 1 - mean(report_inc, na.rm = TRUE),
    prob_diff = prob_advp - prob_adv

  )

# Create a numeric sequence for the x-axis
bucket_prob$bucket_num <- seq_along(bucket_prob$bucket)

# Create labels for the x-axis
int_lab <-
levels(bucket_prob$bucket)[-c(1, length(levels(bucket_prob$bucket)))]
labels <- c("< 0", int_lab, "> .5")

# Add a new variable to indicate the first, last, or neither bucket
#give outter buckets own color
bucket_prob <- bucket_prob %>%
  mutate(bucket_type = case_when(
    bucket_num == 1 ~ "first",
    bucket_num == length(bucket_num) ~ "last",
    TRUE ~ "neither"
  ))

# Reshape the data to long format
bucket_prob_long <- bucket_prob %>%
  pivot_longer(cols =
                 c(prob_diff, prob_adv),
               names_to = "variable", values_to = "value")


# Plot the average exit_year for each bucket
adv_prob_plot <-
ggplot(bucket_prob_long, aes(x = bucket_num, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values =
    c("prob_diff" = "black",
      "prob_adv" = "steelblue"),
    labels =
    c("prob_diff" = "Reported Negligible",
      "prob_adv" = "Not Reported")
  ) +
  labs(x = "R Indifferent", y = "Probability", fill = "Variable") +
  theme_minimal() +
  theme(
    aspect.ratio = 2 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = .5)
  ) +
  scale_x_continuous(breaks = bucket_prob$bucket_num, labels = labels) +
  guides(fill = guide_legend(title = "Advertising?")) +
  ylim(0, 1)

adv_prob_plot


############################################################
############################################################
#     4: Exit analsysis with real int
############################################################
############################################################

#############################################################
#         4.a density
###########################################################


lower_limit <- quantile(data_int$R_adjidf, 0.01)
upper_limit <- quantile(data_int$R_adjidf, 0.95)

advurpplot <-
  ggplot(data_int, aes(x = R_adjidf, fill = factor(adv_incyn))) +
  geom_density(alpha = .4) +
  labs(
       x = "Risk Premium + Depreciation Indifferent",
       y = "Density",
       fill = "Advertising?") +
  scale_fill_manual(values = c("black", "blue", "red")) +
  theme_minimal() +
  xlim(lower_limit, upper_limit) +
  theme(
    aspect.ratio = 3 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16)
  )
#error message just from the limts, not a concern

advurpplot

#############################################################
#      4.b break even rp versus probability of advertising
###########################################################
# Create the buckets and calculate the averages
bucket_prob_rp <- data_int %>%
  mutate(bucket = cut(R_adjidf, breaks, include.lowest = TRUE)) %>%
  group_by(bucket) %>%
  summarise(
    prob_advp = 1 - mean(adv_inc, na.rm = TRUE),
    prob_adv = 1 - mean(report_inc, na.rm = TRUE),
    prob_diff = prob_advp - prob_adv

  )

# Create a numeric sequence for the x-axis
bucket_prob_rp$bucket_num <- seq_along(bucket_prob_rp$bucket)

# Create labels for the x-axis
int_lab <-
levels(bucket_prob_rp$bucket)[-c(1, length(levels(bucket_prob_rp$bucket)))]
labels <- c("< 0", int_lab, "> .5")

# Reshape the data to long format
bucket_prob_long_rp <- bucket_prob_rp %>%
  pivot_longer(cols =
                 c(prob_diff, prob_adv),
               names_to = "variable", values_to = "value")


# Plot the average exit_year for each bucket
adv_prob_rp_plot <-
ggplot(bucket_prob_long_rp, aes(x = bucket_num, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values =
    c("prob_diff" = "black",
      "prob_adv" = "steelblue"),
    labels =
    c("prob_diff" = "Reported Negligible",
      "prob_adv" = "Not Reported")
  ) +
  labs(
       x = "Risk Premium + Depreciation Indifferent",
       y = "Probability", fill = "Variable") +
  theme_minimal() +
  theme(
    aspect.ratio = 2 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = .5)
  ) +
  scale_x_continuous(breaks = bucket_prob$bucket_num, labels = labels) +
  guides(fill = guide_legend(title = "Advertising?")) +
  ylim(0, 1)

adv_prob_rp_plot






############################################################
############################################################
#     5: save plots
############################################################
############################################################

save_f(adv_prob_industry, "adv_prob_industry.pdf", dircs, 13.5, 6.75, TRUE)

save_f(advuccplot, "advucc_plot.pdf", dircs, 13.5, 6.75, TRUE)

save_f(adv_prob_plot, "adv_prob_plot.pdf", dircs, 13.5, 6.75, TRUE)


save_f(advurpplot, "advrp_plot.pdf", dircs, 13.5, 6.75, TRUE)

save_f(adv_prob_rp_plot, "adv_prob_rp_plot.pdf", dircs, 13.5, 6.75, TRUE)
