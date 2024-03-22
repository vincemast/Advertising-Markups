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

#generate entry and exit variables
dset <- exit_vars(dset)

dset <- entry_vars(dset)

#clean data
dset <- clean(dset)

#select only variables we need
data <- dset %>%
  select(GVKEY, fyear, entry, exit, age, life, exit_flag, naics,
         sale, cogs, xsga, ppegt, xopr, dlrsn, industry)

#probability of failure
summary(data$exit_flag)

#generate exit flag in words
data <- data %>%
  mutate(exit_yn = case_when(
    exit_flag == 1 ~ "Yes",
    exit_flag == 0 ~ "No",
    is.na(exit_flag) ~ NA_character_
  ))

############################################################
############################################################
#     2: Exit analsysis start
############################################################
############################################################

#############################################################
#2.a histograms
###########################################################
par(pin = c(8, 8))

dlrsn_mapping <- setNames(
  c("Acquisition or merger",
    "Bankruptcy",
    "Liquidation",
    "Reverse acquisition",
    "No longer fits original format",
    "Leveraged buyout",
    "Other, but pricing continues",
    "Now a private company",
    "Other",
    "No successor settlement code was established.",
    "A successor settlement code was established.",
    "Issue now identified under different SEDOL.",
    "Successor settlement code was established.",
    "Company Active",
    "Not available"),
  c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 20, NA)
)

# Replace dlrsn codes with descriptions
data <- data %>%
  mutate(dlrsn = dlrsn_mapping[as.character(dlrsn)])

# Create bar plots
xit_reasons <- data %>%
  distinct(GVKEY, .keep_all = TRUE) %>%
  pull(dlrsn) %>%
  factor() %>%
  table()


#navigate to folder with images
#setwd(dircs[3])
# Start PDF device


#pdf(file = "exit_reasons.pdf", width = 10, height = 8)

# Increase bottom margin
#par(mar = c(12, 4, 4, 2) + 0.1)

barplot(
  xit_reasons,
  main = "Reason for Exit (As reported by Compustat)",
  cex.axis = 1,
  cex.lab = 1.2,
  las = 2
)

#dev.off()



# Check reasons for exit conditional on marked as exit
exit_reasons_conditional <- data %>%
  filter(exit_flag == 1) %>%
  pull(dlrsn) %>%
  factor() %>%
  table()



#pdf(file = "exit_reasons_conditional.pdf", width = 10, height = 8)

# Increase bottom margin
#par(mar = c(12, 4, 4, 2) + 0.1)

barplot(
  exit_reasons_conditional,
  main = "Reason Listed for Exit Among Firms (Conditional on Bankruptcy)",
  cex.axis = 1,
  cex.lab = 1.2,
  las = 2
)

#dev.off()









#pdf(file = "exit_year.pdf", width = 10, height = 8)

# Increase bottom margin
#par(mar = c(4, 4, 4, 2) + 0.1)
barplot(exit_years,
        main = "Year of Exit",
        cex.axis = 1,
        cex.lab = 1.2,
        las = 2)

#dev.off()




#industry exit probability
exit_prob_sec <- data %>%
  filter(!is.na(exit_flag)) %>%
    group_by(industry) %>% #nolint
    summarise(
      prob_exit = mean(exit_flag, na.rm = TRUE)
    )



#pdf(file = "exit_prob_industry.pdf", width = 10, height = 8)

# Increase bottom margin
#par(mar = c(4, 4, 4, 2) + 0.1)
barplot(exit_prob_sec$prob_exit,
        names.arg = exit_prob_sec$industry,
        main = "Exit Probability by Industry",
        cex.axis = 1,
        cex.lab = 1.2,
        las = 2)
#dev.off()


#############################################################
#2.b exit condition
###########################################################

#Say firms exit if PI<0 ie SALE-COGS-XSGA-(User cost of capital)*PPEGT<0
#ie (User cost of capital) > (SALE-COGS-XSGA)/PPEGT
#generate break even user cost of capital = (SALE-COGS-XSGA)/PPEGT
data <- data %>%
  mutate(break_even_ucc = (sale - cogs - xsga) / ppegt)

summary(data$break_even_ucc)


#but some elements of the user cost of capital vary over time
#user cost of capital = r + delta + risk premium
#r = real interest rate
#call adj risk premium: + delta + risk premium
#break even adj risk premium  = user cost of capital - r


#get real interest rate
macro_vars <- macro %>%
  mutate(real_int = (FEDFUNDS - Ifl) * .01)
macro_vars <- macro_vars %>%
  select(fyear, real_int)

#merge with data by fyear
data_int <- merge(data, macro_vars, by = "fyear")

data_int <- data_int %>%
  mutate(break_even_rp = break_even_ucc - real_int)

summary(data_int$break_even_rp)

############################################################
############################################################
#     3 Break even user cost of capital
############################################################
############################################################

#############################################################
#3.a density
###########################################################

#density of break even user cost of capital given not exit
lower_limit <- quantile(data$break_even_ucc, 0.075)
upper_limit <- quantile(data$break_even_ucc, 0.925)

#cut out the NAs
data_filtered <- data %>%
  filter(!is.na(exit_yn))

beucc_plot <-
  ggplot(data_filtered, aes(x = break_even_ucc, fill = factor(exit_yn))) +
  geom_density(alpha = 0.5) +
  labs(
       x = "Break even user cost of capital",
       y = "Density",
       fill = "Exit Year") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  xlim(lower_limit, upper_limit) +
  theme(
    aspect.ratio = 3 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16)
  )

beucc_plot


#############################################################
#3.b break even user cost of capital versus years to exit
###########################################################


#break even user cost of capital versus life
life_be <- data %>%
  group_by(life) %>%
  summarise(
    mean_break_even_ucc = mean(break_even_ucc, na.rm = TRUE),
    lower_ci = quantile(break_even_ucc, .05, na.rm = TRUE),
    upper_ci = quantile(break_even_ucc, .95, na.rm = TRUE)
  )

#keep only 25 years of life
life_be <- life_be %>%
  filter(life <= 25)

# Plot mean and confidence interval
years_exit_plot <- ggplot(life_be, aes(x = -life, y = mean_break_even_ucc)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(x = "Years To Exit",
       y = "Break-Even User Cost of Capital") +
  theme_minimal() +
  theme(
    aspect.ratio = 2 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16)
  )

#looks much worse if dont trim

years_exit_plot


#############################################################
#3.c probability of exit vs break even user cost of capital
###########################################################

# Define the break points for the buckets
#include rounding bc it wants to put into scientific notation sometimes
int_breaks <- c(round(seq(-.2, 0.6, by = 0.02), 2))
breaks <- c(-Inf, int_breaks, Inf)

# Create the buckets and calculate the averages
bucket_prob <- data %>%
  mutate(bucket = cut(break_even_ucc, breaks, include.lowest = TRUE)) %>%
  group_by(bucket) %>%
  summarise(
    prob_exit = mean(exit_flag, na.rm = TRUE),
    avg_break_even_ucc = mean(break_even_ucc, na.rm = TRUE)
  )

# Create a numeric sequence for the x-axis
bucket_prob$bucket_num <- seq_along(bucket_prob$bucket)

# Create labels for the x-axis
int_lab <- levels(bucket_prob$bucket)[-c(1, length(levels(bucket_prob$bucket)))]
labels <- c("< -.2", int_lab, "> 0.6")

# Add a new variable to indicate the first, last, or neither bucket
#give outter buckets own color
bucket_prob <- bucket_prob %>%
  mutate(bucket_type = case_when(
    bucket_num == 1 ~ "first",
    bucket_num == length(bucket_num) ~ "last",
    TRUE ~ "neither"
  ))

# Plot the average exit_year for each bucket
exit_prob_plot <-
  ggplot(bucket_prob, aes(x = bucket_num, y = prob_exit, fill = bucket_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
                    values = c("first" = "red",
                               "last" = "red", "neither" = "steelblue")) +
  labs(x = "Bucket",
       y = "Probability of Exit") +
  theme_minimal() +
  theme(aspect.ratio = 2 / 4,
        text = element_text(size = 14),
        title = element_text(size = 16),
        axis.text.x
        = element_text(
                       angle = 60,
                       vjust = 0.5,
                       hjust = .5,
                       color = ifelse(bucket_prob$bucket_type != "neither",
                                      "red", "black"))) +
  scale_x_continuous(breaks = bucket_prob$bucket_num, labels = labels) +
  guides(fill = FALSE)

exit_prob_plot

############################################################
############################################################
#     4: Exit analsysis with real int
############################################################
############################################################

#############################################################
#         4.a density
###########################################################


#density of break even user cost of capital given not exit
lower_limit <- quantile(data_int$break_even_rp, 0.075)
upper_limit <- quantile(data_int$break_even_rp, 0.925)

#cut out the NAs
data_filtered <- data_int %>%
  filter(!is.na(exit_yn))

beurp_plot <-
  ggplot(data_filtered, aes(x = break_even_rp, fill = factor(exit_yn))) +
  geom_density(alpha = 0.5) +
  labs(
       x = "Break even Net Premium",
       y = "Density",
       fill = "Exit Year") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  xlim(lower_limit, upper_limit) +
  theme(
    aspect.ratio = 3 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16)
  )

beurp_plot

#############################################################
#      4.b break even rp versus years to exit
###########################################################


#break even user cost of capital versus life
life_be <- data_int %>%
  group_by(life) %>%
  summarise(
    mean_break_even_rp = mean(break_even_rp, na.rm = TRUE),
    lower_ci = quantile(break_even_rp, .05, na.rm = TRUE),
    upper_ci = quantile(break_even_rp, .95, na.rm = TRUE)
  )

#keep only 25 years of life
life_be <- life_be %>%
  filter(life <= 25)

# Plot mean and confidence interval
years_exit_plot_rp <- ggplot(life_be, aes(x = -life, y = mean_break_even_rp)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(x = "Years To Exit",
       y = "Break-Even Net Premium") +
  theme_minimal() +
  theme(
    aspect.ratio = 2 / 4,
    text = element_text(size = 14),
    title = element_text(size = 16)
  )

#looks much worse if dont trim

years_exit_plot_rp



#############################################################
#     4.c probability of exit vs break even rp
###########################################################

# Define the break points for the buckets
#include rounding bc it wants to put into scientific notation sometimes
int_breaks <- c(round(seq(-.2, 0.6, by = 0.02), 2))
breaks <- c(-Inf, int_breaks, Inf)

# Create the buckets and calculate the averages
bucket_prob <- data_int %>%
  mutate(bucket = cut(break_even_rp, breaks, include.lowest = TRUE)) %>%
  group_by(bucket) %>%
  summarise(
    prob_exit = mean(exit_flag, na.rm = TRUE),
    avg_break_even_rp = mean(break_even_rp, na.rm = TRUE)
  )

# Create a numeric sequence for the x-axis
bucket_prob$bucket_num <- seq_along(bucket_prob$bucket)

# Create labels for the x-axis
int_lab <- levels(bucket_prob$bucket)[-c(1, length(levels(bucket_prob$bucket)))]
labels <- c("< -.2", int_lab, "> 0.6")

# Add a new variable to indicate the first, last, or neither bucket
#give outter buckets own color
bucket_prob <- bucket_prob %>%
  mutate(bucket_type = case_when(
    bucket_num == 1 ~ "first",
    bucket_num == length(bucket_num) ~ "last",
    TRUE ~ "neither"
  ))

# Plot the average exit_year for each bucket
exit_prob_plot_rp <-
  ggplot(bucket_prob, aes(x = bucket_num, y = prob_exit, fill = bucket_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
                    values = c("first" = "red",
                               "last" = "red", "neither" = "steelblue")) +
  labs(x = "Bucket (Net Premium)",
       y = "Probability of Exit") +
  theme_minimal() +
  theme(aspect.ratio = 2 / 4,
        text = element_text(size = 14),
        title = element_text(size = 16),
        axis.text.x
        = element_text(
                       angle = 60,
                       vjust = 0.5,
                       hjust = .5,
                       color = ifelse(bucket_prob$bucket_type != "neither",
                                      "red", "black"))) +
  scale_x_continuous(breaks = bucket_prob$bucket_num, labels = labels) +
  guides(fill = FALSE)

exit_prob_plot_rp




############################################################
############################################################
#     5: save plots
############################################################
############################################################




save_f(beucc_plot, "beucc_plot.pdf", dircs, 13.5, 6.75, TRUE)

save_f(years_exit_plot, "years_exit_plot.pdf", dircs, 13.5, 6.75, TRUE)

save_f(exit_prob_plot, "exit_prob_plot.pdf", dircs,13.5, 6.75, TRUE)


save_f(beurp_plot, "beurp_plot.pdf", dircs, 13.5, 6.75, TRUE)

save_f(years_exit_plot_rp, "years_exit_plot_rp.pdf", dircs, 13.5, 6.75, TRUE)

save_f(exit_prob_plot_rp, "exit_prob_plot_rp.pdf", dircs, 13.5, 6.75, TRUE)

