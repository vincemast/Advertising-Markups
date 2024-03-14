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
compu <- read.csv("COMPUSTAT_Exit.csv") # nolint
naics <- read.csv("2022_NAICS_Structure.csv")
colnames(naics) <- c("change", "naics_n", "industry")

#############################################################
#1.b clean data
###########################################################

#rename gvkey to GVKEY
colnames(compu)[colnames(compu) == "gvkey"] <- "GVKEY"

#check reasons for exit
hist(compu$dlrsn, xaxt = "n", xlab = "dlrsn", main = "")
axis(1, at = seq(1, 20, by = 1))
#mostly merger/ aquisition (1) and becoming private (9)

#add industry names following deu and literally using 2 digit naics
dset <- invisible(industry_n_dig_2(compu, 2))
#clean data
dset <- clean(dset)
#generate entry and exit variables
dset <- exit_gen(dset)

#select only variables we need
data <- dset %>%
  select(GVKEY, fyear, entry, exit, age, life, exit_ind, naics,
         sale, cogs, xsga, ppegt, xopr, dlrsn)

#probability of failure
summary(data$exit_ind)

#generate exit flag in words
data <- data %>%
  mutate(exit_yn = ifelse(exit_ind == 1, "Yes", "No"))

############################################################
############################################################
#     2: Exit analysis
############################################################
############################################################

#peak at firms with confirmed exit
#exit not na
confirm_exit <- data %>% filter(exit_ind == 1)

hist(confirm_exit$exit)
#############################################################
#2.a exit condition
###########################################################

#Say firms exit if PI<0 ie SALE-COGS-XSGA-(User cost of capital)*PPEGT<0
#ie (User cost of capital) > (SALE-COGS-XSGA)/PPEGT
#generate break even user cost of capital = (SALE-COGS-XSGA)/PPEGT
data <- data %>%
  mutate(break_even_ucc = (sale - cogs - xsga) / ppegt)

summary(data$break_even_ucc)


#############################################################
#2.b density
###########################################################


#density of break even user cost of capital given not exit
lower_limit <- quantile(data$break_even_ucc, 0.05)
upper_limit <- quantile(data$break_even_ucc, 0.95)

beucc_plot <-
  ggplot(data, aes(x = break_even_ucc, fill = factor(exit_yn))) +
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
#2.c break even user cost of capital versus years to exit
###########################################################


#trim based on user cost of capital
p <- 5
#get p% and 100-p% quantiles (by year and exit condition)
data_t <- data %>%
    group_by(fyear,exit_ind) %>% #nolint
    mutate(
      q1 = quantile(break_even_ucc, 0 +  p / 100, na.rm = TRUE), #nolint
      q99 = quantile(break_even_ucc, 1 - p / 100, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(break_even_ucc >= q1 & break_even_ucc <= q99) #nolint


#break even user cost of capital versus life
life_be <- data_t %>%
  group_by(life) %>%
  summarise(
    mean_break_even_ucc = mean(break_even_ucc, na.rm = TRUE),
    lower_ci = quantile(break_even_ucc, .05, na.rm = TRUE),
    upper_ci = quantile(break_even_ucc, .95, na.rm = TRUE)
  )

#keep only 30 years of life
life_be <- life_be %>%
  filter(life <= 30)

# Plot mean and confidence interval
years_exit_plot <- ggplot(life_be, aes(x = life, y = mean_break_even_ucc)) +
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
#2.d probability of exit vs break even user cost of capital
###########################################################

#need to think of a good way to do this, should probably put things into bucks
#and then plot the probability of exit in each buck
#is there a better way to do this?


#put into middel ranging from -.6 to .9
# Define the break points for the buckets
#include rounding bc it wants to put into scientific notation sometimes
int_breaks <- c(round(seq(0, 0.5, by = 0.025), 2))
breaks <- c(-Inf, int_breaks, Inf)

# Create the buckets and calculate the averages
bucket_prob <- data %>%
  mutate(bucket = cut(break_even_ucc, breaks, include.lowest = TRUE)) %>%
  group_by(bucket) %>%
  summarise(
    prob_exit = mean(exit_ind, na.rm = TRUE),
    avg_break_even_ucc = mean(break_even_ucc, na.rm = TRUE)
  )

# Create a numeric sequence for the x-axis
bucket_prob$bucket_num <- seq_along(bucket_prob$bucket)

# Create labels for the x-axis
int_lab <- levels(bucket_prob$bucket)[-c(1, length(levels(bucket_prob$bucket)))]
labels <- c("< 0", int_lab, "> 0.5")

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
                       angle = 70,
                       vjust = 0.5,
                       hjust = .5,
                       color = ifelse(bucket_prob$bucket_type != "neither",
                                      "red", "black"))) +
  scale_x_continuous(breaks = bucket_prob$bucket_num, labels = labels) +
  guides(fill = FALSE)

exit_prob_plot


############################################################
############################################################
#     3: save plots
############################################################
############################################################