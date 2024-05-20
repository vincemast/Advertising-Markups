############################################################
############################################################
#0: Set working directories
############################################################
############################################################
cat("\014")
rm(list = setdiff(ls()))

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

deu_marks <- c("DEU_st.csv", "DEU_s.csv", "DEU_c.csv")
#select which version of DEU markups to use
use <- deu_marks[2]

#directories
dircs <- c(f_folder, d_folder, p_folder)

############################################################
############################################################
#1: clear all and load functions
############################################################
############################################################
cat("\014")
#variables to keep
keep_vars <- c("dircs", "save_files", "use")
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
source("function_usefull.R")

############################################################
############################################################
#     2: Load and clean data
############################################################
############################################################

############# 1.a load data ##############
#navigate to with data
setwd(dircs[2])

#load accounting data with DEU markups
dset <- read.csv(use) # nolint

#load usercost
Ucost <- read.csv("usercost.csv") # nolint

#generate variables (mainly need)
data <- vargen(dset, Ucost)
data <- laggen(data)

#keep only relevant columns (dont drop full might use later)
data_mu <-
  data %>% dplyr::select(MU, MU_deu, GVKEY, fyear, naics, industry, sale,
                         soc, weight, w_l, lead, MU_l, MU_deu_l)

############################################################
############################################################
#     2: Regressions
############################################################
############################################################




data_sector <- data %>%
  group_by(industry) %>%
  summarise(logMu = mean(log(MU), na.rm = TRUE),
    logMu_deu = mean(log(MU_deu), na.rm = TRUE),
    logtheta = mean(log(MU_deu) - log(soc), na.rm = TRUE),
    logsoc = mean(log(soc), na.rm = TRUE),
    logresid = mean(log(MU) - log(soc), na.rm = TRUE)
)


model_1 <- lm(logMu_deu ~ logsoc, data = data_sector)
model_2 <- lm(logMu_deu ~ logtheta, data = data_sector)
model_3 <- lm(logMu_deu ~ logresid, data = data_sector)
model_4 <- lm(logMu_deu ~ logsoc + logtheta + logresid, data = data_sector)

model_5 <- lm(logMu ~ logsoc, data = data_sector)
model_6 <- lm(logMu ~ logtheta, data = data_sector)
model_7 <- lm(logMu ~ logresid, data = data_sector)
model_8 <- lm(logMu ~ logsoc + logtheta + logresid, data = data_sector)

model_9 <- lm(logsoc ~ logtheta, data = data_sector)
model_10 <- lm(logsoc ~ logresid, data = data_sector)
model_11 <- lm(logsoc ~ logtheta + logresid, data = data_sector)

models <- list("ln(DEU Markup)" = model_1,
               "ln(DEU Markup)" = model_2,
               "ln(DEU Markup)" = model_3,
               "ln(DEU Markup)" = model_4,
               "ln(Cost Accounting Markup)" = model_5,
               "ln(Cost Accounting Markup)" = model_6,
               "ln(Cost Accounting Markup)" = model_7,
               "ln(Cost Accounting Markup)" = model_8,
               "ln(SALE over COGS)"  = model_9,
               "ln(SALE over COGS)"  = model_10,
               "ln(SALE over COGS)"  = model_11
)
models1 <- list("ln(DEU Markup)" = model_1,
               "..." = model_2,
               "..." = model_3,
               "..." = model_4,
               "ln(Cost Accounting Markup)" = model_5,
               "..." = model_6,
               "..." = model_7,
               "..." = model_8
)

models2 <- list(" "  = model_9,
               "ln(SALE over COGS)"  = model_10,
               " "  = model_11
)



# Define the mapping of raw coefficient names to clean names
coef_temp <- data.frame(
  raw = c("logsoc", "logtheta", "logresid"),
  clean = c("ln(SALE over COGS)", "ln(Theta)", "ln(COGS over COGS plus UCC)")
)

# Define the mapping of raw coefficient names to clean names
coef_temp <- c("(Intercept)" = "(Intercept)",
               "logsoc" = "ln(SALE over COGS)",
               "logtheta" = "ln(Theta)",
               "logresid" = "ln(COGS over COGS plus UCC)")


#select GOF measures
gm_temp <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "N", 0,
  "r.squared", "R2", 3,
  "adj.r.squared", "Adj. R2", 3,
)


# Create a summary table of the models
ms <- msummary(models, gof_map = gm_temp, stars = TRUE, coef_map = coef_temp,
  notes = c("")
)

ms

ms_latex <-
  msummary(models, gof_map = gm_temp, stars = TRUE, coef_map = coef_temp,
           output = "latex")

ms_latex

ms_latex1 <-
  msummary(models1, gof_map = gm_temp, stars = TRUE, coef_map = coef_temp,
           output = "latex")

ms_latex1

ms_latex2 <-
  msummary(models2, gof_map = gm_temp, stars = TRUE, coef_map = coef_temp,
           output = "latex")

ms_latex2


############################################################
############################################################
#     2: Scatter Plot
############################################################
############################################################

# Reshape the data
data_long <- data_sector %>%
  gather(key = "markup", value = "value", logMu, logMu_deu)

# Rename the markups
data_long$markup[data_long$markup == "logMu"] <- "ln(Cost Accounting Markup)"
data_long$markup[data_long$markup == "logMu_deu"] <- "ln(DEU Markup)"

# Create the scatter plot with OLS trend lines
soc_scatter <- ggplot(data_long, aes(x = logsoc, y = value, color = markup)) +
  geom_point(size = 3) +  # Adjust the size of the dots
  geom_smooth(method = "lm", se = FALSE) +  # Add OLS trend lines
  labs(x = "ln(SALE over COGS)", y = "Ln(Markup)") +
  theme_minimal() +
  theme(legend.position = "bottom",  # Put the legend at the bottom
        text = element_text(size = 14)) +  # Adjust the size of the text
  scale_color_manual(name = "Markup Type",
                     breaks = c("ln(Cost Accounting Markup)",
                                "ln(DEU Markup)"),
                     values = c("red", "blue"))  # Specify the colors

soc_scatter

save_f(soc_scatter, "soc_scatter.pdf", dircs, 11.5, 12, TRUE)