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
library(purrr)


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
#navigate to folder with markup data
setwd(dircs[2])

######################## 1.a ###############################
#import
#compustat
dset <- read.csv("COMPUSTAT.csv") # nolint
#rename gvkey to GVKEY

#FRED DATA
usercost <- read.csv("usercost.csv") #nolint

#Markup meta data
meta <- read.csv("blp_data.csv") #nolint

######################## 1.b ###############################
#clean a tad

#Either take compustat and clean (done here) or:
#use DEU data (includes both markups)
#comment out line 86 to use this method

colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"
#with DEU markups generated
data <- read.csv("DEU_s.csv")

# comment out to clean differently
#wont allow deu markups
dset <- data

#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"
#apply GDP deflator and generate xad measures
dset <- VariableGen(dset, usercost)
#make a numericl variable of year (might be string by default)
dset <- dset %>%
  mutate(year = as.numeric(fyear)) %>% #nolint
  filter(!is.na(fyear)) #nolint


######################## 1.c ###############################
#create function to generate ac_mu
gen_ac_mu <- function(blp_c, data) {

  # grab naisc (if its a list its seperated by commas)
  codes <-  as.character(blp_c$naics)
  codes <-  as.numeric(unlist(strsplit(codes, ",")))
  i_digits <- as.numeric(blp_c$ind_digits)

  range <-
    as.numeric(blp_c$start):as.numeric(blp_c$end)

  #get data for relevant industry x year subset
  temp <- data %>%
    filter(substr(naics, 1, i_digits) %in% codes) %>% #nolint
    filter(year %in% range) #nolint

  # Check if MU_deu exists in the data (if not will just set to NA)
  if (!"MU_deu" %in% names(temp)) {
    temp$MU_deu <- NA
  }

  # change accounting markup to version in blp before aggregation
  # markup ratio := MU = p/c; markup := (p-c)/c = MU - 1;
  #learner: L = (p-c)/p = 1-1/(MU) nolint
  if (blp_c$measure == "markup") {
    temp <-  temp %>% mutate(mark = MU - 1, mark2 = MU_deu - 1) #nolint
  } else if (blp_c$measure == "ratio") {
     temp <-  temp %>% mutate(mark = MU, mark2 = MU_deu) #nolint
  } else if (blp_c$measure == "lerner") {
     temp <-  temp %>% mutate(mark = 1 - 1 / (MU), mark2 = 1 - 1 / (MU_deu)) #nolint
  } else {
    stop("Invalid measure. It should be 'markup', 'ratio', or 'lerner'.")
  }

  # Remove infinite values from temp$mark
  temp <- temp %>% filter(is.finite(mark)) #nolint
  temp2 <- temp %>% filter(is.finite(mark2)) #nolint

  #get relevant summary of accounting data based on agg
  if (blp_c$agg == "avg") {
    agg_mu <- mean(temp$mark, na.rm = TRUE)
    agg_mudeu <- mean(temp2$mark2, na.rm = TRUE)
  } else if (blp_c$agg == "median") {
    agg_mu <- median(temp$mark, na.rm = TRUE)
    agg_mudeu <- median(temp2$mark2, na.rm = TRUE)
  } else if (blp_c$agg == "sw") {
    agg_mu <- weighted.mean(temp$mark, w = temp$sale, na.rm = TRUE)
    agg_mudeu <- weighted.mean(temp2$mark2, w = temp2$sale, na.rm = TRUE)
  } else if (blp_c$agg == "cw") {
    agg_mu <- weighted.mean(temp$mark,
                            w = (temp$cogs + temp$ppegt * temp$usercost),
                            na.rm = TRUE)
    agg_mudeu <- weighted.mean(temp$mark2,
                               w = (temp2$cogs + temp2$ppegt * temp2$usercost),
                               na.rm = TRUE)
  } else {
    stop("Invalid value for agg. It should be 'avg', 'median', 'cw', or 'sw'.")
  }


  og_mu <- as.numeric(blp_c$mu..in.original.measure.)

  #save as markup (mu-1)
  #also change their measure to markup
  # L = (p-c)/p = 1-1/(mu) #nolint
  # 1/(mu) = 1-L#nolint 
  # mu = 1/(1-l) nolint
  if (blp_c$measure == "markup") {
    blp_c$ca_mu <- agg_mu
    blp_c$deu_mu <- agg_mudeu
    blp_c$mu <- og_mu
  } else if (blp_c$measure == "ratio") {
   blp_c$ca_mu <- agg_mu - 1 #nolint
   blp_c$mu <- og_mu - 1
   blp_c$deu_mu <- agg_mudeu - 1
  } else if (blp_c$measure == "lerner") {
   blp_c$ca_mu <- 1 / (1 - agg_mu) #nolint
   blp_c$mu <- 1 / (1 - og_mu)
   blp_c$deu_mu  <- 1 / (1 - agg_mudeu)
  } else {
    stop("Invalid value for measure. It should be 'ratio' or 'lerner'.")
  }

  # print
  data.frame(blp_c)

}

############################################################
############################################################
#     2: Generate DEU and CA comparison MUs
############################################################
############################################################

# Apply the gen_ac_mu function to each row of the blp data frame
blp <- map_df(1:nrow(meta), ~gen_ac_mu(meta[.x, ], dset)) #nolint

#remove any NAs in ca_mu (also correspond to na in deu_mu)
blp <- blp %>% filter(!is.na(ca_mu)) #nolint

summary(blp)

cor(blp$deu_mu, blp$mu)
plot(blp$deu_mu, blp$mu)


cor(blp$ca_mu, blp$mu)
plot(blp$ca_mu, blp$mu)

############################################################
############################################################
#     3: Make Nice table
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
table <- blp_r %>%
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
#     4: Plot Data
############################################################
############################################################

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
  xlab("Accounting Markup (p-c)/c") +
  ylab("Demand Estimation Markup (p-c)/c") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 10, byrow = TRUE, order = 1),
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
  xlab("DEU (2020) Markup  (p-c)/c") +
  ylab("Demand Estimation Markup  (p-c)/c") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 10, byrow = TRUE, order = 1),
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
model2 <- plm(mu ~ ca_mu + as.factor(method) - 1, data = blp,
              index = "paper", model = "pooling")
model2_deu <- plm(mu ~ deu_mu + as.factor(method) - 1, data = blp,
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
         notes = c("Standard errors in parentheses. Clustered by paper."),
         output = "latex")