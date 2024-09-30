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
library(grid)


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
data <- read.csv("DEU_st.csv")

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

names(data)


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
    temp <-  temp %>% mutate(mark = MU - 1, #nolint
                             mark2 = MU_deu - 1, #nolint
                             soc = sale / cogs - 1) #nolint
  } else if (blp_c$measure == "ratio") {
     temp <-  temp %>% mutate(mark = MU, #nolint
                              mark2 = MU_deu, #nolint
                              soc = sale / cogs) #nolint
  } else if (blp_c$measure == "lerner") {
     temp <-  temp %>% mutate(mark = 1 - (MU)^(-1), #nolint
                             mark2 = 1 - MU_deu^(-1), #nolint
                              soc = 1 - (cogs / sale) ) #nolint
  } else {
    stop("Invalid measure. It should be 'markup', 'ratio', or 'lerner'.")
  }

  # Remove infinite values from temp$mark
  temp <- temp %>% filter(is.finite(mark)) #nolint
  temp <- temp %>% filter(is.finite(mark2)) #nolint
  temp <- temp %>% filter(is.finite(soc)) #nolint

  #get relevant summary of accounting data based on agg
  if (blp_c$agg == "avg") {
    agg_mu <- mean(temp$mark, na.rm = TRUE)
    agg_mudeu <- mean(temp$mark2, na.rm = TRUE)
    agg_soc <- mean(temp$soc, na.rm = TRUE)
  } else if (blp_c$agg == "median") {
    agg_mu <- median(temp$mark, na.rm = TRUE)
    agg_mudeu <- median(temp$mark2, na.rm = TRUE)
    agg_soc <- median(temp$soc, na.rm = TRUE)
  } else if (blp_c$agg == "sw") {
    agg_mu <- weighted.mean(temp$mark, w = temp$sale, na.rm = TRUE)
    agg_mudeu <- weighted.mean(temp$mark2, w = temp$sale, na.rm = TRUE)
    agg_soc <- weighted.mean(temp$soc, w = temp$sale, na.rm = TRUE)
  } else if (blp_c$agg == "cw") {
    agg_mu <- weighted.mean(temp$mark,
                            w = (temp$cogs + temp$ppegt * temp$usercost),
                            na.rm = TRUE)
    agg_mudeu <- weighted.mean(temp$mark2,
                               w = (temp$cogs + temp$ppegt * temp$usercost),
                               na.rm = TRUE)
    agg_soc <- weighted.mean(temp$soc,
                             w = (temp$cogs + temp$ppegt * temp$usercost),
                             na.rm = TRUE)
  } else {
    stop("Invalid value for agg. It should be 'avg', 'median', 'cw', or 'sw'.")
  }


  og_mu <- as.numeric(blp_c$mu..in.original.measure.)

  #save as markup mu
  #also change their measure to markup
  # L = (p-c)/p = 1-1/(mu) #nolint
  # 1/(mu) = 1-L #nolint 
  # mu = 1/(1-l) nolint
  if (blp_c$measure == "markup") {
    blp_c$ca_mu <- agg_mu + 1
    blp_c$deu_mu <- agg_mudeu + 1
    blp_c$mu <- og_mu + 1
    blp_c$soc <- agg_soc + 1
  } else if (blp_c$measure == "ratio") {
   blp_c$ca_mu <- agg_mu #nolint
   blp_c$mu <- og_mu
   blp_c$deu_mu <- agg_mudeu
   blp_c$soc <- agg_soc
  } else if (blp_c$measure == "lerner") {
   blp_c$ca_mu <- (1 - agg_mu)^(-1) #nolint
   blp_c$mu <- (1 - og_mu)^(-1)
   blp_c$deu_mu  <- (1 - agg_mudeu)^(-1)
   blp_c$soc <- (1 - agg_soc)^(-1)
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

blp$error = abs(blp$mu - 1/2*(blp$ca_mu + blp$deu_mu))

view(blp)

summary(blp)

names(blp)

plot(log(blp$ca_mu), log(blp$mu))
plot(log(blp$deu_mu), log(blp$mu))

cor(log(blp$ca_mu), log(blp$mu))^2
cor(log(blp$deu_mu), log(blp$mu))^2

plot(blp$ca_mu, blp$mu)
cor(blp$deu_mu, blp$mu)


blp_nl <- blp %>% filter(measure != "lerner") #nolint

blp_l <- blp %>% filter(measure == "lerner") #nolint

plot(log(blp_nl$ca_mu), log(blp_nl$mu))

cor(log(blp_nl$ca_mu), log(blp_nl$mu))^2

cor(log(blp_nl$deu_mu), log(blp_nl$mu))^2

plot(log(blp_nl$deu_mu), log(blp_nl$mu))


plot(log(blp$ca_mu), log(blp$mu))

plot(log(blp$deu_mu), log(blp$mu))



cor(log(blp$ca_mu), log(blp$mu))

cor(log(blp$deu_mu), log(blp$mu))


plot(log(blp$deu_mu), log(blp$ca_mu))

#filter out airlines
blp_lna <- blp_l %>% filter(industry != "Airline") #nolint

plot(blp_lna$deu_mu, blp_lna$mu)

plot(blp_lna$ca_mu, blp_lna$mu)


plot(log(blp_l$deu_mu), log(blp_l$mu))
plot(log(blp_l$ca_mu), log(blp_l$mu))

lm(log(mu) ~ log(deu_mu)*i(measure) - 1, data = blp)

lm(log(mu) ~ log(ca_mu)*i(measure) - 1, data = blp)

hold <- lm(log(mu) ~  log(ca_mu) + i(measure) + i(industry) - 1, data = blp)

summary(hold)

blp$at <- .5*(as.numeric(blp$start) + as.numeric(blp$start))

hold <- feols(log(ca_mu) ~  log(mu) + i(agg) + i(measure), data = blp)

hold2 <- feols(log(deu_mu) ~  log(mu) + i(agg) + i(measure), data = blp)

etable(list( hold, hold2))

lm(log(ca_mu) ~ log(mu) + i(measure) + i(method) - 1, data = blp)


cor(log(blp_lna$ca_mu), log(blp_lna$mu))
cor(log(blp_lna$deu_mu), log(blp_lna$mu))

summary(blp)

ggplot() +
  geom_density(aes(x = blp$mu, color = "mu"), size = 1) +
  geom_density(aes(x = blp$ca_mu, color = "ca_mu"), size = 1) +
  geom_density(aes(x = blp$deu_mu, color = "deu_mu"), size = 1) +
  labs(x = "Value", y = "Density", title = "Density Plots") +
  scale_color_manual(name = "Variable", values = c("mu" = "blue", "ca_mu" = "red", "deu_mu" = "green")) +
  theme_minimal() +
  theme(text = element_text(size = 16), legend.position = "bottom")

#plot average by at
ggplot(blp) +
  geom_smooth(aes(x = at, y = mu), method = "lm", se = FALSE) +
  geom_smooth(aes(x = at, y = ca_mu), method = "lm", se = FALSE) +
  geom_smooth(aes(x = at, y = deu_mu), method = "lm", se = FALSE) +
  labs(x = "Year", y = "Markup", title = "Markup over Time") +
  theme_minimal() +
  theme(text = element_text(size = 16))



hold4 <- feols(log(mu) ~  at | industry, data = blp)

hold5 <- feols(log(deu_mu) ~  at | industry, data = blp)

hold6 <- feols(log(ca_mu) ~  at | industry, data = blp)

hold7 <- feols(log(mu) ~  at | industry + method, data = blp)

hold8 <- feols(log(deu_mu) ~  at | industry + method, data = blp)

hold9 <- feols(log(ca_mu) ~  at | industry + method, data = blp)

hold10 <- feols(log(mu) ~  at | industry + method + agg, data = blp)

hold11 <- feols(log(deu_mu) ~  at | industry + method + agg, data = blp)

hold12 <- feols(log(ca_mu) ~  at | industry + method + agg, data = blp)

hold13 <- feols(log(mu) ~  at | industry + method + agg + paper, data = blp)

hold14 <- feols(log(deu_mu) ~  at | industry + method + agg + paper, data = blp)

hold15 <- feols(log(ca_mu) ~  at | industry + method + agg + paper, data = blp)

 

etable(list(hold4, hold5, hold6, hold7, hold8,
            hold9, hold10, hold11, hold12, hold13,
            hold14, hold15))


#filter out large
blp_l2 <- blp_l %>% filter(mu < 2.5) #nolint

cor(log(blp_l2$ca_mu), log(blp_l2$mu))^2

plot(blp_l2$ca_mu, blp_l2$mu)

cor(log(blp_l2$deu_mu), log(blp_l2$mu))^2




plot(blp_l$ca_mu, blp_l$mu)

cor(blp_l$ca_mu, blp_l$mu)

cor(log(blp_l$ca_mu), log(blp_l$mu))

cor(log(blp_l$deu_mu), log(blp_l$mu))

plot(log(blp_l$deu_mu), log(blp_l$mu))




cor(log(blp$deu_mu), log(blp$mu))^2
cor(blp$deu_mu, blp$mu)

cor(blp$ca_mu, blp$mu)

plot(log(blp$deu_mu), log(blp$mu))

plot(log(blp$soc), log(blp$mu))

cor(log(blp$soc), log(blp$mu))^2

plot(log(blp$ca_mu), log(blp$mu))


cor(log(blp$ca_mu), log(blp$mu))^2
plot(blp$ca_mu, blp$mu)


mean(blp$mu)
mean(blp$deu_mu)
mean(blp$ca_mu)

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
      agg == "cw" ~ "Cost-weighted average",
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
    Paper == "Pfander (2024)" ~ "Aggregate",
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
                     align = c(">{\\raggedright\\arraybackslash}p{4.5cm}",
                               ">{\\raggedright\\arraybackslash}p{3cm}",
                               ">{\\raggedright\\arraybackslash}p{7cm}",
                               ">{\\raggedright\\arraybackslash}p{3cm}",
                               ">{\\raggedright\\arraybackslash}p{3cm}",
                               ">{\\raggedright\\arraybackslash}p{14cm}",
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
view(blp)
# remove nas
blpn <- blp %>% filter(ca_mu > 0) #nolint
blpn <- blpn %>% filter(deu_mu > 0) #nolint
blpn <- blpn %>% filter(mu > 0) #nolint
################################################################
#### 4.a ca on mu
################################################################

# Calculate ols lines and correlation coef
model_ca <- lm(log(mu) ~ log(ca_mu), data = blpn)
s_mu_ca <- model_ca$coefficients[2]
i_mu_ca <- model_ca$coefficients[1]
model_car <- lm(log(ca_mu) ~ log(mu), data = blpn)
s_ca_mu <- model_car$coefficients[2]
i_ca_mu <- model_car$coefficients[1]

correlation <- cor(log(blpn$ca_mu), log(blpn$mu), use = "pairwise.complete.obs")
# Create the label for the legend
label <- paste0("Correlation Coef. = ", round(correlation, 2),
                ", N. obs =", nrow(blpn))

# Get the unique levels in the paper and method and save as shapes and colors
shapes <- c(15 + seq(0, length(unique(blpn$method)) - 2), 1)
seq(0, length(unique(blpn$method)) - 1) + 30
palette <- rainbow(length(unique(blpn$paper)))

# Create a dummy data frame for the 45-degree line
df_line <- data.frame(ca_mu =  range(c(log(blpn$mu), log(blpn$ca_mu))),
                      mu = range(c(log(blpn$mu), log(blpn$ca_mu))))

############## plot #####################
comp_ca <- ggplot(blpn, aes(x = log(ca_mu), y = log(mu))) +
  geom_point(aes(color = paper, shape = method), size = 4) +
  geom_abline(slope = s_mu_ca,
              intercept = i_mu_ca,
              color = "blue") +
  geom_abline(slope = 1 / s_ca_mu,
              intercept = - i_ca_mu / s_ca_mu,
              color = "red") +
  geom_line(data = df_line, color = "black",
            aes(x = mu, y = mu,
                linetype = "45 Degree line")) +
  geom_line(aes(linetype = "OLS line (Main)"),
            data = data.frame(ca_mu = 0, mu = 0),
            color = "blue") +
  geom_line(aes(linetype = "OLS line (Reverse)"),
            data = data.frame(ca_mu = 0, mu = 0),
            color = "red") +
  scale_linetype_manual(
         values = c("OLS line (Main)" = "solid", # nolint
                    "OLS line (Reverse)" = "solid",
                    "45 Degree line" = "solid")) +
  labs(
    color = "Paper",
    shape = "Method",
    linetype = label
  ) +
  theme(
    text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  coord_cartesian(
    xlim = c(log(min(blpn$ca_mu, blp$mu)), log(max(blpn$ca_mu, blp$mu))),  #nolint
    ylim = c(log(min(blpn$ca_mu, blp$mu)), log(max(blpn$ca_mu, blp$mu)))  #nolint
  ) +
  xlab("Log Accounting Markup (p-mc)/mc") +
  ylab("Log Demand Estimation Markup (p-mc)/mc") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 10, byrow = TRUE, order = 1),
    shape = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3,
    override.aes = list(color = c("black", "blue", "red"))) #nolint
  ) +
  guides(color = "none")  # Remove the color part of the legend

comp_ca

save_f(comp_ca, "BLP_comp_CA.pdf", dircs, 11.5, 12, TRUE)



################################################################
#### 4.b deu on mu
################################################################

# Calculate ols lines and correlation coef
model_deu <- lm(log(mu) ~ log(deu_mu), data = blpn)
s_mu_deu <- model_deu$coefficients[2]
i_mu_deu <- model_deu$coefficients[1]
model_deur <- lm(log(deu_mu) ~ log(mu), data = blpn)
s_deu_mu <- model_deur$coefficients[2]
i_deu_mu <- model_deur$coefficients[1]

correlation <- cor(log(blpn$deu_mu),
                   log(blpn$mu), use = "pairwise.complete.obs")
# Create the label for the legend
labeldeu <- paste0("Correlation Coef. = ", round(correlation, 2),
                   ", N. obs =", nrow(blpn))
# Create a dummy data frame for the 45-degree line
df_line <- data.frame(ca_mu =  range(c(log(blpn$mu), log(blpn$deu_mu))),
                      mu = range(c(log(blpn$mu), log(blpn$deu_mu))))

############## plot #####################
comp_deu <- ggplot(blpn, aes(x = log(deu_mu), y = log(mu))) +
  geom_point(aes(color = paper, shape = method), size = 4) +
  geom_abline(slope = s_deu_mu,
              intercept = i_mu_deu,
              color = "blue") +
  geom_abline(slope = 1 / s_deu_mu,
              intercept = - i_deu_mu / s_deu_mu,
              color = "red") +
  geom_line(data = df_line, color = "black",
            aes(x = mu, y = mu,
                linetype = "45 Degree line")) +
  geom_line(aes(linetype = "OLS line (Main)"),
            data = data.frame(deu_mu = 0, mu = 0),
            color = "blue") +
  geom_line(aes(linetype = "OLS line (Reverse)"),
            data = data.frame(deu_mu = 0, mu = 0),
            color = "red") +
  scale_linetype_manual(
         values = c("OLS line (Main)" = "solid", # nolint
                    "OLS line (Reverse)" = "solid",
                    "45 Degree line" = "solid")) +
  labs(
    color = "Paper",
    shape = "Method",
    linetype = labeldeu
  ) +
  theme(
    text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  coord_cartesian(
    xlim = c(log(min(blpn$deu_mu, blp$mu)),
             log(max(blpn$deu_mu, blp$mu))),  #nolint
    ylim = c(log(min(blpn$deu_mu, blp$mu)),
             log(max(blpn$deu_mu, blp$mu)))  #nolin
  ) +
  xlab("Log Production Function Markup  (p-mc)/mc") +
  ylab("Log Demand Estimation Markup (p-mc)/mc") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 10, byrow = TRUE, order = 1),
    shape = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3,
    override.aes = list(color = c("black", "blue", "red"))) #nolint
  ) +
  guides(color = "none")  # Remove the color part of the legend

comp_deu
save_f(comp_deu, "BLP_comp_DEU.pdf", dircs, 11.5, 12, TRUE)



################################################################
#### 4.a ca on deu
################################################################
#### ca on deu_mu

# Calculate ols lines and correlation coef
model_cadeu <- lm(deu_mu ~ ca_mu, data = blp)
s_mudeu_ca <- model_cadeu$coefficients[2]
i_mudeu_ca <- model_cadeu$coefficients[1]
model_cadeur <- lm(ca_mu ~ deu_mu, data = blp)
s_cadeu_mu <- model_cadeur$coefficients[2]
i_cadeu_mu <- model_cadeur$coefficients[1]

correlation <- cor(blp$ca_mu, blp$deu_mu, use = "pairwise.complete.obs")
# Create the label for the legend
label <- paste0("Correlation Coef. = ", round(correlation, 2),
                ", N. obs =", nrow(blp))

# Create a dummy data frame for the 45-degree line
df_line <- data.frame(ca_mu = range(blp$ca_mu), deu_mu = range(blp$deu_mu))

############## plot #####################
comp_cadeu <- ggplot(blp, aes(x = ca_mu, y = deu_mu)) +
  geom_point(size = 4) +
  geom_abline(slope = s_mudeu_ca,
              intercept = i_mudeu_ca,
              color = "blue") +
  geom_abline(slope = 1 / s_cadeu_mu,
              intercept = - i_cadeu_mu / s_cadeu_mu,
              color = "red") +
  geom_line(data = df_line, color = "black",
            aes(x = deu_mu,
                linetype = "45 Degree line")) +
  geom_line(aes(linetype = "OLS line (Main)"),
            data = data.frame(ca_mu = 0, deu_mu = 0),
            color = "blue") +
  geom_line(aes(linetype = "OLS line (Reverse)"),
            data = data.frame(ca_mu = 0, deu_mu = 0),
            color = "red") +
  scale_linetype_manual(
         values = c("OLS line (Main)" = "solid", # nolint
                    "OLS line (Reverse)" = "solid",
                    "45 Degree line" = "solid")) +
  theme(
    text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  coord_cartesian(
    xlim = c(min(blp$ca_mu, blp$deu_mu), max(blp$ca_mu, blp$deu_mu)),  #nolint
    ylim = c(min(blp$ca_mu, blp$deu_mu), max(blp$ca_mu, blp$deu_mu))  #nolint
  ) +
  labs(linetype = labeldeu) +
  xlab("Accounting Markup (p-mc)/mc") +
  ylab("Production Function Markup (p-mc)/mc") +
  guides(
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3,
    override.aes = list(color = c("black", "blue", "red"))) #nolint
  ) +
  guides(color = "none")  # Remove the color part of the legend

comp_cadeu

save_f(comp_cadeu, "BLP_comp_CA_deu.pdf", dircs, 11.5, 12, TRUE)


############################################################
############################################################
#     5: Regression Table
############################################################
############################################################

# Fit the models with clustered standard errors
m_mu_ca <- feols(log(mu) ~ log(ca_mu), data = blpn,
                 cluster = "paper")
m_mu_deu <- feols(log(mu) ~ log(deu_mu), data = blpn,
                  cluster = "paper")
m_ca_mu <- feols(log(ca_mu) ~ log(mu), data = blpn,
                 cluster = "paper")
m_deu_mu <- feols(log(deu_mu) ~ log(mu), data = blpn,
                  cluster = "paper")
m_ca_deu <- feols(log(ca_mu) ~ log(deu_mu), data = blpn,
                  cluster = "paper")
m_deu_ca <- feols(log(deu_mu) ~ log(ca_mu), data = blpn,
                  cluster = "paper")


#put models into list
models <- list("Demand Markup" = m_mu_ca,
               "Demand Markup" = m_mu_deu,
               "Accounting Markup" = m_ca_mu,
               "Accounting Markup" = m_ca_deu,
               "DEU Markup" = m_deu_mu,
               "DEU Markup" = m_deu_ca)



# Create a named character vector of new names
new_names <- c(
  "log(mu)" = "$\\log\\left(mu_{_{IO}}\\right)$",
  "log(ca_mu)" = "$\\log\\left(mu_{_{CA}}\\right)",
  "log(deu_mu)" = "$\\log\\left(mu_{_{PF}}\\right)$",
  "log(paper)" = "Paper level"
)


coef_order <-
c("Constant", "CA", "PF", "IO")


# Create the summary table with the new names
summary_table <- etable(models, dict = new_names, order = coef_order)

view(summary_table)

# Create the summary table with the new names
l_table <- etable(models, dict = new_names, order = coef_order, tex = TRUE)

# Print the LaTeX-formatted summary table
print(l_table)



#r^2 ratio
summary(m_mu_ca)$sq.cor / summary(m_mu_deu)$sq.cor


############################################################
############################################################
################    Role of sales/ Cogs
############################################################
############################################################

# gen theta and mu resid
blpn <- blpn %>%
  mutate(theta = soc / deu_mu,
         mu_resid = soc / ca_mu)

view(blpn)


# Fit the models with clustered standard errors
m_mu_ca <- feols(log(mu) ~ log(ca_mu), data = blpn,
                 cluster = "paper")
m_mu_deu <- feols(log(mu) ~ log(deu_mu), data = blpn,
                  cluster = "paper")
m_ca_mu <- feols(log(ca_mu) ~ log(mu), data = blpn,
                 cluster = "paper")
m_deu_mu <- feols(log(deu_mu) ~ log(mu), data = blpn,
                  cluster = "paper")
m_ca_deu <- feols(log(ca_mu) ~ log(deu_mu), data = blpn,
                  cluster = "paper")
m_deu_ca <- feols(log(deu_mu) ~ log(ca_mu), data = blpn,
                  cluster = "paper")






############################################################
############################################################
############################################################
################    OLD BELOW
############################################################
############################################################
############################################################







































#select GOF measures
gm_temp <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "N", 0,
  "r.squared", "R2", 4,
)











# Fit the models with clustered standard errors
m_mu_ca <- plm(mu ~ ca_mu, data = blp,
               index = "paper", model = "pooling")
m_mu_deu <- plm(mu ~ deu_mu, data = blp,
                index = "paper", model = "pooling")
m_ca_mu <- plm(ca_mu ~ mu, data = blp,
               index = "paper", model = "pooling")
m_deu_mu <- plm(deu_mu ~ mu, data = blp,
                index = "paper", model = "pooling")
m_ca_deu <- plm(ca_mu ~ deu_mu, data = blp,
                index = "paper", model = "pooling")
m_deu_ca <- plm(deu_mu ~ ca_mu, data = blp,
                index = "paper", model = "pooling")


# Create a summary table of the models
ms <- msummary(models, gof_map = gm_temp, stars = TRUE, notes = c(
  "Right hand side variable listed above.
   Standard errors in parentheses. Clustered by paper."
))

# Print the model summary table with the notes
ms

# Export the summary table as a LaTeX file

ms_latexraw <-
  msummary(models, gof_map = gm_temp, stars = TRUE,
           notes = c("Right hand side variable listed above.
                     Standard errors in parentheses. Clustered by paper."),
           output = "latex")

ms_latex <-   gsub("ca\\_mu", "\\hline \\\\ Accounting Markup",
                   ms_latexraw, fixed = TRUE)

ms_latex <-   gsub("deu\\_mu", "\\hline \\\\ DEU Markup",
                   ms_latex, fixed = TRUE)

ms_latex <-   gsub("mu &", "\\hline \\\\ Demand Markup &",
                   ms_latex, fixed = TRUE)

ms_latex















































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
    formula = "x ~ y",
    method = "lm",
    se = FALSE,
    color = "green",
    fullrange = TRUE
  ) +
  geom_smooth(
    aes(linetype = label),  # Use the label for the OLS line
    method = "lm",
    se = FALSE,
    color = "black",
    fullrange = TRUE
  ) +
  geom_line(data = df_line,
            aes(x = mu, linetype = "45-degree line"), color = "black") +
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
    xlim = c(min(blp$ca_mu, blp$mu), max(blp$ca_mu, blp$mu)),  #nolint
    ylim = c(min(blp$ca_mu, blp$mu), max(blp$ca_mu, blp$mu))  #nolint
  ) +
  xlab("Accounting Markup (p-mc)/mc") +
  ylab("Demand Estimation Markup (p-mc)/mc") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 10, byrow = TRUE, order = 1),
    shape = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3)
  ) +
  guides(color = "none")  # Remove the color part of the legend

comp_ca 

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
    color = "black",
    fullrange = TRUE
  ) +
  geom_line(data = df_line2,
            aes(x = mu, linetype = "45-degree line"), color = "black") +
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
    xlim = c(min(blp$deu_mu, blp$mu), max(blp$deu_mu, blp$mu)),  #nolint
    ylim = c(min(blp$deu_mu, blp$mu), max(blp$deu_mu, blp$mu))  #nolint
  ) +
  xlab("DEU (2020) Markup  (p-mc)/mc") +
  ylab("Demand Estimation Markup  (p-mc)/mc") +
  scale_shape_manual(
    values = shapes,
    labels = unique(blp$method),
    breaks = unique(blp$method)
  ) +
  guides(
    color = guide_legend(nrow = 10, byrow = TRUE, order = 1),
    shape = guide_legend(nrow = 1, byrow = TRUE, order = 2),
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3)
  ) +
  guides(color = "none")  # Remove the color part of the legend

comp_deu


##################### paper legend ###################

dummy_plot <- ggplot(blp, aes(x = mu, y = mu)) +
  geom_point(aes(color = paper), size = 4) +
  labs(
    color = "Paper",
    linetype = ""
  ) +
  theme(
    text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  guides(
    color = guide_legend(nrow = 15, byrow = TRUE, order = 1),
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3)
  ) +
  coord_fixed(ratio = 1)

my_legend <- get_legend(dummy_plot)
as_ggplot(my_legend)

save_f(my_legend, "Paper_legend.pdf", dircs, 10, 5.5, TRUE)

############################################################
############################################################
#     5: regression table
############################################################
############################################################

# Fit the models with clustered standard errors
model_ca <- feols(mu ~ ca_mu,
                  cluster = "paper",
                  data = blp)

model_deu <- feols(mu ~ deu_mu,
                   cluster = "paper",
                   data = blp)



model_ca2 <- plm(mu ~ ca_mu + as.factor(method) - 1, data = blp,
              index = "paper", model = "pooling")
model_deu2 <- plm(mu ~ deu_mu + as.factor(method) - 1, data = blp,
                  index = "paper", model = "pooling")















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
    "r.squared", "R2", 4,
)

# Create a summary table of the models
ms <- msummary(models, gof_map = gm_temp, stars = TRUE, notes = c(
  "Standard errors in parentheses. Clustered by paper.",
  "BLP used as reference method."
))

# Print the model summary table with the notes
ms

# Export the summary table as a LaTeX file

ms_latex <-
  msummary(models, gof_map = gm_temp, stars = TRUE,
           notes = c("Standard errors in parentheses. Clustered by paper."),
           output = "latex")

ms_latex <-   gsub("FRAC", "\\hline & Method Controls \\\\ FRAC",
                   ms_latex, fixed = TRUE)



############################################################
############################################################
#     6: regression table other direction
############################################################
############################################################

# Fit the models with clustered standard errors
model_r <- plm(ca_mu ~ mu, data = blp,
               index = "paper", model = "pooling")
model_deu_r <- plm(deu_mu ~ mu, data = blp,
                   index = "paper", model = "pooling")
model2_r <- plm(ca_mu ~ mu + as.factor(method) - 1, data = blp,
                index = "paper", model = "pooling")
model2_deu_r <- plm(deu_mu ~ mu + as.factor(method) - 1, data = blp,
                    index = "paper", model = "pooling")


# Apply the function to each model
c_model_r <- remove_prefix(model_r)
c_model_deu_r <- remove_prefix(model_deu_r)
c_model2_r <- remove_prefix(model2_r)
c_model2_deu_r <- remove_prefix(model2_deu_r)

# Create a table with the model summaries
#put models into list
models_r <- list("Cost Accounting Markup" = c_model_r,
                 "DEU Markup" = c_model_deu_r,
                 "Cost Accounting Markup" = c_model2_r,
                 "DEU Markup" = c_model2_deu_r)


# Create a summary table of the models
ms_r <- msummary(models_r, gof_map = gm_temp, stars = TRUE, notes = c(
  "Standard errors in parentheses. Clustered by paper."
))

# Print the model summary table with the notes
ms_r

# Export the summary table as a LaTeX file

ms_r_latex <-
  msummary(models_r, gof_map = gm_temp, stars = TRUE,
           notes = c("Standard errors in parentheses. Clustered by paper."),
           output = "latex")

ms_r_latex <-   gsub("FRAC", "\\hline & Method Controls \\\\ FRAC",
                     ms_r_latex, fixed = TRUE)


ms_r_latex







##############################
##########re doing regression table #############

# Fit the models with clustered standard errors
m_mu_ca <- plm(mu ~ ca_mu, data = blp,
               index = "paper", model = "pooling")
m_mu_deu <- plm(mu ~ deu_mu, data = blp,
                index = "paper", model = "pooling")
m_ca_mu <- plm(ca_mu ~ mu, data = blp,
               index = "paper", model = "pooling")
m_deu_mu <- plm(deu_mu ~ mu, data = blp,
                index = "paper", model = "pooling")
m_ca_deu <- plm(ca_mu ~ deu_mu, data = blp,
                index = "paper", model = "pooling")
m_deu_ca <- plm(deu_mu ~ ca_mu, data = blp,
                index = "paper", model = "pooling")

#put models into list
models <- list("Demand Markup" = m_mu_ca,
               "Demand Markup" = m_mu_deu,
               "Accounting Markup" = m_ca_mu,
               "Accounting Markup" = m_ca_deu,
               "DEU Markup" = m_deu_mu,
               "DEU Markup" = m_deu_ca)

#select GOF measures
gm_temp <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "N", 0,
  "r.squared", "R2", 4,
)

#rename
c_rn <-
  c("ca_mu" = "$\\mu_{_{CA}}-1$",
    "deu_mu" = "$\\mu_{_{PF}}-1$",
    "mu" = "$\\mu_{_{IO}}-1$",
    "paper" = "Paper level")


# Print the model summary table with the notes
# Create a summary table of the models
ms <- msummary(models, gof_map = gm_temp, coef_rename = c_rn,
stars = TRUE, notes = c(
  "Right hand side variable listed above.
   Standard errors in parentheses. Clustered by paper."
))

ms

# Export the summary table as a LaTeX file

ms_latexraw <-
  msummary(models, gof_map = gm_temp, stars = TRUE,
           notes = c("Right hand side variable listed above.
                     Standard errors in parentheses. Clustered by paper."),
           output = "latex")

ms_latex <-   gsub("ca\\_mu", "\\hline \\\\ Accounting Markup",
                   ms_latexraw, fixed = TRUE)

ms_latex <-   gsub("deu\\_mu", "\\hline \\\\ DEU Markup",
                   ms_latex, fixed = TRUE)

ms_latex <-   gsub("mu &", "\\hline \\\\ Demand Markup &",
                   ms_latex, fixed = TRUE)

ms_latex



################ backing out params ##############
# let s(i,j) = slope when regressing i on j
# then s(i,j) = (b_j / b_i) ([var(mu) +cov(e_i,e_j)]/[var(mu) +var(e_i)])
# s(k,i)s(i,j)/s(k,j) = ([var(mu) + cov(e_i,e_k)]/[var(mu) + cov(e_i,e_j)]) *
#                       ([var(mu) + cov(e_i,e_j)]/[var(mu) + var(e_i)])
#
# assume cov(e_{DEU},e_{IO}) = cov(e_{CA},e_{IO}) = 0
# then;
#     s(CA,IO)  = (b_{IO}} / b_{CA})  (var(mu)/[var(mu) +var(e_{CA})])
#     s(DEU,IO) = (b_{IO}} / b_{DEU}) (var(mu)/[var(mu) +var(e_{DEU})])
#
# s(IO,CA) s(CA,DEU) / s(IO,DEU)
#      = ([var(mu) + cov(e_{DEU},e_{CA})]/[var(mu) + var(e_{CA})])
# s(IO,DEU) s(DEU,CA) / s(IO,CA)
#      = ([var(mu) + cov(e_{DEU},e_{CA})]/[var(mu) + var(e_{DEU})])
s_mu_ca <- coef(m_ca_mu)[2]
s_mu_deu <- coef(m_deu_mu)[2]
s_ca_mu <- coef(m_mu_ca)[2]
s_deu_mu <- coef(m_mu_deu)[2]
s_ca_deu <- coef(m_deu_ca)[2]
s_deu_ca <- coef(m_ca_deu)[2]

s <- data.frame(
  "y = mu" = c(mu = NA, s_ca_mu, s_deu_mu),
  "y = mu_ca" = c(s_mu_ca, mu_ca = NA, s_deu_ca),
  "y = dey_ca" = c(s_mu_deu, s_ca_deu, deu_mu = NA)
)


#{[var(mu) + var(e_{CA})] / [var(mu) + cov(e_{DEU},e_{CA}) ]} =
#         s(IO,DEU) / [s(IO,CA) s(CA,DEU)]
ca_nm <- s[1, 3] / (s[1, 2] * s[2, 3])
ca_nm

#([var(mu) + cov(e_{DEU},e_{CA})]/[var(mu) + var(e_{DEU})]) =
#         s(IO,CA) / s(IO,DEU) s(DEU,CA)
deu_nm <- s[1, 2] / (s[1, 3] * s[3, 2])
deu_nm

# {[var(e_{CA}) - var(e_{DEU})] / [var(mu) + cov(e_{DEU},e_{CA}) ]} =
#s(IO,DEU) / [s(IO,CA) s(CA,DEU)] - s(IO,CA) / s(IO,DEU) s(DEU,CA)
rat <- (ca_nm - deu_nm) / (ca_nm + deu_nm)

(rat + 1) / (1 - rat)



#old
# let s(i,j) = slope when regressing i on j
# then s(i,j) = b_j / b_i [f(e_i)]
# b_i / b_j = s(k,i)/ s(k,j), ie:
#   b_ca/b_deu = s(mu, deu) / s(mu, ca) #nolint
#   b_ca/b_mu = s(deu, mu) / s(deu, ca) #nolint
#   b_deu/b_mu = s(ca, mu) / s(ca, deu) #nolint

# [f(e_i)] = s(i,j) * (b_j / b_i) = [s(i,j) * s(k,i)]/ s(k,j)
# [f(e_i)] = var(m)/[var(m)+var(e_i)] =  1/[1+var(e_i)/var(m)]
# =>
#   var(e_i)/var(m) = 1/f(e_i) - 1 #nolint
#                   = s(k,j) / (s(i,j) * s(k,i)) - 1

s_mu_ca <- coef(m_ca_mu)[2]
s_mu_deu <- coef(m_deu_mu)[2]
s_ca_mu <- coef(m_mu_ca)[2]
s_deu_mu <- coef(m_mu_deu)[2]
s_ca_deu <- coef(m_deu_ca)[2]
s_deu_ca <- coef(m_ca_deu)[2]

b_h <- data.frame(
  "y = mu" = c(mu = NA, s_ca_mu, s_deu_mu),
  "y = mu_ca" = c(s_mu_ca, mu_ca = NA, s_deu_ca),
  "y = dey_ca" = c(s_mu_deu, s_ca_deu, deu_mu = NA)
)

b_ij <- data.frame(
  "i = mu" = c("j = mu" = 1,
               "j = ca" = b_h[1, 2] / b_h[2, 1],
               "j = deu" = b_h[1, 3] / b_h[3, 1]),
  "i = mu_ca" = c("j = mu" = b_h[2, 1] / b_h[1, 2],
                  "j = ca" = 1,
                  "j = deu" = b_h[2, 3] / b_h[3, 2]),
  "k = deu_ca" = c("j = mu" = b_h[3, 1] / b_h[1, 3],
                   "j = ca" = b_h[3, 2] / b_h[2, 3],
                   "j = deu" = 1)
)

var_e <- data.frame(
  mu = b_h[3, 2] / (b_h[3, 1] * b_h[1, 2]) - 1,
  ca_mu = b_h[1, 3] / (b_h[1, 2] * b_h[2, 3]) - 1,
  deu_mu = b_h[1, 2] / (b_h[1, 3] * b_h[3, 2]) - 1
)

b_h[3, 2] / (b_h[3, 1] * b_h[1, 2]) - 1
b_h[2, 3] / (b_h[2, 1] * b_h[1, 3]) - 1

b_h[1, 3] / (b_h[1, 2] * b_h[2, 3]) - 1
b_h[3, 1] / (b_h[3, 2] * b_h[2, 1]) - 1

b_h[1, 2] / (b_h[1, 3] * b_h[3, 2]) - 1
b_h[2, 1] / (b_h[2, 3] * b_h[3, 1]) - 1




(b_h[2, 3] / b_h[3, 2]) * (b_h[1, 2] / b_h[1, 3])^2


(b_h[2, 1] / b_h[3, 1]) * (b_h[1, 2] / b_h[1, 3])


############################################################
############################################################
#this sample ca on DEU
############################################################
############################################################

#### ca on deu_mu

# Calculate ols lines and correlation coef
model_cadeu <- lm(deu_mu ~ ca_mu, data = blp)
s_mudeu_ca <- model_cadeu$coefficients[2]
i_mudeu_ca <- model_cadeu$coefficients[1]
model_cadeur <- lm(ca_mu ~ deu_mu, data = blp)
s_cadeu_mu <- model_cadeur$coefficients[2]
i_cadeu_mu <- model_cadeur$coefficients[1]

correlation <- cor(blp$ca_mu, blp$deu_mu, use = "pairwise.complete.obs")
# Create the label for the legend
label <- paste0("Correlation Coef. = ", round(correlation, 2),
                ", N. obs =", nrow(blp))

# Create a dummy data frame for the 45-degree line
df_line <- data.frame(ca_mu = range(blp$ca_mu), deu_mu = range(blp$deu_mu))

############## plot #####################
comp_cadeu <- ggplot(blp, aes(x = ca_mu, y = deu_mu)) +
  geom_point(size = 4) +
  geom_abline(slope = s_mudeu_ca,
              intercept = i_mudeu_ca,
              color = "blue") +
  geom_abline(slope = 1 / s_cadeu_mu,
              intercept = - i_cadeu_mu / s_cadeu_mu,
              color = "red") +
  geom_line(data = df_line, color = "black",
            aes(x = deu_mu,
                linetype = "45 Degree line")) +
  geom_line(aes(linetype = "OLS line (Main)"),
            data = data.frame(ca_mu = 0, deu_mu = 0),
            color = "blue") +
  geom_line(aes(linetype = "OLS line (Reverse)"),
            data = data.frame(ca_mu = 0, deu_mu = 0),
            color = "red") +
  scale_linetype_manual(
         values = c("OLS line (Main)" = "solid", # nolint
                    "OLS line (Reverse)" = "solid",
                    "45 Degree line" = "solid")) +
  theme(
    text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  coord_cartesian(
    xlim = c(min(blp$ca_mu, blp$deu_mu), max(blp$ca_mu, blp$deu_mu)),  #nolint
    ylim = c(min(blp$ca_mu, blp$deu_mu), max(blp$ca_mu, blp$deu_mu))  #nolint
  ) +
  labs(linetype = labeldeu) +
  xlab("Accounting Markup (p-mc)/mc") +
  ylab("Production Function Markup (p-mc)/mc") +
  guides(
    linetype = guide_legend(nrow = 1, byrow = TRUE, order = 3,
    override.aes = list(color = c("black", "blue", "red"))) #nolint
  ) +
  guides(color = "none")  # Remove the color part of the legend

comp_cadeu

save_f(comp_cadeu, "BLP_comp_CA_deu.pdf", dircs, 11.5, 12, TRUE)



######################### corr comparison

cor_ca_IO <- cor(blp$ca_mu, blp$mu, use = "pairwise.complete.obs")
cor_deu_IO <- cor(blp$deu_mu, blp$mu, use = "pairwise.complete.obs")

ratio = (cor_deu_IO / cor_ca_IO)^2

1/ratio