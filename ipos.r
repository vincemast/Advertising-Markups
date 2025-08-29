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
library(cowplot)
library(grid)
library(gridExtra)

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

################### 1.a load data #########################
############################################################

#navigate to with data
setwd(dircs[2])

#compustat
dset <- read.csv("COMPUSTAT_wfoot.csv") # nolint
# naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")
colnames(naics) <- c("change", "naics_n", "industry")
#FRED DATA
usercost <- read.csv("usercost.csv") #nolint

################### 1.b clean data #########################
############################################################

#rename gvkey to GVKEY
colnames(dset)[colnames(dset) == "gvkey"] <- "GVKEY"

#back to functions (incase you want to rerun functions following edit)
setwd(dircs[1])

#apply GDP deflator and generate xad measures
dset <- vargen(dset, usercost)
dset <- GDPdef(dset, usercost)

#clean
data <- clean_deu(dset)
data <- data %>%
  filter(!is.na(MU))



#generate sales share
data <- data %>% # nolint
  group_by(fyear) %>% # nolint
  mutate(totsale = sum(sale, na.rm = TRUE)) # nolint
data <- data %>% # nolint
  mutate(sshare = sale *100/ totsale) #nolint

#make numeric indicator of year
#with fyear as index it operates weird when called, new var more convenient
data <- data %>%
  mutate(year = as.integer(fyear)) %>% #nolint
  filter(!is.na(fyear))

data <- data %>%
  mutate(decade = floor(year / 10) * 10)

#select only useful variables
data <- data %>%
  dplyr::select(MU, GVKEY, year, naics, sale,
                age, life, conm, decade, sshare, xsga, entry, exit, decade, decade_e) #nolint

#grab first 2 digits of naics
data$sec <- as.numeric(str_sub(data$naics, 1, 2))

data <- industry_n_dig(data, naics, 2)





############################################################
############################################################
#     2: first year
############################################################
############################################################

#save full sample mean

#save means
mean_ms <- mean(data$sshare, na.rm = TRUE)
mean_mu <- mean(data$MU - 1, na.rm = TRUE)

#indicate first year
data <- data %>%
  mutate(ipo = ifelse(age == 0, 1, 0),
         ipo = ifelse(is.na(age), 0, ifelse(age == 0, 1, 0)))

ipos <- data %>%
  filter(ipo == 1)

nonipos <- data %>%
  filter(ipo == 0)



#generate decade variables

ipo_dec <- data %>%
  group_by(decade) %>%
  summarise(mu_mean = mean(ifelse(ipo == 1,
                                  MU - 1, NA),
                           na.rm = TRUE),
            mu_median = median(ifelse(ipo == 1,
                                      MU - 1, NA),
                               na.rm = TRUE),
            mu_sw = weighted.mean(ifelse(ipo == 1,
                                         MU - 1, NA),
                                  sale, na.rm = TRUE),
            avg_w = mean(ifelse(ipo == 1,
                                sshare, NA),
                         na.rm = TRUE),
            mu_mean_t = mean(ifelse(ipo == 0,
                                    MU - 1, NA),
                             na.rm = TRUE),
            mu_sw_t = weighted.mean(ifelse(ipo == 0,
                                           MU - 1, NA),
                                    sale, na.rm = TRUE),
            avg_w_t = mean(ifelse(ipo == 0,
                                  sshare, NA),
                           na.rm = TRUE))

decmu <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = mu_mean, color = "mu_mean"), size = 4) +
  geom_line(aes(y = mu_mean, color = "mu_mean"), size = .5) +
  geom_point(aes(y = mu_mean_t, color = "mu_mean_t"), size = 4, shape = 2) +
  geom_line(aes(y = mu_mean_t, color = "mu_mean_t"), size = .5) +
  labs(x = "Decade", y = "Average Markup", color = " ") +
  scale_color_manual(values = c("mu_mean" = "blue", "mu_mean_t" = "red"),
                     labels = c("mu_mean" = "IPOs",
                                "mu_mean_t" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$mu_mean))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


decmumed <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = mu_median, color = "ipo"), size = 4) +
  geom_line(aes(y = mu_median, color = "ipo"), size = .5) +
  geom_point(aes(y = mu_median_t, color = "else"), size = 4) +
  geom_line(aes(y = mu_median_t, color = "else"), size = .5) +
  labs(x = "Decade", y = "Median Markup", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$mu_median))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


decmusw <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = mu_sw, color = "mu_mean"), size = 4) +
  geom_line(aes(y = mu_sw, color = "mu_mean"), size = .5) +
  geom_point(aes(y = mu_sw_t, color = "mu_mean_t"), size = 4) +
  geom_line(aes(y = mu_sw_t, color = "mu_mean_t"), size = .5) +
  labs(x = "Decade", y = "Sales Weighted Markup", color = " ") +
  scale_color_manual(values = c("mu_mean" = "blue", "mu_mean_t" = "red"),
                     labels = c("mu_mean" = "IPOs",
                                "mu_mean_t" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$mu_sw))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


decw <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = avg_w, color = "ipo"), size = 4) +
  geom_line(aes(y = avg_w, color = "ipo"), size = .5) +
  geom_point(aes(y = avg_w_t, color = "else"), size = 4, shape = 2) +
  geom_line(aes(y = avg_w_t, color = "else"), size = .5) +
  labs(x = "Decade", y = "Average Market Share (%)", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$avg_w_t))) +
  theme(text = element_text(size = 20), legend.position = "bottom")

decmu

decw

############################################################
############################################################
#     3: By age (full sample)
############################################################
############################################################


#filter to first dec age
ipos10 <- data %>%
  filter(age < 41)


ipo_10_marks <- ipos10 %>%
  group_by(age) %>%
  summarise(
    mu_mean = mean(MU - 1, na.rm = TRUE),
    mu_median = median(MU - 1, na.rm = TRUE),
    mu_sw = weighted.mean(MU - 1, sale, na.rm = TRUE),
    avg_w = mean(sshare, na.rm = TRUE),
    tot_w = sum(sshare, na.rm = TRUE)
  )


marks10 <- ggplot(ipo_10_marks, aes(x = age, y = mu_mean)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = mean_mu,
                 linetype = "Full sample average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Mean Markup") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$mu_mean))) +
  scale_linetype_manual(values = c("Full sample average" = "dashed")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(linetype = guide_legend(title = NULL))

marksmed10 <- ggplot(ipo_10_marks, aes(x = age, y = mu_median)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  labs(x = "Years Since IPO", y = "Median Markup") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$mu_median))) +
  theme(text = element_text(size = 20))

marksw10 <- ggplot(ipo_10_marks, aes(x = age, y = mu_sw)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = mean_swmu,
                 linetype = "Full sample sales weighted average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Sales Weighted Markup") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$mu_sw))) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_linetype_manual(values =
                        c("Full sample sales weighted average" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))



w10 <- ggplot(ipo_10_marks, aes(x = age, y = avg_w)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = mean_ms,
                 linetype = "Full sample Average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Avererage Market Share (%)") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$avg_w))) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_linetype_manual(values =
                        c("Full sample Average" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))


marks10

w10

############################################################
############################################################
#     3: By age by sector by decade
############################################################
############################################################

data_na <- data %>%
  filter(!is.na(industry))

ind_names <- unique(data_na$industry)
# Initialize lists to store the plots
mu_mean_plots <- list()
mu_median_plots <- list()


for (i in seq_along(ind_names)){
  data_temp <- data_na %>%
    filter(industry == ind_names[i])

  temp_data <- data_temp %>%
    group_by(age, decade_e) %>%
    summarise(
      MU_mean = mean(MU, na.rm = TRUE),
      MU_median = median(MU, na.rm = TRUE)
    )
  #remove Na decade
  temp_data <- temp_data %>%
    filter(!is.na(decade_e))
  #save plots

  mu_mean_t <-
    ggplot(temp_data, aes(x = age,
                          y = MU_mean - 1,
                          color = as.factor(decade_e))) +
    geom_line() +
    labs(x = "Age", y = "Mean Markup Ratio (p/mc-1)",
         title = paste(ind_names[i]),
         color = "Decade of Entry") +
    lims(x = c(0, 30)) +
    lims(y = c(0, 1.25)) +
    theme(text = element_text(size = 25), legend.position = "bottom",
          legend.text = element_text(size = 25)) +
    scale_color_discrete(name = "Decade")

  mu_median_t <-
    ggplot(temp_data, aes(x = age,
                          y = MU_median - 1,
                          color = as.factor(decade_e))) +
    geom_line() +
    labs(x = "Age",
         y = "Median Markup Ratio (p/mc-1)",
         color = "Decade of Entry") +
    lims(x = c(0, 30)) +
    lims(y = c(0, 1.25)) +
    theme(text = element_text(size = 10), legend.position = "bottom",
          legend.text = element_text(size = 10)) +
    scale_color_discrete(name = "Decade")

  # Save plots to lists
  mu_mean_plots[[ind_names[i]]] <- mu_mean_t
  mu_median_plots[[ind_names[i]]] <- mu_median_t
}


mu_mean_plots[[2]]



############################################################
############################################################
#     4: Regression:
############################################################
############################################################

data$MU_1 <- data$MU - 1

names(data)

#generate variable if last year in data
data <- data %>%
  group_by(GVKEY) %>%
  mutate(last = ifelse(fyear == max(fyear), 1, 0),
         t = fyear - 1955) %>%
  ungroup()

#generate age up to 20 years
data$age_c <- ifelse(data$age > 19, "20+", data$age)


twfe <- feols(MU_1 ~ i(age_c) + last - 1 |  GVKEY + year,
              cluster = c("GVKEY"),
              data = data)

fixed_effects <- fixef(twfe)
gvkey_fe <- fixed_effects$GVKEY
year_fe <- fixed_effects$year

#take fitted value of average firm in 2018
ref <- year_fe[length(year_fe)-3] + mean(gvkey_fe)

#grabcoefficients and ses (clustered)
twfe_coef <- data.frame(twfe$coefficients)
twfe_se <- data.frame(twfe$se)

#grab age from rownames removing "age_c::
twfe_coef$age <- gsub("age_c::", "", rownames(twfe_coef))
twfe_se$age <- gsub("age_c::", "", rownames(twfe_se))
#merege
twfe_res <- merge(twfe_coef, twfe_se, by = "age")
#remove age = last
twfe_res <- twfe_res %>% filter(age != "last")
names(twfe_res) <- c("age", "coef", "se")
#refer to the 20+ as 20 (for simplicity in plot)
twfe_res$age <- as.numeric(gsub("\\+", "", twfe_res$age))


#add refences for plot
twfe_res$coef_r <- twfe_res$coef + ref

#get upper bound for limits of plot
twfe_res$upper_bound <- twfe_res$coef_r + 1.96 * twfe_res$se
max_upper_bound <- max(twfe_res$upper_bound)



# Plot event study with 20+ as last
twfe_plot <- ggplot(twfe_res, aes(x = age)) +
  geom_point(aes(y = coef_r, color = "Coefficient"), size = 4) +
  geom_line(aes(y = coef_r, color = "Coefficient"), size = .5) +
  geom_errorbar(aes(ymin = coef_r - 1.96 * se,
                    ymax = coef_r + 1.96 * se, color = "eb"),
                width = 0.1) +
  geom_hline(aes(yintercept = ref, color = "Mean"),
             linetype = "dotted", size = 1) +
  labs(x = "Years Since IPO", y = "Markup (p/mc-1)",
       color = " ") +
  scale_color_manual(values = c("Coefficient" = "blue", "Mean" = "red",
                                "eb" = "black"),
                     labels = c("Coefficient" = "Fitted Value",
                                "Mean" = "Reference",
                                "eb" = "95% confidence interval")) +
  scale_linetype_manual("",
                     values = c("Coefficient" = "solid", "Mean" = "dotted",
                                "eb" = "solid")) +
  scale_x_continuous(breaks = c(0:19, 20), labels = c(0:19, "20+")) +
  coord_cartesian(ylim = c(0, max_upper_bound)) +
  theme(text = element_text(size = 20), legend.position = "bottom")

print(twfe_plot)


twfe_plot <- ggplot(twfe_res, aes(x = age)) +
  geom_point(aes(y = coef_r, color = "Fitted Value"), size = 4) +
  geom_line(aes(y = coef_r, color = "Fitted Value"), size = .5) +
  geom_errorbar(aes(ymin = coef_r - 1.96 * se,
                    ymax = coef_r + 1.96 * se,
                    color = "95% confidence interval"),
                width = 0.1) +
  geom_hline(aes(yintercept = ref, color = "Reference"),
             linetype = "dotted", size = 1) +
  labs(x = "Years Since IPO", y = "Markup (p/mc-1)",
       color = " ") +
  scale_color_manual(values = c("Fitted Value" = "blue",
                                "Reference" = "red",
                                "95% confidence interval" = "black")) +
  scale_shape_manual(values = c("Fitted Value" = 16,
                                "95% confidence interval" = NA,
                                "Reference" = NA)) +
  scale_linetype_manual(values = c("Fitted Value" = "solid",
                                   "Reference" = "dotted",
                                   "95% confidence interval" = "solid")) +
  scale_x_continuous(breaks = c(0:19, 20), labels = c(0:19, "20+")) +
  coord_cartesian(ylim = c(0, max_upper_bound)) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(shape = c(NA, 16, NA),
         linetype = c("solid", "solid", "dotted"))))

print(twfe_plot)


#save plot
save_f(twfe_plot, "twfe_plot.pdf", dircs, 12, 8, TRUE)

#save plot
save_f(twfe_plot, "twfe_plot_wide.pdf", dircs, 16, 9, TRUE)

################################################################
############# market share
################################################################

#generate age up to 30 years
data$age_d <- ifelse(data$age > 29, "30+", data$age)

twfe_ms <- feols(sshare ~ i(age_c) - 1 |  GVKEY + year + last,
                 cluster = c("GVKEY"),
                 data = data)

view(etable(twfe_ms, order = c("age_c", "last")))

mean(twfe_ms$fitted.values)

fe_ms <- fixef(twfe_ms)

g_fe_ms <- fe_ms$GVKEY
y_fe_ms <- fe_ms$year

#take fitted value of average firm in 2018
ref_ms <- y_fe_ms[length(y_fe_ms)-3] + mean(g_fe_ms)

#grabcoefficients and ses (clustered)
twfe_coef_ms <- data.frame(twfe_ms$coefficients)
twfe_se_ms <- data.frame(twfe_ms$se)

#grab age from rownames removing "age_c::
twfe_coef_ms$age <- gsub("age_c::", "", rownames(twfe_coef_ms))
twfe_se_ms$age <- gsub("age_c::", "", rownames(twfe_se_ms))
#merege
twfe_res_ms <- merge(twfe_coef_ms, twfe_se_ms, by = "age")
names(twfe_res_ms) <- c("age", "coef", "se")

#refer to the 20+ as 20 (for simplicity in plot)
twfe_res_ms$age <- as.numeric(gsub("\\+", "", twfe_res_ms$age))

#center and add reference for
twfe_res_ms$coef_r <- twfe_res_ms$coef + ref_ms - mean(twfe_res_ms$coef)

#get upper bound for limits of plot
twfe_res_ms$upper_bound <- twfe_res_ms$coef_r + 1.96 * twfe_res_ms$se
max_upper_bound_ms <- max(twfe_res_ms$upper_bound)

# Plot event study with 20+ as last

twfe_plot_ms <- ggplot(twfe_res_ms, aes(x = age)) +
  geom_point(aes(y = coef_r, color = "Fitted Value"), size = 4) +
  geom_line(aes(y = coef_r, color = "Fitted Value"), size = .5) +
  geom_errorbar(aes(ymin = coef_r - 1.96 * se,
                    ymax = coef_r + 1.96 * se,
                    color = "95% confidence interval"),
                width = 0.1) +
  geom_hline(aes(yintercept = ref_ms, color = "Reference"),
             linetype = "dotted", size = 1) +
  labs(x = "Years Since IPO", y = "Market Share (%)",
       color = " ") +
  scale_color_manual(values = c("Fitted Value" = "blue",
                                "Reference" = "red",
                                "95% confidence interval" = "black")) +
  scale_shape_manual(values = c("Fitted Value" = 16,
                                "95% confidence interval" = NA,
                                "Reference" = NA)) +
  scale_linetype_manual(values = c("Fitted Value" = "solid",
                                   "Reference" = "dotted",
                                   "95% confidence interval" = "solid")) +
  scale_x_continuous(breaks = c(0:19, 20), labels = c(0:19, "20+")) +
  coord_cartesian(ylim = c(0, max_upper_bound_ms)) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(shape = c(NA, 16, NA),
         linetype = c("solid", "solid", "dotted"))))








save_f(twfe_plot_ms, "twfe_plot_ms.pdf", dircs, 12, 8, TRUE)

# Generate 10-year age indicator as a single variable
data <- data %>%
  mutate(age_in = case_when(
    age <= 5 ~ " 0-5",
    age <= 10 ~ " 6-10",
    age <= 20 ~ " 11-20",
    age <= 30 ~ " 21-30",
    age > 40 ~ " 40+",
    TRUE ~ "31-40"  # Assuming ages between 31 and 40 should be grouped here
  ))


# Generate 10-year age indicator as a single variable
data <- data %>%
  mutate(age_in = case_when(
    age <= 5 ~ " 0-5",
    age <= 10 ~ " 6-10",
    age <= 20 ~ " 11-20",
    age > 20 ~ " 20+"
  ))


data$MU_1 <- data$MU - 1


#############################################################################################################################
###### regs
  #############################################################################################################################

#sort data by age
data <- data %>%
  arrange(age)
######### regs

model1 <- feols(MU_1 ~ age,
                cluster = c("GVKEY"),
                data = data)

model2 <- feols(MU_1 ~ age + t,
                cluster = c("GVKEY"),
                data = data)

model3 <- feols(MU_1 ~ age - 1 | decade,
                cluster = c("GVKEY"),
                data = data)

model4 <- feols(MU_1 ~ age - 1 | decade + industry,
                cluster = c("GVKEY"),
                data = data)


model5 <- feols(MU_1 ~ age - 1 | GVKEY + decade,
                cluster = c("GVKEY"),
                data = data)

model6 <- feols(MU_1 ~ i(age_in, age) - 1 |  GVKEY + decade,
                cluster = c("GVKEY"),
                data = data)

# add last indicator
model7 <- feols(MU_1 ~ i(age_in, age) + last - 1 |  GVKEY + decade,
                cluster = c("GVKEY"),
                data = data)

# add life indicator
model8 <- feols(MU_1 ~ i(age_in, age) - 1 |  GVKEY + decade + life,
                cluster = c("GVKEY"),
                data = data)

summary(data$MU_1)

models <- list(" " = model1,
                " " = model2,
                " " = model3,
                " " = model4,
                " " = model5,
                " " = model6,
                " " = model7)

new_names <- c(
  "MU_1" = "Markup",
  "age" = "Firm Age",
  "age_in:: 0-5:age" = "$ Age \\times I[Age < 6] $",
  "age_in:: 6-10:age" = "$ Age \\times I[5 < Age < 11] $",
  "age_in:: 11-20:age" = "$ Age \\times I[10 < Age < 20] $",
  "age_in:: 20+:age" = "$ Age \\times i[Age > 20] $",
  "t" = "Time",
  "last" = "Last Year in Sample",
  "life" = "Years To Exit",
  "GVKEY" = "Firm",
  "industry" = "Industry",
  "age_in" = "Years Since IPO Buckets",
  "decade_e" = "Decade of Entry",
  "decade" = "Decade"
)

# Specify the order
age_order <-
c("Age < 6", "5 < Age < 11", "10 < Age < 20", "Age > 20")


coef_order <- 
c("Constant", "Firm Age", "Time", age_order)


# Display the table with coefficients in the specified order
view(etable(models, order = coef_order, dict = new_names))


etable(models, order = coef_order, dict = new_names, tex = TRUE)





############################################################
############################################################
#     9: Save plots
############################################################
############################################################



#save plots
setwd(dircs[3])

#ipo x decade
pdf("ipoxdecade.pdf", width = 16, height = 10)
grid.arrange(decmu, decw,
             ncol = 2)
dev.off()


pdf("ys_ipo.pdf", width = 16, height = 10)
grid.arrange(marks10, w10,
             ncol = 2)
dev.off()

pdf("ys_ipo_wide.pdf", width = 19, height = 9)
grid.arrange(marks10, w10,
             ncol = 2)
dev.off()



pdf("muagedec.pdf", width = 40, height = 60)
grid.arrange(
  mu_mean_plots[[1]], mu_mean_plots[[2]], mu_mean_plots[[3]], mu_mean_plots[[4]],
  mu_mean_plots[[5]], mu_mean_plots[[6]], mu_mean_plots[[7]], mu_mean_plots[[8]],
  mu_mean_plots[[9]], mu_mean_plots[[10]], mu_mean_plots[[11]], mu_mean_plots[[12]],
   mu_mean_plots[[14]], mu_mean_plots[[15]], mu_mean_plots[[16]],
  mu_mean_plots[[17]], mu_mean_plots[[18]], mu_mean_plots[[19]],
  ncol = 4)
dev.off()



pdf("muagedec_wide.pdf", width = 48, height = 27)
grid.arrange(
  mu_mean_plots[[1]], mu_mean_plots[[2]], mu_mean_plots[[3]], mu_mean_plots[[4]],
  mu_mean_plots[[5]], mu_mean_plots[[6]], mu_mean_plots[[7]], mu_mean_plots[[8]],
  mu_mean_plots[[9]], mu_mean_plots[[10]], mu_mean_plots[[11]], mu_mean_plots[[12]],
   mu_mean_plots[[14]], mu_mean_plots[[15]], mu_mean_plots[[16]],
  mu_mean_plots[[17]], mu_mean_plots[[18]], mu_mean_plots[[19]],
  ncol = 6)
dev.off()


























































############################################################
############################################################
############################################################
############################################################
#     9999: OLD
############################################################
############################################################
############################################################
############################################################





































































############################################################
############################################################
#     2: first year
############################################################
############################################################

#filter to first year
data <- data %>%
  mutate(ipo = ifelse(age == 0, 1, 0),
         ipo = ifelse(is.na(age), 0, ifelse(age == 0, 1, 0)))

ipos <- data %>%
  filter(ipo == 1)

nonipos <- data %>%
  filter(ipo == 0)

ipo_marks <- data %>%
  group_by(year) %>%
  mutate(
    is = sum(ipo == 1) / n()
  )

############################################################
#    densities
############################################################

#set limits for plots
xlimits_d <- c(0.4, 7)

#compare densities of IPO and non IPOs
density_comp <- ggplot() +
  geom_density(data = ipos, aes(x = MU, colour = "IPO Markups"),
               alpha = 0.5) +
  geom_density(data = nonipos, aes(x = MU, colour = "Non-IPO Markups"),
               alpha = 0.5) +
  scale_color_manual(values = c("IPO Markups" = "blue",
                                "Non-IPO Markups" = "red")) +
  scale_x_continuous(
                     trans = "log",
                     breaks = c(0.5, 1, 2, 6),
                     limits = xlimits_d,
                     labels = function(x) x - 1) +
  guides(colour = guide_legend(title = NULL)) +
  labs(x = "Markup ([p-mc]/mc, log scale)",
       y = "Density") +
  theme(text = element_text(size = 20), legend.position = "bottom")
#display plot
density_comp


############################################################
#certaom uears
############################################################

#pick years to include
years <- c(1960, 1980, 2000, 2020)
# Filter data for specific years and create a new variable for the year order
ipos_y <- ipos %>%
  filter(fyear %in% years) %>%
  mutate(year_order = factor(fyear, levels = years)
  )

unique(ipos$decade)

nonipos_y <- nonipos %>%
  filter(fyear %in% years) %>%
  mutate(year_order = factor(fyear, levels = years)
  )

unique(ipos_y$decade_order)


# Plot densities for each year
density_comp_years <- ggplot() +
  geom_density(data = ipos_y, aes(x = MU, colour = "IPO Markups"),
               alpha = 0.5) +
  geom_density(data = nonipos_y,
               aes(x = MU, colour = "Non-IPO Markups"),
               alpha = 0.5) +
  scale_color_manual(values = c("IPO Markups" = "blue",
                                "Non-IPO Markups" = "red")) +
  facet_wrap(~ year_order, ncol = 2) +
  scale_x_continuous(limits = xlimits_d,
                     trans = "log10",
                     breaks = c(0.5, 1, 2, 6),
                     labels = function(x) x - 1) +
  guides(colour = guide_legend(title = NULL)) +
  labs(x = "Markup ([p-mc]/mc, log scale)",
       y = "Density") +
  theme(text = element_text(size = 20), legend.position = "bottom")+
  guides(colour = guide_legend(title = NULL))


density_comp_years

############################################################
#by decade
############################################################

decs <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)
ipos_d <- ipos %>%
  filter(!is.na(decade)) %>%
  mutate(decade_order = factor(decade, levels = decs))
nonipos_d <- nonipos %>%
  filter(!is.na(decade)) %>%
  mutate(decade_order = factor(decade, levels = decs))


# Plot densities for each decade
density_comp_decs <- ggplot() +
  geom_density(data = ipos_d, aes(x = MU, colour = "IPO Markups"),
               alpha = 0.5) +
  geom_density(data = nonipos_d,
               aes(x = MU, colour = "Non-IPO Markups"),
               alpha = 0.5) +
  scale_color_manual(values = c("IPO Markups" = "blue",
                                "Non-IPO Markups" = "red")) +
  facet_wrap(~ decade_order, ncol = 4) +
  scale_x_continuous(limits = xlimits_d,
                     trans = "log10",
                     breaks = c(0.5, 1, 2, 6),
                     labels = function(x) x - 1) +
  guides(colour = guide_legend(title = NULL)) +
  labs(x = "Markup ([p-mc]/mc, log scale)",
       y = "Density") +
  theme(text = element_text(size = 20), legend.position = "bottom")+
  guides(colour = guide_legend(title = NULL))


density_comp_decs




############################################################
#    agg stats
############################################################

ipo_marks <- data %>%
  group_by(year) %>%
  summarise(
    mu_mean = mean(ifelse(ipo == 1,
                          MU - 1, NA), na.rm = TRUE),
    mu_median = median(ifelse(ipo == 1,
                              MU - 1, NA), na.rm = TRUE),
    mu_sw = weighted.mean(ifelse(ipo == 1,
                                 MU - 1, NA), sale, na.rm = TRUE),
    mu_var = var(ifelse(ipo == 1,
                        MU - 1, NA), na.rm = TRUE),
    avg_w = mean(ifelse(ipo == 1,
                        sshare, NA), na.rm = TRUE),
    tot_w = sum(sshare * ipo, na.rm = TRUE),
    n = sum(ipo == 1),
    n_t = n() - sum(ipo == 1),
    mu_mean_t = mean(ifelse(ipo == 0,
                            MU - 1, NA), na.rm = TRUE),
    mu_median_t = median(ifelse(ipo == 0,
                                MU - 1, NA), na.rm = TRUE),
    mu_sw_t = weighted.mean(ifelse(ipo == 0,
                                   MU - 1, NA), sale, na.rm = TRUE),
    mu_var_t = var(ifelse(ipo == 0,
                          MU - 1, NA), na.rm = TRUE),
    avg_w_t = mean(ifelse(ipo == 0,
                          sshare, NA), na.rm = TRUE),
    tot_w_t = sum(ifelse(ipo == 0,
                         sshare, NA), na.rm = TRUE),
  )


meanipo <- ggplot(ipo_marks, aes(x = year)) +
  geom_point(aes(y = mu_mean, color = "mu_mean"), size = 4) +
  geom_line(aes(y = mu_mean, color = "mu_mean"), size = .5) +
  geom_point(aes(y = mu_mean_t, color = "mu_mean_t"), size = 4) +
  geom_line(aes(y = mu_mean_t, color = "mu_mean_t"), size = .5) +
  labs(x = "Year", y = "Average Markup", color = " ") +
  scale_color_manual(values = c("mu_mean" = "blue", "mu_mean_t" = "red"),
                     labels = c("mu_mean" = "IPOs",
                                "mu_mean_t" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_marks$mu_mean))) +
  theme(text = element_text(size = 20), legend.position = "bottom")

medianipo <- ggplot(ipo_marks, aes(x = year)) +
  geom_point(aes(y = mu_median, color = "ipo"), size = 4) +
  geom_line(aes(y = mu_median, color = "ipo"), size = .5) +
  geom_point(aes(y = mu_median_t, color = "else"), size = 4) +
  geom_line(aes(y = mu_median_t, color = "else"), size = .5) +
  labs(x = "Year", y = "Median Markup", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_marks$mu_median))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


swipo <- ggplot(ipo_marks, aes(x = year)) +
  geom_point(aes(y = mu_sw, color = "ipo"), size = 4) +
  geom_line(aes(y = mu_sw, color = "ipo"), size = .5) +
  geom_point(aes(y = mu_sw_t, color = "else"), size = 4) +
  geom_line(aes(y = mu_sw_t, color = "else"), size = .5) +
  labs(x = "Year", y = "Sales Weighted Markup", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_marks$mu_sw))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


varipo <- ggplot(ipo_marks, aes(x = year)) +
  geom_point(aes(y = mu_var, color = "ipo"), size = 4) +
  geom_line(aes(y = mu_var, color = "ipo"), size = .5) +
  geom_point(aes(y = mu_var_t, color = "else"), size = 4) +
  geom_line(aes(y = mu_var_t, color = "else"), size = .5) +
  labs(x = "Year", y = "Markup Variance", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, 8)) +
  theme(text = element_text(size = 20), legend.position = "bottom")

nipo <- ggplot(ipo_marks, aes(x = year)) +
  geom_point(aes(y = n * 10, color = "ipo"), size = 4) +
  geom_line(aes(y = n * 10, color = "ipo"), size = .5) +
  geom_point(aes(y = n_t, color = "else"), size = 4) +
  geom_line(aes(y = n_t, color = "else"), size = .5) +
  labs(x = "Year", y = "Number of Firms", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs(10x Scale)",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_marks$n_t))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


avgwipo <- ggplot(ipo_marks, aes(x = year)) +
  geom_point(aes(y = avg_w, color = "ipo"), size = 4) +
  geom_line(aes(y = avg_w, color = "ipo"), size = .5) +
  geom_point(aes(y = avg_w_t, color = "else"), size = 4) +
  geom_line(aes(y = avg_w_t, color = "else"), size = .5) +
  labs(x = "Year", y = "Average Market Share (%)", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_marks$avg_w_t))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


totipo <- ggplot(ipo_marks, aes(x = year)) +
  geom_point(aes(y = tot_w * 10, color = "ipo"), size = 4) +
  geom_line(aes(y = tot_w * 10, color = "ipo"), size = .5) +
  geom_point(aes(y = tot_w_t, color = "else"), size = 4) +
  geom_line(aes(y = tot_w_t, color = "else"), size = .5) +
  labs(x = "Year", y = "Total Market Share (%)", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs (10x scale)",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(text = element_text(size = 20), legend.position = "bottom")



meanipo

swipo

avgwipo

totipo

nipo

varipo



############################################################
############################################################
#     3: early
############################################################
############################################################



############################################################
data_l <- data %>%
mutate(fyear = fyear + 1)

#select relevant variables
data_l <- data_l %>%
  dplyr::select(GVKEY, fyear, MU, sshare, sale, xsga) #nolint

names(data_l) <- c("GVKEY", "fyear", "MU_l", "sshare_l", "sale_l", "xsga_l")

#merge
data <- merge(data, data_l, by = c("GVKEY", "fyear"), all.x = TRUE)


data <- data %>%
mutate(
  sgr = (sshare - sshare_l) / sshare_l,
  mugr = (MU - MU_l) / abs(MU_l - 1),
  fc_s_gr = (xsga / sale) - (xsga_l / sale_l)
  )

#save means
mean_ms <- mean(data$sshare, na.rm = TRUE)
mean_mu <- mean(data$MU - 1, na.rm = TRUE)
mean_swmu <- weighted.mean(data$MU - 1, data$sale, na.rm = TRUE)
meanmugr <- mean(data$mugr, na.rm = TRUE)
meanfc_s_gr <- mean(data$fc_s_gr, na.rm = TRUE)
meanfc_s <- mean(data$xsga / data$sale, na.rm = TRUE)
meansgr <- mean(data$sgr, na.rm = TRUE)




############################################################


#filter to first dec age
ipos10 <- data %>%
  filter(age < 21) 


ipo_10_marks <- ipos10 %>%
  group_by(age) %>%
  summarise(mu_mean = mean(MU - 1, na.rm = TRUE),
    mu_median = median(MU - 1, na.rm = TRUE),
    mu_sw = weighted.mean(MU - 1, sale, na.rm = TRUE),
    mu_var = var(MU - 1, na.rm = TRUE),
    avg_w = mean(sshare, na.rm = TRUE),
    tot_w = sum(sshare, na.rm = TRUE),
    avg_gr = mean(sgr, na.rm = TRUE),
    adv_MU_gr = mean(mugr, na.rm = TRUE),
    fc_sale = mean(xsga / sale, na.rm = TRUE),
    avg_fc_s_gr = mean(fc_s_gr, na.rm = TRUE),
    n = n()
  )

marks10 <- ggplot(ipo_10_marks, aes(x = age, y = mu_mean)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = mean_mu,
                 linetype = "Full sample average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Mean Markup") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$mu_mean))) +
  scale_linetype_manual(values = c("Full sample average" = "dashed")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(linetype = guide_legend(title = NULL))


marksmed10 <- ggplot(ipo_10_marks, aes(x = age, y = mu_median)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  labs(x = "Years Since IPO", y = "Median Markup") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$mu_median))) +
  theme(text = element_text(size = 20))

marksw10 <- ggplot(ipo_10_marks, aes(x = age, y = mu_sw)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = mean_swmu,
                 linetype = "Full sample sales weighted average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Sales Weighted Markup") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$mu_sw))) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_linetype_manual(values =
                        c("Full sample sales weighted average" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))







FC_sale10 <- ggplot(ipo_10_marks, aes(x = age, y = fc_sale)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = meanfc_s,
                 linetype = "Full sample average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Mean Fixed Cost / Revenue") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$fc_sale))) +
  scale_linetype_manual(values = c("Full sample average" = "dashed")) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(linetype = guide_legend(title = NULL))











w10 <- ggplot(ipo_10_marks, aes(x = age, y = avg_w)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = mean_ms,
                 linetype = "Full sample Average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Avererage Market Share (%)") +
  coord_cartesian(ylim = c(0, mean_ms)) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_linetype_manual(values =
                        c("Full sample Average" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))


gr10 <- ggplot(ipo_10_marks, aes(x = age, y = avg_gr * 100)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = meansgr* 100,
                 linetype = "Full sample Average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Avererage Market Share Growth Rate (%)") +
  coord_cartesian(ylim = c(0, max(ipo_10_marks$avg_gr * 100))) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_linetype_manual(values =
                        c("Full sample Average" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))

MUgr10 <- ggplot(ipo_10_marks, aes(x = age, y = adv_MU_gr * 100)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = meanmugr * 100,
                 linetype = "Full sample Average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", y = "Avererage Markup Ratio Growth Rate(%)") +
  coord_cartesian(ylim = c(min(ipo_10_marks$adv_MU_gr * 100), 
                           max(ipo_10_marks$adv_MU_gr * 100))) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_linetype_manual(values =
                        c("Full sample Average" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))








FCSgr10 <- ggplot(ipo_10_marks, aes(x = age, y = avg_fc_s_gr * 100)) +
  geom_point(size = 4) +
  geom_line(size = .5) +
  geom_hline(aes(yintercept = meanfc_s_gr* 100,
                 linetype = "Full sample Average"),
             show.legend = TRUE) +
  labs(x = "Years Since IPO", 
       y = "Avererage Growth Rate of Fixed Cost/ Revenue (%)") +
  coord_cartesian(ylim = c(min(ipo_10_marks$avg_fc_s_gr * 100), 
                           max(ipo_10_marks$avg_fc_s_gr * 100))) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_linetype_manual(values =
                        c("Full sample Average" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))







marks10

marksmed10

marksw10

w10

gr10

MUgr10


FC_sale10

FCSgr10
############################################################
############################################################
#     4: decade
############################################################
############################################################

#generate decade variables

ipo_dec <- data %>%
  group_by(decade) %>% summarise( #nolint
  mu_mean = mean(ifelse(ipo == 1,
                        MU - 1, NA), na.rm = TRUE),
  mu_median = median(ifelse(ipo == 1,
                            MU - 1, NA), na.rm = TRUE),
  mu_sw = weighted.mean(ifelse(ipo == 1,
                               MU - 1, NA), sale, na.rm = TRUE),
  mu_var = var(ifelse(ipo == 1,
                      MU - 1, NA), na.rm = TRUE),
  avg_w = mean(ifelse(ipo == 1,
                      sshare, NA), na.rm = TRUE),
  tot_w = sum(sshare * ipo, na.rm = TRUE),
  n = sum(ipo == 1),
  n_t = n() - sum(ipo == 1),
  mu_mean_t = mean(ifelse(ipo == 0,
                          MU - 1, NA), na.rm = TRUE),
  mu_median_t = median(ifelse(ipo == 0,
                              MU - 1, NA), na.rm = TRUE),
  mu_sw_t = weighted.mean(ifelse(ipo == 0,
                                 MU - 1, NA), sale, na.rm = TRUE),
  mu_var_t = var(ifelse(ipo == 0,
                        MU - 1, NA), na.rm = TRUE),
  avg_w_t = mean(ifelse(ipo == 0,
                        sshare, NA), na.rm = TRUE),
  tot_w_t = sum(ifelse(ipo == 0,
                       sshare, NA), na.rm = TRUE)
  )

decmu <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = mu_mean, color = "mu_mean"), size = 4) +
  geom_line(aes(y = mu_mean, color = "mu_mean"), size = .5) +
  geom_point(aes(y = mu_mean_t, color = "mu_mean_t"), size = 4) +
  geom_line(aes(y = mu_mean_t, color = "mu_mean_t"), size = .5) +
  labs(x = "Decade", y = "Average Markup", color = " ") +
  scale_color_manual(values = c("mu_mean" = "blue", "mu_mean_t" = "red"),
                     labels = c("mu_mean" = "IPOs",
                                "mu_mean_t" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$mu_mean))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


decmumed <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = mu_median, color = "ipo"), size = 4) +
  geom_line(aes(y = mu_median, color = "ipo"), size = .5) +
  geom_point(aes(y = mu_median_t, color = "else"), size = 4) +
  geom_line(aes(y = mu_median_t, color = "else"), size = .5) +
  labs(x = "Decade", y = "Median Markup", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$mu_median))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


decmusw <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = mu_sw, color = "mu_mean"), size = 4) +
  geom_line(aes(y = mu_sw, color = "mu_mean"), size = .5) +
  geom_point(aes(y = mu_sw_t, color = "mu_mean_t"), size = 4) +
  geom_line(aes(y = mu_sw_t, color = "mu_mean_t"), size = .5) +
  labs(x = "Decade", y = "Sales Weighted Markup", color = " ") +
  scale_color_manual(values = c("mu_mean" = "blue", "mu_mean_t" = "red"),
                     labels = c("mu_mean" = "IPOs",
                                "mu_mean_t" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$mu_sw))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


#make n average ipos per year
ipo_dec$num <- ipo_dec$n / 10
ipo_dec$num_t <- ipo_dec$n_t / 10
# data has only 5 years in 1950s, 3 in 2020s
ipo_dec$num[1] <- ipo_dec$n[1] / 5
ipo_dec$num_t[1] <- ipo_dec$n_t[1] / 5
ipo_dec$num[8] <- ipo_dec$n[8] / 3
ipo_dec$num_t[8] <- ipo_dec$n_t[8] / 3



decn <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = num * 10, color = "ipo"), size = 4) +
  geom_line(aes(y = num * 10, color = "ipo"), size = .5) +
  geom_point(aes(y = num_t, color = "else"), size = 4) +
  geom_line(aes(y = num_t, color = "else"), size = .5) +
  labs(x = "Decade", y = "Average Number of Firms Per Year", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs(10x Scale)",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$num_t))) +
  theme(text = element_text(size = 20), legend.position = "bottom")

decw <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = avg_w, color = "ipo"), size = 4) +
  geom_line(aes(y = avg_w, color = "ipo"), size = .5) +
  geom_point(aes(y = avg_w_t, color = "else"), size = 4) +
  geom_line(aes(y = avg_w_t, color = "else"), size = .5) +
  labs(x = "Decade", y = "Average Market Share (%)", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, max(ipo_dec$avg_w_t))) +
  theme(text = element_text(size = 20), legend.position = "bottom")


decvar <- ggplot(ipo_dec, aes(x = decade)) +
  geom_point(aes(y = mu_var, color = "ipo"), size = 4) +
  geom_line(aes(y = mu_var, color = "ipo"), size = .5) +
  geom_point(aes(y = mu_var_t, color = "else"), size = 4) +
  geom_line(aes(y = mu_var_t, color = "else"), size = .5) +
  labs(x = "Decade", y = "Markup Variance", color = " ") +
  scale_color_manual(values = c("ipo" = "blue", "else" = "red"),
                     labels = c("ipo" = "IPOs",
                                "else" = "Other")) +
  coord_cartesian(ylim = c(0, 3)) +
  theme(text = element_text(size = 20), legend.position = "bottom")


decmu

decmusw

decn

decw

decvar

############################################################
############################################################
#     4: decade age
############################################################
############################################################

ipo_agedec <- ipos10 %>%
  group_by(decade, age) %>%
  summarise(
    mu_mean = mean(MU - 1, na.rm = TRUE),
    mu_median = median(MU - 1, na.rm = TRUE),
    mu_sw = weighted.mean(MU - 1, sale, na.rm = TRUE),
    mu_var = var(MU - 1, na.rm = TRUE),
    avg_w = mean(sshare, na.rm = TRUE),
    n = n()
  )

#filter to post 1950
ipo_agedec <- ipo_agedec %>%
  filter(decade > 1950)

# Create the line plot
dage_mu <-
  ggplot(ipo_agedec, aes(x = age, y = mu_mean, color = as.factor(decade))) +
  geom_line() +
  labs(x = "Age", y = "Markup", color = "Decade") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_color_discrete(name = "Decade") +
  coord_cartesian(ylim = c(0, max(ipo_agedec$mu_mean)))


# Create the line plot
dage_w <-
  ggplot(ipo_agedec, aes(x = age, y = avg_w, color = as.factor(decade))) +
  geom_line() +
  labs(x = "Age", y = "Average Market Share (%)", color = "Decade") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_color_discrete(name = "Decade") +
  coord_cartesian(ylim = c(0, max(ipo_agedec$avg_w)))

dage_mu

dage_w


############################################################
############################################################
#     4: sector age
############################################################
############################################################

#break up long industry names to 2 lines
ipos10$industry <- str_wrap(ipos10$industry, width = 40)


ipo_sdec <- ipos10 %>%
  group_by(industry, age) %>%
  summarise(
    mu_mean = mean(MU - 1, na.rm = TRUE),
    mu_median = median(MU - 1, na.rm = TRUE),
    mu_sw = weighted.mean(MU - 1, sale, na.rm = TRUE),
    mu_var = var(MU - 1, na.rm = TRUE),
    avg_w = mean(sshare, na.rm = TRUE),
    n = n()
  )

dages_mu <-
  ggplot(ipo_sdec, aes(x = age, y = mu_mean, color = as.factor(industry))) +
  geom_line() +
  labs(x = "Age", y = "Markup", color = "Decade") +
  theme(text = element_text(size = 20), legend.position = "none") +
  scale_color_discrete(name = "Sector",
                       guide = guide_legend(ncol = 1)) +
  coord_cartesian(ylim = c(0, max(ipo_sdec$mu_mean)))




# Create the line plot
dages_w <-
  ggplot(ipo_sdec, aes(x = age, y = avg_w, color = as.factor(industry))) +
  geom_line() +
  labs(x = "Age", y = "Average Market Share", color = "Decade") +
  theme(text = element_text(size = 20), legend.position = "none") +
  scale_color_discrete(name = "Sector",
                       guide = guide_legend(ncol = 1)) +
  coord_cartesian(ylim = c(0, max(ipo_sdec$avg_w)))

dages_mu

dages_w



# get legend
legendgrab <-
  ggplot(ipo_sdec, aes(x = age, y = mu_mean, color = as.factor(industry))) +
  geom_line() +
  theme(text = element_text(size = 35)) +
  scale_color_discrete(name = "Sector",
                       guide = guide_legend(ncol = 1))


my_legend <- get_legend(legendgrab)





############################################################
############################################################
#     5: raw
############################################################
############################################################
#select interesting vars
ipos$Markup <- ipos$MU - 1

ipos <- ipos %>%
  dplyr::select(Markup, conm, industry, year, sale) #nolint

#sort by MU
ipos <- ipos %>%
  arrange(desc(Markup))

names(ipos) <- c("Markup", "Company", "Industry", "Year", "Sales")



############################################################
#filter to 21st century
ipos21 <- ipos %>%
  filter(Year >= 2000)
view(ipos21)



# Add a column for the ranking
ipos21 <- ipos21 %>%
  mutate(Rank = row_number(desc(Markup)))
#reorder to put rank on far left
ipos21 <- ipos21 %>%
  dplyr::select(Rank, everything())

# Select the top 5 and bottom 5 firms by markup
top_bottom_5 <- ipos21 %>% 
  slice(c(head(row_number(), 10), tail(row_number(), 10)))

# Convert the data frame to LaTeX code
top_bottom_5_latex <- xtable(top_bottom_5)

# Print the LaTeX code
print(top_bottom_5_latex, type = "latex", include.rownames = FALSE)


############################################################
#filter 1980 - 2000
ipos1980 <- ipos %>%
  filter(Year >= 1980, Year < 2000)

view(ipos1980)

# Add a column for the ranking
ipos1980 <- ipos1980 %>%
  mutate(Rank = row_number(desc(Markup)))
#reorder to put rank on far left
ipos1980 <- ipos1980 %>%
  dplyr::select(Rank, everything())

# Select the top 5 and bottom 5 firms by markup
top_bottom_5 <- ipos1980 %>% 
  slice(c(head(row_number(), 10), tail(row_number(), 10)))

# Convert the data frame to LaTeX code
top_bottom_5_latex <- xtable(top_bottom_5)

# Print the LaTeX code
print(top_bottom_5_latex, type = "latex", include.rownames = FALSE)


############################################################
#filter 1980 - 2000
ipos1960 <- ipos %>%
  filter(Year < 1980)

view(ipos1960)

# Add a column for the ranking
ipos1960 <- ipos1960 %>%
  mutate(Rank = row_number(desc(Markup)))
#reorder to put rank on far left
ipos1960 <- ipos1960 %>%
  dplyr::select(Rank, everything())

# Select the top 5 and bottom 5 firms by markup
top_bottom_5 <- ipos1960 %>%
  slice(c(head(row_number(), 10), tail(row_number(), 10)))

# Convert the data frame to LaTeX code
top_bottom_5_latex <- xtable(top_bottom_5)

# Print the LaTeX code
print(top_bottom_5_latex, type = "latex", include.rownames = FALSE)





############################################################
############################################################
#     6: Save Plots
############################################################
############################################################

#navigate to latex
setwd(dircs[3])
#ipo x year
pdf("ipoxyear.pdf", width = 20, height = 14)
grid.arrange(meanipo, swipo, avgwipo, nipo,
             ncol = 2)
dev.off()


pdf("ipoxyear.pdf", width = 20, height = 10)
grid.arrange(meanipo, avgwipo,
             ncol = 2)
dev.off()


#ipos var
pdf("ipovar.pdf", width = 20, height = 14)
grid.arrange(varipo, decvar,
             ncol = 2)
dev.off()


#ipo x decade
pdf("ipoxdecade.pdf", width = 20, height = 14)
grid.arrange(decmu, decmusw,
             decn, decw,
             ncol = 2)
dev.off()


#ipo x decade
pdf("ipoxdecade.pdf", width = 20, height = 10)
grid.arrange(decmu, decw,
             ncol = 2)
dev.off()

#ipo densities
pdf("ipo_density.pdf", width = 20, height = 14)
density_comp_years
dev.off()

#ipo densities decades
pdf("ipo_density_dec.pdf", width = 20, height = 14)
density_comp_decs
dev.off()



#age
pdf("ipoxage.pdf", width = 20, height = 14)
grid.arrange(marks10, w10,
             MUgr10, gr10,
             ncol = 2)
dev.off()


pdf("youngmarks.pdf", width = 20, height = 14)
grid.arrange(marks10, MUgr10,
             ncol = 2)
dev.off()

pdf("younggrowth.pdf", width = 20, height = 14)
grid.arrange(w10, gr10,
             ncol = 2)
dev.off()




pdf("ys_ipo.pdf", width = 20, height = 14)
grid.arrange(marks10, w10,
             ncol = 2)
dev.off()




#decade x age
pdf("decadexage.pdf", width = 20, height = 14)
grid.arrange(dage_mu, dage_w,
             ncol = 2)
dev.off()



#decade x sector
pdf("sectorxage.pdf", width = 24, height = 14)
grid.arrange(dages_mu, dages_w, my_legend,
             ncol = 3)
dev.off()




############################################################
############################################################
#     8: Break even
############################################################
############################################################


# Pi(PY) = YP(1-1/mu)-FC | FC, mu
#BE \equiv PY(BE)=0
#BE = (\mu)/(\mu-1) *FC
#ES = PY/BE

data <- data %>%
  mutate(
    BE = (MU) / ( MU - 1) * xsga,
    ES =  sale / BE
  )

summary(data$BE)

summary(data$ES)


es_age <- data %>%
  group_by(age) %>%
  summarise(
    es_mean = mean(ES, na.rm = TRUE),
    BE_mean = mean(BE, na.rm = TRUE),
    es_median = median(ES, na.rm = TRUE),
    BE_median = median(BE, na.rm = TRUE),
    fc_s_mean = mean(xsga / sale, na.rm = TRUE),
    fc_s_median = median(xsga / sale, na.rm = TRUE)
  )


plot(es_age$age, es_age$fc_s_median)

plot(es_age$age, es_age$fc_s_mean)

plot(es_age$age, es_age$es_mean)


#plot of average against es median
ggplot(es_age, aes(x = age, y = es_median)) +
  geom_line() +
  labs(x = "Age", y = "Median ES") +
  lims(x = c(0, 50)) +
  coord_cartesian(ylim = c(.9, 1.4)) +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  scale_color_discrete(name = "Decade")




es_age_dec <- data %>%
  group_by(age, decade_e) %>%
  summarise(
    es_mean = mean(ES, na.rm = TRUE),
    BE_mean = mean(BE, na.rm = TRUE),
    es_median = median(ES, na.rm = TRUE),
    BE_median = median(BE, na.rm = TRUE),
    FC_S_mean = mean(xsga / sale, na.rm = TRUE),
    FC_S_median = median(xsga / sale, na.rm = TRUE),
    Prof_mean = mean(sale - xsga - sale / MU, na.rm = TRUE),
    Prof_median = median(sale - xsga - sale / MU, na.rm = TRUE),
    Prof_r_mean = mean(sale / (sale / MU + xsga) - 1, na.rm = TRUE),
    Prof_r_median = median(sale / (sale / MU + xsga) - 1, na.rm = TRUE),
    MU_mean = mean(MU, na.rm = TRUE),
    MU_median = median(MU, na.rm = TRUE),
    sshare_mean = mean(sshare, na.rm = TRUE),
    sshare_median = median(sshare, na.rm = TRUE)
  )
#remove Na decade
es_age_dec <- es_age_dec %>%
  filter(!is.na(decade_e))


#age vs efficent scale by decade
es_mean <-
  ggplot(es_age_dec, aes(x = age, y = es_median, color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Median Break Even Scale = Actual Revenue)/(Break Even Revenue) ",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  coord_cartesian(ylim = c(0.5, 3.5)) +
  theme(text = element_text(size = 17), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")
es_median <-
  ggplot(es_age_dec, aes(x = age, y = es_mean, color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Average Break Even Scale = Actual Revenue)/(Break Even Revenue) ",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  coord_cartesian(ylim = c(0.5, 3.5)) +
  theme(text = element_text(size = 17), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")


#age vs fixed costs / revenue by decade
fc_s_mean <-
  ggplot(es_age_dec, aes(x = age, y = FC_S_mean, color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Average Fixed Cost / Revenue",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  coord_cartesian(ylim = c(0, 3.5)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")
fc_s_median <-
  ggplot(es_age_dec, aes(x = age,
                         y = FC_S_median,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Median Fixed Cost / Revenue",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  coord_cartesian(ylim = c(0, 3.5)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")


#age vs PI/Sale
pr_mean <-
  ggplot(es_age_dec, aes(x = age, y = Prof_r_mean,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Mean Profit Ratio =  (1 - Total Cost/Revenue)",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(-.2, .25)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")
pr_median <-
  ggplot(es_age_dec, aes(x = age, y = Prof_r_median,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Median Profit Ratio =  (1 - Total Cost/Revenue)",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(-.2, .25)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")


#age vs total profit
pi_mean <-
  ggplot(es_age_dec, aes(x = age, y = Prof_mean, color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Mean Profits (GDP Deflated to 2020)",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(-250, 1000000)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")

pi_median <-
  ggplot(es_age_dec, aes(x = age,
                         y = Prof_median,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Median Profits (GDP Deflated to 2020)",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(-2500, 100000)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")


#plot of age against es median, with different decades in different colors
mu_mean <-
  ggplot(es_age_dec, aes(x = age,
                         y = MU_mean - 1,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age", y = "Mean Markup Ratio (p/mc-1)", color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(0, 1.25)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")
mu_median <-
  ggplot(es_age_dec, aes(x = age,
                         y = MU_median-1,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Median Markup Ratio (p/mc-1)",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(0, 1.25)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")



#plot of age against marketshare
sshare_mean <-
  ggplot(es_age_dec, aes(x = age,
                         y = sshare_mean,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age", y = "Mean Market Share", color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(0, .1)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")
sshare_median <-
  ggplot(es_age_dec, aes(x = age,
                         y = sshare_median,
                         color = as.factor(decade_e))) +
  geom_line() +
  labs(x = "Age",
       y = "Median Market Share",
       color = "Decade of Entry") +
  lims(x = c(0, 30)) +
  lims(y = c(0, .1)) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30)) +
  scale_color_discrete(name = "Decade")





###########################################################################
###########################################################################

#save plots
setwd(dircs[3])

pdf("efficentscale.pdf", width = 20, height = 14)
grid.arrange(es_mean, es_median,
             ncol = 2)
dev.off()


pdf("fixedcostratio.pdf", width = 20, height = 14)
grid.arrange(fc_s_mean, fc_s_median,
             ncol = 2)
dev.off()


pdf("profitratio.pdf", width = 20, height = 14)
grid.arrange(pr_mean, pr_median,
             ncol = 2)
dev.off()

pdf("profits.pdf", width = 20, height = 14)
grid.arrange(pi_mean, pi_median,
             ncol = 2)
dev.off()


pdf("markupage_dec.pdf", width = 20, height = 14)
grid.arrange(mu_mean, mu_median,
             ncol = 2)
dev.off()


pdf("sshare_decade.pdf", width = 20, height = 14)
grid.arrange(sshare_mean, sshare_median,
             ncol = 2)
dev.off()









###################################################################
###################################################################
# By sector by decade

data_na <- data %>%
  filter(!is.na(industry))

ind_names <- unique(data_na$industry)

for(i in seq_along(ind_names)){
  data_temp <- data_na %>%
    filter(industry == ind_names[i])

  temp_data <- data_temp %>%
    group_by(age, decade_e) %>%
    summarise(
      MU_mean = mean(MU, na.rm = TRUE),
      MU_median = median(MU, na.rm = TRUE)
    )
  #remove Na decade
  temp_data <- temp_data %>%
    filter(!is.na(decade_e))
  #save plots

  mu_mean_t <-
    ggplot(temp_data, aes(x = age,
                          y = MU_mean - 1,
                          color = as.factor(decade_e))) +
    geom_line() +
    labs(x = "Age", y = "Mean Markup Ratio (p/mc-1)",
         color = "Decade of Entry") +
    lims(x = c(0, 30)) +
    lims(y = c(0, 1.25)) +
    theme(text = element_text(size = 30), legend.position = "bottom",
          legend.text = element_text(size = 30)) +
    scale_color_discrete(name = "Decade")

  mu_median_t <-
    ggplot(temp_data, aes(x = age,
                          y = MU_median - 1,
                          color = as.factor(decade_e))) +
    geom_line() +
    labs(x = "Age",
         y = "Median Markup Ratio (p/mc-1)",
         color = "Decade of Entry") +
    lims(x = c(0, 30)) +
    lims(y = c(0, 1.25)) +
    theme(text = element_text(size = 30), legend.position = "bottom",
          legend.text = element_text(size = 30)) +
    scale_color_discrete(name = "Decade")

    # Save plots to lists
  mu_mean_plots[[ind_names[i]]] <- mu_mean_t
  mu_median_plots[[ind_names[i]]] <- mu_median_t
}























































data_na <- data %>%
  filter(!is.na(industry))

ind_names <- unique(data_na$industry)

for (i in seq_along(ind_names)){

  data_temp <- data_na %>%
    filter(industry == ind_names[i])

  #create new market share (by year)
  data_temp <- data_temp %>%
    group_by(fyear) %>%
      mutate(sshare = sale / sum(sale))

  temp_data <- data_temp %>%
    group_by(age, decade_e) %>%
    summarise(
      ms_mean = mean(sshare, na.rm = TRUE),
      ms_median = median(sshare, na.rm = TRUE)
    )
  #remove Na decade
  temp_data <- temp_data %>%
    filter(!is.na(decade_e))
  #save plots

  hold <- temp_data %>%
    filter(age <= 30)

  maxi = max(
    max(hold$ms_median),
    max(hold$mean))

  mu_mean_t <-
    ggplot(temp_data, aes(x = age,
                          y = ms_mean,
                          color = as.factor(decade_e))) +
    geom_line() +
    labs(x = "Age", y = "Mean Market Share",
         color = "Decade of Entry") +
    lims(x = c(0, 30)) +
    lims(y = c(0, maxi)) +
    theme(text = element_text(size = 30), legend.position = "bottom",
          legend.text = element_text(size = 30)) +
    scale_color_discrete(name = "Decade")

  mu_median_t <-
    ggplot(temp_data, aes(x = age,
                          y = ms_median,
                          color = as.factor(decade_e))) +
    geom_line() +
    labs(x = "Age",
         y = "Median Market Share",
         color = "Decade of Entry") +
    lims(x = c(0, 30)) +
    lims(y = c(0, maxi))+
    theme(text = element_text(size = 30), legend.position = "bottom",
          legend.text = element_text(size = 30)) +
    scale_color_discrete(name = "Decade")

  setwd(dircs[3])
  pdf(paste("dec_ms_", ind_names[i], ".pdf", sep = ""), width = 24, height = 12)
  grid.arrange(mu_mean_t, mu_median_t,
               ncol = 2,
               top = textGrob(paste(ind_names[i]),
                              gp = gpar(fontsize = 35, font = 3)))
  dev.off()
}
















###########################################################
######### predicting survivability
#########################################################


ipos <- data %>%
  filter(ipo == 1)


#generate by year markup and fixed cost deciles
ipos <- ipos %>%
  group_by(fyear) %>%
  mutate(MU_dec = ntile(MU, 10),
         xsga_dec = ntile(xsga / sale, 10),
         ms_dec = ntile(sshare, 10)) %>%
  ungroup()

#generate 2, 5, 10, 15, 20 surivial
  #for each survival s generate 0 if life <s, NA if life = NA and 2023 - fyear<s, and 1 else #nolint
ipos <- ipos %>%
  mutate(
    surv_1 = ifelse(2023 - fyear < 1, NA,
                    ifelse(is.na(life), 1,
                           ifelse(life < 1, 0, 1))),
    surv_5 = ifelse(2023 - fyear < 5, NA,
                    ifelse(is.na(life), 1,
                           ifelse(life < 5, 0, 1))),
    surv_10 = ifelse(2023 - fyear < 10, NA,
                     ifelse(is.na(life), 1,
                            ifelse(life < 10, 0, 1))),
    surv_15 = ifelse(2023 - fyear < 15, NA,
                     ifelse(is.na(life), 1,
                            ifelse(life < 15, 0, 1))),
    surv_20 = ifelse(2023 - fyear < 20, NA,
                     ifelse(is.na(life), 1,
                            ifelse(life < 20, 0, 1)))
  )


#generate average survival by decade and decile
ipos_s_mu <- ipos %>%
  group_by(MU_dec) %>%
  summarise(
    sr_mu_1 = mean(surv_1, na.rm = TRUE),
    sr_mu_5 = mean(surv_5, na.rm = TRUE),
    sr_mu_10 = mean(surv_10, na.rm = TRUE),
    sr_mu_15 = mean(surv_15, na.rm = TRUE),
    sr_mu_20 = mean(surv_20, na.rm = TRUE),
    obs = n(),
    .groups = "drop"
  ) %>%
  ungroup()

view(ipos_s_mu)


# Reshape the data to long format
ipos_s_mu_long <- ipos_s_mu %>%
  pivot_longer(
    cols = starts_with("sr_mu"),
    names_to = "survival_period",
    values_to = "survival_rate"
  )

# Create the histogram
sr_mu <-
ggplot(ipos_s_mu_long,
       aes(x = MU_dec,
           y = survival_rate, fill = survival_period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Survival Rates by Markup Decile",
    x = "Survival Rates by Markup Decile (Within Years)",
    y = "Fraction of Firms Surviving to Year",
    fill = "Year"
  ) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30))



















#generate average survival by decade and decile
ipos_s_fc <- ipos %>%
  group_by(xsga_dec) %>%
  summarise(
    sr_mu_1 = mean(surv_1, na.rm = TRUE),
    sr_mu_5 = mean(surv_5, na.rm = TRUE),
    sr_mu_10 = mean(surv_10, na.rm = TRUE),
    sr_mu_15 = mean(surv_15, na.rm = TRUE),
    sr_mu_20 = mean(surv_20, na.rm = TRUE),
    obs = n(),
    .groups = "drop"
  ) %>%
  ungroup()



# Reshape the data to long format
ipos_s_fc <- ipos_s_fc %>%
  pivot_longer(
    cols = starts_with("sr_mu"),
    names_to = "survival_period",
    values_to = "survival_rate"
  )

# Create the histogram
s_r_fc <-
ggplot(ipos_s_fc,
       aes(x = xsga_dec,
           y = survival_rate, fill = survival_period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Survival Rates by Fixed Cost/ Sales Decile",
    x = "Survival Rates by  Fixed Cost/ Sales Decile (Within Years)",
    y = "Fraction of Firms Surviving to Year",
    fill = "Year"
  ) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30))




















#generate average survival by decade and decile
ipos_s_ms <- ipos %>%
  group_by(ms_dec) %>%
  summarise(
    sr_mu_1 = mean(surv_1, na.rm = TRUE),
    sr_mu_5 = mean(surv_5, na.rm = TRUE),
    sr_mu_10 = mean(surv_10, na.rm = TRUE),
    sr_mu_15 = mean(surv_15, na.rm = TRUE),
    sr_mu_20 = mean(surv_20, na.rm = TRUE),
    obs = n(),
    .groups = "drop"
  ) %>%
  ungroup()



# Reshape the data to long format
ipos_s_ms <- ipos_s_ms %>%
  pivot_longer(
    cols = starts_with("sr_mu"),
    names_to = "survival_period",
    values_to = "survival_rate"
  )

# Create the histogram
sr_ms <-
ggplot(ipos_s_ms,
       aes(x = ms_dec,
           y = survival_rate, fill = survival_period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Survival Rates by Market Share Decile",
    x = "Survival Rates by   Market Share Decile (Within Years)",
    y = "Fraction of Firms Surviving to Year",
    fill = "Year"
  ) +
  theme(text = element_text(size = 30), legend.position = "bottom",
        legend.text = element_text(size = 30))






sr_mu
s_r_fc
sr_ms



#save plots
setwd(dircs[3])

pdf("sr_mu.pdf", width = 20, height = 14)
grid.arrange(sr_mu)
dev.off()



pdf("s_r_fc.pdf", width = 20, height = 14)
grid.arrange(s_r_fc)
dev.off()

pdf("sr_ms.pdf", width = 20, height = 14)
grid.arrange(sr_ms)
dev.off()












###################################################################
###################################################################
# By sector by decade







































view(ipos_s_mu)


#generate average survival by decade and decile
ipos_s_xsga <- ipos %>%
  group_by(xsga_dec) %>%
  summarise(
    sr_mu_1 = mean(surv_1, na.rm = TRUE),
    sr_mu_5 = mean(surv_5, na.rm = TRUE),
    sr_mu_10 = mean(surv_10, na.rm = TRUE),
    sr_mu_15 = mean(surv_15, na.rm = TRUE),
    sr_mu_20 = mean(surv_20, na.rm = TRUE),
    obs = n(),
    .groups = "drop"
  ) %>%
  ungroup()


view(ipos_s_xsga)


#generate average survival by decade and decile
ipos_s_j <- ipos %>%
  group_by(xsga_dec, MU_dec) %>%
  summarise(
    sr_mu_1 = mean(surv_1, na.rm = TRUE),
    sr_mu_5 = mean(surv_5, na.rm = TRUE),
    sr_mu_10 = mean(surv_10, na.rm = TRUE),
    sr_mu_15 = mean(surv_15, na.rm = TRUE),
    sr_mu_20 = mean(surv_20, na.rm = TRUE),
    obs = n(),
    .groups = "drop"
  ) %>%
  ungroup()


view(ipos_s_j)




#generate average survival by decade and decile
ipos_s <- ipos %>%
  group_by(decade_e, xsga_dec, MU_dec) %>%
  summarise(
    sr_mu_1 = mean(surv_1, na.rm = TRUE),
    sr_mu_5 = mean(surv_5, na.rm = TRUE),
    sr_mu_10 = mean(surv_10, na.rm = TRUE),
    sr_mu_15 = mean(surv_15, na.rm = TRUE),
    sr_mu_20 = mean(surv_20, na.rm = TRUE),
    obs = n(),
    .groups = "drop"
  ) %>%
  ungroup()

view(ipos_dec)






#generate average survival by decade and decile
ipos_dec_j <- ipos %>%
  group_by(decade_e) %>%
  group_by(MU_dec, xsga_dec) %>%
  summarise(
    sr_j_1 = mean(surv_1, na.rm = TRUE),
    sr_j_5 = mean(surv_5, na.rm = TRUE),
    sr_j_10 = mean(surv_10, na.rm = TRUE),
    sr_j_15 = mean(surv_15, na.rm = TRUE),
    sr_j_20 = mean(surv_20, na.rm = TRUE),
    obs = n()
  ) %>%
  ungroup()

view(ipos_dec)

















ipos <- ipos %>%
  group_by(age, decade_e) %>%
  summarise(
    es_mean = mean(ES, na.rm = TRUE),
    BE_mean = mean(BE, na.rm = TRUE),
    es_median = median(ES, na.rm = TRUE),
    BE_median = median(BE, na.rm = TRUE),
    FC_S_mean = mean(xsga / sale, na.rm = TRUE),
    FC_S_median = median(xsga / sale, na.rm = TRUE),
    Prof_mean = mean(sale - xsga - sale / MU, na.rm = TRUE),
    Prof_median = median(sale - xsga - sale / MU, na.rm = TRUE),
    Prof_r_mean = mean(sale / (sale / MU + xsga) - 1, na.rm = TRUE),
    Prof_r_median = median(sale / (sale / MU + xsga) - 1, na.rm = TRUE),
    MU_mean = mean(MU, na.rm = TRUE),
    MU_median = median(MU, na.rm = TRUE),
    sshare_mean = mean(sshare, na.rm = TRUE),
    sshare_median = median(sshare, na.rm = TRUE)
  )











###########################################################
######### reg
#########################################################

#generate variable if last year in data
data <- data %>%
  group_by(GVKEY) %>%
  mutate(last = ifelse(fyear == max(fyear), 1, 0)) %>%
  ungroup()


# Generate 10-year age indicator as a single variable
data <- data %>%
  mutate(age_in = case_when(
    age <= 5 ~ " 0-5",
    age <= 10 ~ " 6-10",
    age <= 20 ~ " 11-20",
    age <= 30 ~ " 21-30",
    age > 40 ~ " 40+",
    TRUE ~ "31-40"  # Assuming ages between 31 and 40 should be grouped here
  ))

######### regs
model_be1 <- feols(log(ES) ~ t +
                     i(age_in, age) +  I(age * t)  - 1 |
                     industry + age_in,
                   cluster = "GVKEY",
                   data = data)
# add last indicator
model_be2 <- feols(log(ES) ~ t + last +
                     i(age_in, age) +  I(age * t) + I(age * t) + I(age * last)
                   - 1 |
                     industry + age_in,
                   cluster = "GVKEY",
                   data = data)
# add years left in sample
model_be3 <- feols(log(ES) ~ t + life +
                     i(age_in, age) +  I(age * t) + I(age * life)  - 1 |
                     industry + age_in,
                   cluster = "GVKEY",
                   data = data)



models <- list(" " = model_be1,
               " " = model_be2,
               " " = model_be3)


#change names
new_names <- c(
  "age_in:: 0-5:age" = "Years Since IPO x i[YSI:0-5]",
  "age_in:: 6-10:age" = "Years Since IPO x i[YSI:6-10]",
  "age_in:: 11-20:age" = "Years Since IPO x i[YSI:11-20]",
  "age_in:: 21-30:age" = "Years Since IPO x i[YSI:21-30]",
  "age_in:: 31-40:age" = "Years Since IPO x i[YSI:31-40]",
  "age_in:: 40+:age" = "Years Since IPO x i[YSI:>40]",
  "t" = "Time",
  "I(age * t)" = "Years Since IPO x Time",
  "last" = "Last Year in Sample",
  "I(age * last)" = "Years Since IPO x Last Year in Sample",
  "life" = "Years To Exit",
  "I(age * life)" = "Years Since IPO x Years To Exit",
  "GVKEY" = "Firm level",
  "industry" = "Industry",
  "age_in" = "Years Since IPO Buckets"
)
# Specify the order
age_order <- c("Years Since IPO x i\\[YSI:0-5\\]",
               "Years Since IPO x i\\[YSI:6-10\\]",
               "Years Since IPO x i\\[YSI:11-20\\]",
               "Years Since IPO x i\\[YSI:21-30\\]",
               "Years Since IPO x i\\[YSI:31-40\\]",
               "Years Since IPO x i\\[YSI:>40\\]")
coef_order <- c("!Last Year in Sample", "!Years To Exit",
                "Time",
                age_order)

# Display the table with coefficients in the specified order
etable(models, order = coef_order, dict = new_names)