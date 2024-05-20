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
#     2: Load and clean data
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
                         soc, weight, w_l, MU_l, MU_deu_l,
                         age, young, old, unvar)


############################################################
############################################################
#     2: aggs
############################################################
############################################################


############################################################
################# 2.a: Agg Comparison ######################
############################################################

#create data
agg_markups_nw <- data_mu %>%
  group_by(fyear) %>% # nolint
  summarise(Agg_MU = mean(MU, na.rm = TRUE), # nolint
            Agg_MU_DEU = mean(MU_deu, na.rm = TRUE))


agg_mu_comp_nw <- ggplot(data = agg_markups_nw, aes(x = fyear)) +
  geom_line(aes(y = Agg_MU - 1, colour = "Cost Accounting Markup")) +
  geom_line(aes(y = Agg_MU_DEU - 1, colour = "Production Function Markup")) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Average Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL))

agg_mu_comp_nw

############################################################
################# 2.b: Agg Comparison weighted ######################
############################################################

#create data
agg_markups <- data_mu %>%
  group_by(fyear) %>% # nolint
  summarise(a_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            a_MU_DEU = weighted.mean(MU_deu, sale, na.rm = TRUE))


agg_mu_comp <- ggplot(data = agg_markups, aes(x = fyear)) +
  geom_line(aes(y = a_MU - 1, colour = "Cost Accounting Markup")) +
  geom_line(aes(y = a_MU_DEU - 1, colour = "Production Function Markup")) +
  theme(text = element_text(size = 20)) +
  scale_colour_manual(values = c("Cost Accounting Markup" = "blue",
                                 "Production Function Markup" = "red")) +
  scale_x_continuous(limits = c(1955, 2022)) +
  labs(x = "Year", y = "Sales Weighted Markup ([p-mc]/mc)") +
  theme(text = element_text(size = 20), legend.position = "bottom") +
  guides(colour = guide_legend(title = NULL))

agg_mu_comp


##############################################################
##############################################################
################      decomposition    ################
##############################################################
##############################################################

agg_markups$a_MU_l <- lag(agg_markups$a_MU) # nolint
agg_markups$a_MU_DEU_l <- lag(agg_markups$a_MU_DEU) # nolint

decom <- merge(data_mu, agg_markups, by = "fyear")

decom <- decom %>%
  mutate(dmu = MU - MU_l,
    dmu_deu = MU_deu - MU_deu_l,
    dw = weight - w_l,
    tmu = MU - a_MU_l,
    tmu_deu = MU_deu - a_MU_DEU_l,
    tmup = MU_l - a_MU_l,
    tmup_deu = MU_deu_l - a_MU_DEU_l,
    c = !is.na(MU_l),
    nc = is.na(MU_l),
    g = ifelse(MU > MU_l, 1, 0),
    s = ifelse(MU < MU_l, 1, 0)
  )


decomp_m <- decom %>%
  group_by(fyear) %>% # nolint
  summarise(a_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            a_MU_DEU = weighted.mean(MU_deu, sale, na.rm = TRUE)# nolint
            ,
            within  = sum(w_l * dmu * c, na.rm = TRUE),
            mshare = sum(tmup * dw * c, na.rm = TRUE),
            cross = sum(dmu * dw * c, na.rm = TRUE),
            ent = sum(tmu * weight * nc, na.rm = TRUE)
            ,
            within_deu  = sum(w_l * dmu_deu * c, na.rm = TRUE),
            mshare_deu = sum(tmup_deu * dw * c, na.rm = TRUE),
            cross_deu = sum(dmu_deu * dw * c, na.rm = TRUE),
            ent_deu = sum(tmu_deu * weight * nc, na.rm = TRUE)
  )


decomp_m$a_mu_l <- agg_markups$a_MU_l
decomp_m$a_mu_DEU_l <- agg_markups$a_MU_DEU_l 

hold <- decomp_m %>% dplyr::select(fyear, a_MU, a_mu_l, a_MU_DEU, a_mu_DEU_l)

view(hold)

decomp_m <- decomp_m %>%
  mutate(nent = (a_MU - a_mu_l) - (within + mshare + cross),
    nent_deu = (a_MU_DEU - a_mu_DEU_l) - (within_deu + mshare_deu + cross_deu)
  )



###################################################################
################### plot    ######################################
###################################################################


start <- 1965

decomp_mm <- filter(decomp_m, fyear >= start, fyear < 2023)


vars <- c("within", "mshare", "cross", "nent")
vars_deu <- paste0(vars, "_deu")

decomp_mm <- decomp_mm %>%
  arrange(fyear) %>%
  mutate(across(c(vars, vars_deu), ~ cumsum(.), .names = "c_{.col}"))

startmu <- decomp_mm$a_MU[decomp_mm$fyear == start]
startmud <- decomp_mm$a_MU_DEU[decomp_mm$fyear == start]

decomp_mm <- decomp_mm %>%
  mutate(across(c(vars, vars_deu), ~ cumsum(.), .names = "c_{.col}")) %>%
  mutate(da_MU = a_MU - startmu,
         da_MU_DEU = a_MU_DEU - startmud)

ca_decomp <-
  decomp_mm %>% dplyr::select(fyear, da_MU,
                              c_within, c_mshare, c_cross, c_nent)
# Reshape the data
ca_decomp_long <- ca_decomp %>%
  gather(variable, value, -fyear)

# Define new names
ca_labs <- c("da_MU" = "Change in Markup (Cumulative)",
             "c_within" = "Within Effect (Cumulative)",
             "c_mshare" = "Market Share Effect (Cumulative)",
             "c_cross" = "Cross Effect (Cumulative)",
             "c_nent" = "Net Entry Effect (Cumulative)")

# Define new names
deu_labs <- c("da_MU_DEU" = "Change in Markup (Cumulative)",
              "c_within_deu" = "Within Effect (Cumulative)",
              "c_mshare_deu" = "Market Share Effect (Cumulative)",
              "c_cross_deu" = "Cross Effect (Cumulative)",
              "c_nent_deu" = "Net Entry Effect (Cumulative)")


# Plot the data
ca_dplot <-
  ggplot(ca_decomp_long, aes(x = fyear, y = value, color = variable)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Value", color = " ") +
  scale_color_discrete(labels = ca_labs,  guide = guide_legend(nrow = 2)) +
  theme(text = element_text(size = 20), legend.position = "bottom")

ca_dplot

save_f(ca_dplot, "ca_dplot.pdf", dircs, 12, 9, TRUE)


deu_decomp <-
  decomp_mm %>% dplyr::select(fyear, da_MU_DEU,
                              c_within_deu, c_mshare_deu, c_cross_deu,
                              c_nent_deu)
# Reshape the data
deu_decomp_long <- deu_decomp %>%
  gather(variable, value, -fyear)

# Plot the data
deu_plot <-
  ggplot(deu_decomp_long, aes(x = fyear, y = value, color = variable)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Value", color = " ") +
  scale_color_discrete(labels = deu_labs,  guide = guide_legend(nrow = 2)) +
  theme(text = element_text(size = 20), legend.position = "bottom")

deu_plot

save_f(deu_plot, "deu_plot.pdf", dircs, 12, 9, TRUE)


##############################################################
##############################################################
################      further decomp    ################
##############################################################
##############################################################

decomp_w <- decom %>%
  group_by(fyear) %>% # nolint
  summarise(a_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            a_MU_DEU = weighted.mean(MU_deu, sale, na.rm = TRUE)# nolint
            ,
            mshare = sum(tmup * dw * c, na.rm = TRUE),
            mshare_y = sum(tmup * dw * c * young,
                           na.rm = TRUE),
    mshare_o = sum(tmup * dw * c * old,
                   na.rm = TRUE),
    mshare_unver = sum(tmup * dw * c * unvar,
                       na.rm = TRUE),
    mshare_yg = sum(tmup * dw * c * young * g,
                    na.rm = TRUE),
    mshare_ys = sum(tmup * dw * c * young * s,
                    na.rm = TRUE),
    mshare_og = sum(tmup * dw * c * old * g,
                    na.rm = TRUE),
    mshare_os = sum(tmup * dw * c * old * s,
                    na.rm = TRUE),
  )

decomp_w


start <- 1980

decomp_wt <- filter(decomp_w, fyear >= start, fyear < 2023)

vars <- c("mshare", "mshare_y", "mshare_o", "mshare_unver")

decomp_wt <- decomp_wt %>%
  arrange(fyear) %>%
  mutate(across(vars, ~ cumsum(.), .names = "c_{.col}"))

startmu <- decomp_wt$a_MU[decomp_wt$fyear == start]
startmud <- decomp_wt$a_MU_DEU[decomp_wt$fyear == start]

decomp_wt <- decomp_wt %>%
  mutate(across(vars, ~ cumsum(.), .names = "c_{.col}")) %>%
  mutate(da_MU = a_MU - startmu)

decomp_wt <-
  decomp_wt %>% dplyr::select(fyear, da_MU,
                              c_mshare, c_mshare_y, c_mshare_o)

decomp_wt_long <- decomp_wt %>%
  gather(variable, value, -fyear)

# Define new names
d_labs <- c("da_MU" = "Change in Markup (Cumulative)",
              "c_mshare" = "Total Market Share Effect (Cumulative)",
              "c_mshare_y" = "Young (Age <=10) MSE (Cumulative)",
              "c_mshare_o" = "Old (Age >10) MSE (Cumulative)")


# Plot the data
f_dplot <-
  ggplot(decomp_wt_long, aes(x = fyear, y = value, color = variable)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Value", color = " ") +
  scale_color_discrete(labels = d_labs,  guide = guide_legend(nrow = 2)) +
  theme(text = element_text(size = 20), legend.position = "bottom")

f_dplot

save_f(f_dplot, "f_dplot.pdf", dircs, 12, 9, TRUE)













start <- 1980

decomp_wt <- filter(decomp_w, fyear >= start, fyear < 2023)

vars <- c("mshare", "mshare_yg", "mshare_ys", "mshare_og", "mshare_os", 
          "mshare_unver")

decomp_wt <- decomp_wt %>%
  arrange(fyear) %>%
  mutate(across(vars, ~ cumsum(.), .names = "c_{.col}"))

startmu <- decomp_wt$a_MU[decomp_wt$fyear == start]
startmud <- decomp_wt$a_MU_DEU[decomp_wt$fyear == start]

decomp_wt <- decomp_wt %>%
  mutate(across(vars, ~ cumsum(.), .names = "c_{.col}")) %>%
  mutate(da_MU = a_MU - startmu)

decomp_wt <-
  decomp_wt %>% dplyr::select(fyear, da_MU,
                              c_mshare, c_mshare_yg, c_mshare_ys, c_mshare_og, c_mshare_os)

decomp_wt_long <- decomp_wt %>%
  gather(variable, value, -fyear)

# Define new names
d_labs <- c("da_MU" = "Change in Markup (Cumulative)",
              "c_mshare" = "Total Market Share Effect (Cumulative)",
              "c_mshare_yg" = "Growing Young (Age <=10) MSE (Cumulative)",
              "c_mshare_ys" = "Shrinking Young (Age <=10) MSE (Cumulative)",
              "c_mshare_og" = "Growing Old (Age >10) MSE (Cumulative)",
              "c_mshare_os" = "Shrinking Old (Age >10) MSE (Cumulative)")


# Plot the data
f_dplot2 <-
  ggplot(decomp_wt_long, aes(x = fyear, y = value, color = variable)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Value", color = " ") +
  scale_color_discrete(labels = d_labs,  guide = guide_legend(nrow = 3)) +
  theme(text = element_text(size = 20), legend.position = "bottom")

f_dplot2

save_f(f_dplot2, "f_dplot2.pdf", dircs, 12, 9, TRUE)


















##############################################################
##############################################################
################      young and old   ################
##############################################################
##############################################################

age_share <- decom %>%
  group_by(fyear) %>% # nolint
  summarise(
    share_young = sum(weight * young, na.rm = TRUE),
    share_old = sum(weight * old, na.rm = TRUE),
    #share_unvar = sum(weight * unvar, na.rm = TRUE),
    #share = sum(weight, na.rm = TRUE)
  )

age_share <- age_share %>%
  filter(fyear >= 1965)

age_share_long <- age_share %>%
  gather(variable, value, -fyear)


# Define new names
s_labs <- c("share_young" = "Young Firms (Age =< 10)",
            "share_old" = "Old Firms (Age > 10)")


# Plot the data
age_shareplot <-
  ggplot(age_share_long, aes(x = fyear, y = value, color = variable)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Value", color = "Total Market Share") +
  scale_color_discrete(labels = s_labs,  guide = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 20), legend.position = "bottom")

age_shareplot

save_f(age_shareplot, "age_shareplot.pdf", dircs, 12, 9, TRUE)






















##############################################################
##############################################################
################      further decomp    ################
##############################################################
##############################################################

decomp_w <- decom %>%
  group_by(fyear) %>% # nolint
  summarise(a_MU = weighted.mean(MU, sale, na.rm = TRUE), # nolint
            a_MU_DEU = weighted.mean(MU_deu, sale, na.rm = TRUE)# nolint
            ,
            rea = sum((tmup + dmu) * dw * c, na.rm = TRUE),
            rea_y = sum((tmup + dmu) * dw * c * young,
                        na.rm = TRUE),
    rea_o = sum((tmup + dmu) * dw * c * old,
                na.rm = TRUE),
    rea_unver = sum((tmup + dmu) * dw * c * unvar,
                    na.rm = TRUE)
  )

decomp_w

start <- 1980

decomp_wt <- filter(decomp_w, fyear >= start, fyear < 2023)

vars <- c("rea", "rea_y", "rea_o", "rea_unver")

decomp_wt <- decomp_wt %>%
  arrange(fyear) %>%
  mutate(across(vars, ~ cumsum(.), .names = "c_{.col}"))

startmu <- decomp_wt$a_MU[decomp_wt$fyear == start]
startmud <- decomp_wt$a_MU_DEU[decomp_wt$fyear == start]

decomp_wt <- decomp_wt %>%
  mutate(across(vars, ~ cumsum(.), .names = "c_{.col}")) %>%
  mutate(da_MU = a_MU - startmu)

decomp_wt <-
  decomp_wt %>% dplyr::select(fyear, da_MU,
                              c_rea, c_rea_y, c_rea_o)

decomp_wt_long <- decomp_wt %>%
  gather(variable, value, -fyear)

ggplot(decomp_wt_long, aes(x = fyear, y = value, color = variable)) +
  geom_line() +
  labs(title = "Variables over time", x = "Year", y = "Value") +
  theme_minimal()


##############################################################
##############################################################
################      young and old   ################
##############################################################
##############################################################

age_share <- decom %>%
  group_by(fyear) %>% # nolint
  summarise(
    share_young = sum(weight * young, na.rm = TRUE),
    share_old = sum(weight * old, na.rm = TRUE),
    #share_unvar = sum(weight * unvar, na.rm = TRUE),
    share = sum(weight, na.rm = TRUE)
  )

age_share <- age_share %>%
  filter(fyear >= 1965 & fyear < 2023)

age_share_long <- age_share %>%
  gather(variable, value, -fyear)


ggplot(age_share_long, aes(x = fyear, y = value, color = variable)) +
  geom_line() +
  labs(title = "Variables over time", x = "Year", y = "Value") +
    #y min, max to 0,1
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()


