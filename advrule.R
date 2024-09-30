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
library(rsq)

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

############# 1.a load data ##############
#navigate to with data
setwd(dircs[2])

#load accounting data with DEU markups
dset <- read.csv("DEU_st.csv") # nolint

# naisc codes
naics <- read.csv("2022_NAICS_Structure.csv")
colnames(naics) <- c("change", "naics_n", "industry")

#load usercost
usercost <- read.csv("usercost.csv")


#apply GDP deflator and generate xad measures
data <- VariableGen(dset, usercost)

#create lerner index
data <- data %>%
  mutate(l_ca = 1 - 1 / MU,
         l_deu = 1 - 1 / MU_deu)

#add industry names following deu and literally using 2 digit naics
data <- invisible(industry_n_dig_2(data, 2))

names(data)

#keep only relevant columns (dont drop full might use later)
data <-
  data %>% dplyr::select(GVKEY, fyear, industry, naics,
                         MU, MU_deu, l_ca, l_deu, Adr, Adr_MC,
                         sale,xad,cogs, ppegt, xsga, usercost, time, theta) # nolint

#limit to sample reporting XAD and trim XAD/Sale at 1% (by year)
Data <- adv_trim(data, 1)

############################################################x
############################################################x
# 2: Density plots
############################################################x
############################################################x

#advertising density
xaddensity <- xad_density(data)

#markup density
mudensity <- mu_density(Data, data)
#zero_subset <- Data %>%
# filter(is.na(Adr_MC)) # nolint
#1149/82441= 1.39% report 0 advertising

#save files
#save_f(xaddensity, "xad_density.pdf", dircs, 10, 9, save_files)
#save_f(mudensity, "MU_density.pdf", dircs, 10, 9, save_files)

############################################################
############################################################
#3: Aggregate Markup plots
############################################################
############################################################

#at some point go and make this average not sales weighted average

agg_muplot_sw <- agg_mu_plot(data, Data, "sales")

agg_muplot <- agg_mu_plot(data, Data, "")


#save files
save_f(agg_muplot, "agg_mu_plot.pdf", dircs, 10, 9, save_files)

############################################################
############################################################
#3: Scatter plots
############################################################
############################################################

two_d_data <- invisible(industry_n_dig(Data, naics, 2))
#add naics industries

# generate full sample scatter plots
plot_all_ca <- l_advert_plot(Data, "", "Cost Accounting Approach", 1000)
plot_all_deu <- l_advert_plot(Data, "", "Production Approach", 1000)

#save images
save_f(plot_all_ca, "adv_scatter_ca.pdf", dircs, 12, 12, save_files)
save_f(plot_all_deu, "adv_scatter_deu.pdf", dircs, 12, 12, save_files)

############################################################
############################################################
#4: Test 1
############################################################
############################################################

two_d_data <- invisible(industry_n_dig(Data, naics, 2))

two_d_data <- mutate(two_d_data, Industry =industry.y) # nolint

names(two_d_data)

#basic regressions
model_ca_1 <- feols(-log(1 - Adr) ~ log(MU),
                    cluster = "GVKEY",
                    data = two_d_data)



model_deu_1 <- feols(-log(1 - Adr) ~ log(MU_deu),
                     cluster = "GVKEY",
                     data = two_d_data)

#industry fixed effects
model_ca_2 <- feols(-log(1 - Adr) ~ log(MU) | Industry,
                    cluster = "GVKEY",
                    data = two_d_data)

model_deu_2 <- feols(-log(1 - Adr) ~ log(MU_deu) | Industry,
                     cluster = "GVKEY",
                     data = two_d_data)

#time fixed effects
model_ca_3 <- feols(-log(1 - Adr) ~ log(MU) | time,
                    cluster = "GVKEY",
                    data = two_d_data)

model_deu_3 <- feols(-log(1 - Adr) ~ log(MU_deu) |  time,
                     cluster = "GVKEY",
                     data = two_d_data)


model_ca_4 <- feols(-log(1 - Adr) ~ log(MU) | Industry + time,
                    cluster = "GVKEY",
                    data = two_d_data)

model_deu_4 <- feols(-log(1 - Adr) ~ log(MU_deu) | Industry + time,
                     cluster = "GVKEY",
                     data = two_d_data)


# Create a named character vector of new names
new_names <- c(
  "Adr" = "Advertising Share of Revenue",
  "l_ca" = "Lerner Index (CA)",
  "l_deu" = "Lerner Index (DEU)",
  "time" = "Year",
  "GVKEY" = "Firm level"
)


# Create a named list of models
models <- list("Model 1 (CA)" = model_ca_1,
               "Model 1 (DEU)" = model_deu_1,
               "Model 2 (CA)" = model_ca_2,
               "Model 2 (DEU)" = model_deu_2,
               "Model 3 (CA)" = model_ca_3,
               "Model 3 (DEU)" = model_deu_3,
               "Model 4 (CA)" = model_ca_4,
               "Model 4 (DEU)" = model_deu_4)

# Create the summary table with the new names
summary_table <- etable(models, dict = new_names)


# Calculate R² ci from Cohen et al. (2003)
calculate_r2_ci <- function(model, conf.level = 0.95) {
  r2 <- summary(model)$sq.cor
  n <- length(model$fitted.values)
  k = length(summary(model)$coefficients[1])
  se = sqrt(4 * r2 *(1 - r2)^2 * (n - k - 1)^2 / ((n^2 - 1) * (n + 3)))
  ci_lower <- r2 - 1.645 * se
  ci_upper <- r2 + 1.645 * se
  return(paste("[",
               round(ci_lower, 5), ", ",
               round(ci_upper, 5), ")", sep = ""))
}

# Calculate R² ci from Cohen et al. (2003)
calculate_r2_se <- function(model, conf.level = 0.95) {
  r2 <- summary(model)$sq.cor
  n <- length(model$fitted.values)
  k = length(summary(model)$coefficients[1])
  se = sqrt(4 * r2 *(1 - r2)^2 * (n - k - 1)^2 / ((n^2 - 1) * (n + 3)))
  return(paste("(",
               round(se, 5), ")", sep = ""))
}

# Assuming models is a list of fitted models
r2se <- lapply(models, calculate_r2_ci)

print(summary_table)

view(summary_table)

names(two_d_data)

# Create the summary table with the new names
l_table <- etable(models, dict = new_names, tex = TRUE)

# Print the LaTeX-formatted summary table
print(l_table)


############################################################
############################################################
#4: Sector x time average
############################################################
############################################################


names(two_d_data)

# Filter out zero or negative values
filtered_data <- two_d_data %>% filter(Adr > 0, MU > 0, MU_deu > 0)


#generate average level of things
sectort_avg <- filtered_data %>%
  group_by(Industry, time) %>%
  summarize(ln_mu = mean(MU, na.rm = TRUE),
            ln_deu = mean(log(MU_deu), na.rm = TRUE),
            ln_1_xad = mean(-log(1-Adr)), na.rm = TRUE)

#remove nas
sectort_avg <- sectort_avg%>%
  filter(!is.na(ln_mu))%>%
  filter(!is.na(ln_deu))%>%
  filter(!is.na(ln_xad))

#remove infinites
sectort_avg <- sectort_avg%>%
  filter(ln_mu != -Inf)%>%
  filter(ln_deu != -Inf)%>%
  filter(ln_xad != -Inf)


sect_ca1 <- feols(ln_1_xad ~ ln_mu,
                 data = sectort_avg)

sect_deu1 <- feols(ln_1_xad ~ ln_deu,
                  data = sectort_avg)

sect_ca2 <- feols(ln_1_xad ~ ln_mu| Industry,
                  data = sectort_avg)

sect_deu2 <- feols(ln_1_xad ~ ln_deu| Industry,
                  data = sectort_avg)

sect_ca3 <- feols(ln_1_xad ~ ln_mu| time,
                  data = sectort_avg)

sect_deu3 <- feols(ln_1_xad ~ ln_deu| time,
                  data = sectort_avg)

sect_ca4 <- feols(ln_1_xad ~ ln_mu| Industry + time,
                  data = sectort_avg)

sect_deu4 <- feols(ln_1_xad ~ ln_deu| Industry + time,
                  data = sectort_avg)

# Create a named list of models
sectort_models <- list("Model 1 (CA)" = sect_ca1,
                      "Model 1 (DEU)" = sect_deu1,
                      "Model 2 (CA)" = sect_ca2,
                      "Model 2 (DEU)" = sect_deu2,
                      "Model 3 (CA)" = sect_ca3,
                      "Model 3 (DEU)" = sect_deu3,
                      "Model 4 (CA)" = sect_ca4,
                      "Model 4 (DEU)" = sect_deu4)

# Create the summary table
sectort_table <- etable(sectort_models)

view(sectort_table)


#latex
etable(sectort_models, tex = TRUE)

############################################################
############################################################
#4: Sector level
############################################################
############################################################


names(two_d_data)

#generate average level of things
sector_avg <- filtered_data %>%
  group_by(Industry) %>%
  summarize(ln_mu = mean(MU, na.rm = TRUE),
            ln_deu = mean(log(MU_deu), na.rm = TRUE),
            ln_xad = mean(-log(1-Adr)), na.rm = TRUE,
            l_ca = mean(1 - 1 / MU, na.rm = TRUE),
            l_deu = mean(1 - 1 / MU_deu, na.rm = TRUE),
            xad = mean(Adr, na.rm = TRUE))

head(sector_avg)


sec_ca1 <- feols(ln_xad ~ ln_mu,
                 data = sector_avg)

sec_ca2 <- feols(ln_xad ~ ln_mu - 1,
                 data = sector_avg)

sec_deu1 <- feols(ln_xad ~ ln_deu,
                  data = sector_avg)

sec_deu2 <- feols(ln_xad ~ ln_deu-1,
                 data = sector_avg)

# Create a named list of models
sector_models <- list("Model 1 (CA)" = sec_ca1,
                      "Model 2 (CA)" = sec_ca2,
                      "Model 1 (DEU)" = sec_deu1,
                      "Model 2 (DEU)" = sec_deu2)

# Create the summary table
sector_table <- etable(sector_models)

view(sector_table)


sec_ca3 <- feols(xad ~ l_ca,
                 data = sector_avg)

sec_ca4 <- feols(xad ~ l_ca - 1,
                 data = sector_avg)

sec_deu5 <- feols(xad ~ l_deu,
                  data = sector_avg)

sec_deu6 <- feols(xad ~ l_deu - 1,
                  data = sector_avg)

# Create a named list of models
sector_models <- list("Model 1 (CA)" = sec_ca1,
                      "Model 1 (DEU)" = sec_deu1,
                      "Model 2 (CA)" = sec_ca2,
                      "Model 2 (DEU)" = sec_deu2,
                      "Model 3 (CA)" = sec_ca3,
                      "Model 5 (DEU)" = sec_deu5,
                      "Model 4 (CA)" = sec_ca4,
                      "Model 6 (DEU)" = sec_deu6)

# Create the summary table
sector_table <- etable(sector_models)

view(sector_table)

#latex
etable(sector_models, tex = TRUE)


plot(sector_avg$ln_mu, sector_avg$ln_xad)

plot(sector_avg$ln_deu, sector_avg$ln_xad)

plot(sector_avg$l_ca, sector_avg$xad)

plot(sector_avg$l_deu, sector_avg$xad)


############################################################
############################################################
#4: Horse race
############################################################
############################################################


race_1 <- feols(-log(1 - Adr) ~ log(MU) + log(MU_deu),
                    cluster = "GVKEY",
                    data = two_d_data)

race_2 <- feols(-log(1 - Adr) ~ log(MU) + log(MU_deu) | Industry,
                    cluster = "GVKEY",
                    data = two_d_data)

race_3 <- feols(-log(1 - Adr) ~ log(MU) + log(MU_deu) | time,
                     cluster = "GVKEY",
                     data = two_d_data)

race_4 <- feols(-log(1 - Adr) ~ log(MU) + log(MU_deu) | Industry + time,
                    cluster = "GVKEY",
                    data = two_d_data)


# Create a named list of models
race_n <- list("Model 1" = race_1,
               "Model 2" = race_2,
               "Model 3" = race_3,
               "Model 4" = race_4)

# Create the summary table with the new names
race_table <- etable(race_n, dict = new_names)

view(race_table)


# Create the summary table with the new names
etable(race_n, dict = new_names, tex = TRUE)






############################################################
############################################################
# sale / cogs
############################################################
############################################################


test <- feols(-log(1 - Adr) ~ log(MU_deu) + log(MU),
                     cluster = "GVKEY",
                     data = two_d_data)

test2 <- feols(-log(1 - Adr) ~ log(MU_deu) + log(MU) | Industry + time,
                     cluster = "GVKEY",
                     data = two_d_data)



# save vars for sale/cogs, theta, and mu resid
two_d_data <- two_d_data %>%
  mutate(soc = sale / cogs)
two_d_data <- two_d_data %>%
  mutate(theta =  MU_deu / soc,
         mu_resid = MU / soc)

#regs

#basic regressions
model_decom_1 <- feols(-log(1 - Adr) ~ log(soc),
                       cluster = "GVKEY",
                       data = two_d_data)

model_decom_2 <- feols(-log(1 - Adr) ~ log(theta),
                       cluster = "GVKEY",
                       data = two_d_data)

model_decom_3 <- feols(-log(1 - Adr) ~ log(mu_resid),
                        cluster = "GVKEY",
                        data = two_d_data)

# 2
model_decom_4 <- feols(-log(1 - Adr) ~ log(theta) + log(soc),
                       cluster = "GVKEY",
                       data = two_d_data)

model_decom_5 <- feols(-log(1 - Adr) ~ log(mu_resid) + log(soc),
                        cluster = "GVKEY",
                        data = two_d_data)



#all 3
model_decom_6 <- feols(-log(1 - Adr) ~ log(soc) + log(theta) + log(mu_resid),
                       cluster = "GVKEY",
                       data = two_d_data)



# Create a named list of models
models2 <- list("Model 1" = model_decom_1,
                "Model 2" = model_decom_2,
                "Model 3" = model_decom_3,
                "Model 4" = model_decom_4,
                "Model 5" = model_decom_5,
                "Model 6" = model_decom_6)

# Create the summary table with the new names
summary_table2 <- etable(models2, dict = new_names)

view(summary_table2)

# Create the summary table with the new names
l_table <- etable(models, dict = new_names, tex = TRUE)

# Print the LaTeX-formatted summary table
print(etable(models2, dict = new_names, tex = TRUE))






############################################################
############################################################
# robust
############################################################
############################################################







############################################################
#old
head(two_d_data)

r1_ca_1 <- feols(Adr ~ l_ca,
                 cluster = "GVKEY",
                 data = two_d_data)



r1_deu_1 <- feols(Adr ~ l_deu,
                 cluster = "GVKEY",
                 data = two_d_data)

#industry fixed effects
r1_ca_2 <- feols(Adr ~ l_ca | Industry + time,
                 cluster = "GVKEY",
                 data = two_d_data)

r1_deu_2 <- feols(Adr ~ l_deu | Industry + time,
                  cluster = "GVKEY",
                  data = two_d_data)

############################################################
#old logs
r2_ca_1 <- feols(log(Adr) ~ log(l_ca),
                 cluster = "GVKEY",
                 data = two_d_data)



r2_deu_1 <- feols(log(Adr) ~ log(l_deu),
                 cluster = "GVKEY",
                 data = two_d_data)

#industry fixed effects
r2_ca_2 <- feols(log(Adr) ~ log(l_ca) | Industry + time ,
                 cluster = "GVKEY",
                 data = two_d_data)

r2_deu_2 <- feols(log(Adr) ~ log(l_deu) | Industry + time,
                  cluster = "GVKEY",
                  data = two_d_data)


############################################################
#new non logs
r3_ca_1 <- feols(1 / (1 - Adr) ~ MU,
                 cluster = "GVKEY",
                 data = two_d_data)



r3_deu_1 <- feols(1 / (1 - Adr) ~ MU_deu,
                  cluster = "GVKEY",
                  data = two_d_data)

#industry fixed effects
r3_ca_2 <- feols(1 / (1 - Adr) ~ MU | Industry + time,
                 cluster = "GVKEY",
                 data = two_d_data)

r3_deu_2 <- feols(1 / (1 - Adr) ~ MU_deu | Industry + time,
                  cluster = "GVKEY",
                  data = two_d_data)






# Create a named list of models
robust_ms <- list("Model 1 (CA)" = r1_ca_1,
                  "Model 1 (DEU)" = r1_deu_1,
                  "Model 2 (CA)" = r1_ca_2,
                  "Model 2 (DEU)" = r1_deu_2,
                  "Model 3 (CA)" = r2_ca_1,
                  "Model 3 (DEU)" = r2_deu_1,
                  "Model 4 (CA)" = r2_ca_2,
                  "Model 4 (DEU)" = r2_deu_2)

# Create the summary table with the new names
robust_table <- etable(robust_ms, dict = new_names)

view(robust_table)



# Print the LaTeX-formatted summary table
print(etable(robust_ms, tex = TRUE))









############################################################
############################################################
#old
############################################################
############################################################


#basic regression
model_cd_1 <- feols(Adr ~ l_ca + l_deu,
                    cluster = "GVKEY",
                    data = two_d_data)

#industry fixed effects
model_cd_2 <- feols(Adr ~ l_ca + l_deu | Industry,
                    cluster = "GVKEY",
                    data = two_d_data)

#time fixed effects
model_cd_3 <- feols(Adr ~ l_ca + l_deu | time,
                    cluster = "GVKEY",
                    data = two_d_data)

#sector time fes
model_cd_4 <- feols(Adr ~ l_ca + l_deu | Industry + time,
                    cluster = "GVKEY",
                    data = two_d_data)

# Create a named list of models
models2 <- list("Model 1" = model_cd_1, 
               "Model 2" = model_cd_2,
               "Model 3" = model_cd_3,
               "Model 4" = model_cd_4)

# Create the summary table with the new names
summary_table2 <- etable(models2, dict = new_names)

print(summary_table2)

view(summary_table2)

# Create the summary table with the new names
l_table2 <- etable(models2, dict = new_names, tex = TRUE)

# Print the LaTeX-formatted summary table
print(l_table2)


###########################################################
############################################################
#6:  Sales / cogs
############################################################
############################################################



#basic regression
model_cd_1 <- feols(Adr ~ l_ca + l_deu,
                    cluster = "GVKEY",
                    data = two_d_data)

#industry fixed effects
model_cd_2 <- feols(Adr ~ l_ca + l_deu | Industry,
                    cluster = "GVKEY",
                    data = two_d_data)

#time fixed effects
model_cd_3 <- feols(Adr ~ l_ca + l_deu | time,
                    cluster = "GVKEY",
                    data = two_d_data)

#sector time fes
model_cd_4 <- feols(Adr ~ l_ca + l_deu | Industry + time,
                    cluster = "GVKEY",
                    data = two_d_data)

# Create a named list of models
models2 <- list("Model 1" = model_cd_1,
               "Model 2" = model_cd_2,
               "Model 3" = model_cd_3,
               "Model 4" = model_cd_4)

















bias_ca <- feols( log(MU) ~ -log(1 - Adr) | Industry + time,
                    cluster = "GVKEY",
                    data = two_d_data)

bias_deu <- feols(log(MU_deu) ~ -log(1 - Adr) | Industry + time,
                     cluster = "GVKEY",
                     data = two_d_data)

fe_full_deu <- fixef(bias_deu)

fe_full_ca <- fixef(bias_ca)

fixef(model_deu_4)$time

plot(fe_full_ca$time, fe_full_deu$time)



plot(fe_full_deu$Industry, fe_full_ca$Industry)

plot(exp(fe_full_ca$time))

plot(exp(fe_full_deu$time))
























############################################################
############################################################
#6:  By Sector Scatter plots
############################################################
############################################################

names(two_d_data)

#generate average level of things
sector_avg <- two_d_data %>%
  group_by(Industry) %>%
  summarize(agg_l_ca = mean(l_ca),
            agg_l_deu = mean(l_deu),
            Adr_adr = mean(Adr))

head(sector_avg)

#generate average level of things
sector_avg <- two_d_data %>%
  group_by(Industry) %>%
  summarize(l_ca = mean(l_ca, na.rm = TRUE),
            l_deu = mean(l_deu, na.rm = TRUE),
            Adr = mean(Ad, na.rm = TRUEr))

head(sector_avg)


plot_sec_ca <- l_advert_plot(sector_avg, "Sector Average",
                             "Cost Accounting Approach", 0)

plot_sec_deu <- l_advert_plot(sector_avg, "Sector Average",
                              "Production Approach", 0)


#save images
save_f(plot_sec_ca, "adv_scatter_ca_sec.pdf", dircs, 12, 12, save_files)
save_f(plot_sec_deu, "adv_scatter_deu_sec.pdf", dircs, 12, 12, save_files)


############################################################
############################################################
#6:  By time Scatter plots
############################################################
############################################################
#generate average level of things
yr_avg <- two_d_data %>%
  group_by(time) %>%
  summarize(l_ca = mean(l_ca, na.rm = TRUE),
            l_deu = mean(l_deu, na.rm = TRUE),
            Adr = mean(Adr, na.rm = TRUE))

plot_yr_ca <- l_advert_plot(yr_avg, "Average by Year",
                            "Cost Accounting Approach", 0)

plot_yr_deu <- l_advert_plot(yr_avg, "Average by Year",
                             "Production Approach", 0)


#save images
save_f(plot_yr_ca, "adv_scatter_ca_yr.pdf", dircs, 12, 12, save_files)
save_f(plot_yr_deu, "adv_scatter_deu_yr.pdf", dircs, 12, 12, save_files)










############################################################
############################################################
#5: Test table
############################################################
############################################################

two_d_data$Industry <- paste0(two_d_data$Industry, "tst")
two_d_data$Industry <- gsub("Ttst", "", two_d_data$Industry)
two_d_data$Industry <- gsub("tst", "", two_d_data$Industry)

unique(two_d_data$Industry)

testtable <- test_table_new_nt(two_d_data)
testtable[5, 1] <- "Administrative and Support"
lign <- c("l", "l", "c", "|c", "c", "c", "|c", "c", "c", "|c")
view(testtable)

hold <- print(xtable(testtable, align = lign), include.rownames = FALSE)



# "\hline
# \hline
# &  & \multicolumn{3}{c}{Cost Accounting} \vline &  \multicolumn{3}{c}{Production Function} \vline \\"

