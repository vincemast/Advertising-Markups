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
#     0 create empty data set
############################################################
############################################################

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
                    "mu (in original measure)" = numeric())


######################## 1 ###############################
# BLP 1995 https://pages.stern.nyu.edu/~wgreene/Econometrics/BLP.pdf
# 1990 cars below table VIII: average lerner is 0.239

# naisc code: 3361 Motor Vehicle Manufacturing
cars_1990 <- data.frame("Automobile", "avg", "lerner", 3361, 4, 1990, 1990,
                        "BLP (1995)", "RCL",  .239)
names(cars_1990) <- names(blp_0)
blp <- rbind(blp_0, cars_1990)


######################## 2 ###############################
# Goldberg (1995)
#automobiles 1983-1987
# average lerner = 0.35
# naisc code: 3361 Motor Vehicle Manufacturing
cars_1983 <- data.frame("Automobile", "avg", "lerner", 3361, 4, 1983, 1987,
                        "Goldberg (1995)", "RCL",  .35)
names(cars_1983) <- names(blp_0)
blp <- rbind(blp, cars_1983)



######################## 3 ###############################
# Nevo (2001) https://pages.stern.nyu.edu/~acollard/NevoEcma.pdf
# ready to eat cereal
# 1988-1992 (median) lerner =  .422 (table VIII)
nevo <- data.frame("RTE Cereal", "median", "lerner", 31123, 5, 1988, 1992,
                   "Nevo (2001)", "RCL",  .422)
names(nevo) <- names(blp_0)
blp <- rbind(blp, nevo)


######################## 4 ###############################
# benkard (2004)
# nested logit
#aircraft manufactoring naisc 336411
# 1975-1995
# average markup ratio: 1.24 (table 9)

aircraft <- data.frame("Aircraft Manufactoring", "avg", "ratio", 336411, 6,
                       1975, 1995, "Benkard (2004)", "RCL",  1.24)
names(aircraft) <- names(blp_0)
blp <- rbind(blp, aircraft)


######################## 5 ###############################
#Chu, Chintagunta, Vilcassim (2007)
# rcl :1995-1998
# PC manufactoring naisc 334111
#average lerner: .249 (table 6)

pc <- data.frame("PC", "avg", "lerner", 334111, 6, 1995, 1998,
                 "Chu et al. (2007)", "RCL",  .249)
names(pc) <- names(blp_0)
blp <- rbind(blp, pc)


######################## 6 ###############################
# Goeree (2008)
# PC manufactoring naisc 334111
# 1996-1997
# NL
#median markup .15

pc_1996 <- data.frame("PC", "median", "markup", 334111, 6, 1996, 1997,
                      "Goeree (2008)", "NL",  .15)
names(pc_1996) <- names(blp_0)
blp <- rbind(blp, pc_1996)


######################## 7 ###############################
# Hellerstein (2008)
#median lerner .27
#beer manufactoring naisc 312120
# 1991-1994
#RCL

beer <- data.frame("Beer", "median", "lerner", 312120, 6, 1991, 1994,
                   "Hellerstein (2008)", "RCL",  .27)
names(beer) <- names(blp_0)
blp <- rbind(blp, beer)


######################## 8 ###############################
# Cachon Olivares (2009)
# cars
#rcl
# 1996-2004
# markup: .65

cars_1996 <- data.frame("Automobile", "avg", "markup", 3361, 4, 1996, 2004,
                        "Cachon Olivares (2009)", "RCL",  .65)

names(cars_1996) <- names(blp_0)
blp <- rbind(blp, cars_1996)


######################## 9 ###############################
#Berry Jia (2010)
# airline  naics 481111
# RCL
# LERNER: 1999:  0.63 (TABLE 8a); LERNER 2006: .60
#doent say agg method, use sales weighted

airline_1999 <- data.frame("Airline", "sw", "lerner", 481111, 6, 1999, 1999,
                           "Berry, Jia (2010)", "RCL",  .63)
airline_2006 <- data.frame("Airline", "sw", "lerner", 481111, 6, 2006, 2006,
                           "Berry, Jia (2010)", "RCL",  .6)

names(airline_1999) <- names(blp_0)
names(airline_2006) <- names(blp_0)

blp <- rbind(blp, airline_1999, airline_2006)

######################## 10 ###############################
#Nakamura Zerom (2010)
# 2000-2004 coffee naisc 311920
# RCL
#median markup .583

coffee <- data.frame("Coffee", "median", "markup", 311920, 6, 2000, 2004,
                     "Nakamura Zerom (2010)", "RCL",  .583)
names(coffee) <- names(blp_0)
blp <- rbind(blp, coffee)


######################## 11 ###############################
# Eizenberg (2012)
# PC manufactoring naisc 334111
# median lener: 0.078
# 2001-2004
# RCL

pc_2001 <- data.frame("PC", "median", "lerner", 334111, 6, 2001, 2004,
                      "Eizenberg (2012)", "RCL",  .078)
names(pc_2001) <- names(blp_0)
blp <- rbind(blp, pc_2001)


######################## 12 ###############################
# Lou, Prentice, Yin (2012)
# Digital Cameras naisc 333316 2003-2006
# GR is Gowrisankaran and Rysman (2009) (call other)
# RCL mean markup: .88
# GR mean markup: .3

dc <- data.frame("Digital Cameras", "avg", "markup", 333316, 6, 2003, 2006,
                 "Lou et al. (2012)", "Other",  .88)
dc_gr <- data.frame("Digital Cameras", "avg", "markup", 333316, 6, 2003, 2006,
                    "Gowrisankaran, Rysman (2009)", "RCL",  .32)
names(dc) <- names(blp_0)
names(dc_gr) <- names(blp_0)
blp <- rbind(blp, dc, dc_gr)


######################## 13 ###############################
#Daim, Yuan (2013)
# retail banks naisc 522110
#median markup 2000: .371
#median markup 2008: .483
#NL

banks_2000 <- data.frame("Retail Banks", "median", "markup", 522110, 6, 2000,
                         2000, "Daim, Yuan (2013)", "NL",  .371)
banks_2008 <- data.frame("Retail Banks", "median", "markup", 522110, 6, 2008,
                         2008, "Daim, Yuan (2013)", "NL",  .483)
names(banks_2000) <- names(blp_0)
names(banks_2008) <- names(blp_0)
blp <- rbind(blp, banks_2000, banks_2008)


######################## 14 ###############################
#Hottman (2015)
# median markup .29
# other method
# retail naisc 44
#2006-2010

retail <- data.frame("Retail", "median", "markup", 44, 2, 2006, 2010,
                     "Hottman (2015)", "Other",  .29)

names(retail) <- names(blp_0)
blp <- rbind(blp, retail)


######################## 15 ###############################
#Song (2015)
# PCs naisc 334111
# 2001-2004
# use nested logit, BLP1 and Hybrid 1 (other)
# reports average by quater, take average up

#nested
mean(c(1.289, 1.306, 1.252, 1.306, 1.439, 1.423, 1.348, 1.406,
       1.382, 1.384, 1.415, 1.517))
#BLP
mean(c(0.514, 0.528, 0.526, 0.543, 0.576, 0.587, 0.574, 0.572,
       0.555, 0.562, 0.583, 0.610))
#Hybrid
mean(c(0.404, 0.378, 0.351, 0.375, 0.696, 0.353, 0.392, 0.334,
       0.316, 0.358, 0.399, 0.401))

pc_nested <- data.frame("PC", "avg", "markup", 334111, 6, 2001, 2004,
                        "Song (2015)", "NL", 1.37225)
pc_blp <- data.frame("PC", "avg", "markup", 334111, 6, 2001, 2004,
                     "Song (2015)", "RCL", 0.5608333)
pc_hybrid <- data.frame("PC", "avg", "markup", 334111, 6, 2001, 2004,
                        "Song (2015)", "Other", 0.3964167)

names(pc_nested) <- names(blp_0)
names(pc_blp) <- names(blp_0)
names(pc_hybrid) <- names(blp_0)

blp <- rbind(blp, pc_nested, pc_blp, pc_hybrid)


######################## 16 ###############################
# Florez-Acosta (2016)
# ready to eat ceral naisc 31123
# 1990-1994
#RCL
# average markup: .3851
# does some weird stuff with defining markups
# also asks not to cite

cereal <- data.frame("RTE Cereal", "avg", "markup", 31123, 5, 1990, 1994,
                     "Florez-Acosta (2016)", "RCL",  .3851)
names(cereal) <- names(blp_0)
blp <- rbind(blp, cereal)


######################## 17 ###############################
# Miller Weinberg (2017)
# http://www.nathanhmiller.org/mwbeer.pdf
# data from 2001-2011, average lerner (p-mc)/p=.34 ( below table VI in text 34%)
#beer manufactoring naisc 312120

beer <- c("Beer", "avg", "lerner", 312120, 6, 2001, 2011,
          "Miller, Weinberg (2017)", "RCNL", .34)
names(beer) <- names(blp_0)
blp <- rbind(blp, beer)


######################## 18 ###############################
# Murry, Zhou (2018)
# automobiles 2007-2014
#median lerner .29
#naisc 3361
# rcl

cars_2007 <- data.frame("Automobile", "median", "lerner", 3361, 4, 2007, 2014,
                        "Murry, Zhou (2018)", "RCL",  .29)
names(cars_2007) <- names(blp_0)
blp <- rbind(blp, cars_2007)


######################## 19 ###############################
#Golovin (2020)
#cars, 1990-2013
# sales weghted
#markup p-c/p
#RCL
# from table 3 with https://apps.automeris.io/wpd/
#1990 .325
#1991 .305
#1992 .297
#1993 .287
#1994 .267
#1995 .279
#1996 .295
#1997. 253
#1998 .252
#1999 .246
#2000 .263
#2001 .250
#2002 .230
#2003 .211
#2004 .243
#2005 .250
#2006 .244
#2007 .250
#2008 .249
#2009 .247
#2010 .248
#2011 .252
#2012 .242
#2013 .228
#include 1 obs per 5 year window

cars_1995 <- data.frame("Automobile", "sw", "markup", 3361, 4, 1995, 1995,
                        "Golovin (2020)", "RCL",  .279)
cars_2000 <- data.frame("Automobile", "sw", "markup", 3361, 4, 2000, 2000,
                        "Golovin (2020)", "RCL",  .263)
cars_2005 <- data.frame("Automobile", "sw", "markup", 3361, 4, 2005, 2005,
                        "Golovin (2020)", "RCL",  .250)
cars_2010 <- data.frame("Automobile", "sw", "markup", 3361, 4, 2010, 2010,
                        "Golovin (2020)", "RCL",  .248)
cars_2013 <- data.frame("Automobile", "sw", "markup", 3361, 4, 2013, 2013,
                        "Golovin (2020)", "RCL",  .228)

names(cars_1995) <- names(blp_0)
names(cars_2000) <- names(blp_0)
names(cars_2005) <- names(blp_0)
names(cars_2010) <- names(blp_0)
names(cars_2013) <- names(blp_0)

blp <- rbind(blp, cars_1995, cars_2000, cars_2005, cars_2010, cars_2013)


######################## 20 ###############################
#Grieco, Murry, Yurukoglu (2021)
# https://web.stanford.edu/~ayurukog/CarMarkupsJuly2023.pdf
# sales weighted markup ratio US automobile indsutry (1980-2018)
# naisc 3361
#REPLICATION  Citation: Grieco, Paul L. E.; Murry, Charles; Yurukoglu, Ali, 2023, "Replication Data for: 'The Evolution of Market Power in the Automobile Industry'", https://doi.org/10.7910/DVN/CZGOKP, Harvard Dataverse, V1; GMY_Replication.tar.gz [fileName] #nolint
#1980: 1.65345769837934
#1981: 1.60030203970453
#1982: 1.49100874475505
#1983: 1.50559235685491
#1984: 1.46175051694976
#1985: 1.43097357374417
#1986: 1.42627464268108
#1987: 1.38193604111696
#1988: 1.35509517472521
#1989: 1.35470632336354
#1990: 1.36329051479847
#1991: 1.33137646586945
#1992: 1.31823652741050
#1993: 1.34160022559908
#1994: 1.31393042190725
#1995: 1.30144204928541
#1996: 1.29595545534884
# 1.30073429345261
# 1.27430541930831
# 1.26562839751600
#2000: 1.26640829830938
#2001: 1.27222957660155
# 1.24985005946121
# 1.23587867788747
# 1.24551170008831
#2005: 1.25312896711375
#2006: 1.26014994201697
# 1.27068230333538
# 1.28702726146624
# 1.26534228989006
#2010: 1.26993766489373
#2011: 1.27617742871823
# 1.27003617175429
# 1.27635894652096
# 1.27540220195363
#2015 1.25364650207103
#2016: 1.26370811499424
#1.24894965160207
#2018: 1.25655168574207

#one obs per 5 year window

auto_1980 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1980, 1980,
                        "Grieco et al. (2023)", "RCL", 1.65345769837934)
auto_1985 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1985, 1985,
                        "Grieco et al. (2023)", "RCL", 1.43097357374417)
auto_1990 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1990, 1990,
                        "Grieco et al. (2023)", "RCL", 1.36329051479847)
auto_1995 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1995, 1995,
                        "Grieco et al. (2023)", "RCL", 1.30144204928541)
auto_2000 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2000, 2000,
                        "Grieco et al. (2023)", "RCL", 1.26640829830938)
auto_2005 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2005, 2005,
                        "Grieco et al. (2023)", "RCL", 1.25312896711375)
auto_2010 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2010, 2010,
                        "Grieco et al. (2023)", "RCL", 1.26993766489373)
auto_2015 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2015, 2015,
                        "Grieco et al. (2023)", "RCL", 1.25364650207103)
auto_2018 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2018, 2018,
                        "Grieco et al. (2023)", "RCL", 1.25655168574207)

names(auto_1980) <- names(blp_0)
names(auto_1985) <- names(blp_0)
names(auto_1990) <- names(blp_0)
names(auto_1995) <- names(blp_0)
names(auto_2000) <- names(blp_0)
names(auto_2005) <- names(blp_0)
names(auto_2010) <- names(blp_0)
names(auto_2015) <- names(blp_0)
names(auto_2018) <- names(blp_0)

blp <- rbind(blp, auto_1980, auto_1985, auto_1990, auto_1995, auto_2000,
             auto_2005, auto_2010, auto_2015, auto_2018)

######################## 21 ###############################
# Ciliberto, Murry, and Tamer (2021)
# airlines 2012
# medin markup [.28146,.28274]
#NL

airline_2012 <- data.frame("Airline", "median", "markup", 481111, 6, 2012, 2012,
                           "Ciliberto et al. (2021)", "NL",  .2821)

names(airline_2012) <- names(blp_0)
blp <- rbind(blp, airline_2012)


######################## 22 ###############################
# Brand (2022)
# FRAC and RCL
#Median
# full sample Median markup: (Use Naisc 311 (food manufactoring))
#FRAC: (ratio); 1.92 (2006); 2.32 (2017)
#BLP: 1.98 (2006); 2.36 (2017)
brand_frac_2006 <- data.frame("Food Manufactoring", "median", "ratio", 311, 3,
                              2006, 2006, "Brand (2022)", "FRAC", 1.92)
brand_frac_2017 <- data.frame("Food Manufactoring", "median", "ratio", 311, 3,
                              2017, 2017, "Brand (2022)", "FRAC", 2.32)

brand_blp_2006 <- data.frame("Food Manufactoring", "median", "ratio", 311, 3,
                             2006, 2006, "Brand (2022)", "RCL", 1.98)
brand_blp_2017 <- data.frame("Food Manufactoring", "median", "ratio", 311, 3,
                             2017, 2017, "Brand (2022)", "RCL", 2.36)

names(brand_frac_2006) <- names(blp_0)
names(brand_frac_2017) <- names(blp_0)
names(brand_blp_2006) <- names(blp_0)
names(brand_blp_2017) <- names(blp_0)


#FRAC Median
#Own-Price 2006 Markups 2006 Own-Price 2017 Markups 2017
#Fruit Drinks -2.26 1.82 -1.55 2.20
#Soup -4.00 1.97 -2.41 2.90
#Cookies -2.08 2.01 -1.59 2.55
#Pizza -2.31 1.88 -2.22 1.90
#Ice Cream -1.93 2.01 -1.87 2.28
#Entrees -1.92 2.09 -1.55 2.42
#Yogurt -2.28 1.89 -1.44 2.13
#Remaining Fruit -2.40 1.73 -2.04 2.18
#Light Beer -3.13 1.49 -2.23 1.67
#ice cream naisc 311520
ice_2006 <- c("Ice Cream", "median", "ratio", 311520, 6, 2006, 2006,
              "Brand (2022)", "FRAC", 2.01)
ice_2017 <- c("Ice Cream", "median", "ratio", 311520, 6, 2017, 2017,
              "Brand (2022)", "FRAC", 2.28)

# naisc code: 311411 Frozen Fruit, Juice, and Vegetable Manufacturing
juice_b_2006 <- c("Fruit Drinks", "median", "ratio", 311411, 6, 2006, 2006,
                  "Brand (2022)", "FRAC", 1.82)
juice_b_2017 <- c("Fruit Drinks", "median", "ratio", 311411, 6, 2017, 2017,
                  "Brand (2022)", "FRAC", 2.20)

#cookies naisc code: 311821 Cookie and Cracker Manufacturing
cookies_2006 <- c("Cookies", "median", "ratio",  311821, 6, 2006, 2006,
                  "Brand (2022)", "FRAC", 2.01)
cookies_2017 <- c("Cookies", "median", "ratio",  311821, 6, 2017, 2017,
                  "Brand (2022)", "FRAC", 2.55)

#Entrees naisc code: 311412 Frozen Specialty Food Manufacturing
entrees_2006 <- c("Entrees", "median", "ratio", 311412, 6, 2006, 2006,
                  "Brand (2022)", "FRAC", 2.09)
entrees_2017 <- c("Entrees", "median", "ratio", 311412, 6, 2017, 2017,
                  "Brand (2022)", "FRAC", 2.42)

names(ice_2006) <- names(blp_0)
names(ice_2017) <- names(blp_0)
names(juice_b_2006) <- names(blp_0)
names(juice_b_2017) <- names(blp_0)
names(cookies_2006) <- names(blp_0)
names(cookies_2017) <- names(blp_0)
names(entrees_2006) <- names(blp_0)
names(entrees_2017) <- names(blp_0)

#BLP Median
#Own-Price 2006 Markups 2006 Own-Price 2017 Markups 2017
#FruitDrinks-2.26 1.82-1.55 2.20
#Soup-4.00 1.97-2.41 2.90
#Cookies-2.08 2.01-1.59 2.55
#Pizza-2.31 1.88-2.22 1.90
#IceCream-1.93 2.01-1.87 2.28
#Entrees-1.92 2.09-1.55 2.42
#Yogurt-2.28 1.89-1.44 2.13
#RemainingFruit-2.40 1.73-2.04 2.18
#LightBeer-3.13 1.49-2.23 1.67

# naisc code: 311411 Frozen Fruit, Juice, and Vegetable Manufacturing
juice_b_2006 <- c("Fruit Drinks", "median", "ratio", 311411, 6, 2006, 2006,
                  "Brand (WP)", "RCL", 1.82)

juice_b_2017 <- c("Fruit Drinks", "median", "ratio", 311411, 6, 2017, 2017,
                  "Brand (WP)", "RCL", 2.20)

#cookies naisc code: 311821 Cookie and Cracker Manufacturing
cookies_2006 <- c("Cookies", "median", "ratio",  311821, 6, 2006, 2006,
                  "Brand (WP)", "RCL", 2.01)
cookies_2017 <- c("Cookies", "median", "ratio",  311821, 6, 2017, 2017,
                  "Brand (WP)", "RCL", 2.55)

#Entrees naisc code: 311412 Frozen Specialty Food Manufacturing
entrees_2006 <- c("Entrees", "median", "ratio", 311412, 6, 2006, 2006,
                  "Brand (WP)", "RCL", 2.09)
entrees_2017 <- c("Entrees", "median", "ratio", 311412, 6, 2017, 2017,
                  "Brand (WP)", "RCL", 2.42)

names(ice_2006) <- names(blp_0)
names(ice_2017) <- names(blp_0)
names(juice_b_2006) <- names(blp_0)
names(juice_b_2017) <- names(blp_0)
names(cookies_2006) <- names(blp_0)
names(cookies_2017) <- names(blp_0)
names(entrees_2006) <- names(blp_0)
names(entrees_2017) <- names(blp_0)

blp <- rbind(blp, brand_frac_2006, brand_frac_2017, brand_blp_2006,
             brand_blp_2017, ice_2006, ice_2017, juice_b_2006, juice_b_2017,
             cookies_2006, cookies_2017, entrees_2006, entrees_2017)


######################## 23 ###############################
#Li (2021)
# NL
# Uses full set of  NielsenHouseholdPanelData
# use food manufactoring naisc 311
# cost weighted markup (p-c)/c
# 2010-2015
# from figure 1.4 with https://apps.automeris.io/wpd/
#2010, 0.880
#2011 0.874
#2012, 0.875
#2013, 0.865
#2014, 0.870
#2015; 0.864
#2016; 0.852
#use one per 5 year interval

food_2010 <- data.frame("Food Manufactoring", "cw", "markup", 311, 3,
                        2010, 2010, "Li (2021)", "NL", 0.880)
food_2015 <- data.frame("Food Manufactoring", "cw", "markup", 311, 3,
                        2015, 2015, "Li (2021)", "NL", 0.864)

names(food_2010) <- names(blp_0)
names(food_2015) <- names(blp_0)

blp <- rbind(blp, food_2010, food_2015)


######################## 24 ###############################
# Monardo (2021)
# uses own approach (FIL) as well as BLP
# FIL (call other): .3531; BLP: .3400 (lerner)
# ready to eat cereal
# 1988-1992

cereal_1988 <- data.frame("RTE Cereal", "avg", "lerner", 31123, 5, 1988, 1992,
                          "Monardo (2021)", "Other",  .3531)
cereal_1988_blp <- data.frame("RTE Cereal", "avg", "lerner", 31123, 5,
                              1988, 1992, "Monardo (2021)", "RCL",  .34)

names(cereal_1988) <- names(blp_0)
names(cereal_1988_blp) <- names(blp_0)

blp <- rbind(blp, cereal_1988, cereal_1988_blp)


######################## 25 ###############################
# Bontemps, Wei (2021)
# airlines naics 481111
# nested logit
# 2011 average lerner: .27;  2016 .33

airline_2011 <- data.frame("Airline", "avg", "lerner", 481111, 6, 2011, 2011,
                           "Bontemps, Wei (2021)", "NL",  .27)
airline_2016 <- data.frame("Airline", "avg", "lerner", 481111, 6, 2016, 2016,
                           "Bontemps, Wei (2021)", "NL",  .33)

names(airline_2011) <- names(blp_0)
names(airline_2016) <- names(blp_0)

blp <- rbind(blp, airline_2011, airline_2016)


######################## 26 ###############################
# Jingwen (2021)
# 2012
#rcl
# coffee: average: 3.025/3.807 = .795 #nolint
# soda: average: 2.265/1.228 = 1.844 #nolint

coffee_2012 <- data.frame("Coffee", "avg", "markup", 311920, 6, 2012, 2012,
                          "Jingwen (2021)", "RCL",  .795)
soda_2012 <- data.frame("Soda", "avg", "markup", 312111, 6, 2012, 2012,
                        "Jingwen (2021)", "RCL",  1.844)

names(coffee_2012) <- names(blp_0)
names(soda_2012) <- names(blp_0)

blp <- rbind(blp, coffee_2012, soda_2012)


######################## 27 ###############################
#Badruddozal, McCluskey, Carlson (2022)
# median lerner .398
#milk products 2018 niacs 3115

milk_2018 <- data.frame("Milk Products", "median", "lerner", 3115, 6,
                        2018, 2018, "Badruddozal et al. (2022)", "RCL",  .398)

names(milk_2018) <- names(blp_0)

blp <- rbind(blp, milk_2018)


######################## 28 ###############################
# Atalay,  Frost,  Sorensen,  Sullivan, Zhu (2023)
# median lerner (P-mc)/P
# Soft Drinks- Carbonated: 2006: 0.15; 2018: 0.11
# Soft Drinks- Low Calorie : 0.18; 2018: 0.35
# Water-Bottled: 0.25; 2018: 0.40
# Cereal- Ready To Eat: 0.37; 2018: 0.35
# Light Beer (Low Calorie/Alcohol): 0.17; 2018: 0.16
# Toilet Tissue: 0.23; 2018: 0.39
# Fruit Drinks-Other Container: 0.22 ; 2018: 0.21
# Dairy-Milk-Refrigerated: 0.25 ; 2018: 0.36
# Yogurt-Refrigerated: 0.29 ; 2018: 0.37

#for soft drinks will take midpoint of reg and loc cal: 2006: 0.165; 2018: 0.23
# naisc code: 312111 Soft Drink Manufacturing
# lerner
soda_2006 <- c("Soft Drinks", "median", "lerner", 312111, 6, 2006, 2006,
               "Atalay et al. (2023)", "NL", .165)
soda_2018 <- c("Soft Drinks", "median", "lerner", 312111, 6, 2018, 2018,
               "Atalay et al. (2023)", "NL", .23)
names(soda_2006) <- names(blp_0)
names(soda_2018) <- names(blp_0)


# naisc code: 312112 Bottled Water Manufacturing
water_2006 <- c("Water-Bottled", "median", "lerner", 312111, 6, 2006, 2006,
                "Atalay et al. (2023)", "NL", .25)
water_2018 <- c("Water-Bottled", "median", "lerner", 312111, 6, 2018, 2018,
                "Atalay et al. (2023)", "NL", .4)
names(water_2006) <- names(blp_0)
names(water_2018) <- names(blp_0)


# naisc code: 31123 Breakfast Cereal Manufacturing
cereal_2006 <- c("Cereal- Ready To Eat", "median",
                 "lerner", 31123, 5, 2006, 2006,
                 "Atalay et al. (2023)", "NL", .37)
cereal_2018 <- c("Cereal- Ready To Eat", "median",
                 "lerner", 31123, 5, 2018, 2018,
                 "Atalay et al. (2023)", "NL", .35)
names(cereal_2006) <- names(blp_0)
names(cereal_2018) <- names(blp_0)


# naisc code: 322291 Sanitary Paper Product Manufacturing
toilet_2006 <- c("Toilet Tissue", "median", "lerner", 322291, 6, 2006, 2006,
                 "Atalay et al. (2023)", "NL", .23)
toilet_2018 <- c("Toilet Tissue", "median", "lerner", 322291, 6, 2018, 2018,
                 "Atalay et al. (2023)", "NL", .39)
names(toilet_2006) <- names(blp_0)
names(toilet_2018) <- names(blp_0)

# naisc code: 311411 Frozen Fruit, Juice, and Vegetable Manufacturing
juice_2006 <- c("Fruit Drinks", "median", "lerner", 311411, 6, 2006, 2006,
                "Atalay et al. (2023)", "NL", .22)
juice_2018 <- c("Fruit Drinks", "median", "lerner", 311411, 6, 2018, 2018,
                "Atalay et al. (2023)", "NL", .21)
names(juice_2006) <- names(blp_0)
names(juice_2018) <- names(blp_0)

# naisc code: 311511 Fluid Milk Manufacturing
milk_2006 <- c("Dairy-Milk-Refrigerated", "median", "lerner",
               311511, 6, 2006, 2006,
               "Atalay et al. (2023)", "NL", .25)
milk_2018 <- c("Dairy-Milk-Refrigerated", "median", "lerner",
               311511, 6, 2018, 2018,
               "Atalay et al. (2023)", "NL", .36)
names(milk_2006) <- names(blp_0)
names(milk_2018) <- names(blp_0)

#other (page 40+)
#ProductMarket 2006 2018
# Median Mean Median Mean
#UW W UW W UW W UW W
#DogFood-WetType 0.18 0.19 0.18 0.19 1.78 1.90 1.67 1.91
#   NAICS Code 311111 Dog and Cat Food Manufacturing
#Candy-Chocolate 0.31 0.34 0.32 0.35 0.59 0.63 0.59 0.64
#  NAICS 3113 : hershey's naisc code
# Beer 0.34 0.31 0.36 0.36 0.09 0.10 0.09 0.10
#  NAICS 312120 - Breweries

dog_2006 <- c("Dog Food", "median", "lerner", 311111, 6, 2006, 2006,
              "Atalay et al. (2023)", "NL", .18)
dog_2018 <- c("Dog Food", "median", "lerner", 311111, 6, 2018, 2018,
              "Atalay et al. (2023)", "NL", .78)
names(dog_2006) <- names(blp_0)
names(dog_2018) <- names(blp_0)

chocolate_2006 <- c("Chocolate", "median", "lerner", 3113, 4, 2006, 2006,
                    "Atalay et al. (2023)", "NL", .31)
chocolate_2018 <- c("Chocolate", "median", "lerner", 3113, 4, 2018, 2018,
                    "Atalay et al. (2023)", "NL", .59)
names(chocolate_2006) <- names(blp_0)
names(chocolate_2018) <- names(blp_0)

beer_2006 <- c("Beer", "median", "lerner", 312120, 6, 2006, 2006,
               "Atalay et al. (2023)", "NL", .34)
beer_2018 <- c("Beer", "median", "lerner", 312120, 6, 2018, 2018,
                "Atalay et al. (2023)", "NL", .09)
names(beer_2006) <- names(blp_0)
names(beer_2018) <- names(blp_0)

blp <- rbind(blp, soda_2006, soda_2018, water_2006, water_2018,
             cereal_2006, cereal_2018, toilet_2006, toilet_2018,
             juice_2006, juice_2018, milk_2006, milk_2018, dog_2006,
             dog_2018, chocolate_2006, chocolate_2018, beer_2006, beer_2018)


######################## 29 ###############################
# Michel, Mino, Weirgraeber (2023)
# RTE cerial
# median lerner
# RCNL
#1991-1992: 0.414; (1993-1995): 0.188; (1996): 0.045 #nolint

cereal_1991 <- data.frame("RTE Cereal", "median", "lerner", 31123, 5, 1991,
                          1992, "Michel et al. (2023)", "RCNL",  .414)
cereal_1993 <- data.frame("RTE Cereal", "median", "lerner", 31123, 5, 1993,
                          1995, "Michel et al. (2023)", "RCNL",  .188)
cereal_1996 <- data.frame("RTE Cereal", "median", "lerner", 31123, 5, 1996,
                          1996, "Michel et al. (2023)", "RCNL",  .045)

names(cereal_1991) <- names(blp_0)
names(cereal_1993) <- names(blp_0)
names(cereal_1996) <- names(blp_0)

blp <- rbind(blp, cereal_1991, cereal_1993, cereal_1996)


######################## 30 ###############################
# Conlon, Rao (2023)
#2013
#median .233
# alchol naisc 312120, 312130, 312140
alcohol_2013 <- data.frame("Alcohol", "median", "lerner", "31212, 31213, 31214",
                           5, 2013, 2013, "Conlon, Rao (2023)", "RCNL",  .233)
names(alcohol_2013) <- names(blp_0)
blp <- rbind(blp, alcohol_2013)


######################## 31 ###############################
# Miravete, Seim, Thurk (2023)
# RCNL
#RTE Cereal median  lerner 1988: 0.338
# from fig 3 using webplot digitizer

cereal_1988 <- data.frame("RTE Cereal", "median", "lerner", 31123, 5, 1988,
                          1988, "Miravete et al. (2023)", "RCNL",  .338)

names(cereal_1988) <- names(blp_0)
blp <- rbind(blp, cereal_1988)


######################## 32 ###############################
#Miller, Osborne, Sheu, Sileo (2023)
# RCL
# 1974-2019
# concrete products naisc 3273
# from figure 6 with https://apps.automeris.io/wpd/
# median markup ratio

#1976, 1.178089552238806
#1977, 1.1759402985074625
#1978, 1.1688557213930346
#1979, 1.1749054726368158
#1980, 1.1854129353233829
#1981, 1.1986268656716417
#1983, 1.2127164179104477
#1984, 1.2105671641791045
#1986, 1.2152636815920397
#1987, 1.2208358208955223
#1988, 1.2258507462686565
#1989, 1.2288756218905472
#1990, 1.233731343283582
#1991, 1.261273631840796
#1992, 1.2486965174129352
#1993, 1.2486169154228854
#1994, 1.2436019900497512
#1995, 1.2424079601990048
#1996, 1.2379502487562188
#1997, 1.2314228855721392
#1998, 1.228318407960199
#1999, 1.2279203980099502
#2000, 1.2356417910447761
#2001, 1.2485373134328357
#2002, 1.2569751243781093
#2003, 1.2569751243781093
#2005, 1.2326169154228854
#2006, 1.2206766169154228
#2008, 1.2398606965174128
#2010, 1.25044776119403
#2013, 1.2475820895522387
#2016, 1.2502089552238806

#use 1 every 5 years

concrete_1976 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            1976, 1976, "Miller et al. (2023)", "RCL", 1.178)
concrete_1981 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            1981, 1981, "Miller et al. (2023)", "RCL", 1.199)
concrete_1986 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            1986, 1986, "Miller et al. (2023)", "RCL", 1.215)
concrete_1991 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            1991, 1991, "Miller et al. (2023)", "RCL", 1.261)
concrete_1996 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            1996, 1996, "Miller et al. (2023)", "RCL", 1.238)
concrete_2001 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            2001, 2001, "Miller et al. (2023)", "RCL", 1.249)
concrete_2006 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            2006, 2006, "Miller et al. (2023)", "RCL", 1.221)
concrete_2010 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            2011, 2011, "Miller et al. (2023)", "RCL", 1.250)
concrete_2016 <- data.frame("Concrete Products", "median", "ratio", 3273, 5,
                            2016, 2016, "Miller et al. (2023)", "RCL", 1.250)

names(concrete_1976) <- names(blp_0)
names(concrete_1981) <- names(blp_0)
names(concrete_1986) <- names(blp_0)
names(concrete_1991) <- names(blp_0)
names(concrete_1996) <- names(blp_0)
names(concrete_2001) <- names(blp_0)
names(concrete_2006) <- names(blp_0)
names(concrete_2010) <- names(blp_0)
names(concrete_2016) <- names(blp_0)

blp <- rbind(blp, concrete_1976, concrete_1981, concrete_1986, concrete_1991,
             concrete_1996, concrete_2001, concrete_2006, concrete_2010,
             concrete_2016)


######################## 33 ###############################
# Yigma (2023)
# airline
# mean lerner: .247
#2002-2012

airline_2002 <- data.frame("Airline", "avg", "lerner", 481111, 6, 2002, 2012,
                           "Yigma (2023)", "NL",  .247)
names(airline_2002) <- names(blp_0)
blp <- rbind(blp, airline_2002)


######################## 34 ###############################
# Grieco, Murry, Yurukolglu (2024)
# sales weighted markup ratio
# RCL
#US automobile indsutry
# naisc 3361
# from figure VIII with https://apps.automeris.io/wpd/
#1980, 1.7751515151515151
#1985, 1.521212121212121
#1990, 1.4042424242424243
#1995, 1.3357575757575757
#2000, 1.3036363636363637
#2005, 1.2854545454545452
#2010, 1.3157575757575755
#2015, 1.2987878787878786
# take all

auto_1980 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1980, 1980,
                        "Grieco et al. (2024)", "RCL", 1.775)
auto_1985 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1985, 1985,
                        "Grieco et al. (2024)", "RCL", 1.521)
auto_1990 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1990, 1990,
                        "Grieco et al. (2024)", "RCL", 1.404)
auto_1995 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 1995, 1995,
                        "Grieco et al. (2024)", "RCL", 1.336)
auto_2000 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2000, 2000,
                        "Grieco et al. (2024)", "RCL", 1.304) 
auto_2005 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2005, 2005,
                        "Grieco et al. (2024)", "RCL", 1.285)
auto_2010 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2010, 2010,
                        "Grieco et al. (2024)", "RCL", 1.316)
auto_2015 <- data.frame("Automobile", "sw", "ratio", 3361, 4, 2015, 2015,
                        "Grieco et al. (2024)", "RCL", 1.299)

names(auto_1980) <- names(blp_0)
names(auto_1985) <- names(blp_0)
names(auto_1990) <- names(blp_0)
names(auto_1995) <- names(blp_0)
names(auto_2000) <- names(blp_0)
names(auto_2005) <- names(blp_0)
names(auto_2010) <- names(blp_0)
names(auto_2015) <- names(blp_0)

blp <- rbind(blp, auto_1980, auto_1985, auto_1990, auto_1995, auto_2000,
             auto_2005, auto_2010, auto_2015)


######################## 35 ###############################
#  Ganapati (2024)
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

names(wholesale_1997) <- names(blp_0)
names(wholesale_2002) <- names(blp_0)
names(wholesale_2007) <- names(blp_0)

blp <- rbind(blp, wholesale_1997, wholesale_2002, wholesale_2007)


######################## 36 ###############################
# Dooper et al (2023)
# use food manufactoring naisc 311
# annual median lerner (from figure 3)
# https://apps.automeris.io/wpd/:
#NL
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
#take one evert 5 years

food_2006 <- c("Food Manufactoring", "median", "lerner", 311, 3, 2006, 2006,
               "Dooper et al. (2023)", "RCL",  .454)
food_2011 <- c("Food Manufactoring", "median", "lerner", 311, 3, 2011, 2011,
               "Dooper et al. (2023)", "RCL",  .509)
food_2016 <- c("Food Manufactoring", "median", "lerner", 311, 3, 2016, 2016,
               "Dooper et al. (2023)", "RCL",  .595)
food_2019 <- c("Food Manufactoring", "median", "lerner", 311, 3, 2019, 2019,
               "Dooper et al. (2023)", "RCL",  .6)

names(food_2006) <- names(blp_0)
names(food_2011) <- names(blp_0)
names(food_2016) <- names(blp_0)
names(food_2019) <- names(blp_0)

blp <- rbind(blp, food_2006, food_2011, food_2016, food_2019)

######################## 37 ###############################
# Pfander (2024)
#mean transport p/mc-1: .035 (table 2) 2017 commercial transportation
#naisc code: 48
#NL
#representative so sales weighted

transport_2017 <- c("Transportation", "sw", "markup", 48, 2, 2017, 2017,
                    "Pfander (2024)", "NL",  .035)

names(transport_2017) <- names(blp_0)

blp <- rbind(blp, transport_2017)


############################################################
############################################################
# Save to csv
#navigate to folder with data
setwd(dircs[2])


write.csv(blp, "blp_data.csv", row.names = TRUE)
