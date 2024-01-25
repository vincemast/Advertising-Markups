
require(dplyr)
require(ggplot2)
require(fixest)
require(scales)
############################################################
############################################################
##############   1: Densities  #######################
##############  Figures 1 and 2  #######################
############################################################

############################################################
##############   1.a XAD density  ############################
############################################################

xad_density <- function(Data) {# nolint
  xaddensity <- ggplot(Data, aes(x = Adr_MC)) + # nolint
    geom_density() +
    theme(text = element_text(size = 20)) +
    labs(x = "Advertising/Marginal Cost (Log-scale)", y = "Density") +
    scale_x_continuous(trans = log10_trans(), # nolint
                       limits = c(.00001, 30), labels = comma) #nolint

  print(xaddensity)
}

############################################################
##############   1.b MU density  ############################
############################################################

mu_density <- function(Data,Dset) {# nolint
  mudensity <- ggplot() +
    geom_density(data = Dset, aes(x = MU, color = "Full Sample")) + # nolint
    geom_density(data = Data, aes(x = MU, color = "XAD Reported")) +
    theme(text = element_text(size = 20)) +
    labs(x = "Markup (Log-scale)", y = "Density") +
    scale_x_continuous(trans = pseudo_log_trans(), # nolint
                       limits = c(-.1, 6),
                       breaks = c(0, 1, 6, 11),
                       labels = function(x) comma(x - 1)) +
    theme(legend.position = "bottom")

  print(mudensity)
}

############################################################
############################################################
##############   2:Aggregate Markups #######################
##############    figure 3           #######################
############################################################
############################################################

############################################################
##############   2.a create data  ############################
############################################################
#aggregate markups
agg_mu <- function(MU_data, reweight_data) { # nolint
  tempdata <- MU_data %>%
    filter(!is.na(usercost)) %>% # nolint
    filter(MU >= 0) %>% # nolint
    filter(MU < 100000)
    #drop crazy values

  tempdata_2 <- tempdata %>%
    group_by(fyear) %>% # nolint
    summarise(weighted.mean(MU, sale, na.rm = TRUE)) # nolint

  names(tempdata_2) <- c("year", "Ag_MU")
  #rename

  tempdata_2

}

#Aggregate markups with weight
agg_mu_reweight <- function(MU_data, reweight_data) {# nolint


tempdata_MU <- MU_data %>% # nolint
  filter(!is.na(naics)) %>% # nolint
  filter(!is.na(MU)) %>% # nolint
  filter(!is.na(sale)) # nolint
tempdata_weight <- reweight_data %>%
  filter(!is.na(naics)) %>% # nolint
  filter(!is.na(MU)) %>% # nolint
  filter(!is.na(sale)) %>% # nolint
  filter(sale > 0)
  #remove NAs for sector

  tempdata <- tempdata_MU %>%
    group_by(fyear,naics) %>% # nolint
  summarise(weighted.mean(MU, sale, na.rm = TRUE)) # nolint
  #generate sales weighted average markups within each sector year
  names(tempdata) <- c("year", "naics", "MU")
  #rename

tempdata_2 <- tempdata_weight %>% # nolint
  group_by(fyear, naics) %>% # nolint
  summarise(sum(sale, na.rm = TRUE)) # nolint
  #generate weights for each secgor within each year
  names(tempdata_2) <- c("year", "naics", "sale")
  #rename

tempdata_3 <- merge(tempdata, tempdata_2) # nolint
  #merge with weights

tempdata_4 <- tempdata_3 %>% # nolint
  group_by(year) %>% # nolint
  summarise(weighted.mean(MU, sale, na.rm = TRUE)) # nolint
  #apply weights
  names(tempdata_4) <- c("year", "Ag_MU")

  tempdata_4

}

############################################################
##############   2.b Plot  ############################
############################################################

agg_mu_plot <- function(fullsample, subsample) {

  agg_mu_all <- invisible(agg_mu(fullsample))
  agg_mu_insamp <- invisible(agg_mu(subsample))
  agg_mu_rew <- invisible(agg_mu_reweight(subsample, fullsample))

  agg_mu_plot <-  ggplot() +
    geom_line(data = agg_mu_all,
              aes(y = Ag_MU - 1, x = year, color = "Full Sample")) + # nolint
    geom_line(data = agg_mu_insamp,
              aes(y = Ag_MU - 1, x = year, color = "XAD Reported")) +
    geom_line(data = agg_mu_rew,
              aes(y = Ag_MU - 1, x = year, color = "Reweighted")) +
    theme(text = element_text(size = 20)) +
    labs(x = "Year", y = "Sales Weighted Markup") +
    theme(legend.position = "bottom")

  print(agg_mu_plot)
}

############################################################
############################################################
##############   3: Scatter plot and table  ################
##############   figure 4 and table 2  ################
############################################################
############################################################

############################################################
#   3.a scatter plots with sample points and trend lines  ##
############################################################

MU_advert_plot <- function(Sub_Panel_data, sub_panel_name, N){ # nolint

  ############################################################
  ###1: clean

  #filter vars
  tempData <-  Sub_Panel_data[, c( # nolint
    "MU_1", "Adr_MC"
  )]

  ############################################################
  ###2: objects to plot

  #sample
  set.seed(123456)
  samp <- tempData[sample(nrow(tempData), size = N), ]

  #no int regression line
  model <- lm(tempData$Adr_MC ~ tempData$MU_1 - 1)
  #calculate confidence interval for regression coefficient for 'hours'
  con <- round(confint(model, "tempData$MU_1", level = 0.95), digits = 4)
  con_int <- paste("[", con[, 1], ",", con[, 2], "]", sep = "")
  reg_line <- c(paste("Slope=", # nolint
                      round(model$coefficients, digits = 4)),
                paste("95% interval:", con_int))

  #int regression line
  model_2 <- lm(tempData$Adr_MC ~ tempData$MU_1)
  #calculate confidence interval for regression coefficient for 'hours'
  con_2 <- round(confint(model_2)[1, ], digits = 4)
  con_int_int <- paste("[", con_2[1], ",", con_2[2], "]", sep = "")

  con_3 <- round(confint(model_2)[2, ], digits = 4)
  con_int_slope <- paste("[", con_3[1], ",", con_3[2], "]", sep = "")
  reg_line_2 <-c(paste("Intercept=", round(model_2$coefficients[1], digits = 4)), # nolint
                 paste("95% interval:", con_int_int),
                 paste("Slope=", round(model_2$coefficients[2], digits = 4)),
                 paste("95% interval:", con_int_slope))

  caption_temp <- paste("No intercept: Slope =", # nolint
                        round(model$coefficients, digits = 4),
                        " (95% confidence interval: ", con_int,
                        ").\n", "W/ intercept: Slope =",
                        round(model_2$coefficients[2], digits = 4),
                        " (95% confidence interval: ", con_int_slope,
                        "), ", "Intercept=",
                        round(model_2$coefficients[1], digits = 4),
                        " (95% confidence interval: ", con_int_int, ").",
                        sep = "")

  ###########################################################
  ###3: title
  #create title
  title_temp <- paste(
                      "Trend lines +", N, " sample points", "\n(",
                      sub_panel_name, " firms)",
                      sep = "")

  colors <- c("Sample points" = "black",
              "OLS line (with intercept)" = "black",
              "OLS line (No intercept)" = "blue")

  ############################################################
  ###4: plot

  plot <- ggplot() +
    geom_point(aes(samp$MU_1, samp$Adr_MC), shape = 1) +
    geom_line(aes(tempData$MU_1, predict(model_2),
                  color = "OLS line (with intercept)"), size = 1.5) +
    geom_line(aes(tempData$MU_1, predict(model),
                  color = "OLS line (No intercept)") ,
              linetype = "dashed", size = 1.5) +
    ylim(quantile(tempData$Adr_MC, probs = .00001), .6) +
    xlim(quantile(tempData$MU_1, probs = .003), 5.5) +
    ylab("Advertising share") +
    xlab("Markup") +
    #labs(title = title_temp, caption = caption_temp,color="legend")+
    #optinal caption
    labs(title = title_temp, color = "legend") +
    theme(legend.position = "bottom") +
    theme(text = element_text(size = 25)) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0)) +
    scale_color_manual(values = colors)

  print(plot)

}

############################################################
##################   3.b test table  ########################
############################################################
test_table <- function(data, naics, n) {


  #run Industry_n_dig to make data
  temp_data <- industry_n_dig(data, naics, n) # nolint

  ################# Store industry names##################

  #remove NAs
  temp_data <- temp_data %>%
    filter(!is.na(industry)) # nolint

  #collect sector names
  sectors_ <- data.frame(unique(temp_data$industry))
  names(sectors_) <- "industry"
  #only industries with >2 obs will produce an estimate
  #get number of observations for each industry
  Ind_count_temp <- temp_data %>% group_by(industry) %>% tally() # nolint
  names(Ind_count_temp) <- c("industry", "n") # nolint
  hold <- merge(sectors_, Ind_count_temp) # nolint
  sectors_temp <- merge(sectors_, Ind_count_temp) %>% filter(n > 2)
  sectors <- sectors_temp$industry

  ################# table  #################
  #fill in full sample row

  #no int regression
  tempmodel_1 <- feols(Adr_MC ~ MU_1 - 1,
                       data = temp_data)
  #w/ int regression
  tempmodel_2 <- feols(Adr_MC ~ MU_1,
                       data = temp_data)


  #fill in point estimates in row 1 (default to not)
  table <- data.frame(
    "All",
    round(tempmodel_1$coefficients[1], digits = 4),
    round(tempmodel_2$coefficients[2], digits = 4),
    round(tempmodel_2$coefficients[1], digits = 4),
    "No"
  )
  #fill in se's on row below (in parenthesis)
  table[2, 2] <- paste("(",
                       format(round(summary(tempmodel_1)$se[1], digits = 4),
                              scientific = FALSE), ")", sep = "")
  table[2, 3] <- paste("(",
                       format(round(summary(tempmodel_2)$se[2], digits = 4),
                              scientific = FALSE), ")", sep = "")
  table[2, 4] <- paste("(",
                       format(round(summary(tempmodel_2)$se[1], digits = 4),
                              scientific = FALSE), ")", sep = "")
  #check if confidence interval crosses 0
  test <- confint(tempmodel_2, level = 0.95)[1, 1] * confint(tempmodel_2,
                                level = 0.95)[1, 2] # nolint
  #<0 iff crosses 0
  if (test > 0) {
    table[1, 5] <- "Yes"
  }

  names(table) <- c(
                    "Sample", "Slope(No intercept)", "Slope",
                    "Intercept", "Reject Intercept=0?")

  ################# loop over sectors  #################


  for (i in 1:length(sectors)) { # nolint
    tempname <- paste(sectors[i])

    sector_temp <- temp_data %>%
      filter(industry == sectors[i]) # nolint

    #no int regression
    tempmodel_1 <- feols(Adr_MC ~ MU_1 - 1,
                         data = sector_temp)
    #w/ int regression
    tempmodel_2 <- feols(Adr_MC ~ MU_1,
                         data = sector_temp)

    #fill in point estimates in row 1 (default to not)
    table[2 * i + 1, ] <- data.frame(
      tempname,
      round(tempmodel_1$coefficients[1], digits = 4),
      round(tempmodel_2$coefficients[2], digits = 4),
      round(tempmodel_2$coefficients[1], digits = 4), "No"
    )

    #fill in se's on row below (in parenthesis)
    table[2 * i + 2, 2] <- paste("(",
            format(round(summary(tempmodel_1)$se[1], digits = 4), # nolint
                   scientific = FALSE), ")", sep = "")

    table[2 * i + 2, 3] <- paste("(",
                                 format(round(summary(tempmodel_2)$se[2],
                                              digits = 4), scientific = FALSE),
                                 ")", sep = "")

    table[2 * i + 2, 4] <- paste("(",
                                 format(round(summary(tempmodel_2)$se[1],
                                              digits = 4), scientific = FALSE),
                                 ")", sep = "")
    #check if confidence interval crosses 0
    test <- confint(tempmodel_2,
                    level = 0.95)[1, 1] * confint(tempmodel_2,
                                                  level = 0.95)[1, 2]
    #<0 iff crosses 0
    if (test > 0) {
      table[2 * i + 1, 5] <- "Yes"
    }

  }


  print(table)
}

############################################################
############################################################
##############   4: Efficency plots ########################
##############   figure 5 and others  #####################
############################################################
############################################################

############################################################
##############   4.a simple sector efficency plot  ##########
############################################################

#####  #general simple efficincy plot (sector)
Efficency_plot <- function(model,Ind_count,N) { # nolint


temp_data <- merge(model, Ind_count, by = "industry", all = TRUE) # nolint

  #filter out ones with less observations
  temp_data <- temp_data %>%
    filter(n >= N)
  temp_data <- temp_data %>%
    filter(!is.na(industry)) # nolint
  temp_data <- temp_data %>%
    filter(!is.na(fit)) # nolint

  temp_data$industry <- reorder(temp_data$industry, -temp_data$fit)

  Max <- round(summary(temp_data$fit)[6], digits = 0) # nolint

  breaks <- unique(c(0, .01, .05, .1, .2, .5, seq(1, Max, by = 1)))

  plot <- ggplot(temp_data, aes(fit * 100,industry)) + # nolint
    geom_bar(stat = "identity") +
    labs(title = "Estimated Advertising Efficency (2022)",
         x = "Advertising Efficency (Log Scale)",
         y = "Industry (3 Digit Naics Code)") +
    theme(text = element_text(size = 8)) +
    scale_y_discrete(labels = wrap_format(60)) + # nolint
    scale_x_continuous(trans = pseudo_log_trans(base = 10), # nolint
                       breaks = breaks * 100,
                       labels = scales::label_comma(scale = .01))

  print(plot)
}

############################################################
##############   4.b 3-stack sector efficency plot  ##########
############################################################

#####  generate MU and Adr neeed for next plot#####
Sector_MU_Adr <- function(Dset, naics, N) { # nolint

  #generate N digit industry names
  temp_data <- industry_n_dig(Dset, naics, N) # nolint

  ################# Store industry names################################

  #remove NAs
  temp_data <- temp_data %>%
    filter(!is.na(industry)) # nolint

  temp_data <- temp_data %>%
    filter(!is.na(usercost)) %>% # nolint
    filter(MU > -100) %>% # nolint
    filter(MU < 100000)


  tempdata_2 <- temp_data %>%
    group_by(industry) %>% # nolint
    summarise(weighted.mean(MU_1, sale, na.rm = TRUE)) # nolint

  names(tempdata_2) <- c("industry", "Ag_MU")
  #rename


  tempdata_3 <- temp_data %>%
    group_by(industry) %>% # nolint
    summarise(weighted.mean(Adr_MC, sale, na.rm = TRUE)) # nolint

  names(tempdata_3) <- c("industry", "Ag_adr")
  #rename

  tempdata <- merge(tempdata_2, tempdata_3)

  tempdata

}

############################################################
########## plot ##############

Efficency_plot_stacked <- function(hold,labs) { # nolint

  hold2 <- hold %>% select(-se)

  names(hold2) <- c("industry", "C_MU", "B_Adr", "A_Exad")

  temp_data <- gather(hold2, variable,value, -industry) # nolint

  temp_data2 <- merge(temp_data, hold)

  temp_data2 <- temp_data2 %>%
    mutate(vmin = (value - 1.96 * se) * (variable == "A_Exad")) %>% # nolint
    mutate(vmax = (value + 1.96 * se) * (variable == "A_Exad"))

  temp_data2 <- temp_data2 %>%
    mutate(vmin = na_if(vmin, 0)) %>% # nolint
    mutate(vmax = na_if(vmax, 0)) # nolint

  plot <- ggplot(temp_data2, aes(x = value, y = reorder(industry, - fit))) + # nolint
    geom_col() +
    theme_bw() +
    facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labs)) +
    theme(text = element_text(size = 20)) +
    scale_y_discrete(labels = wrap_format(22)) + # nolint
    geom_errorbar(aes(xmin = vmin, xmax = vmax), # nolint
                  width = 0.2, color = "darkblue") +
    labs(title = NULL, x = NULL, y = "Industry (2 Digit NAICS Code)")


  print(plot)

}

############################################################
##############   2.b time  ############################
############################################################

############################################################
########## flexible time plot ##############
time_plot <- function(year_coef, tit, D) { # nolint

  tempdata <- year_coef

  #create title allowing for input of title and number of digits
  temptitle <- paste(tit, " Time Trend\n(2022 Reference year, ",
                     D, " Digit NAICS)", sep = "")


  timecoplot3d <-
    ggplot(tempdata, aes(x = year, y = fit, group = year)) + #nolint
    geom_boxplot() +
    geom_errorbar(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
                  width = 0.2, color = "darkblue") +
    geom_hline(aes(yintercept = 0), colour = "black",
               linetype = "dashed", size = 1) +
    #geom_text(aes(1985, 0, label = "2022 Reference",
    #             vjust = -1), size = 4, colour = "black") +
    labs(x = "Year", y = "") +
    ggtitle(temptitle) +
    guides(fill = guide_legend(title = NULL)) +
    theme(text = element_text(size = 20)) +
    theme_minimal()

  print(timecoplot3d)

}


############################################################
############################################################
##############   5: trend plots ########################
##############   figure 5 and others  #####################
############################################################
############################################################

############################################################
##############   5.a Exad over time plot  ##########
############################################################
exad_ot_plot <- function(data, textsize, linetype) {

  #checks if linetype ="smooth"
  #pass through text size as textsize

  data$industry <- str_wrap(data$industry, width = 20) # nolint
  #space industry

  line <- geom_line()

  titlee <- "Estimated Advertising Response Elasticity"
  #create title noting if geometric smoothing

  if (linetype == "smooth") {
    titlee <- paste(titlee, " (Smoothed via LOESS)", sep = "")
    line <- geom_smooth()
  }

  plot <- ggplot(data, aes(x = year, y = fit, group = industry)) +# nolint
    line +
    facet_wrap(~industry, scales = "free_y") +
    theme(text = element_text(size = textsize)) +
    labs(x = "Year", y = "", title = titlee)

  print(plot)
}


############################################################
##############   5.b coefficent regression plots  ##########
############################################################

coef_reg_plot_stacked <- function(hold) { # nolint

  labs <- c(C_MU = "Markup Trend", B_Adr = "xad Trend",
            A_Exad = "Coefficent Trend")

  hold2 <- hold %>% select(-Exad_se, -adv_se, -MU_se) # nolint
  names(hold2) <- c("industry", "A_Exad", "B_Adr", "C_MU")
  temp_data <- gather(hold2, variable,value, -industry) # nolint

  hold3 <- hold %>% select(-Exad_trend, -adv_trend, -MU_trend) # nolint
  names(hold3) <- c("industry", "A_Exad", "B_Adr", "C_MU")
  tempdata <- gather(hold3, variable,se, -industry) # nolint

  temp_data2 <- merge(temp_data, tempdata)

  order <- hold %>% select(Sample, Exad_trend) # nolint
  order <- order %>% mutate(Exad_trend = 1000 * (Sample == "All") + Exad_trend) # nolint
  names(order) <- c("industry", "order")

  temp_data2 <- merge(temp_data2, order)

  plot <- ggplot(temp_data2, aes(x = value,  # nolint
                                 y = reorder(industry, order))) + # nolint
    geom_col() +
    theme_bw() +
    facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labs)) +
    theme(text = element_text(size = 20)) +
    scale_y_discrete(labels = wrap_format(30)) + # nolint
    geom_errorbar(aes(xmin = value+se*1.96, xmax = value-se*1.96), # nolint
                  width = 0.2, color = "darkblue") +
    labs(title = NULL, x = NULL, y = "Industry (2 Digit NAICS Code)")

  print(plot)

}


############################################################
############################################################
##############   6: intercept plots ########################
##############   figure 6? and others  #####################
############################################################
############################################################

############################################################
##############   6.a stacked intercept plots  ##########
############################################################

intercept_plot_stacked <- function(intercept, agg_2_digit, labs) {
  # Remove the row names
  rownames(intercept) <- NULL
  names(intercept) <- c("fit", "se", "industry")
  temp <- intercept %>% select(industry, fit, se) # nolint

  #merge with agg_2_digit
  hold2 <- merge(agg_2_digit, temp)

  #run plot
  intercept_co_plot_2d <- Efficency_plot_stacked(hold2, labs)
  intercept_co_plot_2d
}

############################################################
##############   6.a stacked intercept plots  ##########
############################################################
############################################################

time_plot <- function(year_coef, tit, D) { # nolint

  tempdata <- year_coef

  #create title allowing for input of title and number of digits
  temptitle <- paste(tit, " Time Trend\n(2022 Reference year, ",
                     D, " Digit NAICS)", sep = "")


  timecoplot3d <-
    ggplot(tempdata, aes(x = year, y = fit, group = year)) + #nolint
    geom_boxplot() +
    geom_errorbar(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
                  width = 0.2, color = "darkblue") +
    geom_hline(aes(yintercept = 0), colour = "black",
               linetype = "dashed", size = 1) +
    #geom_text(aes(1985, 0, label = "2022 Reference",
    #             vjust = -1), size = 4, colour = "black") +
    labs(x = "Year", y = "") +
    ggtitle(temptitle) +
    guides(fill = guide_legend(title = NULL)) +
    theme(text = element_text(size = 20)) +
    theme_minimal()

  print(timecoplot3d)

}

############################################################
##############   6.b time plot  ############################
############################################################
############################################################

ints_timeplot <- function(ints, title, D) { # nolint

  ints$year <- as.numeric(gsub("fyear", "", rownames(ints)))
  # Remove the row names and name for time plot
  rownames(ints) <- NULL
  names(ints) <- c("fit", "se", "year")

  plot <- time_plot(ints,
                    title, D)

  plot

}


############################################################
############################################################
##############   7: Correcting markups #####################
############################################################
############################################################

############################################################
#############   7.a: main specification ####################
############################################################

mu_correction <- function(data, naics, n) { #nolint

  temp_all <- industry_n_dig(data, naics, n) #nolint

  #run regression to gen slope
  temp_results <- invisible(regression_output_n(data, naics, n)) #nolint
  #save slopes
  sector_coef_2d <- data.frame(temp_results[3])
  year_coef_2d <- data.frame(temp_results[4])
  year_slope_2d <- year_coef_2d
  names(year_slope_2d) <- c("year_slope", "fyear", "year_s_se")
  sector_slope_2d <- sector_coef_2d
  names(sector_slope_2d) <- c("sec_slope", "industry", "sec_s_se")

  #gen intercept coefs
  temp_intercepts <- get_intercepts_and_errors(data, naics, 2) #nolint
  industry_ints <- data.frame(temp_intercepts$industry)
  year_ints <- data.frame(temp_intercepts$fyear)
  #save intercepts
  industry_ints$industry <- gsub("industry", "", rownames(industry_ints))
  year_ints$fyear <- as.numeric(gsub("fyear", "", rownames(year_ints)))
  #intercepts
  year_int_2d <- year_ints
  names(year_int_2d) <- c("year_int", "year_i_se", "fyear")
  sec_int_2d <- industry_ints
  names(sec_int_2d) <- c("sec_int", "sec_i_se", "industry")

  #merge
  year_corrects <- merge(year_slope_2d, year_int_2d, by = "fyear")
  sec_corrects <- merge(sector_slope_2d, sec_int_2d, by = "industry")

  temp <- merge(temp_all, sec_corrects, by = "industry")

  temp <- merge(temp, year_corrects, by = "fyear")

  temp <- temp %>%
    mutate(slope = year_slope + sec_slope)  %>% #nolint
    mutate(intercept = year_int + sec_int) %>% #nolint
    mutate(correction = 1 - intercept / slope) #nolint


  temp <- temp %>%
    mutate(MU_C = MU / correction) #nolint

  temp

}

############################################################
################   7.b: main reverse ######################
############################################################

mu_correction_r <- function(data, naics, n) { #nolint

  temp_all <- industry_n_dig(data, naics, n) #nolint

  model_reverse <- feols(
    MU ~ i(industry, Adr_MC) + i(fyear, Adr_MC, ref = 2022) - Adr_MC
    | industry + fyear, data = temp_all #nolint
  )

  year_correction <- data.frame(fixef(model_reverse)$fyear)
  year_correction$fyear <- row.names(year_correction)
  names(year_correction) <- c("year_correction", "fyear")
  sec_correction <- data.frame(fixef(model_reverse)$industry)
  sec_correction$industry <- row.names(sec_correction)
  names(sec_correction) <- c("sec_correction", "industry")


  rev_hold <- merge(temp_all, sec_correction, by = "industry")

  rev_hold_2 <- merge(rev_hold, year_correction, by = "fyear")

  rev_hold_2 <- rev_hold_2 %>%
    mutate(correction = year_correction + sec_correction) %>%
    mutate(MU_C = MU / correction) #nolint

}

############################################################
##########   7.c: sector x time specification ##############
############################################################

#done in rolling_window in regression.R
# set window to 1 for no rolling
#saved as results$corrections

############################################################
############   7.d: reverse specification ##################
############################################################

mu_correction_st_reverse <- function(Data, naics, n) { #nolint

  sector_time_estimates_n <- sector_time_reverse(Data, naics, n) #nolint

  sector_time_correction <- sector_time_estimates_n %>%
    filter(industry != "Full Sample") %>% #nolint
    select(-fit, -se) #nolint

  names(sector_time_correction) <- c("fyear", "industry", "correction")

  use_data <- industry_n_dig(Data, naics, n) %>% #nolint
    select(-naics_n, -GVKEY, -xopr, -gind, -naics, -emp, -ppegt, #nolint
          -xad, -xlr, -cogs, -xsga, -entry, -age, -life, -X, -CPI, #nolint
          -Adr, -Adr_MC, -time, -time2, -time3, -exit, -change) #nolint

  data_correction <- merge(use_data, sector_time_correction,
                           by = c("industry", "fyear"))

  data_correction <- data_correction %>%
    mutate(MU_C = MU / correction) #nolint

  data_correction

}


############################################################
#################   7.e: plot density ######################
############################################################

mu_c_plot <- function(correctdata) {

  mudensity <- ggplot() +
    geom_density(data = correctdata, aes(x = MU_C, color = "Corrected")) + # nolint
    geom_density(data = correctdata, aes(x = MU, color = "Raw")) + # nolint
    theme(text = element_text(size = 20)) +
    labs(x = "Markup (Log-scale)", y = "Density") +
    scale_x_continuous(trans = log10_trans(), # nolint
                       limits = c(.1, 13),
                       labels = function(x) comma(x - 1)) +
    theme(legend.position = "bottom")

  print(mudensity)

}

############################################################
#################   7.f:  plot agg MU ######################
############################################################

agg_mu_c <-  function(correctdata) {

  correctdata_c <- correctdata %>%
    mutate(MU = MU_C) #nolint

  agg_temp <- agg_mu(correctdata)
  agg_temp_c <- agg_mu(correctdata_c) #nolint

  agg_mu_c_plot <-  ggplot() +
    geom_line(data = agg_temp_c,
              aes(y = Ag_MU -1 , x = year, color = "Corrected")) + #nolint
    geom_line(data = agg_temp,
                aes(y = Ag_MU - 1, x = year, color = "Raw")) + # nolint
    theme(text = element_text(size = 20)) +
    labs(x = "Year", y = "Sales Weighted Markup") +
    theme(legend.position = "bottom")

  print(agg_mu_c_plot)

}

############################################################
###########   7.g:  rolling window correction ##############
############################################################

#done in rolling_window in regression.R
# set window to 1 for no rolling
#saved as results$corrections