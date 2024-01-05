
require(dplyr)
require(ggplot2)
require(fixest)
############################################################
############################################################
##############   0:Aggregate Markups #######################
############################################################
############################################################

#aggregate markups
agg_mu <- function(MU_data, reweight_data) { # nolint
  tempdata <- MU_data %>%
    filter(!is.na(usercost)) %>% # nolint
    filter(MU > -100) %>% # nolint
    filter(MU < 100000)

  tempdata_2 <- tempdata %>%
    group_by(fyear) %>% # nolint
    summarise(weighted.mean(MU, sale, na.rm = TRUE)) # nolint

  names(tempdata_2) <- c("year", "Ag_MU")
  #rename

  print(tempdata_2)

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

  print(tempdata_4)

}



############################################################
############################################################
##############   1: make MU / ADR  #########################
############################################################
############################################################


#scatter plot with sample points and trend lines
MU_advert_plot <- function(Sub_Panel_data, sub_panel_name, N){ # nolint

  ############################################################
  ###1: clean
  ############################################################

  #filter vars
  tempData <-  Sub_Panel_data[, c( # nolint
    "MU_1", "Adr_MC"
  )]

  ############################################################
  ###2: objects to plot
  ############################################################

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
  ############################################################
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
  ############################################################


  ggplot() +
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

}





#loop
Scatter_plot_loop <- function(Data, naics, D, N) {# nolint

  #run Industry_n_dig to make data

  #clean industry names, store names
  # loop over industry
  # run MU_advert_plot

  ########################################################################
  ################## run Industry_n_dig to make data ##################
  ########################################################################

  #generate N digit industry names
  temp_data <- Industry_n_dig(Data, naics, D) # nolint

  ####################################################################
  ################# clean industry names, store names  #################
  ####################################################################

  #get rid of annoying T's
  temp_data$industry <- paste(temp_data$industry, "")
  temp_data$industry <- gsub("T  ", "", temp_data$industry)
  temp_data$industry <- gsub("T ", "", temp_data$industry)
  temp_data$industry <- gsub("  ", "", temp_data$industry)


  ####################################################################
  ################# loop over sectors  #################
  ####################################################################

  output_list <- list()

  for (i in unique(temp_data$industry[!is.na(temp_data$industry)])) {
    tempname <- paste(i)

    sector_temp <- temp_data %>%
      filter(industry == i) # nolint

    #make sure not requesting more points that available
    n <- min(N, length(sector_temp[, 1]))

    tempplot <- MU_advert_plot(sector_temp, tempname, n)

    #Plot within sector
    output_list[[i]] <- tempplot
  }

  output_list
}


########################################################################
#                   tableloop
########################################################################
Test_table <- function(Data, naics, N) { # nolint

  #run Industry_n_dig to make data

  #clean industry names, store names

  # loop over industry
  # run MU_advert_plot

  ########################################################################
  ##################run Industry_n_dig to make data ##################
  ########################################################################

  #generate N digit industry names
  temp_data <- Industry_n_dig(Data, naics, N) # nolint

  ####################################################################
  ################# clean industry names, store names  #################
  ####################################################################

  #get rid of annoying T's and NA
  temp_data$industry <- paste(temp_data$industry, "")
  temp_data$industry <- gsub("T  ", "", temp_data$industry)
  temp_data$industry <- gsub("T ", "", temp_data$industry)
  temp_data$industry <- gsub("  ", "", temp_data$industry)
  temp_data <- temp_data %>%
    filter(!industry == "NA ") # nolint

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

  ####################################################################
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

  ####################################################################
  ################# loop over sectors  #################
  ####################################################################


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
##############   2: Efficency  ############################
############################################################
############################################################



#####  #general simple efficincy plot#
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

  ggplot(temp_data, aes(fit * 100,industry)) + # nolint
    geom_bar(stat = "identity") +
    labs(title = "Estimated Advertising Efficency (2022)",
         x = "Advertising Efficency (Log Scale)",
         y = "Industry (3 Digit Naics Code)") +
    scale_x_continuous(trans = pseudo_log_trans(base = 10), # nolint
                       breaks = breaks * 100,
                       labels = scales::label_comma(scale = .01))
}





############################################################
############################################################
##############   3: Multiple Bar plots  ####################
############################################################
############################################################

#grab sector level aggregate MU and adr
Sector_MU_Adr <- function(Dset, naics, N) { # nolint

  #generate N digit industry names
  temp_data <- Industry_n_dig(Dset, naics, N) # nolint

  ####################################################################
  ################# clean industry names, store names  #################
  ####################################################################

  #get rid of annoying T's and NA
  temp_data$industry <- paste(temp_data$industry, "")
  temp_data$industry <- gsub("T  ", "", temp_data$industry)
  temp_data$industry <- gsub("T ", "", temp_data$industry)
  temp_data$industry <- gsub("  ", "", temp_data$industry)
  temp_data <- temp_data %>%
    filter(!industry == "NA ") # nolint

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

  print(tempdata)

}


########## Make Efficency + MU & Adr ##############
Efficency_plot_stacked <- function(hold) { # nolint

  hold2 <- hold %>% select(-se)

  names(hold2) <- c("industry", "C_MU", "B_Adr", "A_Exad")

  labs <- c(C_MU = "Markup (Sales Weighted)", B_Adr = "xad (Sales Weighted)",
            A_Exad = "Advertising Efficency (2022)")

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


  plot

}