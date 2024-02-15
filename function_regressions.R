
require(dplyr)
require(fixest)
require(modelsummary)
require(stringr)
require(lmtest)
require(sandwich)
############################################################
############################################################
##############   1: Main Regression  #######################
############################################################
############################################################

regression_output_n <- function(data, naics, n) {

  #run Industry_n_dig to make data
  #clean industry names, store names
  #run regressions 1_9, store ugly table
  #make nice table, store


  ##################run Industry_n_dig to make data ##################

  #generate N digit industry names
  temp_data <- industry_n_dig(data, naics, n) #nolint

  ################# Store industry names ##############################

  #get number of observations for each industry
  ind_count_temp <- temp_data %>% count(industry) #nolint

  #only industries with >1 obs will produce an estimate
  industries_temp <- ind_count_temp[ind_count_temp$n > 1, ]
  #number of industries (last column is N/A so -1)
  numberofcategories_temp <- length(industries_temp$industry) - 1


  ################# run regressions 1_9, store ugly table  #################

  ## Model 1 - no industry, no intercept
  model_1 <- feols(
    Adr_MC ~ 0 + MU_1,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 2 - no industry,  intercept
  model_2 <- feols(
    Adr_MC ~ MU_1,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 3 - industry, no intercept
  model_3 <- feols(
    Adr_MC ~ 0 + i(industry, MU_1) - MU_1,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 4 - industry, industry intercepts
  model_4 <- feols(
    Adr_MC ~ i(industry, MU_1) - MU_1 | industry,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 5 - industry, industry intercept, time trend (not interacted)
  model_5 <- feols(
    Adr_MC ~ 0 + i(industry, MU_1) + time - MU_1 | industry,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 6 - industry, industry intercept, time trend ( interacted)
  model_6 <- feols(
    Adr_MC ~ i(industry, MU_1) + MU_1 * time + time - MU_1 | industry,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 7 - industry, industry intercept, time trend, time quad
  #(neither interacted)
  model_7 <- feols(
    Adr_MC ~ i(industry, MU_1) + time + time2 - MU_1 | industry,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 8 - industry, industry intercept, time trend, time quad
  #(both interacted)
  model_8 <- feols(
    Adr_MC ~ i(industry, MU_1) + MU_1 * time + MU_1 * time2
    + time + time2 - MU_1 | industry,
    cluster = "GVKEY",
    data = temp_data
  )

  ## Model 9 - industry, industry intercept, time dummy (+ interacted)
  model_9 <- feols(
    Adr_MC ~ i(industry, MU_1) + i(fyear, MU_1, ref = 2022) - MU_1
    | industry + fyear,
    cluster = "GVKEY",
    data = temp_data
  )

  #grab all models
  notused <- list(model_5, model_6, model_7) #nolint
  models_temp <- list(model_1, model_2, model_3, model_4, model_8, model_9)

  #create ugly table and save
  ugly_table <- modelsummary(models_temp, stars = TRUE)

  ################# make nice table, store  #################


  #grab industry coefficient names from table 9
  #grab industry coefficient names- remove 'industry' prefix
  indnames_temp <- gsub(
    "industry::", "", names(model_9$coefficients[1:numberofcategories_temp])
  )
  #shorten longest name
  indnames_temp <- gsub(
    "and Waste Management and Remediation", "", indnames_temp
  )
  #rephrase mu for table
  indnames_table <- gsub(
    ":MU_1", " × (MU-1)", indnames_temp
  )
  #remove mu for names (use later)
  indnames_temp <- gsub(
    ":MU_1", "", indnames_temp
  )


  #coefficient labels
  labels_temp <- c("Intercept", "MU-1", indnames_table, "time",
                   "time × (MU-1)", "time²", "time² × (MU-1)")
  #defining names is how coef_maps identify which variable name its referring to
  names(labels_temp) <- c(
    names(model_2$coefficients[1:2]),                       #intercept and MU
    names(model_9$coefficients[1:length(indnames_temp)]), #industry #nolint
    names(model_8$coefficients[(length(model_8$coefficients) - 3)
                               :length(model_8$coefficients)])
    #time interacted
  )

  # define columns want to keep;
  #ie the first 2 (MU and intercept) + number of industries,
  #+ 4 (time, time^2 and interact)
  keepcol_temp <- 1:(length(indnames_temp) + 6)

  #pick goodness of fit measures
  #needs to be tribble
  gm_temp <- tribble(
    ~raw,        ~clean,      ~fmt,
    "nobs", "N", 0,
    "adj.r.squared", "Adj. R2", 5,
    "vcov.type", "Clustering", 0,
    "FE: industry", "Industry FEs", 0,
    "FE: fyear", "Year FEs", 0,
  )

  #add column to note the MU_1*time inclusion
  new_row_temp <- data.frame("Year × (MU-1) (2022 reference)",
                             "", "", "", "", "", "X")

  #make table
  table_temp <- modelsummary(models_temp, coef_omit = -keepcol_temp,
                             coef_map = labels_temp, gof_map = gm_temp,
                             add_rows = new_row_temp, fmt = 4, stars = TRUE)

  #make table (latex)
  table_temp_latex <- modelsummary(models_temp, coef_omit = -keepcol_temp,
                                   coef_map = labels_temp, gof_map = gm_temp,
                                   add_rows = new_row_temp,
                                   fmt = 4, stars = TRUE, output = "latex")

  ################# output  #################################

  #Nice table, ugly table, model 9 sector coefficients (well named),
  #model 9 year coefficients (well named), models, latex table,
  #obs per industry

  #grab year coefficents
  year_cos <- data.frame(model_9$coefficients[(numberofcategories_temp + 1):
                                                length(model_9$coefficients)])

  year_cos$year <- as.numeric(gsub("fyear::", "",
                                   gsub(":MU_1", "", rownames(year_cos))))

  rownames(year_cos) <- NULL
  names(year_cos) <- c("fit", "year")
  year_cos$se <-  summary(model_9, cluster = "GVKEY")$
    se[(numberofcategories_temp + 1):length(model_9$coefficients)]

  #grab sector coefficents
  sector_cos <- data.frame(model_9$coefficients[1:numberofcategories_temp])
  sector_cos$indust <- indnames_temp
  rownames(sector_cos) <- NULL
  names(sector_cos) <- c("fit", "industry")

  sector_cos$se <-
    summary(model_9, cluster = "GVKEY")$se[1:numberofcategories_temp]

  #make into list
  output <- list(
                 table_temp, ugly_table, sector_cos, year_cos,
                 models_temp, table_temp_latex, ind_count_temp,
                 Main_model = model_9)
  #print list so that it appears as output of function
  output

}

############################################################
############################################################
##############   2: by sector time trend Regression  #######
############################################################
############################################################

############################################################
##############   2.a: by sector time trend Regression  #####
############################################################

#rolling window, set window to 1 for no rolling
#also outputs corrections
#coefficents saved as results$coefs

rolling_window <- function(data, naics, d, n) {

  temp <- industry_n_dig(data, naics, d) #nolint

  # Get the range of years
  two_d_data <- temp %>%
    mutate(fyear = as.numeric(fyear)) %>% #nolint
    filter(!is.na(fyear))

  years <- range(two_d_data$fyear)

  # Create a sequence of n-year windows
  all_years <- seq(from = years[1], to = years[2])

  # Initialize an empty lists to store the results
  results <- list()
  corrections <- list()
  coefs <- list()

  # Iterate over each window
  for (current_year in all_years) {
    # Subset the data to the 7-year window
    data_window <- two_d_data %>%
      filter(fyear >= current_year - (n - 1) / 2, #nolint
       fyear <= current_year + (n - 1) / 2) #nolint

    # Run the model by sector
    model_pooled <- feols(
      Adr_MC ~ i(industry, MU_1) - MU_1 | industry,
      data = data_window
    )

    # Process the coefficients
    model_pooled_cos <- data.frame(model_pooled$coefficients)
    model_pooled_cos$industry <-
      gsub(":MU_1", "", gsub("industry::", "", rownames(model_pooled_cos)))
    rownames(model_pooled_cos) <- NULL
    model_pooled_cos$se <- model_pooled$se
    names(model_pooled_cos) <- c("fit", "industry", "se")


    # Process the intercepts
    model_pooled_ints <- data.frame(fixef(model_pooled)$industry)
    model_pooled_ints$industry <- rownames(model_pooled_ints)
    rownames(model_pooled_ints) <- NULL
    names(model_pooled_ints) <- c("intercept", "industry")

    # Merge and compute correction
    model_pooled_ints <- merge(model_pooled_cos, model_pooled_ints)
    model_pooled_c <- model_pooled_ints %>%
      mutate(correction = 1 - intercept / fit) #nolint

    # Merge with original data and compute MU_C
    hold <- merge(data_window, model_pooled_c, by = "industry")
    hold <- hold %>%
      mutate(MU_C = MU_1 / correction) #nolint

    # Add the start year to the results
    hold$start_year <- current_year #nolint

    # Append the results to the list
    corrections[[length(corrections) + 1]] <- hold

    # run full sample regression and save results
    model_full <- feols(
      Adr_MC ~ MU_1,
      data = data_window
    )
    full_sampr <- data.frame(
      fit = model_full$coefficients[2],
      industry = "Full Sample",
      se = model_full$se[2],
      intercept = model_full$coefficients[1]
    )
    row.names(full_sampr) <- NULL

    #add full sample results to model ints, add year
    temp_coefs <- rbind(model_pooled_ints, full_sampr)
    temp_coefs$year <- current_year

    #add the results to its list
    coefs[[length(coefs) + 1]] <- temp_coefs

  }

  # Combine the corrections and coefs into a single data frames
  corrections <- bind_rows(corrections)
  coefs <- bind_rows(coefs)

  #add to results and print
  results$corrections <- corrections
  results$coefs <- coefs
  results
}

############################################################
########  2.b: regression of coefficents over time  ########
############################################################

coef_regression <- function(coefs, data, naics, n) {

  #run Industry_n_dig to make full sample data
  temp_data <- industry_n_dig(data, naics, n) # nolint
  ################# grab names ##################
  sectors <- unique(subset(coefs,industry!="Full Sample")$industry) # nolint

  ################# table  #################
  #fill in full sample row

  #coef regression
  coef_model <- feols(log(fit) ~ as.numeric(year), # nolint
                       data = subset(coefs, industry == "Full Sample")) # nolint
  #adv regression
  adv_model <- feols(log(Adr) ~ fyear, # nolint
                       data = temp_data) # nolint
  #MU regression
  MU_model <- feols(log(MU) ~ fyear, # nolint
                       data = temp_data) # nolint


  #fill in point estimates in row 1 (default to not)
  table <- data.frame(
    "All",
    coef_model$coefficients[2],
    adv_model$coefficients[2],
    MU_model$coefficients[2],
    summary(coef_model)$se[2],
    summary(adv_model)$se[2],
    summary(MU_model)$se[2]
  )

  names(table) <- c(
                    "Sample", "Exad_trend", "adv_trend", "MU_trend",
                    "Exad_se", "adv_se", "MU_se")

  ################# loop over sectors  #################

  for (i in sectors) { # nolint

    #coef regression
    coef_model = feols(log(fit) ~ log(year-1960), # nolint
                        data = subset(coefs, industry == i)) # nolint
    #adv regression
    adv_model <- feols(log(Adr) ~ log(fyear-1960), # nolint
                        data = subset(temp_data,industry == i)) # nolint
    #MU regression
    MU_model <- feols(log(MU_1+1) ~ log(fyear-1960), # nolint
                        data = subset(temp_data,industry == i)) # nolint

    #fill in point estimates in row 1 (default to not)
    temp_table <- data.frame(
      i,
      coef_model$coefficients[2],
      adv_model$coefficients[2],
      MU_model$coefficients[2],
      summary(coef_model)$se[2],
      summary(adv_model)$se[2],
      summary(MU_model)$se[2]
    )
    names(temp_table) <- c(
                     "Sample", "Exad_trend", "adv_trend", "MU_trend",
                     "Exad_se", "adv_se", "MU_se")


    table <- rbind(table, temp_table)
  }

  rownames(table) <- NULL
  print(table)
}


############################################################
############## 2.c: Ses on intercepts  #######################
############################################################

get_intercepts_and_errors <- function(data, naics, n) {

  data <- industry_n_dig(data, naics, n) #nolint

  # Convert 'industry' and 'fyear' to factor variables
  data$industry <- as.factor(data$industry)
  data$fyear <- as.factor(data$fyear)

  # Run the regression
  model <- lm(Adr_MC ~ industry * MU_1 + fyear * MU_1 - MU_1, data = data)

  # Compute the clustered standard errors
  clustered_se <- sqrt(diag(vcovCL(model, cluster = ~industry, type = "HC1")))

  # Get the coefficients
  coefficients <- coef(model)

  # Save the intercepts and their clustered standard errors for 'industry'
  industry_intercepts <- coefficients[grep("industry", names(coefficients))]
  l <- length(industry_intercepts) / 2 # number of industries are in first half
  industry_intercepts <- industry_intercepts[1:l]
  industry_clustered_se <- clustered_se[grep("industry", names(clustered_se))]
  industry_clustered_se <- industry_clustered_se[1:l]

  # Save the intercepts and their clustered standard errors for 'fyear'
  fyear_intercepts <- coefficients[grep("fyear", names(coefficients))]
  l <- length(fyear_intercepts) / 2 # number of industries are in first half
  fyear_intercepts <- fyear_intercepts[1:l]
  fyear_clustered_se <- clustered_se[grep("fyear", names(clustered_se))]
  fyear_clustered_se <- fyear_clustered_se[1:l]

  # Return a list containing the intercepts and their clustered standard errors
  return(
    list(
      industry = list(
        intercepts = industry_intercepts,
        std_errors = industry_clustered_se
      ),
      fyear = list(
        intercepts = fyear_intercepts,
        std_errors = fyear_clustered_se
      )
    )
  )
}

############################################################
#### 2.d: reverse regression of coefficents over time  #####
############################################################

#reverse the estimation equation to get:
# MU = adv*(1/E_xad) + \theta

sector_time_reverse <- function(data, naics, n) {

  ################## run Industry_n_dig to make data ##################

  #generate N digit industry names
  temp_data <- industry_n_dig(data, naics, n) #nolint


  ################# full sample  #################
  model_temp <- feols(
    MU ~ i(fyear, Adr_MC) | fyear,
    cluster = "GVKEY",
    data = temp_data #nolint
  )
  #extract coefficients and ses
  ceos_temp <- data.frame(gsub(":Adr_MC", "",
                               gsub("fyear::", "",
                                    names(model_temp$coefficients))))
  #create holder for intercepts
  intercept_temp <- ceos_temp
  #input data
  ceos_temp$efficency <- model_temp$coefficients
  ceos_temp$se <- summary(model_temp, cluster = "GVKEY")$se
  ceos_temp$industry <- "Full Sample"
  intercept_temp$intercept <- fixef(model_temp)$fyear
  #clean names
  names(ceos_temp) <- c("year", "fit", "se", "industry")
  names(intercept_temp) <- c("year", "intercept")
  #merge to output
  output <- merge(ceos_temp, intercept_temp, by = "year")

  ################# store industry names,##############################
  #get number of observations for each industry
  ind_count_temp <- temp_data %>% count(industry) #nolint
  #only industries with >1 obs will produce an estimate
  industries_temp <- ind_count_temp[ind_count_temp$n > 2, ]

  ################# loop over sectors  #################
  for (i in industries_temp$industry[!is.na(industries_temp$industry)]){
    #allow to continue if error
    tryCatch({
      #estimate model within sector
      model_temp <- feols(
        MU ~ i(fyear, Adr_MC) | fyear,
        cluster = "GVKEY",
        data = subset(temp_data, industry == i) #nolint
      )

      #extract coefficients and ses
      ceos_temp <- data.frame(gsub(":Adr_MC", "",
                                   gsub("fyear::", "",
                                         names(model_temp$coefficients))))
      #input data
      ceos_temp$efficency <- model_temp$coefficients
      ceos_temp$se <- summary(model_temp, cluster = "GVKEY")$se
      ceos_temp$industry <- i
      #create holder for intercepts
      intercept_temp <- data.frame(gsub(":Adr_MC", "",
                                        gsub("fyear::", "",
                                             names(fixef(model_temp)$fyear))))
      intercept_temp$intercept <- fixef(model_temp)$fyear
      #clean names
      names(ceos_temp) <- c("year", "fit", "se", "industry")
      names(intercept_temp) <- c("year", "intercept")
      #merge to output temp
      output_temp <- merge(ceos_temp, intercept_temp, by = "year")
      #combine output
      output <- rbind(output, output_temp)
    }, error = function(e) {
      # What to do when an error occurs: just print the error message
      print(paste("Error:", e$message))
    })
  }
  #some cleaning
  output$year <- as.numeric(output$year)
  output$industry <- str_wrap(output$industry, width = 20) # nolint
  output
}