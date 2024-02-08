require(plm)
require(gmm)
require(dplyr)

############################################################
############################################################
##################   0: useful    #####################
############################################################
############################################################

#generate polynominals, defining seperate saves a few lines later

#generates polys that still need to be merged back
poly_gen <- function(panel, var_names){

  # Add the polynomial variables to a matrix
  matrix_vars <- data.matrix(panel[var_names])

  # Generate 3rd degree polynomial including interactions
  poly_vars <- poly(matrix_vars, degree = 3, raw = TRUE)

  poly_vars

}


############################################################
############################################################
##################   1: First Stage    #####################
############################################################
############################################################

#adapted from:
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5GH8XO #nolint

first_stage_nomshare <- function(panel) {

  #loggd vars
  panel <- panel %>% #nolint
    mutate(c = log(cogs), k = log(ppegt)) #nolint

  # list of variables for polynominal
  vars <- c("c", "k")

  # Add the polynomial variables to a matrix
  matrix_vars <- data.matrix(panel[vars])

  # Generate 3rd degree polynomial including interactions
  poly_vars <- poly(matrix_vars, degree = 3, raw = TRUE)

  #merge back
  poly_var_names <- colnames(poly_vars)
  panel <- cbind(panel, poly_vars)

  # estimate nonlinear regression
  model <- lm(log(sale) ~ ., data = panel[, c("sale", poly_var_names)])

  # Get the residuals and predicted
  residuals_df <- data.frame(err = residuals(model))
  predicted_df <- data.frame(phi = fitted.values(model))

  # Add the residuals back to the original panel data
  panel$err <- residuals_df$err
  panel$phi <- predicted_df$phi

  # Create the lagged variables
  panel <- panel %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      phi_l = lag(phi),  #nolint
      c_l = lag(c),
      k_l = lag(k),  #nolint
      err_l = lag(err)) %>% #nolint
    ungroup()

  #log corrected sales
  panel <- panel %>% #nolint
      mutate(sale_l = log(lag(sale)) -err) #nolint
  panel$sale_c <- exp(panel$sale_l) #nolint

  panel

}


#adds market share in first stage as DEU 2020
first_stage <- function(panel) {

  #loggd vars
  panel <- panel %>% #nolint
    mutate(c = log(cogs), k = log(ppegt)) #nolint

#give shorter names to market shares
  panel <- panel %>% #nolint
    mutate(m = industry_share, #nolint
           m3 = industry_share_3, #nolint
           m4 = industry_share_4) #nolint

  # list of variables for polynominal
  vars <- c("c", "k", "m", "m3", "m4")

  # Add the polynomial variables to a matrix
  matrix_vars <- data.matrix(panel[vars])

  # Generate 3rd degree polynomial including interactions
  poly_vars <- poly(matrix_vars, degree = 3, raw = TRUE)

  #merge back
  poly_var_names <- colnames(poly_vars)
  panel <- cbind(panel, poly_vars)

  # estimate nonlinear regression
  model <- lm(log(sale) ~ ., data = panel[, c("sale", poly_var_names)])

  # Get the residuals and predicted
  residuals_df <- data.frame(err = residuals(model))
  predicted_df <- data.frame(phi = fitted.values(model))

  # Add the residuals back to the original panel data
  panel$err <- residuals_df$err
  panel$phi <- predicted_df$phi

  # Create the lagged variables
  panel <- panel %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      phi_l = lag(phi),  #nolint
      c_l = lag(c),
      k_l = lag(k),  #nolint
      m_l = lag(m), #nolint
      m3_l = lag(m3), #nolint
      m4_l = lag(m4), #nolint
      err_l = lag(err)) %>% #nolint
    ungroup()

  #log corrected sales
  panel <- panel %>% #nolint
      mutate(sale_l = log(lag(sale)) -err) #nolint
  panel$sale_c <- exp(panel$sale_l) #nolint

  panel

}

first_stage_deu <- function(panel) {

  #loggd vars
  panel <- panel %>% #nolint
    mutate(c = log(cogs), k = log(ppegt)) #nolint

  #give shorter names to market shares
  panel <- panel %>% #nolint
    mutate(m = industry_share, #nolint
           m3 = industry_share_3, #nolint
           m4 = industry_share_4) #nolint

  # list of variables for polynominal
  vars <- c("c", "k", "m", "m3", "m4")

  # Add the polynomial variables to a matrix
  matrix_vars <- data.matrix(panel[vars])

  # Generate 3rd degree polynomial including interactions
  poly_vars <- poly(matrix_vars, degree = 3, raw = TRUE)

  #merge back
  poly_var_names <- colnames(poly_vars)
  panel <- cbind(panel, poly_vars)

  # estimate nonlinear regression
  model <- lm(log(sale) ~ ., data = panel[, c("sale", poly_var_names)])

  # Get the residuals and predicted
  residuals_df <- data.frame(err = residuals(model))
  predicted_df <- data.frame(phi = fitted.values(model))

  # Add the residuals back to the original panel data
  panel$err <- residuals_df$err
  panel$phi <- predicted_df$phi

  # Create the lagged variables
  panel <- panel %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      phi_l = lag(phi),  #nolint
      c_l = lag(c),
      k_l = lag(k),  #nolint
      m_l = lag(m), #nolint
      m3_l = lag(m3), #nolint
      m4_l = lag(m4), #nolint
      err_l = lag(err)) %>% #nolint
    ungroup()

  #generate market shares interacted with inputs
  #following Prices, Markups and Trade Reform (2015)
  panel <- panel %>%
  group_by(GVKEY) %>% #nolint
  mutate(
    mc = m * c,  #nolint
    mk = m * k, #nolint
    mc3 = m3 * c, #nolint
    mk3 = m3 * k,
    mc4 = m4 * c, #nolint
    mk4 = m4 * k) %>% #nolint
    ungroup()

  #lag those as well
  panel <- panel %>%
  group_by(GVKEY) %>% #nolint
  mutate(
    mc_l = lag(mc),  #nolint
    mk_l = lag(mk), #nolint
    mc3_l = lag(mc3), #nolint
    mk3_l = lag(mk3), #nolint
    mc4_l = lag(mc4), #nolint
    mk4_l = lag(mk4)) %>% #nolint
    ungroup()

  #log corrected sales
  panel <- panel %>% #nolint
      mutate(sale_l = log(lag(sale)) -err) #nolint
  panel$sale_c <- exp(panel$sale_l) #nolint

  panel

}

############################################################
############################################################
#################   2: Second Stage    #####################
############################################################
############################################################

# Define the moment conditions for the GMM estimation
dwl_momment <- function(theta, panel) {
  #grab the vars we need
  phi <- panel$phi
  phi_l <- panel$phi_l
  c <- panel$c
  k <- panel$k
  c_l <- panel$c_l
  k_l <- panel$k_l
  sale <- panel$sale #nolint

  X <- cbind(1, c, k) #nolint
  X_l <- cbind(1, c_l, k_l) #nolint

  omega <- phi - X %*% theta
  omega_l <- phi_l - X_l %*% theta
  omega_l_pol <- cbind(1, omega_l)

  g_b <- solve(t(omega_l_pol) %*% omega_l_pol) %*% t(omega_l_pol) %*% omega
  xi <- omega - omega_l_pol %*% g_b

  Z <- cbind(1, c_l, k) #nolint

  m <- t(Z) %*% xi #nolint

  t(m) %*% m
}

# Define the moment conditions for the GMM estimation
deu_momment <- function(theta, input) {
  #grab panel
  panel <- input$panel

  #grab the vars we need
  phi <- panel$phi
  phi_l <- panel$phi_l
  c <- panel$c
  k <- panel$k
  c_l <- panel$c_l
  k_l <- panel$k_l
  sale <- panel$sale #nolint

  ms <- input$ms
  ms_l <- input$ms_l

  X <- cbind(1, c, k, ms) #nolint
  X_l <- cbind(1, c_l, k_l, ms_l) #nolint

  omega <- phi - X %*% theta
  omega_l <- phi_l - X_l %*% theta
  omega_l_pol <- cbind(1, omega_l)

  g_b <- solve(t(omega_l_pol) %*% omega_l_pol) %*% t(omega_l_pol) %*% omega
  xi <- omega - omega_l_pol %*% g_b

  Z <- cbind(1, c_l, k, ms, ms_l) #nolint

  m <- t(Z) %*% xi #nolint

  t(m) %*% m
}

#throw up big number if issues arise
deu_momment_checked <- function(params, ...) {
  # Try to compute the objective function
  tryCatch({
    value <- deu_momment(params, ...)
    if (is.finite(value)) {
      return(value)
    } else {
      return(1e40)  # Return a large value for non-finite values
    }
  },
  error = function(e) {
    # If an error occurs (e.g., non-invertible matrix), return a large value
    return(1e40)
  })
}


#second stage estimation
second_stage <- function(panel) {

  #drop nas
  panel <- panel[complete.cases(panel[, c("phi",
                                          "phi_l",
                                          "c",
                                          "k",
                                          "c_l",
                                          "k_l",
                                          "sale")]), ]
  #ols for initial values
  ols <- lm(log(sale) ~ c + k, data = panel)
  theta_init <-   ols$coefficients
  #make sure not crazy values for start
  theta_init[2] <- max(.8, theta_init[2])
  theta_init[3] <- max(.1, theta_init[3])
  theta_init[2] <- min(1, theta_init[2])
  theta_init[3] <- min(1, theta_init[3])

  # Solve the GMM problem using moment condition defined above
  #step size of .1 as in DWL replication docs
  result <- optim(theta_init, dwl_momment, #nolint
                  method = "Nelder-Mead", panel = panel)

  result

}

#second stage estimation (just 2 digit market shares)
second_stage_deu <- function(panel) {

  need <-
    c("phi", "phi_l", "c", "k", "c_l", "k_l", "sale",
    "m", "mc", "mk", #nolint
    "m_l", "mc_l",
    "mk_l")

  #drop nas
  panel <-
    panel[complete.cases(panel[, need]), ]

  #generate polynominal of market shares
  ms_vars <- c("m", "mc", "mk")
  ms_l_vars <-
    c("m_l", "mc_l", "mk_l")

  #create list with panel and market share polys
  input <- list()
  input$panel <- panel
  input$ms_l <- poly_gen(panel, ms_l_vars)
  input$ms <- poly_gen(panel, ms_vars)

  main <- c("sale", "c", "k")
  hold <- cbind(panel[, main], input$ms)

  #ols for initial values
  main <- c("sale", "c", "k")
  ols <- lm(log(sale) ~ ., data = hold)
  theta_init <-   ols$coefficients
  # Replace NA values with 0
  theta_init[is.na(theta_init)] <- 0

  # Solve the GMM problem using moment condition defined above
  #step size of .1 as in DWL replication docs
  result <- optim(theta_init, deu_momment_checked,
                  method = "CG", input = input,
                  control = list(trace = FALSE, maxit = 5000))

  result

}

#second stage estimation (uses market shares at 3 levels)
second_stage_deu_all <- function(panel) {

  need <-
    c("phi", "phi_l", "c", "k", "c_l", "k_l", "sale",
    "m", "m3", "m4", "mc", "mk", "mc3", "mk3", #nolint
    "mc4", "mk4",
    "m_l", "m3_l", "m4_l", "mc_l",
    "mk_l", "mc3_l", "mk3_l", "mc4_l", "mk4_l")

  #drop nas
  panel <-
    panel[complete.cases(panel[, need]), ]

  #generate polynominal of market shares
  ms_vars <- c("m", "m3", "m4", "mc", "mk", "mc3", "mk3", "mc4", "mk4")
  ms_l_vars <-
    c("m_l", "m3_l", "m4_l", "mc_l", "mk_l", "mc3_l", "mk3_l", "mc4_l", "mk4_l")

  #create list with panel and market share polys
  input <- list()
  input$panel <- panel
  input$ms_l <- poly_gen(panel, ms_l_vars)
  input$ms <- poly_gen(panel, ms_vars)

  main <- c("sale", "c", "k")
  hold <- cbind(panel[, main], input$ms)

  #ols for initial values
  main <- c("sale", "c", "k")
  ols <- lm(log(sale) ~ ., data = hold)
  theta_init <-   ols$coefficients
  #make sure elasticities not crazy values for start
  theta_init[2] <- max(.8, theta_init[2])
  theta_init[3] <- max(.1, theta_init[3])
  theta_init[2] <- min(1, theta_init[2])
  theta_init[3] <- min(1, theta_init[3])
  # Replace NA values with 0
  theta_init[is.na(theta_init)] <- 0

  # Solve the GMM problem using moment condition defined above
  #step size of .1 as in DWL replication docs
  result <- optim(theta_init, deu_momment_checked,
                  method = "Nelder-Mead", input = input,
                  control = list(trace = FALSE, maxit = 5000,
                                 reltol = .00001))

  result

}

############################################################
############################################################
#################   3: Estimation    #####################
############################################################
############################################################

acf_rolling_window <- function(tempdata, r) {

  #sort so it prints nice
  tempdata <- tempdata %>% arrange(year, industry) #nolint

  #initiate empty theta
  thetas <- data.frame()

  #get sector names and years
  sectors <- unique(tempdata$industry)
  # Get the range of years
  years <- range(tempdata$year)
  all_years <- seq(from = years[1], to = years[2])

  #loop over sectors
  for (i in 1:length(sectors)) { #nolint

    # Subset the data to the rolling windows
    pdata_sector <- tempdata %>% filter(industry == sectors[i]) #nolint

    #loop over years
    for (current_year in  all_years) {

      # Initialize a flag for errors
      error_flag <- FALSE

      # Subset the data to the rolling windows
      pdata_window <- pdata_sector %>% filter(year >= current_year - (r - 1) / 2, #nolint
                                              year <= current_year + (r - 1) / 2) #nolint

      # Run the first stage and handle errors
      panelf <- tryCatch({
        first_stage(pdata_window)
      }, error = function(e) {
        error_flag <<- TRUE
        NA
      })

      # Run the second stage and handle errors
      result <- tryCatch({
        result <- second_stage(panelf)
        result
      }, error = function(e) {
        error_flag <- TRUE #nolint
        result$par <- c(NA, NA, NA)
        result$convergence <<- NA
        result
      })

      #temp save the things we want
      theta_est <- result$par[2]
      theta_kest <- result$par[3]
      convergence <- result$convergence

      #to be added to output of loop
      theta_temp <- c(
        sector = sectors[i],
        year = current_year,
        theta = theta_est,
        convergence = convergence,
        n = nrow(pdata_window),
        theta_k = theta_kest
      )

      thetas <- rbind(thetas, theta_temp)

    }
    if (error_flag) {
      print(paste("!!Sector", sectors[i],
                  "estimation complete WITH ERRORS!!"))
    } else {
      print(paste("Sector", sectors[i], "estimation complete without errors."))
    }
  }
  #clean output and names
  thetas <- data.frame(thetas)
  rownames(thetas) <- NULL
  names(thetas) <-
    c("industry", "fyear", "theta", "convergence", "n.obs", "theta_k")

  thetas$theta <- as.numeric(as.character(thetas$theta))
  thetas$fyear <- as.numeric(as.character(thetas$fyear))
  thetas$convergence <- as.numeric(as.character(thetas$convergence))
  thetas$n.obs <- as.numeric(as.character(thetas$n.obs))
  thetas$theta_k <- as.numeric(as.character(thetas$theta_k))
  thetas
}


acf_bysector <- function(tempdata) {

  #sort so it prints nice
  tempdata <- tempdata %>% arrange(industry) #nolint

  #initiate empty theta
  thetas <- NULL

  #get sector names
  sectors <- unique(tempdata$industry)

  #loop over sectors
  thetas <- data.frame()
  for (i in 1:length(sectors)) { #nolint

    # Initialize a flag for errors
    error_flag <- FALSE

    # Subset the data to the rolling windows
    pdata_sector <- tempdata %>% filter(industry == sectors[i]) #nolint

    # Run the first stage and handle errors
    panelf <- tryCatch({
      first_stage(pdata_sector) #nolint
    }, error = function(e) {
      error_flag <<- TRUE
      NA
    })

    # Run the second stage and handle errors
    result <- tryCatch({
      result <- second_stage(panelf)
      result
    }, error = function(e) {
      error_flag <- TRUE #nolint
      result$par <- c(NA, NA, NA)
      result$convergence <<- NA
      result
    })

    #temp save the things we want
    theta_est <- result$par[2]
    theta_kest <- result$par[3]
    convergence <- result$convergence

    #to be added to output of loop
    theta_temp <- c(
      sector = sectors[i],
      theta = theta_est,
      convergence = convergence,
      n = nrow(pdata_sector),
      theta_k = theta_kest
    )
    thetas <- rbind(thetas, theta_temp)

    if (error_flag) {
      print(paste("!!Sector", sectors[i], "estimation complete WITH ERRORS!!"))
    } else {
      print(paste("Sector", sectors[i], "estimation complete without errors."))
    }
  }
  #clean output and names
  thetas <- data.frame(thetas)
  rownames(thetas) <- NULL
  names(thetas) <-
    c("industry", "theta", "convergence", "n.obs", "theta_k")

  thetas$theta <- as.numeric(as.character(thetas$theta))
  thetas$convergence <- as.numeric(as.character(thetas$convergence))
  thetas$n.obs <- as.numeric(as.character(thetas$n.obs))
  thetas$theta_k <- as.numeric(as.character(thetas$theta_k))
  thetas
}


acf_rolling_window_deu <- function(tempdata, r) {

  #sort so it prints nice
  tempdata <- tempdata %>% arrange(year, industry) #nolint

  #initiate empty theta
  thetas <- data.frame()

  #get sector names and years
  sectors <- unique(tempdata$industry)
  # Get the range of years
  years <- range(tempdata$year)
  all_years <- seq(from = years[1], to = years[2])

  # Initialize a flag for errors
  error_flag <- FALSE

  #loop over sectors
  for (i in 1:length(sectors)) { #nolint

    # Subset the data to the rolling windows
    pdata_sector <- tempdata %>% filter(industry == sectors[i]) #nolint

    #loop over years
    for (current_year in  all_years) {

      # Initialize a flag for errors
      error_flag <- FALSE

      # Subset the data to the rolling windows
      pdata_window <- pdata_sector %>% filter(year >= current_year - (r - 1) / 2, #nolint
                                              year <= current_year + (r - 1) / 2) #nolint

      # Run the first stage and handle errors
      panelf <- tryCatch({
        first_stage_deu(pdata_window)
      }, error = function(e) {
        error_flag <<- TRUE
        NA
      })

      # Run the second stage and handle errors
      result <- tryCatch({
        result <- second_stage_deu(panelf)
        result
      }, error = function(e) {
        error_flag <- TRUE #nolint
        result$par <- c(NA, NA, NA)
        result$convergence <<- NA
        result
      })

      #temp save the things we want
      theta_est <- result$par[2]
      theta_kest <- result$par[3]
      convergence <- result$convergence

      #to be added to output of loop
      theta_temp <- c(
        sector = sectors[i],
        year = current_year,
        theta = theta_est,
        convergence = convergence,
        n = nrow(pdata_window),
        theta_k = theta_kest
      )

      thetas <- rbind(thetas, theta_temp)

      print(paste ("I:", sectors[i], "Y:", current_year, "cc:", convergence))
    }
    if (error_flag) {
      print(paste("\n \n !!Sector", sectors[i],
                  "estimation complete WITH ERRORS!! \n \n"))
    } else {
      print(paste("\n \n Sector", sectors[i], "estimation complete without errors. \n \n"))
    }
  }
  #clean output and names
  thetas <- data.frame(thetas)
  rownames(thetas) <- NULL
  names(thetas) <-
    c("industry", "fyear", "theta", "convergence", "n.obs", "theta_k")

  thetas$theta <- as.numeric(as.character(thetas$theta))
  thetas$fyear <- as.numeric(as.character(thetas$fyear))
  thetas$convergence <- as.numeric(as.character(thetas$convergence))
  thetas$n.obs <- as.numeric(as.character(thetas$n.obs))
  thetas$theta_k <- as.numeric(as.character(thetas$theta_k))
  thetas
}



acf_bysector_deu <- function(tempdata) {

  #sort so it prints nice
  tempdata <- tempdata %>% arrange(industry) #nolint

  #initiate empty theta
  thetas <- NULL

  #get sector names
  sectors <- unique(tempdata$industry)

  #loop over sectors
  thetas <- data.frame()
  for (i in 1:length(sectors)) { #nolint

    # Initialize a flag for errors
    error_flag <- FALSE

    # Subset the data to the rolling windows
    pdata_sector <- tempdata %>% filter(industry == sectors[i]) #nolint

    # Run the first stage and handle errors
    panelf <- tryCatch({
      first_stage_deu(pdata_sector) #nolint
    }, error = function(e) {
      error_flag <<- TRUE
      NA
    })

    # Run the second stage and handle errors
    result <- tryCatch({
      result <- second_stage_deu(panelf)
      result
    }, error = function(e) {
      error_flag <- TRUE #nolint
      result$par <- c(NA, NA, NA)
      result$convergence <<- NA
      result
    })

    #temp save the things we want
    theta_est <- result$par[2]
    theta_kest <- result$par[3]
    convergence <- result$convergence

    if (is.na(convergence)) {
      convergence <- 99
    }
    if (convergence != 0) {
      error_flag <- TRUE
    }

    #to be added to output of loop
    theta_temp <- c(
      sector = sectors[i],
      theta = theta_est,
      convergence = convergence,
      n = nrow(pdata_sector),
      theta_k = theta_kest
    )
    thetas <- rbind(thetas, theta_temp)

    if (error_flag) {
      print(paste("!!Sector", sectors[i], "estimation complete WITH ERRORS!!"))
    } else {
      print(paste("Sector", sectors[i], "estimation complete without errors."))
    }
    print(paste("Convergence Code:", convergence))
  }
  #clean output and names
  thetas <- data.frame(thetas)
  rownames(thetas) <- NULL
  names(thetas) <-
    c("industry", "theta", "convergence", "n.obs", "theta_k")

  thetas$theta <- as.numeric(as.character(thetas$theta))
  thetas$convergence <- as.numeric(as.character(thetas$convergence))
  thetas$n.obs <- as.numeric(as.character(thetas$n.obs))
  thetas$theta_k <- as.numeric(as.character(thetas$theta_k))
  thetas
}