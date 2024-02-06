require(plm)
require(gmm)
require(dplyr)
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

  # list of variables for polynominal
  vars <- c("c", "k", "industry_share", "industry_share_3", "industry_share_4")

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

############################################################
############################################################
#################   2: Second Stage    #####################
############################################################
############################################################

# Define the moment conditions for the GMM estimation
dwl_momment <- function(theta, panel) {
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
  result <- optim(theta_init, dwl_momment,
                  method = "Nelder-Mead", panel = panel)

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