require(plm)
require(gmm)
require(dplyr)
library(MASS)
############################################################
############################################################
###########   0: Set up Data for estimation    #############
############################################################
############################################################

#generates all necessary lags
# and gives names for objects used in first and second stage
#uses functions defined in useful

#takes inputs:
#yvar - output
# xvars - inputs
# fszvars - first stage controls
# zvars - controls in second stage (if included should be same as fszvars)
# orthogx - orthogonal momments of x data
# actual moment also combines poly of second stage

# panel - panel data

#returns:
#output$data - panel data with all lags and polys
#output$xvar - hands back input names
#output$y - hands back output name
#output$fs_rhs - first stage right hand side variables (includes polys)
# used in second stage
#output$xvar_l - names of lags of xvars
#output$ss_controls - names of controls in second stage
#output$ss_controls_l - names of lags of controls in second stage
#output$orthogs - names of orthogonal moments (adds in controls)
#     name any xvar lags as "name"_l, will be generated

set_up_o <- function(yvar, xvars,  fszvars, zvars, orthogx, panel) {

  temp <- panel

  ################# 1. gen all lags before polys (saves time) #####

  #generate xlags
  temp <- gen_lags(temp, xvars)
  xvar_l <- lag_names(xvars)

  #generate zlags and xz lags
  #first get xz
  temp <- gen_xz(xvars, zvars, temp)
  xz <- xz_names(xvars, zvars)
  #generate lags of xz and z
  temp <- gen_lags(temp, c(xz, zvars))

  xzz_l <- lag_names(c(xz, zvars))

  ################# 2. gen first stage rhs ##################

  #generate first stage polynominals
  temp <- poly_gen(temp, c(xvars, fszvars))
  fs_rhs <- poly_gen_names(c(xvars, fszvars))

  ################# 3. gen second stage controls ##################

  #check if zvars are given
  #default to NULL
  if (!is.null(zvars)) {
    #generate second stage control polynominals
    temp <- poly_gen(temp, c(xz, zvars))
    ss_controls <<- poly_gen_names(c(xz, zvars))
    #generate lags of second stage controls poly lags
    temp <- poly_gen(temp, xzz_l)
    ss_controls_l <<- poly_gen_names(xzz_l)
  } else {
    ss_controls <- NULL
    ss_controls_l <- NULL
  }

  ################# 3. intruments ##################
  orthogs <- c(orthogx, ss_controls_l)


  #return list
  return <- list()
  return$data <- temp
  return$y <- yvar
  return$xvars <- xvars
  return$y <- yvar
  return$xvar_l <- xvar_l
  return$fs_rhs <- fs_rhs
  return$ss_controls <- ss_controls
  return$ss_controls_l <- ss_controls_l
  return$orthogs <- orthogs
  #return
  return

}





set_up <- function(yvar, xvars,  fszvars, zvars, orthogx, panel) {

  temp <- panel

  ################# 1. gen all lags before polys (saves time) #####

  #generate xlags
  temp <- gen_lags(temp, xvars)
  xvar_l <- lag_names(xvars)

  #generate zlags and xz lags
  #first get xz
  temp <- gen_xz(xvars, zvars, temp)
  xz <- xz_names(xvars, zvars)
  #generate lags of xz and z
  temp <- gen_lags(temp, c(xz, zvars))

  z_l <- lag_names(c(zvars))

  ################# 2. gen first stage rhs ##################

  #generate first stage polynominals
  temp <- poly_gen(temp, c(xvars, fszvars))
  fs_rhs <- poly_gen_names(c(xvars, fszvars))

  ################# 3. gen second stage controls ##################

  #check if zvars are given
  #default to NULL
  if (!is.null(zvars)) {
    #generate second stage control polynominals
    temp <- poly_gen(temp, c(zvars))
    ss_controls <<- poly_gen_names(c(zvars))
    #generate lags of second stage controls poly lags
    temp <- poly_gen(temp, z_l)
    ss_controls_l <<- poly_gen_names(z_l)
  } else {
    ss_controls <- NULL
    ss_controls_l <- NULL
  }

  ################# 3. intruments ##################
  orthogs <- c(orthogx, ss_controls_l)


  #return list
  return <- list()
  return$data <- temp
  return$y <- yvar
  return$xvars <- xvars
  return$y <- yvar
  return$xvar_l <- xvar_l
  return$fs_rhs <- fs_rhs
  return$ss_controls <- ss_controls
  return$ss_controls_l <- ss_controls_l
  return$orthogs <- orthogs
  #return
  return

}


############################################################
############################################################
##################   1: First Stage    #####################
############################################################
############################################################

#adapted from:
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5GH8XO #nolint

#need to make more flexible, currently hard coding in time index and GVKEY

#runs regression using rhs (should be polys)
first_stage_exp <- function(yvar, rhs, panel) {

  # estimate nonparametric regression
  # Create the formula
  formula <- as.formula(paste(yvar, "~ ."))
  model <- lm(formula, data = panel[, c(yvar, rhs)])

  # Get the residuals and predicted
  residuals_df <- data.frame(err = residuals(model))
  predicted_df <- data.frame(phi = fitted.values(model))

  # Add the residuals and pred back to the original panel data
  panel$err <- residuals_df$err
  panel$phi <- predicted_df$phi

  # Create the lagged phi
  panel <- p_lag(panel, "year", "GVKEY", "phi", "_l")
  panel

}

############################################################
############################################################
#################   2: Second Stage    #####################
############################################################
############################################################

# Define the moment conditions for the GMM estimation
momment_exp <- function(theta, input) {

  #grab the vars we need
  phi <- input$phi
  phi_l <- input$phi_l
  controls <- data.matrix(input$controls)
  controls_l <- data.matrix(input$controls_l)
  x <- data.matrix(input$x)
  x_l <- data.matrix(input$xl)
  orthog <- data.matrix(input$orthog)

  X <- cbind(1, x, controls) #nolint
  X_l <- cbind(1, x_l, controls_l) #nolint

  omega <- phi - X %*% theta
  omega_l <- phi_l - X_l %*% theta
  omega_l_pol <- cbind(1, omega_l)

  #get rho of ar1 from projecting omega on lagged omega
  #using ginv from MASS to protect against non-invertible matrix
  g_b <- ginv(t(omega_l_pol) %*% omega_l_pol) %*%
    t(omega_l_pol) %*% omega

  xi <- omega - omega_l_pol %*% g_b

  Z <- cbind(1, orthog) #nolint

  m <- t(Z) %*% xi #nolint

  t(m) %*% m
}


#throw up big number if issues arise
momment_exp_checked <- function(params, ...) {
  # Try to compute the objective function
  tryCatch({
    value <- momment_exp(params, ...)
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

#second stage e}stimation
second_stage_exp <-
  function(panel, yvar, xvars, xvar_l,
           ss_controls, ss_controls_l, orthogs, theta_init) {

    #drop nas (this should be a sufficent check)
    panel <- panel[complete.cases(panel[, c("phi",
                                            "phi_l")]), ]

    #get data for gmm
    input <- list()
    input$phi <- panel$phi
    input$phi_l <- panel$phi_l
    input$x <- panel[, xvars]
    input$xl <- panel[, xvar_l]
    input$controls <- panel[, ss_controls]
    input$controls_l <- panel[, ss_controls_l]
    input$orthog <- panel[, orthogs]

    #constraints (only used if method set to L-BFGS-B)
    lower <- rep(-Inf, length(theta_init))
    upper <- rep(Inf, length(theta_init))
    lower[c(2)] <- .5
    upper[c(2)] <- 1
    lower[c(3)] <- 0
    upper[c(3)] <- .6


    # Solve the GMM problem using moment condition defined above
    #step size of .1 as in DWL replication docs
    result <- optim(theta_init, momment_exp, #nolint
                    method = "BFGS", #lower = lower, upper = upper,
                    input = input,
                    control = list(
                      trace = FALSE, maxit = 1e9, reltol = 1e-10
                                   #,factr = 1e8 #nolint
                                   )) #nolint

    result

  }


############################################################
############################################################
#################   3: Estimation    #####################
############################################################
############################################################

acf_bysector_exp <-
  function(temp, yvar, xvars, xvar_l, fs_rhs,
    ss_controls, ss_controls_l, orthogs) { #nolint

    ##################### 0 prep inputs for 2 stages ##########################
    #get length of xvars and ss_controls
    l <- length(xvars) + 1
    lt <- l + length(ss_controls)
    #start thetat at 0 (will update from ols in loop)
    theta_init <- rep(0, lt) #nolint

    #sort so it prints nice
    temp <- temp %>% arrange(industry) #nolint

    #initiate empty theta
    thetas <- NULL

    #get sector names
    sectors <- unique(temp$industry)

    #loop over sectors
    thetas <- data.frame()
    for (i in 1:length(sectors)) { #nolint

      # Initialize a flag for errors
      error_flag <- FALSE

      # Subset the data to the sector
      pdata_sector <- temp %>% filter(industry == sectors[i]) #nolint

      # Run the first stage and handle errors
      panelf <- tryCatch({
        first_stage_exp(yvar, fs_rhs, pdata_sector) #nolint
      }, error = function(e) {
        error_flag <<- TRUE
        NA
      })

      #get inital value for second stage from ols
      # Create the formula
      formula <- as.formula(paste(yvar, "~ ."))
      ols <- lm(formula, data = panelf[, c(yvar, xvars)])
      theta_init[1:l] <-   ols$coefficients

      # Run the second stage and handle errors
      result <- tryCatch({
        result <-
          second_stage_exp(
                           panelf, yvar, xvars, xvar_l,
                           ss_controls, ss_controls_l, orthogs,
                           theta_init)
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

      #check convergence
      if (is.na(convergence)) {
        convergence <- 99
      }
      if (convergence != 0) {
        error_flag <- TRUE
      }


      if (error_flag) {
        print(
              paste("!!Sector", sectors[i],
                    "estimation complete WITH ERRORS!!"))
      } else {
        print(
              paste("Sector", sectors[i],
                    "estimation complete without errors."))
      }
      print(paste("CC:", convergence, "theta_c:", theta_est))
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







acf_rolling_window <-
  function(temp, yvar, xvars, xvar_l, fs_rhs,
    ss_controls, ss_controls_l, orthogs, r, nmin) { #nolint


    ##################### 0 prep inputs for 2 stages ##########################
    #get length of xvars and ss_controls
    l <- length(xvars) + 1
    lt <- l + length(ss_controls)
    theta_init <- rep(0, lt) #nolint

    #sort so it prints nice
    tempdata <- temp %>% arrange(year, industry) #nolint
    #initiate empty theta
    thetas <- data.frame()

    #get sector names and years
    sectors <- unique(tempdata$industry)
    # Initialize a flag for errors
    error_flag <- FALSE

    ##################### 1 loop over sectors ##########################
    for (i in 1:length(sectors)) { #nolint

      # Subset the data to the rolling windows
      pdata_sector <- tempdata %>% filter(industry == sectors[i]) #nolint

      # Get the range of years
      years <- range(pdata_sector$year)
      all_years <- seq(from = years[1], to = years[2])


      ##############################################################
      ##################### 1 rolling window ##########################
      ##############################################################
      #loop over years
      for (current_year in  all_years) {

        # Initialize a flag for errors
        error_flag <- FALSE

        # Subset the data to the rolling windows
        pdata_window <- pdata_sector %>% filter(year >= current_year - (r - 1) / 2, #nolint
                                                year <= current_year + (r - 1) / 2) #nolint

        #increase window if not enough obs
        e_r <- 0

        while (sum(complete.cases(pdata_window[, c(xvars, xvar_l)])) < nmin) {
          e_r <- e_r + 1
          pdata_window <- pdata_sector %>% filter(year >= current_year - ((r - 1) / 2 + e_r), #nolint
                                                    year <= current_year + ((r - 1) / 2 + e_r)) #nolint
          if (e_r > years[2] - years[1]) {
            break
          }
        }


        # Run the first stage and handle errors
        panelf <- tryCatch({
          first_stage_exp(yvar, fs_rhs, pdata_window)
        }, error = function(e) {
          error_flag <<- TRUE
          NA
        })

        #ols for starting value
        formula <- as.formula(paste(yvar, "~ ."))
        ols <- lm(formula, data = pdata_window[, c(yvar, xvars)])
        theta_init[1:l]<- ols$coefficients #nolint


        #update starting value to last value
        #(not doing, use ols on full sample for all starting values)
        #if (!is.na(result$par[1])) {
        #  theta_init <- result$par #nolint
        #}

        # Run the second stage and handle errors
        result <- tryCatch({
          result <-
            second_stage_exp(
                             panelf, yvar, xvars, xvar_l,
                             ss_controls, ss_controls_l, orthogs,
                             theta_init)
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
        counts <-  result$counts[1]

        #to be added to output of loop
        theta_temp <- c(
          sector = sectors[i],
          year = current_year,
          theta = theta_est,
          convergence = convergence,
          n = sum(complete.cases(pdata_window[, c("c", "c_l")])),
          theta_k = theta_kest,
          w_width = r + 2 * e_r,
          w_start = current_year - ((r - 1) / 2 + e_r),
          w_end = current_year + ((r - 1) / 2 + e_r),
          counts =  counts
        )

        thetas <- rbind(thetas, theta_temp)

        print(paste(
                    "Y:", current_year, "I:", sectors[i], "theta_c:",
                    round(theta_est, 5), "cc:", convergence, "its", counts))

      }
      if (error_flag) {
        print(paste("\n \n !!Sector", sectors[i],
                    "estimation complete WITH ERRORS!! \n \n"))
      } else {
        print(
              paste("\n \n Sector",
                    sectors[i], "estimation complete without errors. \n \n"))
      }
    }
    #clean output and names
    thetas <- data.frame(thetas)
    rownames(thetas) <- NULL
    names(thetas) <-
      c("industry", "fyear", "theta", "convergence", "n.obs", "theta_k",
        "w_width", "w_start", "w_end", "iterations")

    thetas$theta <- as.numeric(as.character(thetas$theta))
    thetas$fyear <- as.numeric(as.character(thetas$fyear))
    thetas$convergence <- as.numeric(as.character(thetas$convergence))
    thetas$n.obs <- as.numeric(as.character(thetas$n.obs))
    thetas$theta_k <- as.numeric(as.character(thetas$theta_k))
    thetas$w_width <- as.numeric(as.character(thetas$w_width))
    thetas$iterations <- as.numeric(as.character(thetas$iterations))
    thetas
  }













acf_rolling_window_exp <-
  function(temp, yvar, xvars, xvar_l, fs_rhs,
    ss_controls, ss_controls_l, orthogs, r, block) { #nolint


    ##################### 0 prep inputs for 2 stages ##########################
    #get length of xvars and ss_controls
    l <- length(xvars) + 1
    lt <- l + length(ss_controls)
    theta_init <- rep(0, lt) #nolint

    #sort so it prints nice
    tempdata <- temp %>% arrange(year, industry) #nolint
    #initiate empty theta
    thetas <- data.frame()

    #get sector names and years
    sectors <- unique(tempdata$industry)
    # Get the range of years
    years <- range(tempdata$year)
    all_years <- seq(from = years[1] + block, to = years[2])

    # Initialize a flag for errors
    error_flag <- FALSE

    ##################### 1 loop over sectors ##########################
    for (i in 1:length(sectors)) { #nolint

      # Subset the data to the rolling windows
      pdata_sector <- tempdata %>% filter(industry == sectors[i]) #nolint

      #ols for starting value (using full sample)
      formula <- as.formula(paste(yvar, "~ ."))
      ols <- lm(formula, data = pdata_sector[, c(yvar, xvars)])
      theta_init[1:l]<- ols$coefficients #nolint

      ##############################################################
      ##################### 1 block window ##########################
      ##############################################################
      pdata_window <- pdata_sector %>% filter(year >= years[1], #nolint
                                              year <= years[1] + block) #nolint

      # Run the first stage and handle errors
      panelf <- tryCatch({
        first_stage_exp(yvar, fs_rhs, pdata_window)
      }, error = function(e) {
        error_flag <<- TRUE
        NA
      })

      # Run the second stage and handle errors
      result <- tryCatch({
        result <-
          second_stage_exp(
                           panelf, yvar, xvars, xvar_l,
                           ss_controls, ss_controls_l, orthogs,
                           theta_init)
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

      #fill in for all of block
      for (j in 1:block) {
        # Add the year to theta_temp
        theta_temp <- c(
          sector = sectors[i],
          year = years[1] + j - 1,
          theta = theta_est,
          convergence = convergence,
          n = sum(complete.cases(pdata_window[, c("c", "c_l")])),
          theta_k = theta_kest
        )

        thetas <- rbind(thetas, theta_temp)
      }

      print(paste(
                  "BLOCK YEAR", "I:", sectors[i], "theta_c:",
                  round(theta_est, 5), "cc:", convergence))

      ##############################################################
      ##################### 1 rolling window ##########################
      ##############################################################
      #loop over years
      for (current_year in  all_years) {

        # Initialize a flag for errors
        error_flag <- FALSE

        # Subset the data to the rolling windows
        pdata_window <- pdata_sector %>% filter(year >= current_year - (r - 1) / 2, #nolint
                                                year <= current_year + (r - 1) / 2) #nolint

        # Run the first stage and handle errors
        panelf <- tryCatch({
          first_stage_exp(yvar, fs_rhs, pdata_window)
        }, error = function(e) {
          error_flag <<- TRUE
          NA
        })

        #update starting value to last value 
        #(not doing, use ols on full sample for all starting values)
        #if (!is.na(result$par[1])) {
        #  theta_init <- result$par
        #}

        # Run the second stage and handle errors
        result <- tryCatch({
          result <-
            second_stage_exp(
                             panelf, yvar, xvars, xvar_l,
                             ss_controls, ss_controls_l, orthogs,
                             theta_init)
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
          n = sum(complete.cases(pdata_window[, c("c", "c_l")])),
          theta_k = theta_kest
        )

        thetas <- rbind(thetas, theta_temp)

        print(paste(
                    "Y:", current_year, "I:", sectors[i], "theta_c:",
                    round(theta_est, 5), "cc:", convergence))

      }
      if (error_flag) {
        print(paste("\n \n !!Sector", sectors[i],
                    "estimation complete WITH ERRORS!! \n \n"))
      } else {
        print(
              paste("\n \n Sector",
                    sectors[i], "estimation complete without errors. \n \n"))
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



############################################################
############################################################
##################   3: useful    #####################
############################################################
############################################################

p_lag <- function(data, tvar, index, var, suffix) {
  data_l <- data
  #time + 1
  data_l[tvar] <- data_l[tvar]  + 1
  #rename to lag
  lagname <- paste0(var, suffix)
  data_l[lagname] <- data_l[var]
  #keep just new var and things needed for merge
  data_l <- data_l[c(index, tvar, lagname)]
  #merge
  data <- merge(data, data_l, by = c(index, tvar), all.x = TRUE)

  data

}




#generate polynominals, defining seperate saves a few lines later

#generates polys
poly_gen <- function(df, vars, degree = 3) {
  # Convert the selected variables to a matrix
  matrix_vars <- data.matrix(df[vars])

  # Generate the polynomial terms
  poly_vars <- poly(matrix_vars, degree, raw = TRUE)

  # Get the original names generated by poly function
  orig_names <- colnames(poly_vars)

  # Function to create new names
  create_new_name <- function(orig_name, vars) {
    # Split the original name into powers
    powers <- strsplit(orig_name, "\\.")[[1]]

    # Create a new name by combining the variable names with their powers
    new_name <- paste0(mapply(function(var, power) {
      paste0(var, "^", power)
    }, vars, powers), collapse = "")

    return(new_name)
  }

  # Create new names for all variables
  new_names <- sapply(orig_names, create_new_name, vars = vars)

  # Assign the new names to the columns of poly_vars
  colnames(poly_vars) <- new_names

  # Add the new variables to the data frame
  df <- cbind(df, poly_vars)

  return(df)
}

#names created by poly_gen
poly_gen_names <- function(vars, degree = 3) {
  # Generate a matrix of dummy data to get the variable names
  dummy_data <- matrix(0, nrow = 2, ncol = length(vars))
  poly_vars <- poly(dummy_data, degree, raw = TRUE)

  # Get the original names generated by poly function
  orig_names <- colnames(poly_vars)

  # Function to create new names
  create_new_name <- function(orig_name, vars) {
    # Split the original name into powers
    powers <- strsplit(orig_name, "\\.")[[1]]

    # Create a new name by combining the variable names with their powers
    new_name <- paste0(mapply(function(var, power) {
      paste0(var, "^", power)
    }, vars, powers), collapse = "")

    return(new_name)
  }

  # Create new names for all variables
  new_names <- sapply(orig_names, create_new_name, vars = vars)

  names(new_names) <- NULL

  return(new_names)
}

#generate lags
gen_lags <- function(panel, vars) {
  for (i in vars) {
    panel <- p_lag(panel, "year", "GVKEY", i, "_l")
  }
  panel
}

#generate names of lags following generate lags
lag_names <- function(vars) {

  lagged <- c()
  for (i in vars) {
    new_var_name <- paste0(i, "_l")
    lagged <- c(lagged, new_var_name)
  }
  return(lagged)
}


#generate x time z
gen_xz <- function(x, z, panel) {

  for (i in x) {
    for (j in z) {
      new_var_name <- paste0(i, j)
      panel[[new_var_name]] <- panel[[i]] * panel[[j]]
    }
  }
  return(panel)
}

#generate x time z names
xz_names <- function(x, z) {
  xz <- c()
  for (i in x) {
    for (j in z) {
      new_var_name <- paste0(i, j)
      xz <- c(xz, new_var_name)
    }
  }
  return(xz)
}
