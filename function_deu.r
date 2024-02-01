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
#adds market share in first stage as DEU 2020

first_stage <- function(panel) {

  #loggd vars
  panel <- panel %>% #nolint
    mutate(c = log(cogs), k = log(ppegt)) #nolint

  # Create the polynomial terms
  panel$poly_cogs <- poly(panel$c, degree = 3, raw = TRUE)
  panel$poly_ppegt <- poly(panel$k, degree = 3, raw = TRUE)
  panel$m_share <- poly(log(panel$industry_share), degree = 3, raw = TRUE)
  # Run the regression
  model <- lm(log(sale) ~ poly_cogs * poly_ppegt, data = panel)

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
  ols <- lm(phi ~ c + k, data = panel)
  theta_init <-   ols$coefficients

  # Solve the GMM problem using moment condition defined above
  result <- optim(theta_init, dwl_momment,
                  method = "Nelder-Mead", panel = panel)

  result$par

}