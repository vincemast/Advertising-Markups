require(ggplot2)
############################################################
############################################################
#########   1: save plot with approiate directs    #########
############################################################
############################################################

save_f <- function(plot, name, directories, w, h, switch) {

  #only run if switch is on
  if (switch == 1) {
    #set wd to folder for saved images
    setwd(directories[3])

    #save plot
    ggsave(plot = plot, filename = name, width = w, height = h, units = "in")

    #return wd to functions folder
    setwd(directories[1])
  }

}


############################################################
############################################################
#########   2: extract relevant bootstrapped CEs #########
############################################################
############################################################

be_extra <- function(boot_out, shock, variable, n_boot, ahead, n_vars, ci) {

  output <- matrix(NA, nrow = n_boot, ncol = ahead)

  #bootstrapped IRS are indexed as follows
  #1:n_ah is names denoting periods
  #then come responses in order eps_yi -> yj looping over j than i
  #thus n_ah*(1+ [i-1]*n_vars +j)*[1:ahead] gives eps_yi -> yj
  strt <- ahead * ((variable - 1) * n_vars + shock) + 1
  end <- strt + ahead - 1


  for (i in 1:n_boot) {
    # Extract the IRF for the i-th bootstrap sample
    irf_i <- boot_out$bootstrap[[i]]
    irf_i <- data.frame(unlist(irf_i))
    output[i, ] <- irf_i[strt:end, 1]
  }

  output

  bs_lb <- apply(output, 2, quantile, probs = (100 - ci) / 200)
  bs_ub <- apply(output, 2, quantile, probs = 1 - (100 - ci) / 200)

  out <- cbind(bs_lb, bs_ub)

  out

}
