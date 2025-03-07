####################
# Run bootstrap on simulated data
run_bootstrap <- function(simdata, sim_beta_hat, nboot, nboot_t, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Matrix to store results
  bs_results <- matrix(
    NA,
    nrow = nboot, ncol = 2,
    dimnames = list(NULL, c("beta_hat_b", "t_star"))
  )

  n_bs <- nrow(simdata)
  
  ####################
  # Start of Bootstrap
  start_of_bs <- proc.time()[3] # for timing
  for (b in 1:nboot) {
    ####################
    # sample with replacement from simdata
    bs_idx <- sample(1:n_bs, size = n_bs, replace = TRUE)
    bs_sample <- simdata[bs_idx, ]  # Direct indexing

    ####################
    # apply model to bs_sample
    bs_fit <- fit_model(bs_sample)

    ####################
    # calculate bootstrap estimates of beta
    bs_results[b, "beta_hat_b"] <- get_estimates(model_fit = bs_fit)
  }
  end_of_bs <- proc.time()[3]
  bs_duration_total <- end_of_bs - start_of_bs
  
  bs_runtime <- c(bs_duration_total, bs_duration_total)
  names(bs_runtime) <- c("wald", "percentile")
  
  # End of Bootstrap
  
  return(list(bs_results, bs_runtime))
}

