####################################################################
# Robert Wan
# January 2025
#
# This file produces simulations for linear regression under different data
# generating scenarios
####################################################################

library(stats)

###############################################################
## define or source functions used in code below
###############################################################

# Generate simulated data from a specified distribution.
source(here::here("10_source", "01_simulate_data.R"))

# Fit a linear model
source(here::here("10_source", "02_apply_methods.R"))

# Get estimates of one bootstrap iteration.
source(here::here("10_source", "03_extract_estimates.R"))

# Extract estimates from finished bootstraps
source(here::here("10_source", "04_two_bootstrap_estimates.R"))

# Run bootstrap on simulated data
source(here::here("10_source", "05_run_bootstrap.R"))

###############################################################
## set simulation design elements
###############################################################

# nsim calculated based on desired coverage of 95%
# with Monte Carlo error of no more than 1%
nsim <- 475
nboot <- 10000

n <- c(20)
beta_true <- c(0, 0.5)
error_form <- c("normal", "gamma")
error_sigma2 <- c(2)

params_grid <- expand.grid(
  n = n,
  nsim = nsim,
  beta_true = beta_true,
  error_form = error_form,
  error_sigma2 = error_sigma2
)

###############################################################
# start simulation code ####
###############################################################
# Generate a random seed for each simulated dataset
seed <- floor(runif(nsim, 1, 10000))

# Loop through all simulations
for (scenario in 1:nrow(params_grid)) {
  params <- params_grid[scenario, ]

  print(paste("Running scenario: ", scenario))
  print(Sys.time())
  
  wald_results <- matrix(
    NA,
    nrow = nsim, ncol = 7,
    dimnames = list(NULL, c("sim_beta_hat", "se_b", "ci_lower", "ci_upper", "coverage", "typeI", "power"))
  )
  
  percentile_results <- matrix(
    NA,
    nrow = nsim, ncol = 7,
    dimnames = list(NULL, c("sim_beta_hat", "se_b", "ci_lower", "ci_upper", "coverage", "typeI", "power"))
  )
  
  bs_runtime_results <- matrix(
    NA,
    nrow = nsim, ncol = 2,
    dimnames = list(NULL, c("wald", "percentile"))
  )
  
  # Run simulation in parallel
  for(i in 1:nsim){
    # Set seed for reproducibility within each worker
    set.seed(seed[i])
    
    # if (i %% 50 == 0) {
    #   message(paste("Reached simulation: ", i))
    #   flush.console()
    # }

    ####################
    # Simulate data
    simdata <- get_simdata(
      n = params$n,
      beta_treat = params$beta_true,
      error_form = params$error_form,
      error_sigma2 = params$error_sigma2
    )

    ####################
    # Apply model on simulated data
    sim_fit <- fit_model(simdata)

    ####################
    # Calculate simulation estimates
    sim_beta_hat <- get_estimates(model_fit = sim_fit)

    #####################
    # Run bootstrap on simulated data using different methods
    loop_results <- run_bootstrap(simdata, sim_beta_hat, nboot)
    
    bs_results <- loop_results[[1]]
    bs_runtime <- loop_results[[2]]
    
    #####################
    # Calculate bootstrap estimates
    start_wald <- proc.time()[3]
    wald = wald_estimates(sim_beta_hat, bs_results, beta_true = params$beta_true, alpha = 0.05)
    end_wald <- proc.time()[3]
    bs_runtime['wald'] <- bs_runtime['wald'] + end_wald - start_wald
    
    start_percentile <- proc.time()[3]
    percentile = percentile_estimates(sim_beta_hat, bs_results, beta_true = params$beta_true, alpha = 0.05)
    end_percentile <- proc.time()[3]
    bs_runtime['percentile'] <- bs_runtime['percentile'] + end_percentile - start_percentile

    # Store results
    wald_results[i, ] <- wald
    percentile_results[i, ] <- percentile
    bs_runtime_results[i, ] <- bs_runtime
  }
  
  final_results <- list(
    wald_results = wald_results,
    percentile_results = percentile_results,
    bs_runtime_results = bs_runtime_results
  )
  
  ####################
  # save results
  # note that I am saving results outside of the for loop. For slow simulations,
  # you may want to save each iteration separately
  filename <- paste0("20250307_scenario_", scenario, ".RDS")
  saveRDS(final_results,
    file = here::here("30_results", filename)
  )
  
  print(paste("Finished scenario: ", scenario))
  print(Sys.time())
}


