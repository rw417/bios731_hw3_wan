####################################################################
# Robert Wan
# March 2025
#
# This file loads in simulation estimates, calculates peformance measures, and saves
# a dataset for bias and a dataset for coverage
####################################################################


library(dplyr)
library(tidyr)


###############################################################
## Load and merge data from each simulation scenario
###############################################################

scenarios = list.files(here::here("30_results"), pattern = "2025", full.names = TRUE)

results = matrix(ncol=9)
bs_runtime = matrix(ncol=3)

for(i in 1:length(scenarios)){
  scenario = readRDS(scenarios[i])
  results = rbind(results, cbind(scenario$wald_results, "wald", i))
  results = rbind(results, cbind(scenario$percentile_results, "percentile", i))
  bs_runtime = rbind(bs_runtime, cbind(scenario$bs_runtime,i))
}

results = as.data.frame(results[-1,])
colnames(results) = c(colnames(results)[1:7], "method", "scenario")
# Type the columns
results[c("sim_beta_hat", "se_b", "ci_lower", "ci_upper", "coverage", "typeI", "power")] = lapply(results[c("sim_beta_hat", "se_b", "ci_lower", "ci_upper", "coverage", "typeI", "power")], as.numeric)

bs_runtime = as.data.frame(bs_runtime[-1,])
colnames(bs_runtime) = c(colnames(bs_runtime)[1:2], "scenario")

###############################################################
## Add information from params_grid to results
###############################################################
nsim = 475
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

results$beta_true = params_grid[results$scenario, "beta_true"]
results$error_form = params_grid[results$scenario, "error_form"]
results$n = params_grid[results$scenario, "n"]

###############################################################
## estimate performance measures
###############################################################

# calculating bias of beta_hat
bias_df = results %>%
  filter(!is.na(sim_beta_hat)) %>%
  group_by(method, beta_true, n, error_form) %>%
  summarize(nsim = nsim, n = factor(n), 
            bias = mean(sim_beta_hat, na.rm = TRUE)-beta_true,
            var_bias = sum((sim_beta_hat-mean(sim_beta_hat, na.rm = TRUE))^2)/(nsim*(nsim-1)),
            se_bias = sqrt(var_bias)) %>%
  ungroup() %>%
  unique()


# calculate coverage of beta_hat for both methods
coverage_df = results %>%
  group_by(method, beta_true, n, error_form) %>%
  summarize(nsim = nsim, n = factor(n),
            coverage = sum(coverage) / n(),
            var_coverage = coverage * (1-coverage)/n(),
            se_coverage = sqrt(var_coverage)
            ) %>%
  ungroup() %>%
  unique()

# calculate typeI error rate  for both methods
typeI_df = results %>%
  group_by(method, beta_true, n, error_form) %>%
  summarize(nsim = nsim, n = factor(n),
            typeI = sum(typeI) / n(),
            var_typeI = typeI * (1-typeI)/n(),
            se_typeI = sqrt(typeI)
  ) %>%
  ungroup() %>%
  unique() %>%
  filter(!is.na(typeI))

# calculate power for both methods
power_df = results %>%
  group_by(method, beta_true, n, error_form) %>%
  summarize(nsim = nsim, n = factor(n),
            power = sum(power) / n(),
            var_power = power * (1-power)/n(),
            se_power = sqrt(power)
  ) %>%
  ungroup() %>%
  unique() %>%
  filter(!is.na(power))

power_df = results %>%
  group_by(method, beta_true, n, error_form) %>%
  summarize(nsim = nsim, n = factor(n),
            power = sum(power) / nsim,
            var_power = power * (1-power)/nsim,
            se_power = sqrt(power)
  ) %>%
  ungroup() %>%
  unique() %>%
  filter(!is.na(power))


# reshape bs_runtime to combine "wald" and "percentile" into one column
time_df = bs_runtime %>% 
  pivot_longer(cols = c("wald", "percentile"), 
  names_to = "method", 
  values_to = "run_time")



###############################################################
## Save performance measure datasets
###############################################################

save(bias_df, coverage_df,typeI_df, power_df, time_df,
     file = here::here("30_results", "performance_measures.RDA"))

