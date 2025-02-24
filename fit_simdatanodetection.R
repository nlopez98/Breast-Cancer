library(rjags)
library(dplyr)

# Number of patients to simulate
n_patients <- 28000
set.seed(888)

# Age at diagnosis distribution (mean and sd for different stages)
mean_age_stage <- c(52.5, 56.7, 57.9, 60.4)  # Calculated from the safe room export
sd_age_stage <- c(13, 12.6, 12.6, 12.2)    # Calculated from the safe room export

# Year range
year_start <- 1970
year_end <- 2019
year_buckets <- seq(1970, 2019, by = 5)
bucket_probs <- c(0.016, 0.021, 0.029, 0.043, 0.062, 0.092, 0.130, 0.159, 0.198, 0.250)  # Calculated from the safe room export, proportions of each 5y block
year_bucket_indices <- sample(1:length(bucket_probs), n_patients, replace = TRUE, prob = bucket_probs)
year_buckets_selected <- year_buckets[year_bucket_indices]

# Distribute years uniformly within each bucket
diagnosis_year <- year_buckets_selected + sample(0:4, n_patients, replace = TRUE)

# Ethnicity distribution
ethnicity_prob <- c(0.82, 0.06, 0.1, 0.02) # Calculated from the safe room export

# Stage distribution
stage_prob <- c(0.33, 0.39, 0.18, 0.1) # Calculated from the safe room export


# Assign stages based on defined probabilities
stages <- sample(1:4, n_patients, replace = TRUE, prob = stage_prob)

# Generate age at diagnosis based on the stage-specific distribution
age_at_diagnosis <- mapply(rnorm, n = 1, mean = mean_age_stage[stages], sd = sd_age_stage[stages])

# Ensure ages are within the range (10 to 120)
age_at_diagnosis <- as.integer(pmin(pmax(age_at_diagnosis, 10), 120))

# Calculate birth year
birth_year <- diagnosis_year - round(age_at_diagnosis)

# Assign ethnicities based on defined probabilities
ethnicity <- sample(1:4, n_patients, replace = TRUE, prob = ethnicity_prob)

# Create a data frame
simulated_data <- data.frame(
  #  Cohort = birth_year/1000,
  Age = age_at_diagnosis,
  Stage = stages,
  Race = ethnicity
)

# Convert numeric ethnicity and stage to factors with labels
ethnicity_labels <- c("CN", "IN", "MY", "XX")

simulated_data$Race <- factor(simulated_data$Race, labels = ethnicity_labels)

data_list <- list(Age = simulated_data$Age, Stage = simulated_data$Stage, Race_numeric = as.numeric(factor(simulated_data$Race)), N = nrow(simulated_data))


# Define the combined model string with Gompertz function and negative binomial roll for transition ages
combined_model_string <- "
model {

  # Priors for race effect parameters
  for (k in 1:4) {
    gamma_race[k] ~ dgamma(1, 1)
  }

  # Priors for baseline hazard rates, now replaced by transition parameters for each stage
  for (j in 1:4) {
    lambda_transition_positive[j] ~ dnorm(3,3)
    lambda_transition[j] <- -lambda_transition_positive[j]  # Ensuring lambda_transition is negative
  }

  # Priors for transition parameters
  beta_time_in_stage ~ dunif(0,0.01)

  # Likelihood for stages and transition ages
  for (i in 1:N) {    
    # Initialize baseline age
    baseline_age[i] ~ dunif(40, 60)
  ##  theta_cohort[i] ~ dunif(0,1)
    for (s in 1:4) {
      log_q[i, s] <- lambda_transition[s] + beta_time_in_stage * Age[i] + gamma_race[Race_numeric[i]] 
      q[i, s] <- exp(log_q[i, s])  # Convert to probability
      years_in_stage[i, s] ~ dexp(q[i,s])
    }

    transition_age[i, 1] <- baseline_age[i] + years_in_stage[i,1]
    for (s in 2:4) {
      transition_age[i, s] <- transition_age[i, s-1] + years_in_stage[i,s]
    }

    for (s in 1:4) {
      accept[i, s] <- ifelse(Stage[i] == s, step(25 - abs(transition_age[i, s] - Age[i])), 1)
    }

    accept_iter[i] <- prod(accept[i, ])
  }
}
"
# Compile the modified combined model with 4 chains
combined_jags_model <- jags.model(textConnection(combined_model_string), data = data_list, n.chains = 4)

# Burn-in
update(combined_jags_model, 500)

# Define monitored parameters
monitored_params <- c("lambda_transition", "gamma_race", "beta_time_in_stage", "baseline_age", "transition_age", "accept_iter")

# Sampling from the model across 4 chains
combined_jags_samples <- coda.samples(combined_jags_model, 
                                      variable.names = monitored_params, 
                                      n.iter = 2000, 
                                      thin = 1)

# Convert samples to a data frame for all chains
samples_df_all_chains <- as.data.frame(do.call(rbind, lapply(combined_jags_samples, as.matrix)))

# Extract the accept_iter matrix across all chains
accept_iter_matrix_all_chains <- as.matrix(samples_df_all_chains[, grep("accept_iter", colnames(samples_df_all_chains))])

# Acceptance threshold
acceptance_threshold <- 0.8

# Check acceptance criterion for all iterations across chains
accepted_iterations_all_chains <- apply(accept_iter_matrix_all_chains, 1, function(row) mean(row) >= acceptance_threshold)

# Extract only accepted iterations across chains
accepted_samples_all_chains <- samples_df_all_chains[which(accepted_iterations_all_chains == TRUE),]
acceptance_rate <- nrow(accepted_samples_all_chains) / (1000 * 4) # Updated for 4 chains

# Extract and print each parameter for all chains
lambda_transition_samples_all_chains <- accepted_samples_all_chains[, grep("lambda_transition", colnames(accepted_samples_all_chains))]
baseline_age_samples_all_chains <- accepted_samples_all_chains[, grep("baseline_age", colnames(accepted_samples_all_chains))]
gamma_race_samples_all_chains <- accepted_samples_all_chains[, grep("gamma_race", colnames(accepted_samples_all_chains))]
beta_time_in_stage_samples_all_chains <- accepted_samples_all_chains[, grep("beta_time_in_stage", colnames(accepted_samples_all_chains))] 
transition_age_samples_all_chains <- accepted_samples_all_chains[, grep("transition_age", colnames(accepted_samples_all_chains))]
#theta_cohort_samples_all_chains <- accepted_samples_all_chains[,grep("theta_cohort", colnames(accepted_samples_all_chains))]

# Compute group means across all chains
num_groups <- 4
cols_per_group <- ncol(transition_age_samples_all_chains) / num_groups
group_means_all_chains <- sapply(1:num_groups, function(group) {
  start_col <- (group - 1) * cols_per_group + 1
  end_col <- group * cols_per_group
  rowMeans(transition_age_samples_all_chains[, start_col:end_col])
})

group_means_df_all_chains <- data.frame(group_means_all_chains)
colnames(group_means_df_all_chains) <- paste0("Stage_", 1:num_groups)

# Print the parameter summaries across all chains
print(colMeans(lambda_transition_samples_all_chains))
print(mean(rowMeans(baseline_age_samples_all_chains)))
print(colMeans(gamma_race_samples_all_chains))
print(mean(beta_time_in_stage_samples_all_chains))
print(colMeans(group_means_df_all_chains))
#print(mean(rowMeans(theta_cohort_samples_all_chains)))
race_labels <- c("Chinese", "Malay", "Indian", "Others")

#####Plots
library(coda)
# Trace plots for lambda_transition parameters
for (j in 1:4) { 
  png(paste0("traceplots_lambda_transition_", j, ".png"), width = 800, height = 800)
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2x2 layout for 4 chains
  for (i in 1:4) {
    traceplot(
      combined_jags_samples[[i]][, paste0("lambda_transition[", j, "]")], 
      main = paste("lambda_transition[", j, "] (Chain ", i, ")", sep = ""), 
      col = "darkslateblue"
    )
  }
  dev.off()  # Save PNG for this j
}

# Trace plots for gamma_race parameters
for (j in 1:4) { 
  png(paste0("traceplots_gamma_race_", race_labels[j], ".png"), width = 800, height = 800)
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2x2 layout for 4 chains
  for (i in 1:4) {
    traceplot(
      combined_jags_samples[[i]][, paste0("gamma_race[", j, "]")], 
      main = paste(race_labels[j], " (Chain ", i, ")", sep = ""), 
      col = "darkslateblue"
    )
  }
  dev.off()  # Save PNG for this j
}


# Trace plot for beta_time_in_stage across all chains
png("traceplots_beta_time_in_stage.png", width = 800, height = 800)  # Open a PNG device for beta_time_in_stage
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2 rows, 2 columns layout
for (i in 1:4) {
  traceplot(combined_jags_samples[[i]][, paste0("beta_time_in_stage")], 
            main = paste("Trace plot for Chain ", i, sep = ""), col = "darkslateblue")
}
dev.off()  # Close the PNG device for beta_time_in_stage


for (j in 1:4) { 
  png(paste0("densityplots_lambda_transition_", j, ".png"), width = 800, height = 800)
  plot(
    density(combined_jags_samples[[1]][, paste0("lambda_transition[", j, "]")]), 
    main = paste("Density plot for lambda_transition[", j, "]"), 
    col = "darkslateblue", 
    lwd = 2
  )
  for (i in 2:4) {
    lines(
      density(combined_jags_samples[[i]][, paste0("lambda_transition[", j, "]")]), 
      col = i + 1, 
      lwd = 2
    )
  }
  dev.off()
}

# Trace plots for gamma_race parameters across all chains

for (j in 1:4) { 
  png(paste0("densityplots_gamma_race_", race_labels[j], ".png"), width = 800, height = 800)
  plot(
    density(combined_jags_samples[[1]][, paste0("gamma_race[", j, "]")]), 
    main = paste("Density plot for ", race_labels[j]), 
    col = "darkslateblue",
    lwd = 2
  )
  for (i in 2:4) {
    lines(density(combined_jags_samples[[i]][, paste0("gamma_race[", j, "]")]), col = i + 1, lwd = 2)
  }
  dev.off()
}



# Trace plot for beta_time_in_stage across all chains
png("densityplots_beta_time_in_stage.png", width = 800, height = 800)  # Open a PNG device for beta_time_in_stage
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 2 rows, 2 columns layout
for (i in 1:4) {
  densplot(combined_jags_samples[[i]][, paste0("beta_time_in_stage")], 
            main = paste("Density plot for Chain ", i, sep = ""), col = "darkslateblue")
}
dev.off()  # Close the PNG device for beta_time_in_stage


# Parameters
n <- nrow(simulated_data)  # number of individuals
initial_state <- 1
max_state <- 4
initial_age_range <- c(31, 60) # Generate 20 initial ages for each individual

# Provided beta and lambda values
beta <- mean(beta_time_in_stage_samples_all_chains)
lambdas <- colMeans(lambda_transition_samples_all_chains)

# Function to calculate transition time
calc_transition_time <- function(beta, lambda, t0, U) {
  t <- (1 / beta) * (log(-beta * log(U) + exp(lambda + beta * t0))) - (lambda / beta)
  return(t)
}

# Initialize a list for storing ages at each state
simulation_data <- list()

# Generate 30 initial ages for each individual and simulate their transition ages
set.seed(123)  # for reproducibility
for (i in 1:n) {
  if (i%%100 == 0){print(i)}
  initial_ages <- runif(30, initial_age_range[1], initial_age_range[2])
  individual_data <- data.frame(
    id = i,
    initial_age = initial_ages,
    state_1_age = NA,
    state_2_age = NA,
    state_3_age = NA,
    state_4_age = NA
  )
  
  for (j in 1:30) {
    current_age <- initial_ages[j]
    for (state in initial_state:max_state) {
      if (state == max_state) {
        individual_data[[paste0('state_', state, '_age')]][j] <- current_age
        break
      } else {
        lambda <- lambdas[state + 1]
        U <- runif(1, 0, 1)  # Generate a random uniform number
        next_age <- calc_transition_time(beta, lambda, current_age, U)
        current_age <- next_age
        individual_data[[paste0('state_', state, '_age')]][j] <- current_age
      }
    }
  }
  
  simulation_data[[i]] <- individual_data
}

# Combine all individual data into a single dataframe
simulation_data_df <- bind_rows(simulation_data)

# Add a column for id based on row number
simulated_data$id <- seq_len(nrow(simulated_data))

# Calculate the relative differences and find the best initial age for each individual
best_initial_ages <- data.frame(id = numeric(), best_initial_age = numeric(), percent_difference = numeric(), best_age = numeric(), stage = numeric())
for (i in 1:n) {
  if (i%%100 == 0){print(i)}
  individual_simulation_data <- simulation_data_df %>% filter(id == i)
  individual_provided_data <- simulated_data %>% filter(id == i)
  
  percent_differences <- numeric()
  
  for (j in 1:20) {
    differences <- numeric()
    stage <- individual_provided_data$Stage
    actual_age <- individual_provided_data$Age
    simulated_age <- individual_simulation_data[,stage + 2][j]
    differences <- c(differences, abs(actual_age - simulated_age)*100/actual_age)
    percent_differences <- c(percent_differences, mean(differences, na.rm = TRUE))
  }
  
  best_index <- which.min(percent_differences)
  best_initial_age <- individual_simulation_data$initial_age[best_index]
  best_percent_difference <- percent_differences[best_index]
  best_age <- individual_simulation_data[best_index, paste0("state_", stage, "_age")]
  
  best_initial_ages <- rbind(best_initial_ages, data.frame(id = i, best_initial_age = best_initial_age, percent_difference = best_percent_difference, best_age = best_age, stage = stage))
}

# Calculate and print the overall mean best initial age
mean_best_initial_age <- mean(best_initial_ages$best_initial_age, na.rm = TRUE)
print(mean_best_initial_age)
print(t.test(best_initial_ages$best_initial_age, na.rm = TRUE))

for (i in 1:4){
  print(t.test(best_initial_ages[best_initial_ages$stage == i,]$best_age)$conf.int)
}
lambda_zero <- t.test(lambda_transition_samples_all_chains[,1])$conf.int
lambda_zero <- c(mean(lambda_transition_samples_all_chains[,1]), lambda_zero)
lambda_one <- t.test(lambda_transition_samples_all_chains[,2])$conf.int
lambda_one <- c(mean(lambda_transition_samples_all_chains[,2]), lambda_one)
lambda_two <- t.test(lambda_transition_samples_all_chains[,3])$conf.int
lambda_two <- c(mean(lambda_transition_samples_all_chains[,3]), lambda_two)
lambda_three <- t.test(lambda_transition_samples_all_chains[,4])$conf.int
lambda_three <- c(mean(lambda_transition_samples_all_chains[,4]), lambda_three)

gamma_chinese <- t.test(gamma_race_samples_all_chains[,1])$conf.int
gamma_chinese <- c(mean(gamma_race_samples_all_chains[,1]), gamma_chinese)
gamma_indian <- t.test(gamma_race_samples_all_chains[,2])$conf.int
gamma_indian <- c(mean(gamma_race_samples_all_chains[,2]), gamma_indian)
gamma_malay <- t.test(gamma_race_samples_all_chains[,3])$conf.int
gamma_malay <- c(mean(gamma_race_samples_all_chains[,3]), gamma_malay)
gamma_other <- t.test(gamma_race_samples_all_chains[,4])$conf.int
gamma_other <- c(mean(gamma_race_samples_all_chains[,4]), gamma_other)

beta_ci <- t.test(beta_time_in_stage_samples_all_chains)$conf.int
beta_ci <- c(mean(beta_time_in_stage_samples_all_chains), beta_ci)

stagezero <- mean_best_initial_age
stageone <- mean(best_initial_ages[best_initial_ages$stage == 1,]$best_age)
stagetwo <-mean(best_initial_ages[best_initial_ages$stage == 2,]$best_age)
stagethree <-mean(best_initial_ages[best_initial_ages$stage == 3,]$best_age)
stagefour <-mean(best_initial_ages[best_initial_ages$stage == 4,]$best_age)

quantile(best_initial_ages$best_initial_age, na.rm = TRUE,probs = 0.75)
quantile(best_initial_ages$best_initial_age, na.rm = TRUE,probs = 0.25)
quantile(best_initial_ages[best_initial_ages$stage == 1,]$best_age, probs = 0.75)
quantile(best_initial_ages[best_initial_ages$stage == 1,]$best_age, probs = 0.25)
quantile(best_initial_ages[best_initial_ages$stage == 2,]$best_age, probs = 0.75)
quantile(best_initial_ages[best_initial_ages$stage == 2,]$best_age, probs = 0.25)
quantile(best_initial_ages[best_initial_ages$stage == 3,]$best_age, probs = 0.75)
quantile(best_initial_ages[best_initial_ages$stage == 3,]$best_age, probs = 0.25)
quantile(best_initial_ages[best_initial_ages$stage == 4,]$best_age, probs = 0.75)
quantile(best_initial_ages[best_initial_ages$stage == 4,]$best_age, probs = 0.25)

print(c(stagezero, stageone, stagetwo, stagethree, stagefour))
print(1/(stageone - stagezero))
print(1/(stagetwo - stageone))
print(1/(stagethree - stagetwo))
print(1/(stagefour - stagethree))
#print(colMeans(sensitivity_samples))
#print(mean(alpha_samples))
result <- c()
result <- c(result, stagezero, stageone, stagetwo, stagethree, stagefour)
#result <- c(result, colMeans(sensitivity_samples), mean(alpha_samples))
result <- c(result, 1/(stageone - stagezero), 1/(stagetwo - stageone), 1/(stagethree - stagetwo), min(1, 1/(stagefour - stagethree)))
result <- c(result, c(sd(best_initial_ages$best_initial_age), sd(best_initial_ages[best_initial_ages$stage == 1,]$best_age),sd(best_initial_ages[best_initial_ages$stage == 2,]$best_age), 
                      sd(best_initial_ages[best_initial_ages$stage == 3,]$best_age), sd(best_initial_ages[best_initial_ages$stage == 4,]$best_age)))
result <- c(result, c(t.test(best_initial_ages$best_initial_age)$conf.int, t.test(best_initial_ages[best_initial_ages$stage == 1,]$best_age)$conf.int, t.test(best_initial_ages[best_initial_ages$stage == 2,]$best_age)$conf.int,
                      t.test(best_initial_ages[best_initial_ages$stage == 3,]$best_age)$conf.int, t.test(best_initial_ages[best_initial_ages$stage == 4,]$best_age)$conf.int))
result <- data.frame(result)
result$names <- c("Stage0", "Stage1", "Stage2", "Stage3", "Stage4", "prob_1", "prob_2", "prob_3", "prob_4", "sd_0", "sd_1", "sd_2", "sd_3", "sd_4", "lower_Stage0", "upper_Stage0", "lower_Stage1", "upper_Stage1",
                  "lower_Stage2", "upper_Stage2", "lower_Stage3", "upper_Stage3", "lower_Stage4", "upper_Stage4")

params_result <- c()
params_result <- c(params_result, lambda_zero, lambda_one, lambda_two, lambda_three, gamma_chinese, gamma_malay, gamma_indian, gamma_other, beta_ci)
params_result <- data.frame(params_result)
params_result$names <- c("lambda_zero_mean", "lambda_zero_lower", "lambda_zero_upper","lambda_one_mean", "lambda_one_lower", "lambda_one_upper",
                         "lambda_two_mean", "lambda_two_lower", "lambda_two_upper","lambda_three_mean", "lambda_three_lower", "lambda_three_upper",
                         "gamma_chinese_mean", "gamma_chinese_lower", "gamma_chinese_upper","gamma_malay_mean", "gamma_malay_lower", "gamma_malay_upper",
                         "gamma_indian_mean", "gamma_indian_lower", "gamma_indian_upper","gamma_other_mean", "gamma_other_lower", "gamma_other_upper",
                         "beta_mean", "beta_lower", "beta_upper")

saveRDS(result, "result.RDS")
saveRDS(params_result, "params_result.RDS")
