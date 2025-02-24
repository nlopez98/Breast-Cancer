library(pbapply)
library(parallel)


# Define the bootstrap function from your file
bootstrap_function <- function(seed) {
  set.seed(seed) 



library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(scales)  # For label formatting


#Load and filter data
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(gridExtra)
  library(scales)  # For label formatting
  
  
  #Load and filter data
  populationdata <- readRDS("populationdata_912.rds")
  populationdatafemale <- as.data.table(populationdata)[gender == "female" & stage1 > 0] #suffers
  populationdatafemale$racenumeric <- as.integer(populationdatafemale$index/2) + 1# scale by 1 because r is 1 indexed
  
  ##hard coded
  avg_forty <- c(0.2562365, 0.1497439, 0.2636013, 0.2626916)
  avg_fifties <- c(0.2997404, 0.1996974, 0.3565241, 0.3549556)
  avg_selection <- c(0.2389845, 0.3697223, 0.4540364, 0.5930262)
  chinese_mask <- populationdatafemale$racenumeric==1
  malay_mask <- populationdatafemale$racenumeric==2
  indian_mask <- populationdatafemale$racenumeric==3
  # Define screening cost and age range for screening
  screening_cost_per_person <- 200
  screening_age_start <- 30
  screening_age_end <- 69
  
  # Define analysis timeframe
  start_year <- 1990
  end_year <- 2050
  years <- seq(start_year, end_year)
  
  # Create a matrix to store age for each individual across years
  population_size <- nrow(populationdatafemale)
  age_matrix <- outer(years, populationdatafemale$year_of_birth, `-`)
  
  
  # Determine health states for each individual and year
  state_matrix <- matrix("healthy", nrow = length(years), ncol = population_size)
  
  # Assign states based on stage years, adjusting according to your logic
  for (i in 1:population_size) {
    # Get the years in which the individual reaches each stage
    stage1_year <- populationdatafemale$stage1[i]
    stage2_year <- populationdatafemale$stage2[i]
    stage3_year <- populationdatafemale$stage3[i]
    stage4_year <- populationdatafemale$stage4[i]
    death_year <- populationdatafemale$year_of_death[i]
    
    # If stage year is 0, they never reached that stage and stay at the previous stage
    if (stage1_year == 0) { stage1_year <- Inf }  # No stage 1, stay healthy
    if (stage2_year == 0) { stage2_year <- Inf }  # No stage 2, stay at stage 1
    if (stage3_year == 0) { stage3_year <- Inf }  # No stage 3, stay at stage 2
    if (stage4_year == 0) { stage4_year <- Inf }  # No stage 4, stay at stage 3
    
    # Healthy until stage 1
    state_matrix[years < stage1_year, i] <- "healthy"
    
    # Stage 1 from the year they reach stage 1 until the year before stage 2
    state_matrix[years >= stage1_year & years < stage2_year, i] <- "stage1"
    
    # Stage 2 from the year they reach stage 2 until the year before stage 3
    state_matrix[years >= stage2_year & years < stage3_year, i] <- "stage2"
    
    # Stage 3 from the year they reach stage 3 until the year before stage 4
    state_matrix[years >= stage3_year & years < stage4_year, i] <- "stage3"
    
    # Stage 4 from the year they reach stage 4 until their death year
    if (death_year > 0) {
      state_matrix[years >= stage4_year & years < death_year, i] <- "stage4"
    } else if (death_year == 0) {
      state_matrix[years >= stage4_year, i] <- "stage4"
    }
    
    # If year_of_death is 0, person is considered alive throughout and never dead
    if (death_year != 0) {
      # Mark as dead after death year
      state_matrix[years >= death_year, i] <- "dead"
    }
  }
  
  # Copy the state matrix to apply screening without modifying the original one
  state_matrix_with_screening <- state_matrix
  
  # Track the year each individual is caught, if at all
  caught_years <- rep(NA, population_size)
  
  # Function to compute screening rates as a function of coverage and interval
  compute_screening_rate <- function(coverage, interval) {
    return(coverage * (1 / interval))
  }
  
  
  
  # Function to generate scenarios
  generate_scenarios <- function(start_age, end_age) {
    # Define parameter ranges
    coverages <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)  # Coverage varies from 0.4 to 1
    intervals_30_49 <- c(1:10)  # Intervals for 30-49 or 40-49
    interval_50_69 <- 2  # Fixed interval for 50-69
    
    # Create dataframe for scenarios
    scenarios <- expand.grid(
      Coverage = coverages,
      Interval_30_49 = intervals_30_49
    )
    
    # Calculate rates for each scenario
    scenarios$Rate_30_49 <- mapply(compute_screening_rate, 
                                   scenarios$Coverage, 
                                   scenarios$Interval_30_49)
    scenarios$Rate_50_69 <- compute_screening_rate(scenarios$Coverage, interval_50_69)
    scenarios$Start_Age <- start_age
    scenarios$End_Age <- end_age
    
    return(scenarios)
  }
  
  # Generate sets of scenarios
  scenarios_40_69 <- generate_scenarios(40,69)
  scenarios_30_69 <- generate_scenarios(30,69)
  scenarios_31_69 <- generate_scenarios(31, 69)
  scenarios_32_69 <- generate_scenarios(32, 69)
  scenarios_33_69 <- generate_scenarios(33, 69)
  scenarios_34_69 <- generate_scenarios(34, 69)
  scenarios_35_69 <- generate_scenarios(35, 69)
  scenarios_36_69 <- generate_scenarios(36, 69)
  scenarios_37_69 <- generate_scenarios(37, 69)
  scenarios_38_69 <- generate_scenarios(38, 69)
  scenarios_39_69 <- generate_scenarios(39, 69)
  # Combine scenarios into a single dataframe
  all_scenarios <- rbind(scenarios_40_69, scenarios_30_69, scenarios_31_69, scenarios_32_69,
                         scenarios_33_69, scenarios_34_69, scenarios_35_69, scenarios_36_69,
                         scenarios_37_69, scenarios_38_69, scenarios_39_69)
  all_scenarios[dim(all_scenarios)[1] + 1,] <- c(0, 1,  0.1759254, 0.1162213,40,69)
  all_scenarios$scenario_id <- as.numeric(rownames(all_scenarios))
  # Revised screening function that uses scenarios dataframe
  screen_individuals <- function(state_matrix, caught_years, age_matrix,  scenarios_df, scenario_id) {
    # Extract scenario-specific parameters
    scenario <- scenarios_df[scenario_id, ]
    coverage <- scenario$Coverage
    interval_30_49 <- scenario$Interval_30_49
    rate_30_49 <- scenario$Rate_30_49
    rate_50_69 <- scenario$Rate_50_69
    start_age <- scenario$Start_Age
    
    # Define a dynamic screening probability function based on the scenario
    screening_prob_function <- function(age, race) {
      if (age >= 30 & age <= 49 & age >= start_age) {
        return(coverage)  # Use the rate for 30-49
      } else if (age >= 50 & age <= 69) {
        return(coverage)  # Use the rate for 50-69
      } else {
        return(0)  # No screening for others}
      }
    }
    
    screening_prob_function_race <- function(age, race) {
      if (age >= 30 & age <= 49 & age >= start_age) {
        return(avg_forty[race])  # Use the rate for 30-49
      } else if (age >= 50 & age <= 69) {
        return(avg_fifties[race])  # Use the rate for 50-69
      } else {
        return(0)  # No screening for others}
      }
    }
    
    
    
    # Initialize caught years and screening mask
    caught_years <- rep(NA, population_size)
    screening_mask <- matrix(FALSE, nrow = length(years), ncol = population_size)
    
    for (year_idx in seq_along(years)) {
      current_year <- years[year_idx]
      current_age <- age_matrix[year_idx, ]
      current_state <- state_matrix[year_idx, ]
      
      # Determine eligibility
      eligible <- current_age >= start_age & 
        current_age <= screening_age_end & 
        is.na(caught_years) & 
        mapply(function(age, start) {
          if (age < 50) {
            return((age - start) %% interval_30_49 == 0 && (age >= start))
          } else {

            return((age - 50) %% 2 == 0)
          }
        }, current_age, start_age)
      
      
      
      eligible_indices <- which(eligible)
      eligible_ages <- current_age[eligible]
      eligible_states <- current_state[eligible]
      eligible_races <- populationdatafemale$racenumeric[eligible_indices]
      eligible_agerace <- data.frame(eligible_ages, eligible_races)
      
      
      
      if(scenario_id==max(all_scenarios$scenario_id)){
        screened <- apply(eligible_agerace, 1, function(row) {
          age <- row["eligible_ages"]
          race <- row["eligible_races"]
          rbinom(1, 1, screening_prob_function_race(as.numeric(age), as.numeric(race))) == 1
        })
      } else {# Apply screening probabilities
        screened <- apply(eligible_agerace, 1, function(row) {
          age <- row["eligible_ages"]
          race <- row["eligible_races"]
          rbinom(1, 1, screening_prob_function(as.numeric(age), as.numeric(race))) == 1
        })}
      
      screened_indices <- eligible_indices[screened]
      
      # Update screening mask
      screening_mask[year_idx, screened_indices] <- TRUE
      
      # Identify not healthy and adjust probabilities
      not_healthy <- eligible_states[screened] != "healthy"
      to_check_indices <- screened_indices[not_healthy]
      to_check_states <- eligible_states[screened][not_healthy]
      to_check_ages <- eligible_ages[screened][not_healthy]
      to_check_races <- populationdatafemale$racenumeric[to_check_indices]
      to_check_agerace <- data.frame(to_check_ages, to_check_races)
      
      if(scenario_id == max(all_scenarios$scenario_id)){
        caught <- sapply(seq_along(to_check_indices), function(idx) {
          state_multiplier <- switch(to_check_states[idx],
                                     "stage1" = avg_selection[1],
                                     "stage2" = avg_selection[2],
                                     "stage3" = avg_selection[3],
                                     "stage4" = avg_selection[4],
                                     0)
          adjusted_prob <-  state_multiplier
          rbinom(1, 1, adjusted_prob) == 1
        })
      } else{
        caught <- sapply(seq_along(to_check_indices), function(idx) {
          state_multiplier <- switch(to_check_states[idx],
                                     "stage1" = avg_selection[1],
                                     "stage2" = avg_selection[2],
                                     "stage3" = avg_selection[3],
                                     "stage4" = avg_selection[4],
                                     0)
          adjusted_prob <- state_multiplier
          rbinom(1, 1, adjusted_prob) == 1
        })
      }
      
      
      # Record the year they are caught
      caught_years[to_check_indices[caught]] <- current_year
    }
    
    return(list(caught_years = caught_years, screening_mask = screening_mask))
  }
  
  # Apply stage freezing and ensure dead states consistency across all scenarios
  
  # Function to apply stage freezing
  apply_stage_freezing <- function(state_matrix_with_screening, caught_years, state_matrix, population_size, years) {
    for (i in 1:population_size) {
      caught_year <- caught_years[i]
      if (!is.na(caught_year)) {
        caught_indices <- which(years >= caught_year)
        for (year_idx in caught_indices) {
          state_matrix_with_screening[year_idx, i] <- state_matrix[caught_indices[1], i]
        }
      }
    }
    return(state_matrix_with_screening)
  }
  
  
  
  # Define stage-condition cost and utility matrix
  stage_condition_matrix <- list(
    stage1 = list(
      cost = c(27954, 6812, 29217/2),
      utility = c(0.731, 0.841, 0.352)
    ),
    stage2 = list(
      cost = c(39437, 9572, 29217/2),
      utility = c(0.731, 0.841, 0.352)
    ),
    stage3 = list(
      cost = c(50186, 11726, 29217/2),
      utility = c(0.599, 0.688, 0.352)
    ),
    stage4 = list(
      cost = c(55133, 25786, 29217/2),
      utility = c(0.352, 0.405, 0.352)
    )
  )
  
  # Function to determine the condition for a given state and year
  determine_condition <- function(year, stage_year, caught_year, death_year) {
    if (!is.na(death_year) && year == death_year - 1) {
      return(3)  # One year before death
    } else if ((!is.na(caught_year) && year <= caught_year) || (is.na(caught_year) && !is.na(death_year))) {
      return(1)  # Caught this year
    } else {
      return(2)  # Other years
    }
  }
  
  
  
  # Extend cost and utility calculations to process caught years for all scenarios
  # Initialize cost and utility matrices for all scenarios
  cost_matrix <- utility_matrix <- matrix(NA, nrow = length(years), ncol = population_size)
  
  # Function to compute costs and utilities for a given scenario
  compute_cost_utility <- function(caught_years_scenario, state_matrix, cost_matrix, utility_matrix) {
    for (i in 1:population_size) {
      death_year <- populationdatafemale$year_of_death[i]
      for (year_idx in seq_along(years)) {
        year <- years[year_idx]
        state <- state_matrix[year_idx, i]  # Base state remains the same across scenarios
        
        if (state %in% names(stage_condition_matrix)) {
          # Determine the condition based on the state and year
          stage_year <- switch(state,
                               stage1 = populationdatafemale$stage1[i],
                               stage2 = populationdatafemale$stage2[i],
                               stage3 = populationdatafemale$stage3[i],
                               stage4 = populationdatafemale$stage4[i]
          )
          caught_year <- caught_years_scenario[i]
          condition <- determine_condition(year, stage_year, caught_year, death_year)
          
          # Assign cost and utility for the stage and condition
          if (!is.na(caught_year) && year >= caught_year){
            cost_matrix[year_idx, i] <- stage_condition_matrix[[state]]$cost[condition]
          } else {cost_matrix[year_idx, i] <- 0}
          
          utility_matrix[year_idx, i] <- stage_condition_matrix[[state]]$utility[condition]
        } else if (state == "healthy") {
          # Use default values for the healthy state
          cost_matrix[year_idx, i] <- 0
          utility_matrix[year_idx, i] <- 1.0
        } else if (state == "dead") {
          cost_matrix[year_idx, i] <- 0
          utility_matrix[year_idx, i] <- 0
        }
      }
    }
    list(cost_matrix = cost_matrix, utility_matrix = utility_matrix)
  }
  
  
  # Define parameters
  discount_rate <- 0.03
  base_year <- 2002
  years <- 1990:2050  # Range of years
  
  # Compute discount factors for all years
  discount_factors <- 1 / (1 + discount_rate)^(years - base_year)
  
  # Apply discount to cost and utility matrices
  apply_discount <- function(cost_matrix) {
    # Create matrices to store discounted values
    discounted_cost_matrix <- cost_matrix
    # Apply discount factors to each year
    for (year_idx in seq_along(years)) {
      discounted_cost_matrix[year_idx, ] <- cost_matrix[year_idx, ] * discount_factors[year_idx]
    }
    
    return(discounted_cost_matrix)
  }# Initialize total cost, total QALYs, and ICER variables
  
  total_cost <- numeric(nrow(all_scenarios))
  total_qalys <- numeric(nrow(all_scenarios))
  chinese_cost <- numeric(nrow(all_scenarios))
  chinese_qalys <- numeric(nrow(all_scenarios))
  malay_cost <- numeric(nrow(all_scenarios))
  malay_qalys <- numeric(nrow(all_scenarios))
  indian_cost <- numeric(nrow(all_scenarios))
  indian_qalys <- numeric(nrow(all_scenarios))
  total_screening <- numeric(nrow(all_scenarios))
  #icer <- numeric(nrow(all_scenarios))
  start <- Sys.time()
  # Loop over each scenario in all_scenarios
  for (scenario_id in 1:(max(all_scenarios$scenario_id) )) {
    print(scenario_id)
    if(scenario_id %% 10 == 0) {
      cat("Scenario:", scenario_id, "\n")
      cat("Elapsed Time:", Sys.time() - start, "\n")
    }
    # Filter the data for the current scenario
    scenario_data <- all_scenarios[all_scenarios$scenario_id == scenario_id, ]
    
    # Generate the results for the current scenario
    result <- screen_individuals(
      state_matrix = state_matrix,
      caught_years = caught_years,
      age_matrix = age_matrix,
      scenarios_df = all_scenarios,
      scenario_id = scenario_id
    )
    
    # Apply the stage freezing for each scenario
    state_matrix_with_screening <- apply_stage_freezing(state_matrix, result$caught_years, state_matrix, population_size, years)
    
    # Ensure "dead" states are consistent
    state_matrix_with_screening[state_matrix == "dead"] <- "dead"
    
    # Compute the cost and utility matrices for the scenario
    scenario_cost_utility <- compute_cost_utility(
      caught_years_scenario = result$caught_years, 
      state_matrix = state_matrix_with_screening, 
      cost_matrix = cost_matrix, 
      utility_matrix = utility_matrix
    )
    
    # Apply discount to the cost matrix
    scenario_cost_utility$cost_matrix <- apply_discount(scenario_cost_utility$cost_matrix)
    
    # Calculate total cost for the scenario
    screening_cost <- sum(colSums(result$screening_mask)) * screening_cost_per_person
    screening_cost_chinese <- sum(colSums(result$screening_mask[,chinese_mask])) * screening_cost_per_person
    screening_cost_malay <- sum(colSums(result$screening_mask[,malay_mask])) * screening_cost_per_person
    screening_cost_indian <- sum(colSums(result$screening_mask[,indian_mask])) * screening_cost_per_person
    total_cost[scenario_id] <- sum(colSums(scenario_cost_utility$cost_matrix)) + screening_cost
    chinese_cost[scenario_id] <- sum(colSums(scenario_cost_utility$cost_matrix[,chinese_mask])) + screening_cost_chinese
    malay_cost[scenario_id] <- sum(colSums(scenario_cost_utility$cost_matrix[,malay_mask])) + screening_cost_malay
    indian_cost[scenario_id] <- sum(colSums(scenario_cost_utility$cost_matrix[,indian_mask])) + screening_cost_indian
    # Calculate total QALYs for the scenario
    total_qalys[scenario_id] <- sum(colSums(scenario_cost_utility$utility_matrix))
    chinese_qalys[scenario_id] <- sum(colSums(scenario_cost_utility$utility_matrix[,chinese_mask]))
    malay_qalys[scenario_id] <- sum(colSums(scenario_cost_utility$utility_matrix[,malay_mask]))
    indian_qalys[scenario_id] <- sum(colSums(scenario_cost_utility$utility_matrix[,indian_mask]))
    total_screening[scenario_id] <- sum(colSums(result$screening_mask))
    
    
    # Remove intermediate objects no longer needed
    # 
    # Create directories if they don't exist
    dir.create(file.path("Cost", scenario_id), recursive = TRUE)
    dir.create(file.path("Utility", scenario_id), recursive = TRUE)
    
    # Define the file paths
    costname <- paste0("Cost/", scenario_id, "/cost_", seed, "_scenario_", scenario_id, ".rds")
    utilityname <- paste0("Utility/", scenario_id, "/util_", seed, "_scenario_", scenario_id, ".rds")
    
    # Save the matrices in the specified folder structure
    saveRDS(scenario_cost_utility$cost_matrix, costname)
    saveRDS(scenario_cost_utility$utility_matrix, utilityname)
    
    rm(result, state_matrix_with_screening, scenario_cost_utility, screening_cost)
    gc()  # Trigger garbage collection to free memory
  }
# Combine results into a data frame
icer <- data.frame(
  Scenario_ID = all_scenarios$scenario_id,
  #ICER = icer,
  QALY = total_qalys,
  Chinese_QALY = chinese_qalys,
  Malay_QALY = malay_qalys,
  Indian_QALY = indian_qalys,
  chinese_cost = chinese_cost,
  malay_cost = malay_cost,
  indian_cost = indian_cost,
  Cost = total_cost,
  Screening = total_screening
)
##recalculate icer
for (scenario_id in 1: max(all_scenarios$scenario_id)){
  if (scenario_id != max(all_scenarios$scenario_id)) {
    delta_cost <- total_cost[scenario_id] - total_cost[max(all_scenarios$scenario_id)]
    delta_qalys <- total_qalys[scenario_id] - total_qalys[max(all_scenarios$scenario_id)]
    icer$ICER[scenario_id] <- delta_cost / delta_qalys
  } else {
    icer$ICER[scenario_id] <- NA  # Baseline scenario
  }
}
icer <- cbind(icer, all_scenarios)
filename <- paste0("icer_", seed, ".rds")

# Save the data frame to an .rds file
saveRDS(icer, filename)
return(filename)  # Return the filename for confirmation
}

# Generate 500 unique random seeds
set.seed(1234)  # Ensure reproducibility of seed generation
unique_seeds <- sample(1:1e6, 500, replace = FALSE)

# Set up parallel environment
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, varlist = c("bootstrap_function"))

pboptions(type = "txt")  # Type 3 is a progress bar shown in the terminal
# Run the bootstraps in parallel
bootstrap_results <- pblapply(unique_seeds, bootstrap_function, cl = cl)

# Stop the cluster
stopCluster(cl)

# Print confirmation
cat("Bootstrap results saved to files:\n", paste(bootstrap_results, collapse = "\n"), "\n")
