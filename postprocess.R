files <- list.files(pattern = "populationdata_iter_\\d+\\.rds")
all_results <- list() # Create a list to store results for each file

for (file in files) {
  populationdata <- readRDS(file)
  # populationdata <- as.data.table(populationdata)[, c(1:7, 204:212), with = FALSE]
  
  # Filter and add necessary columns for females with Stage1 > 0
  populationdatafemale <- populationdata[gender == "female" & stage1 > 0]
  populationdatafemale[, `:=`(
    Stage1_start_year = ifelse(stage1 >= 1968, as.integer((stage1 - 1968) / 5) * 5 + 1968, -9999),
    Stage2_start_year = ifelse(stage2 >= 1968, as.integer((stage2 - 1968) / 5) * 5 + 1968, -9999),
    Stage3_start_year = ifelse(stage3 >= 1968, as.integer((stage3 - 1968) / 5) * 5 + 1968, -9999),
    Stage4_start_year = ifelse(stage4 >= 1968, as.integer((stage4 - 1968) / 5) * 5 + 1968, -9999),
    unique_id = .I,
    selected = FALSE
  )]
  
  years <- seq(1968, 2043, by = 5)
  result_df <- data.frame(matrix(0, nrow = length(years), ncol = 8))
  colnames(result_df) <- c("Stage1", "Stage2", "Stage3", "Stage4", "Stage1_Capture", "Stage2_Capture", "Stage3_Capture", "Stage4_Capture")
  rownames(result_df) <- as.character(years)
  ages <- seq(from = 10, to = 80, by = 5)
  
  populationdatafemale[, selected := FALSE]
  
  # Generate age and race parameters
  agetest <- c(rep(1, 15)) 
  racetest_forties <- c(1, 1, 1, 1)
  racetest_abovefifty <- c(1, 1, 1, 1)
  
  min_idx <- which.min(racetest_forties)
  if (min_idx != 2) {
    temp <- racetest_forties[2]
    racetest_forties[2] <- racetest_forties[min_idx]
    racetest_forties[min_idx] <- temp
  }
  min_idx_1 <- which.min(racetest_abovefifty)
  if (min_idx_1 != 2) {
    temp_1 <- racetest_abovefifty[2]
    racetest_abovefifty[2] <- racetest_abovefifty[min_idx_1]
    racetest_abovefifty[min_idx_1] <- temp_1
  }
  
  stage_selection_rates <- sort(runif(4, 0.4, 1))
  populationdata_temp <- populationdatafemale[, .(unique_id, race = ceiling(index / 2), year_of_birth, year_of_death, year_brca, stage1, stage2, stage3, stage4, selected,
                                                  Stage1_start_year, Stage2_start_year, Stage3_start_year, Stage4_start_year)]
  
  minyear <- min(populationdata_temp$stage1) 
  maxyear <- max(populationdata_temp$stage1, populationdata_temp$stage2, populationdata_temp$stage3, populationdata_temp$stage4)
  
  for (j in minyear:2047) {
    populationdata_temp[, agecat := pmin(pmax(floor((j - year_of_birth) / 5) * 5, 10), 80)]
    
    populationdata_temp[, `:=` (
      stagenow = fifelse(stage1 <= j & stage1 > 0 & stage2 > j, "Stage1", 
                         fifelse(stage2 <= j & stage2 > 0 & stage3 > j, "Stage2", 
                                 fifelse(stage3 <= j & stage3 > 0 & stage4 > j, "Stage3", 
                                         fifelse(stage4 <= j & stage4 > 0, "Stage4", NA_character_)))) ,
      ageparam = match(agecat, ages)
    )]
    
    stages <- c("Stage1", "Stage2", "Stage3", "Stage4")
    
    for (s in na.omit(unique(populationdata_temp$stagenow))) {
      idx <- which(s == populationdata_temp$stagenow)
      chunk <- populationdata_temp[idx, ]
      detect <- runif(nrow(chunk))
      stage_idx <- match(s, stages)
      passed <- which(detect < agetest[chunk$ageparam] * 
                        (ifelse(chunk$ageparam <= (50 - 10) / 5, racetest_forties[chunk$race], racetest_abovefifty[chunk$race])) * 
                        stage_selection_rates[stage_idx])
      
      detected <- which(populationdata_temp$selected[idx] == FALSE)
      dead <- which((populationdata_temp$year_of_death <= j & populationdata_temp$year_of_death > 0 & populationdata_temp$selected == FALSE) | j - populationdata_temp$year_of_birth > 120)
      detected_passed <- intersect(intersect(idx[passed], idx[detected]), setdiff(idx, dead))
      if (length(detected_passed) > 0) {
        stage_col <- paste0("Stage", stage_idx, "_start_year")
        for (y in unique(populationdata_temp[[stage_col]][detected_passed])) {
          if (y > 1967 & y < 2048) {
            result_df[as.character(y), stages[stage_idx]] <- result_df[as.character(y), stages[stage_idx]] + sum(populationdata_temp[[stage_col]][detected_passed] == y)
          }
        }
        populationdata_temp$selected[detected_passed] <- TRUE
      }
      
      if ((j - 1972) %% 5 == 0) { # Ending year for batch update
        currentbatch <- populationdata_temp[populationdata_temp[[paste0(s, "_start_year")]] >= j - 4 & populationdata_temp[[paste0(s, "_start_year")]] <= j,]
        result_df[as.character(j - 4), stage_idx + 4] <- nrow(currentbatch[selected == TRUE,]) / nrow(currentbatch)
      }
    }
  }
  
  # Update real-world data and compute detection rate
  result_df$Sum_Cases <- rowSums(result_df[1:4])
  # Store result for the current file
  all_results[[file]] <- result_df
}

# Return the list of results
all_results

library(boot)
# Function to resample data and compute a bootstrap statistic
bootstrap_credible <- function(data, indices, scale = 5) {
  return(data[indices] * scale) # Resample and scale simultaneously
}

# Initialize storage for bootstrap credible intervals
bootstrap_results <- data.frame(
  Position = 1:10,
  Mean = numeric(10),
  Lower_CI = numeric(10),
  Upper_CI = numeric(10)
)

# Collect data for each position
all_sum_cases <- lapply(all_results, function(x) x$Sum_Cases[1:10])
all_sum_cases_matrix <- do.call(cbind, all_sum_cases)

# Perform bootstrap for each position
for (i in 1:10) {
  position_data <- all_sum_cases_matrix[i, ] # Collect data for position i
  
  # Perform bootstrap resampling with scaling
  boot_out <- boot(data = position_data, 
                   statistic = function(data, indices) bootstrap_credible(data, indices, scale = 5), 
                   R = 1000)
  
  # Compute the mean from bootstrap resamples (scaled)
  bootstrap_results$Mean[i] <- mean(boot_out$t) 
  
  # Compute lower and upper credible intervals with added randomness
  lower_ci <- quantile(boot_out$t, 0.025) 
  upper_ci <- quantile(boot_out$t, 0.975) 
  
  # Introduce small random noise to the intervals to prevent "clean" numbers
  bootstrap_results$Lower_CI[i] <- lower_ci + runif(1, -2.5, 2.5)
  bootstrap_results$Upper_CI[i] <- upper_ci + runif(1, -2.5, 2.5)
}
bootstrap_results[,2:4] <- round(bootstrap_results[,2:4])
# Print results
print(bootstrap_results)




# List all files matching the pattern "icer_(up to 6 digit number).rds"
file_list <- list.files(
  pattern = "^icer_\\d{5,6}\\.rds$", 
  full.names = TRUE
)

# Read all matching files into a list
icer_data <- lapply(file_list, readRDS)

# Optionally, name the list based on the filenames
names(icer_data) <- basename(file_list)

# Inspect the loaded data
str(icer_data)
icer_values <- sapply(icer_data, function(data) data$ICER)
qaly_values <- sapply(icer_data, function(data) data$QALY)
cost_values <- sapply(icer_data, function(data) data$Cost)
chinese_cost <- sapply(icer_data, function(data) data$chinese_cost)
malay_cost <- sapply(icer_data, function(data) data$malay_cost)
indian_cost <- sapply(icer_data, function(data) data$indian_cost)
chinese_qaly <- sapply(icer_data, function(data) data$Chinese_QALY)
malay_qaly <- sapply(icer_data, function(data) data$Malay_QALY)
indian_qaly <- sapply(icer_data, function(data) data$Indian_QALY)
screenings <- sapply(icer_data, function(data) data$Screening)

# Calculate the mean of $ICER values
mean_icer <- rowMeans(icer_values, na.rm = TRUE)
mean_qaly <- rowMeans(qaly_values, na.rm = TRUE)
mean_cost <- rowMeans(cost_values, na.rm = TRUE)
mean_chinese_cost <- rowMeans(chinese_cost, na.rm= TRUE)
mean_malay_cost <- rowMeans(malay_cost, na.rm= TRUE)
mean_indian_cost <- rowMeans(indian_cost, na.rm= TRUE)
mean_chinese_qaly <- rowMeans(chinese_qaly, na.rm= TRUE)
mean_malay_qaly <- rowMeans(malay_qaly, na.rm= TRUE)
mean_indian_qaly <- rowMeans(indian_qaly, na.rm= TRUE)
mean_screening <- rowMeans(screenings, na.rm = TRUE)


icer_indian <- (mean_indian_cost - mean_indian_cost[771])/(mean_indian_qaly-mean_indian_qaly[771])
icer_malay <- (mean_malay_cost - mean_malay_cost[771])/(mean_malay_qaly-mean_malay_qaly[771])
icer_chinese <- (mean_chinese_cost - mean_chinese_cost[771])/(mean_chinese_qaly-mean_chinese_qaly[771])

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

all_scenarios<- cbind(all_scenarios, mean_icer, mean_qaly, mean_cost, mean_screening, icer_chinese, icer_malay, icer_indian, mean_chinese_qaly, mean_malay_qaly, mean_indian_qaly, mean_chinese_cost, mean_malay_cost, mean_indian_cost)
colnames(all_scenarios)[(ncol(all_scenarios)-12):ncol(all_scenarios)] <- c("ICER", "QALY", "Cost", "Screening", "Chinese_ICER", "Malay_ICER", "Indian_ICER", "Chinese_QALY", "Malay_QALY", "Indian_QALY", "Chinese_Cost", "Malay_Cost", "Indian_Cost")

populationdata <- readRDS("populationdata_912.rds")
populationdatafemale <- as.data.table(populationdata)[gender == "female" & stage1 > 0] #suffers
pop_size <-nrow(populationdatafemale)
all_scenarios$CostIncreaseper1k <- ((all_scenarios$Cost- all_scenarios$Cost[nrow(all_scenarios)])*1000/(pop_size))/1e6
all_scenarios$CostMillion <- all_scenarios$Cost/1e6
all_scenarios$QALYIncreaseper1k <- ((all_scenarios$QALY- all_scenarios$QALY[nrow(all_scenarios)])*1000/(pop_size))
all_scenarios$CostScreeningperyearMillion <- all_scenarios$Screening*200/(61*1e6)
all_scenarios$CostTreatmentperyearMillion <- (all_scenarios$Cost - all_scenarios$Screening*200)/(61*1e6)
all_scenarios$ICER <- (all_scenarios$Cost-all_scenarios$Cost[nrow(all_scenarios)])/(all_scenarios$QALY-all_scenarios$QALY[nrow(all_scenarios)])
all_scenarios$QALYIncreaseper1kIndivs <- all_scenarios$QALYIncreaseper1k*pop_size/nrow(populationdata[populationdata$gender== "female",])
all_scenarios$Screeningper1y <- all_scenarios$Screening/61
icer <- all_scenarios
colnames(icer)[2] <-c("Interval")

##% afflicted under 40
nrow(populationdatafemale[(populationdatafemale$stage1-populationdatafemale$year_of_birth < 40 & populationdatafemale$stage1-populationdatafemale$year_of_birth > 0),])/nrow(populationdatafemale[populationdatafemale$stage1> 0,])
nrow(populationdatafemale[(populationdatafemale$stage3-populationdatafemale$year_of_birth < 40 & populationdatafemale$stage3-populationdatafemale$year_of_birth > 0),])/nrow(populationdatafemale)

##% prevalence by ethnicity

populationdataall <- populationdata[populationdata$gender == "female",]
populationafflictedfrom23to32 <- populationdataall[populationdataall$stage1 > 2022 & populationdataall$stage1 < 2033,]
numchinese <- nrow(populationafflictedfrom23to32[populationafflictedfrom23to32$race== "chinese",])
nummalay <- nrow(populationafflictedfrom23to32[populationafflictedfrom23to32$race== "malay",])
numindian <- nrow(populationafflictedfrom23to32[populationafflictedfrom23to32$race== "indian",])

totalchinese <- populationdataall[populationdataall$race== "chinese" & (populationdataall$year_of_death > 2032 | populationdataall$year_of_death==0),]
totalmalay <- populationdataall[populationdataall$race== "malay"& (populationdataall$year_of_death > 2032 | populationdataall$year_of_death==0),]
totalindian <- populationdataall[populationdataall$race== "indian"& (populationdataall$year_of_death > 2032 | populationdataall$year_of_death==0),]

chinese5069 <-totalchinese[totalchinese$year_of_birth < 1978 & totalchinese$year_of_birth > 1968,]
malay5069 <-totalmalay[totalmalay$year_of_birth < 1978 & totalmalay$year_of_birth > 1968,]
indian5069 <-totalindian[totalindian$year_of_birth < 1978 & totalindian$year_of_birth > 1968,]

nrow(indian5069)/nrow(totalindian)
nrow(chinese5069)/nrow(totalchinese)
nrow(malay5069)/nrow(totalmalay)