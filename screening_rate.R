library(data.table)
library(coda)
library(ggplot2)
library(gridExtra)
library(patchwork)

#Load and filter data
populationdata <- readRDS("populationdata_912.rds")
populationdatafemale <- as.data.table(populationdata)[gender == "female" & stage1 > 0]

# Add necessary columns to populationdatafemale
populationdatafemale[, `:=` (
  Stage1_start_year = ifelse(stage1 >= 1968, as.integer((stage1 - 1968) / 5) * 5 + 1968, -9999),
  Stage2_start_year = ifelse(stage2 >= 1968, as.integer((stage2 - 1968) / 5) * 5 + 1968, -9999),
  Stage3_start_year = ifelse(stage3 >= 1968, as.integer((stage3 - 1968) / 5) * 5 + 1968, -9999),
  Stage4_start_year = ifelse(stage4 >= 1968, as.integer((stage4 - 1968) / 5) * 5 + 1968, -9999),
  unique_id = .I,  # row number as unique ID
  selected = FALSE  # New column to track selection status
)]

# # Function for one iteration of MCMC with acceptance/rejection
# run_mcmc_iteration <- function(chain_id) {
#   years <- seq(1968, 2043, by = 5)
#   result_df_1 <- data.frame(matrix(0, nrow = length(years), ncol = 8))
#   colnames(result_df_1) <- c("Stage1", "Stage2", "Stage3", "Stage4", "Stage1_Capture", "Stage2_Capture", "Stage3_Capture", "Stage4_Capture")
#   rownames(result_df_1) <- as.character(years)
#   ages <- seq(from = 10, to = 80, by = 5)
#   
#   populationdatafemale[, selected := FALSE]
#   
#   # Generate age and race parameters
#   agetest <- c(rep(0,6), rep(1,2), rep(0.5,4), rep(0,3)) 
#   racetest_forties <- c(runif(1, 0, 0.4), runif(1, 0, 0.3), runif(2, 0, 0.4))
#   racetest_abovefifty <- c(runif(1, 0.1, 0.5), runif(1, 0, 0.4), runif(2, 0.1, 0.5))
#   
#   min_idx <- which.min(racetest_forties)
#   if (min_idx != 2) {
#     temp <- racetest_forties[2]
#     racetest_forties[2] <- racetest_forties[min_idx]
#     racetest_forties[min_idx] <- temp
#   }
#   min_idx_1 <- which.min(racetest_abovefifty)
#   if (min_idx_1 != 2) {
#     temp_1 <- racetest_abovefifty[2]
#     racetest_abovefifty[2] <- racetest_abovefifty[min_idx_1]
#     racetest_abovefifty[min_idx_1] <- temp_1
#   }
#   #stage_selection_rates <- runif(4, 0.4, 1)
#   stage_selection_rates <- sort(runif(4, 0.4, 1))
#   populationdata_temp <- populationdatafemale[, .(unique_id, race = ceiling(index / 2), year_of_birth, year_of_death, year_brca, stage1, stage2, stage3, stage4, selected,
#                                                   Stage1_start_year, Stage2_start_year, Stage3_start_year, Stage4_start_year)]
#   
#   minyear <- min(populationdata_temp$stage1) 
#   maxyear <- max(populationdata_temp$stage1, populationdata_temp$stage2, populationdata_temp$stage3, populationdata_temp$stage4)
#   
#   for (j in minyear:2047) {
#     populationdata_temp[, agecat := pmin(pmax(floor((j - year_of_birth) / 5) * 5, 10), 80)]
#     
#     populationdata_temp[, `:=` (
#       stagenow = fifelse(stage1 <= j & stage1 > 0 & stage2 > j, "Stage1", 
#                          fifelse(stage2 <= j & stage2 > 0 & stage3 > j, "Stage2", 
#                                  fifelse(stage3 <= j & stage3 > 0 & stage4 > j, "Stage3", 
#                                          fifelse(stage4 <= j& stage4 > 0, "Stage4", NA_character_)))) ,
#       ageparam = match(agecat, ages)
#     )]
#     
#     stages <- c("Stage1", "Stage2", "Stage3", "Stage4")
#     
#     for (s in na.omit(unique(populationdata_temp$stagenow))) {
#       idx <- which(s == populationdata_temp$stagenow)
#       chunk <- populationdata_temp[idx, ]
#       detect <- runif(nrow(chunk))
#       stage_idx <- match(s, stages)
#       passed <- which(detect < agetest[chunk$ageparam] * 
#                         (ifelse(chunk$ageparam <= (50 - 10) / 5, racetest_forties[chunk$race], racetest_abovefifty[chunk$race])) * 
#                         stage_selection_rates[stage_idx])
#       
#       detected <- which(populationdata_temp$selected[idx] == FALSE)
#       #detected_passed <- intersect(idx[passed], detected)
#       dead <- which((populationdata_temp$year_of_death <= j & populationdata_temp$year_of_death > 0 & populationdata_temp$selected == FALSE) | j - populationdata_temp$year_of_birth > 120)
#       detected_passed <- intersect(intersect(idx[passed], idx[detected]),setdiff(idx, dead))
#       if (length(detected_passed) > 0) {
#         stage_col <- paste0("Stage", stage_idx, "_start_year")
#         for (y in unique(populationdata_temp[[stage_col]][detected_passed])) {
#           if (y > 1967 & y < 2048) {
#             result_df_1[as.character(y), stages[stage_idx]] <- result_df_1[as.character(y), stages[stage_idx]] + sum(populationdata_temp[[stage_col]][detected_passed] == y)
#           }
#         }
#         populationdata_temp$selected[detected_passed] <- TRUE
#         #populationdata_temp$selected[dead] <- TRUE
#         #populationdata_temp$selected[dead] <- FALSE
#       }
#       
#       if ((j-1972) %% 5 == 0) { # Ending year for batch update
#         currentbatch <- populationdata_temp[populationdata_temp[[paste0(s, "_start_year")]] >= j - 4 & populationdata_temp[[paste0(s, "_start_year")]] <= j,]
#         result_df_1[as.character(j-4), stage_idx + 4] <- nrow(currentbatch[selected == TRUE,]) / nrow(currentbatch)
#       }
#     }
#   }
#   
#   # Update real-world data and compute detection rate
#   result_df_1$Sum_Cases <- rowSums(result_df_1[1:4])
#   real_world_data <- c(672, 863, 1237, 1739, 2635, 3606, 5582, 6860, 8566, 10868)
#   splinefit <- spline(years[1:10], real_world_data, xout = c(2018, 2023))
#   real_world_data[11:12] <- round(splinefit$y)
#   result_df_1 <- head(result_df_1, 12)
#   result_df_1$Actual <- real_world_data
#   result_df_1$Proportion_Caught <- result_df_1$Actual * 100 / result_df_1$Sum_Cases
#   
#   detection_rate <- result_df_1$Proportion_Caught[10]
#   target_detection_rate <- 100
#   
#   # Acceptance criteria
#   accepted <- abs((detection_rate - target_detection_rate) / target_detection_rate) < 0.1 &
#     abs(rowSums(result_df_1[1:2])[10] / result_df_1$Sum_Cases[10] - 0.7) < 0.2 &
#     abs(rowSums(result_df_1[3:4])[10] / result_df_1$Sum_Cases[10] - 0.3) < 0.2
#   
#   return(list(result_df = result_df_1, accepted = accepted, agetest = agetest, racetest_forties = racetest_forties, racetest_abovefifty = racetest_abovefifty, stage_selection_rates = stage_selection_rates))
# }
# 
# # Run 1 chains
# num_iterations <- 15
# num_chains <- 1
# chain_results_accepted <- vector("list", num_chains)
# chain_results <- vector("list", num_chains)
# 
# for (chain in 1:num_chains) {
#   cat("Running Chain", chain, "\n")
#   mcmc_results <- list()
#   acceptance_counts <- 0
#   accepted_samples <- list()
#   start <- Sys.time()
#   
#   for (iter in 1:num_iterations) {
#     if (iter %% 50 == 0) {
#       cat("Iteration:", iter, "\n")
#       cat("Elapsed Time:", Sys.time() - start, "\n")
#       cat("Acceptance Rate:", acceptance_counts / iter, "\n")
#     }
#     
#     result <- run_mcmc_iteration(chain_id = chain)
#     mcmc_results[[iter]] <- result
#     if (result$accepted) {
#       acceptance_counts <- acceptance_counts + 1
#       accepted_samples[[acceptance_counts]] <- result
#     }
#     
#   }
# 
#   accepted_mcmc_results <- lapply(accepted_samples, function(x) x$result_df)
#   chain_results_accepted[[chain]] <- accepted_samples
#   chain_results[[chain]] <- mcmc_results
#   saveRDS(chain_results_accepted[[chain]], paste0("accepted_results_chain_", chain, "script1.rds"))
#   saveRDS(chain_results[[chain]], paste0("results_chain_", chain, "script1.rds"))
# }
# 
# saveRDS(chain_results, "all_chains_results.rds")
# saveRDS(chain_results_accepted, "all_chains_accepted_results.rds")
# print("Done!")
# #chains <- readRDS("all_chains_accepted_results.rds")
chain_1 <- readRDS("results_chain_1script1.rds")
chain_2 <-  readRDS("results_chain_1script2.rds")
chain_3 <-  readRDS("results_chain_1script3.rds")
chain_4 <- readRDS("results_chain_1script4.rds")

chain_1_accepted <- readRDS("accepted_results_chain_1script1.rds")
chain_2_accepted <- readRDS("accepted_results_chain_1script2.rds")
chain_3_accepted <- readRDS("accepted_results_chain_1script3.rds")
chain_4_accepted <- readRDS("accepted_results_chain_1script4.rds")
chain_results <- list(chain_1, chain_2, chain_3, chain_4)


# Function to extract a specific variable across all iterations from a single chain
extract_chain_variable <- function(chain, var_name) {
  do.call(rbind, lapply(chain, function(sample) sample[[var_name]]))
}

# Extract the variables for each chain
racetest_forties_chain_1 <- extract_chain_variable(chain_1, "racetest_forties")
racetest_forties_chain_2 <- extract_chain_variable(chain_2, "racetest_forties")
racetest_forties_chain_3 <- extract_chain_variable(chain_3, "racetest_forties")
racetest_forties_chain_4 <- extract_chain_variable(chain_4, "racetest_forties")

racetest_abovefifty_chain_1 <- extract_chain_variable(chain_1, "racetest_abovefifty")
racetest_abovefifty_chain_2 <- extract_chain_variable(chain_2, "racetest_abovefifty")
racetest_abovefifty_chain_3 <- extract_chain_variable(chain_3, "racetest_abovefifty")
racetest_abovefifty_chain_4 <- extract_chain_variable(chain_4, "racetest_abovefifty")

stage_selection_rates_chain_1 <- extract_chain_variable(chain_1, "stage_selection_rates")
stage_selection_rates_chain_2 <- extract_chain_variable(chain_2, "stage_selection_rates")
stage_selection_rates_chain_3 <- extract_chain_variable(chain_3, "stage_selection_rates")
stage_selection_rates_chain_4 <- extract_chain_variable(chain_4, "stage_selection_rates")

# Convert each chain's data into mcmc objects
racetest_forties_mcmc <- mcmc.list(mcmc(racetest_forties_chain_1),
                                   mcmc(racetest_forties_chain_2),
                                   mcmc(racetest_forties_chain_3),
                                   mcmc(racetest_forties_chain_4))

racetest_abovefifty_mcmc <- mcmc.list(mcmc(racetest_abovefifty_chain_1),
                                      mcmc(racetest_abovefifty_chain_2),
                                      mcmc(racetest_abovefifty_chain_3),
                                      mcmc(racetest_abovefifty_chain_4))

stage_selection_rates_mcmc <- mcmc.list(mcmc(stage_selection_rates_chain_1),
                                        mcmc(stage_selection_rates_chain_2),
                                        mcmc(stage_selection_rates_chain_3),
                                        mcmc(stage_selection_rates_chain_4))

# Define colors and labels
chain_colors <- c("red", "blue", "green", "yellow")
chain_labels <- c("Chain 1", "Chain 2", "Chain 3", "Chain 4")
race_labels <- c("Chinese", "Malay", "Indian", "Others")
stage_labels <- c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
# Function to plot with legends for each race
plot_with_legend <- function(mcmc_obj, var_name, race) {
  par(pin = c(4, 1.5))  # Set the aspect ratio of the plot area
  traceplot(mcmc_obj, main = paste("Traceplot for", var_name, "-", race), col = chain_colors)
  legend("topright", legend = chain_labels, col = chain_colors, lty = 1, cex = 0.8, 
         xpd = TRUE, inset = c(-0.3, 0))  # Position legend outside plot area
}

# Save traceplots with legends for each race
png("traceplots_racetest_forties_12.png", width = 800, height = 800)  # Set output file
par(mar = c(5, 4, 4, 3) + 0.1, mfrow = c(2, 1))  # Two plots per page

# racetest_forties traceplots
plot_with_legend(racetest_forties_mcmc[,1], "racetest_forties", race_labels[1])
plot_with_legend(racetest_forties_mcmc[,2], "racetest_forties", race_labels[2])

dev.off()

# New page for the next two races
png("traceplots_racetest_forties_34.png", width = 800, height = 800)
par(mar = c(5, 4, 4, 3) + 0.1, mfrow = c(2, 1))

plot_with_legend(racetest_forties_mcmc[,3], "racetest_forties", race_labels[3])
plot_with_legend(racetest_forties_mcmc[,4], "racetest_forties", race_labels[4])

dev.off()  # Close the graphics device

# Save traceplots for racetest_abovefifty
png("traceplots_racetest_abovefifty_12.png", width = 800, height = 800)  # Set output file
par(mar = c(5, 4, 4, 3) + 0.1, mfrow = c(2, 1))

# racetest_abovefifty traceplots
plot_with_legend(racetest_abovefifty_mcmc[,1], "racetest_abovefifty", race_labels[1])
plot_with_legend(racetest_abovefifty_mcmc[,2], "racetest_abovefifty", race_labels[2])

dev.off()

# New page for the next two races
png("traceplots_racetest_abovefifty_34.png", width = 800, height = 800)
par(mar = c(5, 4, 4, 3) + 0.1, mfrow = c(2, 1))

plot_with_legend(racetest_abovefifty_mcmc[,3], "racetest_abovefifty", race_labels[3])
plot_with_legend(racetest_abovefifty_mcmc[,4], "racetest_abovefifty", race_labels[4])

dev.off()  # Close the graphics device

# Save traceplots for stage_selection_rates
png("traceplots_stage_selection_rates_12.png", width = 800, height = 800)  # Set output file
par(mar = c(5, 4, 4, 3) + 0.1, mfrow = c(2, 1))

plot_with_legend(stage_selection_rates_mcmc[,1], "stage_selection_rates", stage_labels[1])
plot_with_legend(stage_selection_rates_mcmc[,2], "stage_selection_rates", stage_labels[2])

dev.off()

# New page for the next two races
png("traceplots_stage_selection_rates_34.png", width = 800, height = 800) 
par(mar = c(5, 4, 4, 3) + 0.1, mfrow = c(2, 1))

plot_with_legend(stage_selection_rates_mcmc[,3], "stage_selection_rates", stage_labels[3])
plot_with_legend(stage_selection_rates_mcmc[,4], "stage_selection_rates", stage_labels[4])

dev.off()  # Close the graphics device



# Save density plots for racetest_forties
png("density_racetest_forties.png", width = 800, height = 800)  # Set output file
# Reset the plotting layout to 2x2
par(mfrow = c(2,2))
for (i in 1:4) {
  densplot(racetest_forties_mcmc[, i],
           main = paste("Density for racetest_forties -",  race_labels[i]),
           xlab = "racetest_forties",
           col = chain_colors[i],
           lwd = 2)
}
dev.off()  # Close the graphics device

# Save density plots for racetest_abovefifty
png("density_racetest_abovefifty.png", width = 800, height = 800)  # Set output file
par(mfrow = c(2,2))
for (i in 1:4) {
  densplot(racetest_abovefifty_mcmc[, i],
           main = paste("Density for racetest_abovefifty -",  race_labels[i]),
           xlab = "racetest_abovefifty",
           col = chain_colors[i],
           lwd = 2)
}
dev.off()  # Close the graphics device

# Save density plots for stage_selection_rates
png("density_stage_selection_rates.png", width = 800, height = 800)  # Set output file
par(mfrow = c(2,2))
for (i in 1:4) {
  densplot(stage_selection_rates_mcmc[, i],
           main = paste("Density for stage_selection_rates -",  stage_labels[i]),
           xlab = "stage_selection_rates",
           col = chain_colors[i],
           lwd = 2)
}
dev.off()  # Close the graphics device

# Reset layout to default
par(mfrow = c(1, 1))

# Define the quantiles for the CI, here using 2.5% and 97.5% for a 95% CI
ci_levels <- c(0.025, 0.975)

accepted_racetest_forties_chain_1 <- extract_chain_variable(chain_1_accepted, "racetest_forties")
accepted_racetest_forties_chain_2 <- extract_chain_variable(chain_2_accepted, "racetest_forties")
accepted_racetest_forties_chain_3 <- extract_chain_variable(chain_3_accepted, "racetest_forties")
accepted_racetest_forties_chain_4 <- extract_chain_variable(chain_4_accepted, "racetest_forties")

accepted_racetest_abovefifty_chain_1 <- extract_chain_variable(chain_1_accepted, "racetest_abovefifty")
accepted_racetest_abovefifty_chain_2 <- extract_chain_variable(chain_2_accepted, "racetest_abovefifty")
accepted_racetest_abovefifty_chain_3 <- extract_chain_variable(chain_3_accepted, "racetest_abovefifty")
accepted_racetest_abovefifty_chain_4 <- extract_chain_variable(chain_4_accepted, "racetest_abovefifty")

accepted_stage_selection_rates_chain_1 <- extract_chain_variable(chain_1_accepted, "stage_selection_rates")
accepted_stage_selection_rates_chain_2 <- extract_chain_variable(chain_2_accepted, "stage_selection_rates")
accepted_stage_selection_rates_chain_3 <- extract_chain_variable(chain_3_accepted, "stage_selection_rates")
accepted_stage_selection_rates_chain_4 <- extract_chain_variable(chain_4_accepted, "stage_selection_rates")



accepted_racetest_forties_chain_1_mcmc <- mcmc(accepted_racetest_forties_chain_1)
accepted_racetest_forties_chain_2_mcmc <- mcmc(accepted_racetest_forties_chain_2)
accepted_racetest_forties_chain_3_mcmc <- mcmc(accepted_racetest_forties_chain_3)
accepted_racetest_forties_chain_4_mcmc <- mcmc(accepted_racetest_forties_chain_4)

accepted_racetest_abovefifty_chain_1_mcmc <- mcmc(accepted_racetest_abovefifty_chain_1)
accepted_racetest_abovefifty_chain_2_mcmc <- mcmc(accepted_racetest_abovefifty_chain_2)
accepted_racetest_abovefifty_chain_3_mcmc <- mcmc(accepted_racetest_abovefifty_chain_3)
accepted_racetest_abovefifty_chain_4_mcmc <- mcmc(accepted_racetest_abovefifty_chain_4)

accepted_stage_selection_rates_chain_1_mcmc <- mcmc(accepted_stage_selection_rates_chain_1)
accepted_stage_selection_rates_chain_2_mcmc <- mcmc(accepted_stage_selection_rates_chain_2)
accepted_stage_selection_rates_chain_3_mcmc <- mcmc(accepted_stage_selection_rates_chain_3)
accepted_stage_selection_rates_chain_4_mcmc <- mcmc(accepted_stage_selection_rates_chain_4)
# Combine chains for the "above fifty" age group
accepted_racetest_abovefifty_all <- rbind(
  accepted_racetest_abovefifty_chain_1, 
  accepted_racetest_abovefifty_chain_2, 
  accepted_racetest_abovefifty_chain_3, 
  accepted_racetest_abovefifty_chain_4
)
ci_levels <- c(0.025, 0.975)
# Calculate mean
avg_fifties <- colMeans(accepted_racetest_abovefifty_all)

# Calculate 95% CI
lower_bound_fifties <- apply(accepted_racetest_abovefifty_all, 2, quantile, probs = 0.25)
upper_bound_fifties <- apply(accepted_racetest_abovefifty_all, 2, quantile, probs = 0.75)

cat("Avg Testing Rate for > 50:", avg_fifties, "\n") 

# Combine chains for the "forties" age group
accepted_racetest_forties_all <- rbind(
  accepted_racetest_forties_chain_1, 
  accepted_racetest_forties_chain_2, 
  accepted_racetest_forties_chain_3, 
  accepted_racetest_forties_chain_4
)

# Calculate mean 
avg_forty <- colMeans(accepted_racetest_forties_all)

# Calculate 95% CI
lower_bound_forty <- apply(accepted_racetest_forties_all, 2, quantile, probs = 0.25)
upper_bound_forty <- apply(accepted_racetest_forties_all, 2, quantile, probs = 0.75)

cat("Avg Testing Rate for 40s:", avg_forty, "\n") 

# Combine chains for stage selection rates
accepted_stage_selection_rates_all <- rbind(
  accepted_stage_selection_rates_chain_1, 
  accepted_stage_selection_rates_chain_2, 
  accepted_stage_selection_rates_chain_3, 
  accepted_stage_selection_rates_chain_4
)

# Calculate mean and standard error
avg_selection <- colMeans(accepted_stage_selection_rates_all)


# Calculate 95% CI

lower_bound_selection <- apply(accepted_stage_selection_rates_all, 2, quantile, probs = 0.25)
upper_bound_selection <- apply(accepted_stage_selection_rates_all, 2, quantile, probs = 0.75)
cat("Avg Stage Selection:", avg_selection, "\n") 

#
#
# # # Filter the mcmc_results for accepted iterations
# accepted_mcmc_results <- lapply(accepted_samples, function(x) x$result_df)
# 
# # Extract column 7 from each accepted result and store in a list
# col11_accepted <- lapply(accepted_mcmc_results, function(df) df[, 11])
# # Combine all column 7 vectors row-wise into a matrix
# col11_matrix <- do.call(cbind, col11_accepted)
# # Calculate the mean of column 7 across iterations for each row
# mean_col11 <- rowMeans(col11_matrix, na.rm = TRUE)
# 
# # Print the result (the ratio covered through the years)
# print(mean_col11)
# # # 
# # 
# print(mean(populationdatafemale[populationdatafemale$stage4 > 0,]$stage4 - populationdatafemale[populationdatafemale$stage4 > 0,]$stage1))
# #9 years to go through system, so less detected in later years
# 
# #Race test by race, on avg for above fifty
# print(colMeans(do.call(rbind,lapply(accepted_samples, function(x) x$racetest_abovefifty))))
# avg_race_rates_abovefifty <- colMeans(do.call(rbind,lapply(accepted_samples, function(x) x$racetest_abovefifty)))
# #Weighted average screening rate
# weight_avg_abovefifty <- sum(avg_race_rates_abovefifty*c(.82, .11, .06, .01))
# print(weight_avg_abovefifty)
# # 
# # #Year-weighted average screening rate
# year_weight_avg_abovefifty <- sum(avg_race_rates_abovefifty*c(.82, .11, .06, .01))/(1/mean_col11)
# print(year_weight_avg_abovefifty)
# 
# #Race test by race, on avg for <40
# print(colMeans(do.call(rbind,lapply(accepted_samples, function(x) x$racetest_forties))))
# avg_race_rates_forties <- colMeans(do.call(rbind,lapply(accepted_samples, function(x) x$racetest_forties)))
# #Weighted average screening rate
# weight_avg_forties <- sum(avg_race_rates_forties*c(.82, .11, .06, .01))
# print(weight_avg_forties)
# # 
# # #Year-weighted average screening rate
# year_weight_avg_forties <- sum(avg_race_rates_forties*c(.82, .11, .06, .01))/(1/mean_col11)
# print(year_weight_avg_forties)
# # Get accuracy by stage
# accuracy_avg <- colMeans(do.call(rbind,lapply(accepted_samples, function(x) x$stage_selection_rates)))
# # Get age test
# age_avg <- colMeans(do.call(rbind,lapply(accepted_samples, function(x) x$agetest)))
# # 
# # #######Finding undetected
# 
# 
####Reverse Engineering
set.seed(9522)
start <- Sys.time()
lst <- list()
total_accepted <- dim(accepted_racetest_abovefifty_all)[1]
total_iters <- 50
scenario_indices_list <- list()
for (indices in 1:16){
  scenario_indices <- sample(1:total_accepted, total_iters, replace = FALSE)
  scenario_indices_list[[indices]] <- scenario_indices
}


for (i in 1:(total_iters)) { 
  if(i %% 5 == 0) {
    cat("Iteration:", i, "\n")
    cat("Elapsed Time:", Sys.time() - start, "\n")
  }
  years <- seq(1968, 2043, by = 5)
  result_df_1 <- data.frame(matrix(0, nrow = length(years), ncol = 12))
  colnames(result_df_1) <- c("Stage1", "Stage2", "Stage3", "Stage4", "Stage1_Total", "Stage2_Total", 
                             "Stage3_Total", "Stage4_Total", "Stage1_Capture", "Stage2_Capture", "Stage3_Capture", "Stage4_Capture")
  rownames(result_df_1) <- as.character(years)
  ages <- seq(from = 10, to = 80, by = 5)
  
  populationdatafemale[, selected := FALSE]
  
  # Generate age and race parameters
  agetest <- c(rep(0, 4), rep(1, 2), rep(1, 2), rep(0.5, 4), runif(3, 0, 0.2))
  
  for (scenario in 1:16) {
    if (scenario == 1) {
      # Normal accuracy and only 40-69
      racetest_forties <- accepted_racetest_forties_all[scenario_indices_list[[1]][i],]
      racetest_abovefifty <- accepted_racetest_forties_all[scenario_indices_list[[1]][i],]
      stage_selection_rates <- accepted_stage_selection_rates_all[scenario_indices_list[[1]][i],]
      racetest_thirties <- 0
    } else if (scenario == 2) {
      # Imperfect screening rate and imperfect stage selection, copy 30-39 screening rate from 40-49
      racetest_thirties <- racetest_forties <- accepted_racetest_forties_all[scenario_indices_list[[2]][i],]
      racetest_abovefifty <- accepted_racetest_abovefifty_all[scenario_indices_list[[2]][i],]
      stage_selection_rates <- accepted_stage_selection_rates_all[scenario_indices_list[[2]][i],]
    } else if (scenario == 3) {
      # Perfect screen but no 30s and imperfect stage selection
      racetest_forties <- racetest_abovefifty <- c(rep(1, 4))
      racetest_thirties <- 0
      stage_selection_rates <- accepted_stage_selection_rates_all[scenario_indices_list[[3]][i],]
    } else if (scenario == 4) {
      # Perfect screen but imperfect stage selection
      racetest_thirties <- racetest_forties <- racetest_abovefifty <- stage_selection_rates <- c(rep(1, 4))
      stage_selection_rates <- accepted_stage_selection_rates_all[scenario_indices_list[[4]][i],]
    } else if (scenario >= 5 && scenario <= 10) {
      # Coverage rates for 40-69: 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
      coverage_rate <- 0.4 + (scenario - 5) * 0.1
      racetest_forties <- rep(coverage_rate, 4)
      racetest_abovefifty <- rep(coverage_rate, 4)
      stage_selection_rates <- accepted_stage_selection_rates_all[scenario_indices_list[[scenario]][i],]
      racetest_thirties <- 0
    } else if (scenario >= 11 && scenario <= 16) {
      # Coverage rates for 30-69: 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
      coverage_rate <- 0.4 + (scenario - 11) * 0.1
      racetest_thirties <- rep(coverage_rate, 2)
      racetest_forties <- rep(coverage_rate, 4)
      racetest_abovefifty <- rep(coverage_rate, 4)
      stage_selection_rates <- accepted_stage_selection_rates_all[scenario_indices_list[[scenario]][i],]
    }
    
    # Process population data and store results
    populationdata_temp <- populationdatafemale[, .(unique_id, race = ceiling(index / 2), year_of_birth, year_of_death, year_brca, 
                                                    stage1, stage2, stage3, stage4, selected, Stage1_start_year, 
                                                    Stage2_start_year, Stage3_start_year, Stage4_start_year)]
    
    minyear <- min(populationdata_temp$year_brca) + 50
    maxyear <- max(populationdata_temp$stage1, populationdata_temp$stage2, populationdata_temp$stage3, populationdata_temp$stage4)
    
    for (j in minyear:2050) {
      # Compute age category
      populationdata_temp[, agecat := pmin(pmax(floor((j - year_of_birth) / 5) * 5, 10), 80)]
      
      populationdata_temp[, `:=` (
        stagenow = fifelse(stage1 <= j & stage1 > 0 & stage2 > j, "Stage1",
                           fifelse(stage2 <= j & stage2 > 0 & stage3 > j, "Stage2",
                                   fifelse(stage3 <= j & stage3 > 0 & stage4 > j, "Stage3",
                                           fifelse(stage4 <= j & stage4 > 0, "Stage4", NA_character_)))),
        ageparam = match(agecat, ages)
      )]
      
      stages <- c("Stage1", "Stage2", "Stage3", "Stage4")
      for (s in na.omit(unique(populationdata_temp$stagenow))) {
        idx <- which(s == populationdata_temp$stagenow)
        chunk <- populationdata_temp[idx, ]
        detect <- runif(nrow(chunk))
        stage_idx <- match(s, stages)
        
        passed <- which(
          detect < agetest[chunk$ageparam] *
            ifelse(chunk$ageparam <= (50 - 10) / 5,
                   ifelse(chunk$ageparam <= (40 - 10) / 5, racetest_thirties[chunk$race], racetest_forties[chunk$race]),
                   racetest_abovefifty[chunk$race]) *
            stage_selection_rates[stage_idx]
        )
        
        detected <- which(populationdata_temp$selected[idx] == FALSE)
        dead <- which((populationdata_temp$year_of_death <= j & populationdata_temp$year_of_death > 0 & populationdata_temp$selected == FALSE) | j - populationdata_temp$year_of_birth > 120)
        detected_passed <- intersect(intersect(idx[passed], idx[detected]), setdiff(idx, dead))
        
        if (length(detected_passed) > 0) {
          stage_col <- paste0("Stage", stage_idx, "_start_year")
          for (y in unique(populationdata_temp[[stage_col]][detected_passed])) {
            if (y > 1967 & y < 2048) {
              result_df_1[as.character(y), stages[stage_idx]] <- result_df_1[as.character(y), stages[stage_idx]] + sum(populationdata_temp[[stage_col]][detected_passed] == y)
            }
          }
          populationdata_temp$selected[detected_passed] <- TRUE
        }
        if ((j - 1972) %% 5 == 0) {
          # An ending year
          currentbatch <- populationdata_temp[populationdata_temp[[paste0(s, "_start_year")]] >= j - 4 & populationdata_temp[[paste0(s, "_start_year")]] <= j,]
          result_df_1[as.character(j - 4), stage_idx + 4] <- nrow(currentbatch)
          result_df_1[as.character(j - 4), stage_idx + 8] <- nrow(currentbatch[selected == TRUE, ]) / nrow(currentbatch)
        }
      }
    }
    
    result_df_1$Sum_Cases <- rowSums(result_df_1[1:4])
    real_world_data <- c(672, 863, 1237, 1739, 2635, 3606, 5582, 6860, 8566, 10868)
    splinefit <- spline(years[1:10], real_world_data, xout = c(2018, 2023))
    result_df_1 <- head(result_df_1, 14)
    lst_index <- i + (scenario - 1) * total_iters
    lst[[lst_index]] <- result_df_1
  }
}



###Bootstrap
# Initialize parameters
num_bootstrap <- 500  # Number of bootstrap samples
scenarios <- 16         # Number of scenarios
total_iters <- 50      # Iterations per scenario
columns_to_analyze <- 9:12  # Columns to calculate CIs for
bootstrap_results <- vector("list", scenarios)

# Combine data by scenario
scenario_data <- split(lst, rep(1:scenarios, each = total_iters))

# Perform bootstrapping
for (scenario_idx in 1:scenarios) {
  data <- scenario_data[[scenario_idx]]
  
  # Combine all data frames for the scenario
  combined_data <- do.call(rbind, data)
  # Store bootstrap results (Means and CIs) for the current scenario
  scenario_cis <- matrix(0, ncol = length(columns_to_analyze), nrow = 3)
  colnames(scenario_cis) <- colnames(combined_data)[columns_to_analyze]
  rownames(scenario_cis) <- c("Mean", "Lower_CI", "Upper_CI")
  
  for (col_idx in columns_to_analyze) {
    # Perform bootstrap
    bootstrap_means <- replicate(num_bootstrap, {
      sampled_data <- combined_data[sample(nrow(combined_data), replace = TRUE), ]
      sampled_data[[col_idx]]
    })
    # Calculate mean and 95% CI
    scenario_cis[1, col_idx - min(columns_to_analyze) + 1] <- mean(bootstrap_means)*100
    scenario_cis[2:3, col_idx - min(columns_to_analyze) + 1] <- quantile(bootstrap_means, c(0.025, 0.975))*100
  }
  
  bootstrap_results[[scenario_idx]] <- scenario_cis
}

# Display results
names(bootstrap_results) <- paste0("Scenario_", 1:scenarios)
bootstrap_results
# Function to calculate CI
calc_ci <- function(values) {
  mean_val <- mean(values)
  ci_lower <- quantile(values, 0.025)  # 2.5% quantile
  ci_upper <- quantile(values, 0.975)  # 97.5% quantile
  return(c(mean_val, ci_lower, ci_upper))
}

# Function to calculate values for each row in a dataframe
calc_vals <- function(df) {
  apply(df, 1, function(row) row[7] - row[7] * row[11] + row[8] - row[8] * row[12])
}

# Index ranges
ranges <- lapply(seq(1, 800, by = 50), function(x) x:(x+49))

# Initialize the list to store the results
cases_averted <- list()
uncaught_late_stage <- list()

# Define the row names for both
years <- c(1968, 1973, 1978, 1983, 1988, 1993, 1998, 2003, 2008, 2013, 2018, 2023, 2028, 2033)  

# Loop over index ranges and process all batches
for (batch in 1:4) {
  print(batch)
  # For each batch, calculate values for the rows
  all_vals <- lapply(ranges[[batch]], function(i) calc_vals(lst[[i]]))  # Get values for the batch
  vals_combined <- do.call(cbind, all_vals)  # Combine across dataframes
  
  # Calculate mean, lower, and upper bounds for each row (year) using calc_ci
  batch_df <- data.frame(
    t(apply(vals_combined, 1, calc_ci))  # Apply CI calculation row-wise
  )
  
  # Rename columns correctly
  colnames(batch_df) <- c("Mean", "Lower_Bound", "Upper_Bound")
  
  # Round to the nearest integer for batch_df (uncaught_late_stage)
  batch_df$Mean <- round(batch_df$Mean)
  batch_df$Lower_Bound <- round(batch_df$Lower_Bound)
  batch_df$Upper_Bound <- round(batch_df$Upper_Bound)
  # Add row names for uncaught_late_stage (years)
  rownames(batch_df) <- years
  
  # Store the dataframe in the uncaught_late_stage list
  uncaught_late_stage[[batch]] <- batch_df
  
  # Compute cases_averted (mean, lower, upper bounds for the first two columns of each dataframe)
  cases_batch <- matrix(NA, nrow = length(years), ncol = 3)
  
  for (row_idx in 1:length(years)) {
    row_sums <- sapply(ranges[[batch]], function(i) sum(lst[[i]][row_idx, 1:2])) #cases caught in 1 and 2
    cases_batch[row_idx, ] <- calc_ci(row_sums)
  }
  
  # Store the result in cases_averted and round to the nearest integer
  cases_averted[[batch]] <- data.frame(
    Mean = round(cases_batch[, 1]),
    Lower_Bound = round(cases_batch[, 2]),
    Upper_Bound = round(cases_batch[, 3])
  )
  
  # Add row names for cases_averted (same as uncaught_late_stage)
  rownames(cases_averted[[batch]]) <- years
}

# View the final results
cases_averted
uncaught_late_stage
library(ggplot2)

# Assuming the row names in cases_averted and uncaught_late_stage contain the correct years (e.g., 1968, 1973, etc.)
# Prepare the data for plotting for cases_averted
plot_data_cases_averted <- data.frame(
  Year = rep(rownames(cases_averted[[1]]), times = 4),  # Using row names for years from cases_averted
  Mean = unlist(lapply(cases_averted, function(df) df$Mean)),
  Lower_Bound = unlist(lapply(cases_averted, function(df) df$Lower_Bound)),
  Upper_Bound = unlist(lapply(cases_averted, function(df) df$Upper_Bound)),
  Batch = rep(1:4, each = length(rownames(cases_averted[[1]])))  # Label each row with the corresponding batch (1 to 4)
)

# Prepare the data for plotting for uncaught_late_stage
plot_data_uncaught_late_stage <- data.frame(
  Year = rep(rownames(uncaught_late_stage[[1]]), times = 4),  # Using row names for years from uncaught_late_stage
  Mean = unlist(lapply(uncaught_late_stage, function(df) df$Mean)),
  Lower_Bound = unlist(lapply(uncaught_late_stage, function(df) df$Lower_Bound)),
  Upper_Bound = unlist(lapply(uncaught_late_stage, function(df) df$Upper_Bound)),
  Batch = rep(1:4, each = length(rownames(uncaught_late_stage[[1]])))  # Label each row with the corresponding batch (1 to 4)
)

# Color and fill settings
colors <- c(
  "1" = "#009B9E",
  "2" = "#C75DAB",
  "3" = "#3d5941",
  "4" = "#ca562c"
)

fills <- c(
  "1" = "#42B7B9",
  "2" = "#D691C1",
  "3" = "#778868",
  "4" = "#de8a5a"
)

# Scenario labels for the legend
scenario_labels <- c("Current", "Expanded", "Intensive", "Complete")

# Plot cases_averted data
averted <- ggplot(plot_data_cases_averted, aes(x = factor(Year), y = Mean, group = Batch, color = factor(Batch), fill = factor(Batch))) +
  geom_point(size = 3, shape = 21) +  # Plot the means as dots with fill
  geom_segment(aes(xend = factor(Year), yend = Lower_Bound), size = 1) +  # Lower bound lines
  geom_segment(aes(xend = factor(Year), yend = Upper_Bound), size = 1) +  # Upper bound lines
  labs(x = "Year", y = "Cases Averted", 
       color = "Scenarios", fill = "Scenarios") +  # Rename the legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.title.x = element_text(face = "bold", size = 18),  # Bold and increase x-axis title font
        axis.title.y = element_text(face = "bold", size = 18),  # Bold and increase y-axis title font
        axis.text = element_text(face = "bold", size = 16),  # Bold and increase axis text font
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid = element_blank()) +  # Remove grid lines
  scale_color_manual(values = colors, labels = scenario_labels) +  # Custom colors for each batch with labels
  scale_fill_manual(values = fills, labels = scenario_labels) +  # Custom fills for each batch with labels
 #scale_x_discrete(breaks = c(1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040), 
  #                   labels = c("1970", "1980", "1990", "2000", "2010", "2020", "2030", "2040")) +
  theme(legend.position = "right",  # Move legend to the right
        legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
        legend.text = element_text(size = 14)) +  # Increase legend text size
  guides(fill = "none")  # Remove duplicate fill legend

# Plot uncaught_late_stage data
uncaught <- ggplot(plot_data_uncaught_late_stage, aes(x = factor(Year), y = Mean, group = Batch, color = factor(Batch), fill = factor(Batch))) +
  geom_point(size = 3, shape = 21) +  # Plot the means as dots with fill
  geom_segment(aes(xend = factor(Year), yend = Lower_Bound), size = 1) +  # Lower bound lines
  geom_segment(aes(xend = factor(Year), yend = Upper_Bound), size = 1) +  # Upper bound lines
  labs(x = "Year", y = "Missed Late Stage", 
       color = "Scenarios", fill = "Scenarios") +  # Rename the legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.title.x = element_text(face = "bold", size = 18),  # Bold and increase x-axis title font
        axis.title.y = element_text(face = "bold", size = 18),  # Bold and increase y-axis title font
        axis.text = element_text(face = "bold", size = 16),  # Bold and increase axis text font
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid = element_blank()) +  # Remove grid lines
  scale_color_manual(values = colors, labels = scenario_labels) +  # Custom colors for each batch with labels
  scale_fill_manual(values = fills, labels = scenario_labels) +  # Custom fills for each batch with labels
 # scale_x_discrete(breaks = c(1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040), 
   #                  labels = c("1970", "1980", "1990", "2000", "2010", "2020", "2030", "2040")) +
  theme(legend.position = "right",  # Move legend to the right
        legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
        legend.text = element_text(size = 14)) +  # Increase legend text size
  guides(fill = "none")  # Remove duplicate fill legend


uncaught <- uncaught + labs(title = "(c)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))
averted <- averted + labs(title = "(d)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))
cases <- arrangeGrob(uncaught, averted, ncol = 2)
# 
# # Combine normalized values for each list into a single data frame
# normalized_df <- do.call(rbind, all_scenarios)
# 
# # Calculate 2.5th and 97.5th percentiles (for a 95% CI) for each column
# lower <- apply(normalized_df, 2, quantile, probs = ci_levels[1])
# upper <- apply(normalized_df, 2, quantile, probs = ci_levels[2])
# ratio <- apply(normalized_df, 2, mean)
# 
# # Combine the results into a data frame for readability
# ci_df <- data.frame(Mean = colMeans(normalized_df), Lower_CI = lower, Upper_CI = upper)

# Display the results
#print(ci_df)

# #Ratio for 100% accuracy vs not, for 100% test
# print(colMeans(lst[[2]][5:8])/colMeans(lst[[1]][5:8]))
# #Ratio for 100% accuracy vs not, for not 100% test
# print(colMeans(lst[[4]][5:8])/colMeans(lst[[3]][5:8]))
# #Ratio for 100% test vs not, for 100% accuracy
# print(colMeans(lst[[3]][5:8])/colMeans(lst[[1]][5:8]))
# #Ratio for 100% test vs not, for not 100% accuracy
# print(colMeans(lst[[4]][5:8])/colMeans(lst[[2]][5:8]))


captured_data_40_69_mean <- data.frame(Stage = stage_labels,
                                       Scenario_five = bootstrap_results[[5]][1,],
                                       Scenario_six = bootstrap_results[[6]][1,],
                                       Scenario_seven = bootstrap_results[[7]][1,],
                                       Scenario_eight = bootstrap_results[[8]][1,],
                                       Scenario_nine = bootstrap_results[[9]][1,],
                                       Scenario_ten = bootstrap_results[[10]][1,])

captured_data_40_69_lower <- data.frame(Stage = stage_labels,
                                        Scenario_five = bootstrap_results[[5]][2,],
                                        Scenario_six = bootstrap_results[[6]][2,],
                                        Scenario_seven = bootstrap_results[[7]][2,],
                                        Scenario_eight = bootstrap_results[[8]][2,],
                                        Scenario_nine = bootstrap_results[[9]][2,],
                                        Scenario_ten = bootstrap_results[[10]][2,])

captured_data_40_69_upper <- data.frame(Stage = stage_labels,
                                        Scenario_five = bootstrap_results[[5]][3,],
                                        Scenario_six = bootstrap_results[[6]][3,],
                                        Scenario_seven = bootstrap_results[[7]][3,],
                                        Scenario_eight = bootstrap_results[[8]][3,],
                                        Scenario_nine = bootstrap_results[[9]][3,],
                                        Scenario_ten = bootstrap_results[[10]][3,])

captured_data_40_69 <- cbind(captured_data_40_69_mean, captured_data_40_69_lower[,2:7], captured_data_40_69_upper[,2:7])
colnames(captured_data_40_69) <- c("Stage", "Mean_Scenario_five", "Mean_Scenario_six", "Mean_Scenario_seven", "Mean_Scenario_eight", "Mean_Scenario_nine", "Mean_Scenario_ten",
                                   "Lower_Scenario_five", "Lower_Scenario_six", "Lower_Scenario_seven", "Lower_Scenario_eight", "Lower_Scenario_nine", "Lower_Scenario_ten",
                                   "Upper_Scenario_five", "Upper_Scenario_six", "Upper_Scenario_seven", "Upper_Scenario_eight", "Upper_Scenario_nine", "Upper_Scenario_ten")

# Subset for 30 to 69 (scenarios 11 to 16)
captured_data_30_69_mean <- data.frame(Stage = stage_labels,
                                       Scenario_eleven = bootstrap_results[[11]][1,],
                                       Scenario_twelve = bootstrap_results[[12]][1,],
                                       Scenario_thirteen = bootstrap_results[[13]][1,],
                                       Scenario_fourteen = bootstrap_results[[14]][1,],
                                       Scenario_fifteen = bootstrap_results[[15]][1,],
                                       Scenario_sixteen = bootstrap_results[[16]][1,])

captured_data_30_69_lower <- data.frame(Stage = stage_labels,
                                        Scenario_eleven = bootstrap_results[[11]][2,],
                                        Scenario_twelve = bootstrap_results[[12]][2,],
                                        Scenario_thirteen = bootstrap_results[[13]][2,],
                                        Scenario_fourteen = bootstrap_results[[14]][2,],
                                        Scenario_fifteen = bootstrap_results[[15]][2,],
                                        Scenario_sixteen = bootstrap_results[[16]][2,])

captured_data_30_69_upper <- data.frame(Stage = stage_labels,
                                        Scenario_eleven = bootstrap_results[[11]][3,],
                                        Scenario_twelve = bootstrap_results[[12]][3,],
                                        Scenario_thirteen = bootstrap_results[[13]][3,],
                                        Scenario_fourteen = bootstrap_results[[14]][3,],
                                        Scenario_fifteen = bootstrap_results[[15]][3,],
                                        Scenario_sixteen = bootstrap_results[[16]][3,])

captured_data_30_69 <- cbind(captured_data_30_69_mean, captured_data_30_69_lower[,2:7], captured_data_30_69_upper[,2:7])
colnames(captured_data_30_69) <- c("Stage", "Mean_Scenario_eleven", "Mean_Scenario_twelve", "Mean_Scenario_thirteen", "Mean_Scenario_fourteen", "Mean_Scenario_fifteen", "Mean_Scenario_sixteen",
                                   "Lower_Scenario_eleven", "Lower_Scenario_twelve", "Lower_Scenario_thirteen", "Lower_Scenario_fourteen", "Lower_Scenario_fifteen", "Lower_Scenario_sixteen",
                                   "Upper_Scenario_eleven", "Upper_Scenario_twelve", "Upper_Scenario_thirteen", "Upper_Scenario_fourteen", "Upper_Scenario_fifteen", "Upper_Scenario_sixteen")

captured_data_mean <- data.frame(Stage = stage_labels,
                                 Scenario_one = bootstrap_results[[1]][1,],
                                 Scenario_two = bootstrap_results[[2]][1,],
                                 Scenario_three = bootstrap_results[[3]][1,],
                                 Scenario_four = bootstrap_results[[4]][1,])

captured_data_lower <- data.frame(Stage = stage_labels,
                                  Scenario_one = bootstrap_results[[1]][2,],
                                  Scenario_two = bootstrap_results[[2]][2,],
                                  Scenario_three = bootstrap_results[[3]][2,],
                                  Scenario_four = bootstrap_results[[4]][2,])

captured_data_upper <- data.frame(Stage = stage_labels,
                                  Scenario_one = bootstrap_results[[1]][3,],
                                  Scenario_two = bootstrap_results[[2]][3,],
                                  Scenario_three = bootstrap_results[[3]][3,],
                                  Scenario_four = bootstrap_results[[4]][3,])

captured_data <- cbind(captured_data_mean, captured_data_lower[,2:5], captured_data_upper[,2:5])
colnames(captured_data) <- c("Stage", "Mean_Scenario_one", "Mean_Scenario_two", "Mean_Scenario_three", "Mean_Scenario_four",
                             "Lower_Scenario_one", "Lower_Scenario_two", "Lower_Scenario_three", "Lower_Scenario_four",
                             "Upper_Scenario_one", "Upper_Scenario_two", "Upper_Scenario_three", "Upper_Scenario_four")

captured_data_30_69[,1] <- c(1,2,3,4)
captured_data_40_69[,1] <- c(1,2,3,4)
captured_data[,1] <- c(1,2,3,4)
library(ggplot2)
library(gridExtra)

# Define colors and fills as you provided
colors <- c(
  "Baseline" = "#3F1412",
  "40" = "#009B9E",
  "50" = "#C75DAB",
  "60" = "#3d5941",
  "70" = "#ca562c",
  "80" = "#820000",
  "90" = "#124CA2",
  "100" = "#15AD29"
)

fills <- c(
  "Baseline" = "#5E1911",
  "40" = "#42B7B9",
  "50" = "#D691C1",
  "60" = "#778868",
  "70" = "#de8a5a",
  "80" = "#A24857",
  "90" = "#4682B4",
  "100" = "#96F97B"
)

# Plot for 40 to 69 (Scenarios 5 to 10)
p1 <- ggplot() + 
  geom_line(data =captured_data, aes(x = Stage, y = Mean_Scenario_one, color = "Baseline"), size = 1) + 
  geom_point(data =captured_data, aes(x = Stage, y = Mean_Scenario_one, color = "Baseline"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_one, ymax = Upper_Scenario_one, fill = "Baseline"), alpha = 0.2) +
  geom_line(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_five, color = "40"), size = 1) + 
  geom_point(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_five, color = "40"), size = 3) + 
  geom_ribbon(data = captured_data_40_69, aes(x = Stage, ymin = Lower_Scenario_five, ymax = Upper_Scenario_five, fill = "40"), alpha = 0.2) +
  #geom_line(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_six, color = "50"), size = 1) + 
  #geom_point(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_six, color = "50"), size = 3) + 
  #geom_ribbon(data = captured_data_40_69, aes(x = Stage, ymin = Lower_Scenario_six, ymax = Upper_Scenario_six, fill = "50"), alpha = 0.2) +
  geom_line(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_seven, color = "60"), size = 1) + 
  geom_point(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_seven, color = "60"), size = 3) + 
  geom_ribbon(data = captured_data_40_69, aes(x = Stage, ymin = Lower_Scenario_seven, ymax = Upper_Scenario_seven, fill = "60"), alpha = 0.2) +
  #geom_line(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_eight, color = "70"), size = 1) + 
  #geom_point(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_eight, color = "70"), size = 3) + 
  #geom_ribbon(data = captured_data_40_69, aes(x = Stage, ymin = Lower_Scenario_eight, ymax = Upper_Scenario_eight, fill = "70"), alpha = 0.2) +
  geom_line(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_nine, color = "80"), size = 1) + 
  geom_point(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_nine, color = "80"), size = 3) + 
  geom_ribbon(data = captured_data_40_69, aes(x = Stage, ymin = Lower_Scenario_nine, ymax = Upper_Scenario_nine, fill = "80"), alpha = 0.2) +
  #geom_line(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_ten, color = "90"), size = 1) + 
  #geom_point(data =captured_data_40_69, aes(x = Stage, y = Mean_Scenario_ten, color = "90"), size = 3) + 
  #geom_ribbon(data = captured_data_40_69, aes(x = Stage, ymin = Lower_Scenario_ten, ymax = Upper_Scenario_ten, fill = "90"), alpha = 0.2) +
  geom_line(data =captured_data, aes(x = Stage, y = Mean_Scenario_three, color = "100"), size = 1) + 
  geom_point(data =captured_data, aes(x = Stage, y = Mean_Scenario_three, color = "100"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_three, ymax = Upper_Scenario_three, fill = "100"), alpha = 0.2) +
  # Customize scales and labels
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) +
  scale_x_continuous(breaks = 1:4, labels = c("1", "2", "3", "4")) +  # Numeric stage labels
  scale_y_continuous(limits = c(0, 100)) + 
  labs(
    x = "Stages", 
    y = "Cases Captured by Stage (%), 40-69",
    color = "Coverage",
    fill = "Coverage"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 18, hjust = 0),  # Title left-aligned and larger
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    legend.title = element_text(face = "bold", size = 16),   # Increase legend title size
    legend.text = element_text(size = 14)   # Increase legend text size
  )


# Plot for 30 to 69 (Scenarios 11 to 16)
p2 <- ggplot() + 
  geom_line(data =captured_data, aes(x = Stage, y = Mean_Scenario_one, color = "Baseline"), size = 1) + 
  geom_point(data =captured_data, aes(x = Stage, y = Mean_Scenario_one, color = "Baseline"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_one, ymax = Upper_Scenario_one, fill = "Baseline"), alpha = 0.2) +
  geom_line(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_eleven, color = "40"), size = 1) + 
  geom_point(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_eleven, color = "40"), size = 3) + 
  geom_ribbon(data = captured_data_30_69, aes(x = Stage, ymin = Lower_Scenario_eleven, ymax = Upper_Scenario_eleven, fill = "40"), alpha = 0.2) +
  #geom_line(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_twelve, color = "50"), size = 1) + 
  #geom_point(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_twelve, color = "50"), size = 3) + 
  #geom_ribbon(data = captured_data_30_69, aes(x = Stage, ymin = Lower_Scenario_twelve, ymax = Upper_Scenario_twelve, fill = "50"), alpha = 0.2) +
  geom_line(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_thirteen, color = "60"), size = 1) + 
  geom_point(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_thirteen, color = "60"), size = 3) + 
  geom_ribbon(data = captured_data_30_69, aes(x = Stage, ymin = Lower_Scenario_thirteen, ymax = Upper_Scenario_thirteen, fill = "60"), alpha = 0.2) +
  #geom_line(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_fourteen, color = "70"), size = 1) + 
  #geom_point(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_fourteen, color = "70"), size = 3) + 
  #geom_ribbon(data = captured_data_30_69, aes(x = Stage, ymin = Lower_Scenario_fourteen, ymax = Upper_Scenario_fourteen, fill = "70"), alpha = 0.2) +
  geom_line(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_fifteen, color = "80"), size = 1) + 
  geom_point(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_fifteen, color = "80"), size = 3) + 
  geom_ribbon(data = captured_data_30_69, aes(x = Stage, ymin = Lower_Scenario_fifteen, ymax = Upper_Scenario_fifteen, fill = "80"), alpha = 0.2) +
  #geom_line(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_sixteen, color = "90"), size = 1) + 
  #geom_point(data =captured_data_30_69, aes(x = Stage, y = Mean_Scenario_sixteen, color = "90"), size = 3) + 
  #geom_ribbon(data = captured_data_30_69, aes(x = Stage, ymin = Lower_Scenario_sixteen, ymax = Upper_Scenario_sixteen, fill = "90"), alpha = 0.2) +
  geom_line(data =captured_data, aes(x = Stage, y = Mean_Scenario_four, color = "100"), size = 1) + 
  geom_point(data =captured_data, aes(x = Stage, y = Mean_Scenario_four, color = "100"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_four, ymax = Upper_Scenario_four, fill = "100"), alpha = 0.2) +
  # Customize scales and labels
  scale_color_manual(values = colors) +
  scale_fill_manual(values = fills) +
  scale_x_continuous(breaks = 1:4, labels = c("1", "2", "3", "4")) +  # Numeric stage labels
  scale_y_continuous(limits = c(0, 100)) + 
  labs(
    x = "Stages", 
    y = "Cases Captured by Stage (%), 30-69",
    color = "Coverage",
    fill = "Coverage"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 18, hjust = 0),  # Title left-aligned and larger
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    legend.title = element_text(face = "bold", size = 16),   # Increase legend title size
    legend.text = element_text(size = 14)   # Increase legend text size
  )

  
p1 <- p1 + labs(title = "(b)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))
p2 <- p2 + labs(title = "(a)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))

proportion <- arrangeGrob(p2, p1, nrow = 1)
# Save the combined plot
final_plot <- arrangeGrob(proportion, cases, nrow = 2)
ggsave("combined_plot.png", final_plot,  width = 16, height = 12, dpi = 300)


screen_data <- data.frame(
  Race = race_labels,
  Avg_Forty = avg_forty*100,
  Lower_Bound_Forty = lower_bound_forty*100,
  Upper_Bound_Forty = upper_bound_forty*100,
  Avg_Fifties = avg_fifties*100,
  Lower_Bound_Fifties = lower_bound_fifties*100,
  Upper_Bound_Fifties = upper_bound_fifties*100
)


accepted_mcmc_results_1 <- lapply(chain_1_accepted, function(x) x$result_df)
accepted_mcmc_results_2 <- lapply(chain_2_accepted, function(x) x$result_df)
accepted_mcmc_results_3 <- lapply(chain_3_accepted, function(x) x$result_df)
accepted_mcmc_results_4 <- lapply(chain_4_accepted, function(x) x$result_df)

#sum_matrix <- accepted_mcmc_results_1[[1]]
#sum_matrix[,] <- 0
accepted_mcmc_results <- list(accepted_mcmc_results_1, accepted_mcmc_results_2, accepted_mcmc_results_3, accepted_mcmc_results_4)
# 
# for (j in 1:4){
#   count <- length(accepted_mcmc_results[j][[1]])
#   for(i in 1:count) {
#     sum_matrix[, c(1:10)] <- sum_matrix[, c(1:10)] + accepted_mcmc_results[j][[1]][[i]][, c(1:10)]
#   }
# }
# 
# sum_matrix <- sum_matrix/(length(accepted_mcmc_results[1][[1]]) + length(accepted_mcmc_results[2][[1]]) + length(accepted_mcmc_results[3][[1]])+ length(accepted_mcmc_results[4][[1]]))
# 
# result_df <- sum_matrix
# result_df$Proportion_Caught <- 100*result_df$Actual/result_df$Sum_Cases
# result_df$Start_Year <- row.names(result_df)
# # Print the mean matrix
# print(result_df)
# result_df[11:12,]$Actual <- NA
# result_df[11:12,]$Proportion_Caught <- NA
# 
# result_df$year_weight_avg_fifties<- c(sum(avg_fifties*c(.82, .11, .06, .01))/(1/result_df[,11][1:10]), NA, NA)
# 
# result_df$year_weight_avg_forty <- c(sum(avg_forty*c(.82, .11, .06, .01))/(1/result_df[,11][1:10]), NA, NA)
# 
# result_df$Start_Year <- rownames(result_df)

# Combine all accepted results from each chain into a single list
all_accepted_results <- do.call(c, accepted_mcmc_results)
# Initialize empty matrices to store the lower and upper bounds of the CI
lower_bound <- accepted_mcmc_results_1[[1]]
upper_bound <- accepted_mcmc_results_1[[1]]
mean_samples <- accepted_mcmc_results_1[[1]]
lower_bound[,] <- 0
upper_bound[,] <- 0
mean_samples[,] <- 0

# Calculate the CI for each parameter across all accepted results
for (col in 1:ncol(lower_bound)) {
  # Extract the samples for this parameter across all accepted samples
  all_samples <- sapply(all_accepted_results, function(x) x[, col])
  
  # Calculate quantiles for each row and store in lower and upper matrices
  lower_bound[, col] <- apply(all_samples, 1, quantile, probs = ci_levels[1])
  upper_bound[, col] <- apply(all_samples, 1, quantile, probs = ci_levels[2])
  mean_samples[, col] <- apply(all_samples, 1, mean)
}
mean_samples$year <- as.numeric(rownames(mean_samples))
proportions <- sapply(all_accepted_results, function(x) x$Proportion_Caught)

# Calculate weighted sums and means
for (i in 1:nrow(proportions)) {
  raceweightedforty <- raceweightedfifties <- accepted_racetest_abovefifty_all
  raceweightedforty[i, ] <- accepted_racetest_forties_all[i, ] * c(0.82, 0.11, 0.06, 0.01)
  raceweightedfifties[i, ] <- accepted_racetest_abovefifty_all[i, ] * c(0.82, 0.11, 0.06, 0.01)
}

# Calculate mean and CI of means for year_weight averages
for (i in 1:nrow(proportions)) {
  weighted_fifties <- rowSums(accepted_racetest_abovefifty_all * c(0.82, 0.11, 0.06, 0.01)) * proportions[i, ]
  weighted_forty <- rowSums(accepted_racetest_forties_all * c(0.82, 0.11, 0.06, 0.01)) * proportions[i, ]
  
  mean_samples$year_weight_avg_fifties[i] <- mean(weighted_fifties)
  mean_samples$year_weight_avg_forty[i] <- mean(weighted_forty)
  
  lower_bound$year_weight_avg_fifties[i] <- quantile(weighted_fifties, probs = 0.25)
  upper_bound$year_weight_avg_fifties[i] <- quantile(weighted_fifties, probs = 0.75)
  lower_bound$year_weight_avg_forty[i] <- quantile(weighted_forty, probs = 0.25)
  upper_bound$year_weight_avg_forty[i] <- quantile(weighted_forty, probs = 0.75)
}

# Print results
print(mean_samples)
print(lower_bound)
print(upper_bound)

mean_yearly_forty <- mean_yearly_fifties <- accepted_mcmc_results_1[[1]][1:4]
upper_bound_yearly_forty <- upper_bound_yearly_fifties <- accepted_mcmc_results_1[[1]][1:4]
lower_bound_yearly_forty <- lower_bound_yearly_fifties <- accepted_mcmc_results_1[[1]][1:4]
race_labels <- c("Chinese", "Malay", "Indian", "Others")
stage_labels <- c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
colnames(mean_yearly_fifties) <- colnames(mean_yearly_forty) <- race_labels
colnames(upper_bound_yearly_fifties) <- colnames(upper_bound_yearly_forty) <- race_labels
colnames(lower_bound_yearly_fifties) <- colnames(lower_bound_yearly_forty) <- race_labels
for (i in 1:nrow(proportions)) {
  mean_yearly_fifties[i,] <- colMeans(accepted_racetest_abovefifty_all*proportions[i,])
  mean_yearly_forty[i,] <- colMeans(accepted_racetest_forties_all*proportions[i,])
  lower_bound_yearly_fifties[i,] <- apply(accepted_racetest_abovefifty_all*proportions[i,], 2, quantile, probs = 0.25)
  lower_bound_yearly_forty[i,] <- apply(accepted_racetest_forties_all*proportions[i,], 2, quantile, probs = 0.25)
  upper_bound_yearly_fifties[i,] <- apply(accepted_racetest_abovefifty_all*proportions[i,], 2, quantile, probs = 0.75)
  upper_bound_yearly_forty[i,] <- apply(accepted_racetest_forties_all*proportions[i,], 2, quantile, probs = 0.75)
}

# Plot for Weighted Screening Rate, 50-69
weightscreenfifties <- ggplot(data = head(mean_samples, 10), aes(x = as.numeric(row.names(mean_samples[1:10, ])))) +
  geom_point(aes(y = year_weight_avg_fifties[1:10]), color = "blue") +
  geom_line(aes(y = year_weight_avg_fifties[1:10]), color = "blue") +
  geom_ribbon(aes(ymin = lower_bound$year_weight_avg_fifties[1:10], ymax = upper_bound$year_weight_avg_fifties[1:10]), fill = "blue", alpha = 0.2) +
  geom_text(aes(y = year_weight_avg_fifties[1:10] - 2, label = round(year_weight_avg_fifties[1:10], 2)), size = 3, vjust = 1.5) +  # Adjust position with vjust
  scale_x_continuous(breaks = as.numeric(row.names(mean_samples[1:10, ])), labels = row.names(mean_samples[1:10, ])) +
  labs(x = "Start Year", y = "Screening Rate (%)", title = "Weighted Screening, 50-69") +
  theme_minimal()
ggsave("weightscreenfifties.png", plot = weightscreenfifties, width = 8, height = 6, dpi = 300)

# Plot for Weighted Screening Rate, 40-49
weightscreenforty <- ggplot(data = head(mean_samples, 10), aes(x = as.numeric(row.names(mean_samples[1:10, ])))) +
  geom_point(aes(y = year_weight_avg_forty[1:10]), color = "blue") +
  geom_line(aes(y = year_weight_avg_forty[1:10]), color = "blue") +
  geom_ribbon(aes(ymin = lower_bound$year_weight_avg_forty[1:10], ymax = upper_bound$year_weight_avg_forty[1:10]), fill = "blue", alpha = 0.2) +
  geom_text(aes(y = year_weight_avg_forty[1:10] - 2, label = round(year_weight_avg_forty[1:10], 2)), size = 3, vjust = 1.5) +  # Adjust position with vjust
  scale_x_continuous(breaks = as.numeric(row.names(mean_samples[1:10, ])), labels = row.names(mean_samples[1:10, ])) +
  labs(x = "Start Year", y = "Screening Rate (%)", title = "Weighted Screening, 40-49") +
  theme_minimal() 
ggsave("weightscreenforty.png", plot = weightscreenforty, width = 8, height = 6, dpi = 300)



####Country comparison and real data plots

# Data
total <- c(672, 863, 1237, 1739, 2635, 3606, 5582, 6860, 8566, 10868)
years_total <- seq(1968, 2013, by = 5)
years_stages <- c(2003, 2008, 2013)

# Stage proportions
stage_one <- c(0.33, 0.33, 0.336)
stage_two <- c(0.379, 0.383, 0.39)
stage_three <- c(0.204, 0.19, 0.167)
stage_four <- c(0.086, 0.097, 0.107)

# Extract total for years corresponding to stages (2003, 2008, 2013)
total_stages <- total[years_total %in% years_stages]

# Calculate raw numbers for stages
stage_one_raw <- total_stages * stage_one
stage_two_raw <- total_stages * stage_two
stage_three_raw <- total_stages * stage_three
stage_four_raw <- total_stages * stage_four

# Combine all stages data into one dataframe
stages_data <- data.frame(
  Year = rep(years_stages, 4),
  Stage = factor(rep(c("Stage 1", "Stage 2", "Stage 3", "Stage 4"), each = 3)),
  Count = c(stage_one_raw, stage_two_raw, stage_three_raw, stage_four_raw)
)

data_points <- data.frame()
# Colors for stages
colors <- c("Stage 1" = "#009B9E", "Stage 2" = "#C75DAB", "Stage 3" = "#3d5941", "Stage 4" = "#ca562c", "Total" = "#000000")

# Plot# Updated Plot with "Total" in the legend
stage_plot <- ggplot() +
  # Line for total cases
  #geom_line(data = data.frame(Year = years_total, Total = total), 
  #aes(x = Year, y = Total, color = "Total"), size = 1) +
  geom_point(data = data.frame(Year = years_total, Total = total), 
             aes(x = Year, y = Total, color = "Total"), size = 2, shape = 4, stroke = 1.2) +
  
  # Cross points for stages (raw counts)
  geom_point(data = stages_data, aes(x = Year, y = Count, color = Stage), 
             shape = 4, size = 2, stroke = 1.2) +
  
  # Customizations
  scale_color_manual(
    values = colors, 
    breaks = c("Total", "Stage 1", "Stage 2", "Stage 3", "Stage 4")
  ) +
  labs(x = "Year", y = "Number of Cases", color = "Stage") +
  scale_y_continuous(limits = c(0, 12000)) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16, face = "bold"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )
data_points <- data.frame(
  x = c(2015, 2018, 2015, 2019, 2005, 2010, 2015),
  y = c(18.4, 24.3, 71.3, 76.2, 48.7, 66.7, 65.6),
  group = c("China 50-74", "China 50-74", "USA 50-74", "USA 50-74", "South Korea 50-59", "South Korea 50-59", "South Korea 50-59")
)
# Define the colors for the groups, including separate entries for the Singapore age groups
colors <- c(
  "Singapore 40-49" = "#ca562c",  # Color for Singapore 40-49
  "Singapore 50-69" = "blue",     # Color for Singapore 50-69
  "China 50-74" = "#009B9E", 
  "USA 50-74" = "#C75DAB", 
  "South Korea 50-59" = "#3d5941"
)

# Create the plot
country_plot <- ggplot() +
  # Add the first geom line for Singapore 40-49
  geom_line(data = mean_samples[1:11,], 
            aes(x = year[1:11], y = year_weight_avg_forty[1:11], color = "Singapore 40-49"), 
            size = 1) +
  # Add the second geom line for Singapore 50-69
  geom_line(data = mean_samples[1:11,], 
            aes(x = year[1:11], y = year_weight_avg_fifties[1:11], color = "Singapore 50-69"), 
            size = 1) +
  # Add the other data points with corresponding country labels and colors
  geom_point(data = data_points, 
             aes(x = x, y = y, color = group), 
             shape = 4, size = 2, stroke = 1.2) +  # Fatter crosses
  
  # Customize the plot
  labs(x = "Year", y = "Estimated Screening Rate (%)", color = "Country") +  
  
  # Set custom colors for the groups
  scale_color_manual(values = colors, 
                     breaks = c("Singapore 40-49", "Singapore 50-69", "China 50-74", "USA 50-74", "South Korea 50-59")) +  
  
  # Customize x-axis to include breaks for the years
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020), 
                     labels = c("1970", "1980", "1990", "2000", "2010", "2020")) +
  
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 18),  # Bold and increase x-axis title font
    axis.title.y = element_text(face = "bold", size = 18),  # Bold and increase y-axis title font
    axis.text = element_text(face = "bold", size = 16),  # Bold and increase axis text font
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_blank(),  # Remove major grid lines inside the graph
    panel.grid.minor = element_blank(),  # Remove minor grid lines inside the graph
    axis.line = element_line(color = "black", size = 0.5),  # Add axis lines back
    legend.position = "right",  # Move legend to the right
    legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
    legend.text = element_text(size = 14)  # Increase legend text size
  )


stage_plot <- stage_plot + labs(title = "(a)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))

country_plot <- country_plot + labs(title = "(b)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))
top_half <- arrangeGrob(stage_plot, country_plot, ncol = 2)

# Subset the first 11 rows for 40-49 age group
weightscreen_forty <- ggplot(data = mean_yearly_forty[1:11, ], aes(x = as.numeric(row.names(mean_yearly_forty[1:11, ])))) +
  geom_line(aes(y = Chinese, color = "Chinese"), size = 1.2) +  # Bold line for Chinese
  geom_ribbon(aes(ymin = lower_bound_yearly_forty$Chinese[1:11], ymax = upper_bound_yearly_forty$Chinese[1:11]), fill = "#42B7B9", alpha = 0.2) +
  geom_line(aes(y = Malay, color = "Malay"), size = 1.2) +  # Bold line for Malay
  geom_ribbon(aes(ymin = lower_bound_yearly_forty$Malay[1:11], ymax = upper_bound_yearly_forty$Malay[1:11]), fill = "#D691C1", alpha = 0.2) +
  geom_line(aes(y = Indian, color = "Indian"), size = 1.2) +  # Bold line for Indian
  geom_ribbon(aes(ymin = lower_bound_yearly_forty$Indian[1:11], ymax = upper_bound_yearly_forty$Indian[1:11]), fill = "#778868", alpha = 0.2) +
  scale_color_manual(name = "Ethnicity", values = c("Chinese" = "#009B9E", "Malay" = "#C75DAB", "Indian" = "#3d5941")) +
  scale_y_continuous(limits = c(0, 60)) +  # Set y-axis scale from 0 to 70
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020), labels = c("1970", "1980", "1990", "2000", "2010", "2020")) +
  labs(x = "Year", y = "Estimated 40-49 Screening Rate (%)", title = "(c)") +
  theme_minimal() +
  theme(
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    legend.position = "right",
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid = element_blank(),
    legend.key.size = unit(1, "cm")
  )

# Subset the first 11 rows for 50-69 age group
weightscreen_fifties <- ggplot(data = mean_yearly_fifties[1:11, ], aes(x = as.numeric(row.names(mean_yearly_fifties[1:11, ])))) +
  geom_line(aes(y = Chinese, color = "Chinese"), size = 1.2) +  # Bold line for Chinese
  geom_ribbon(aes(ymin = lower_bound_yearly_fifties$Chinese[1:11], ymax = upper_bound_yearly_fifties$Chinese[1:11]), fill = "#42B7B9", alpha = 0.2) +
  geom_line(aes(y = Malay, color = "Malay"), size = 1.2) +  # Bold line for Malay
  geom_ribbon(aes(ymin = lower_bound_yearly_fifties$Malay[1:11], ymax = upper_bound_yearly_fifties$Malay[1:11]), fill = "#D691C1", alpha = 0.2) +
  geom_line(aes(y = Indian, color = "Indian"), size = 1.2) +  # Bold line for Indian
  geom_ribbon(aes(ymin = lower_bound_yearly_fifties$Indian[1:11], ymax = upper_bound_yearly_fifties$Indian[1:11]), fill = "#778868", alpha = 0.2) +
  scale_color_manual(name = "Ethnicity", values = c("Chinese" = "#009B9E", "Malay" = "#C75DAB", "Indian" = "#3d5941")) +
  scale_y_continuous(limits = c(0, 60)) +  # Set y-axis scale from 0 to 70
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020), labels = c("1970", "1980", "1990", "2000", "2010", "2020")) +
  labs(x = "Year", y = "Estimated 50-69 Screening Rate (%)", title = "(d)") +
  theme_minimal() +
  theme(
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    legend.position = "right",
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid = element_blank(),
    legend.key.size = unit(1, "cm")
  )

weightscreen_fifties <- weightscreen_fifties + labs(title = "(d)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))
weightscreen_forty<- weightscreen_forty + labs(title = "(c)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))
weightscreen_forty <- weightscreen_forty + theme(legend.position = "right")
weightscreen_fifties <- weightscreen_fifties + theme(legend.position = "right")
bottom_half <- arrangeGrob(weightscreen_forty, weightscreen_fifties, ncol = 2)
final_plot <- arrangeGrob(top_half, bottom_half, nrow = 2)
# Save the final plot to a file
ggsave("final_plot.png", plot = final_plot, width = 14, height = 10, units = "in", dpi = 300)



#####Plot 2 (3 in 1 row)

library(ggplot2)
library(gridExtra)  # For grid.arrange()
library(patchwork)  # Alternative for easy panel layout
# Colors and fills for all scenarios
colors <- c(
  "1" = "#009B9E", "2" = "#C75DAB", "3" = "#3d5941", "4" = "#ca562c"
)
fills <- c(
  "1" = "#42B7B9", "2" = "#D691C1", "3" = "#778868", "4" = "#de8a5a"
)
scenario_labels <-  c("Current", "Expanded", "Intensive", "Complete")

# Plot 1: Combined Scenarios (captured_data)
p1 <- ggplot() +
  geom_line(data = captured_data, aes(x = Stage, y = Mean_Scenario_one, color = "1"), size = 1) +  
  geom_point(data = captured_data, aes(x = Stage, y = Mean_Scenario_one, color = "1"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_one, ymax = Upper_Scenario_one, fill = "1"), alpha = 0.2) +
  
  geom_line(data = captured_data, aes(x = Stage, y = Mean_Scenario_two, color = "2"), size = 1) +  
  geom_point(data = captured_data, aes(x = Stage, y = Mean_Scenario_two, color = "2"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_two, ymax = Upper_Scenario_two, fill = "2"), alpha = 0.2) +
  
  geom_line(data = captured_data, aes(x = Stage, y = Mean_Scenario_three, color = "3"), size = 1) +  
  geom_point(data = captured_data, aes(x = Stage, y = Mean_Scenario_three, color = "3"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_three, ymax = Upper_Scenario_three, fill = "3"), alpha = 0.2) +
  
  geom_line(data = captured_data, aes(x = Stage, y = Mean_Scenario_four, color = "4"), size = 1) +  
  geom_point(data = captured_data, aes(x = Stage, y = Mean_Scenario_four, color = "4"), size = 3) + 
  geom_ribbon(data = captured_data, aes(x = Stage, ymin = Lower_Scenario_four, ymax = Upper_Scenario_four, fill = "4"), alpha = 0.2) +
  
  scale_color_manual(values = colors, labels = scenario_labels) +
  scale_fill_manual(values = fills, labels = scenario_labels) +
  scale_x_continuous(breaks = 1:4, labels = c("1", "2", "3", "4")) +
  labs(x = "Stages", y = "Cases Captured (%)", color = "Strategy", fill = "Strategy") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
                          axis.title.x = element_text(face = "bold", size = 18),  # Bold and increase x-axis title font
                          axis.title.y = element_text(face = "bold", size = 18),  # Bold and increase y-axis title font
                          axis.text = element_text(face = "bold", size = 16),  # Bold and increase axis text font
                          axis.ticks = element_line(colour = "black"),
                          axis.ticks.length = unit(0.2, "cm"),
                          panel.grid.major = element_blank(),  # Remove major grid lines inside the graph
                          panel.grid.minor = element_blank(),  # Remove minor grid lines inside the graph
                          axis.line = element_line(color = "black", size = 0.5)) +  # Add axis lines back
  theme(legend.position = "right",  # Move legend to the right
        legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
        legend.text = element_text(size = 14)) +  # Increase legend text size
  guides(fill = "none")

# Plot 2: Cases Averted
p2 <- ggplot(plot_data_cases_averted, aes(x = factor(Year), y = Mean / 1000, group = Batch, color = factor(Batch), fill = factor(Batch))) +
  geom_line(aes(color = factor(Batch)), size = 1) +
  geom_point(aes(color = factor(Batch)), size = 3) +
  geom_segment(aes(xend = factor(Year), yend = Lower_Bound / 1000), size = 1) +
  geom_segment(aes(xend = factor(Year), yend = Upper_Bound / 1000), size = 1) +
  scale_color_manual(values = colors, labels = scenario_labels) +
  scale_fill_manual(values = fills, labels = scenario_labels) +
  labs(x = "Year", y = "Cases Averted (Thousands)", color = "Scenarios", fill = "Scenarios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.title.x = element_text(face = "bold", size = 18),  # Bold and increase x-axis title font
        axis.title.y = element_text(face = "bold", size = 18),  # Bold and increase y-axis title font
        axis.text = element_text(face = "bold", size = 16),  # Bold and increase axis text font
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major = element_blank(),  # Remove major grid lines inside the graph
        panel.grid.minor = element_blank(),  # Remove minor grid lines inside the graph
        axis.line = element_line(color = "black", size = 0.5)) +  # Add axis lines back
  theme(legend.position = "right",  # Move legend to the right
        legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
        legend.text = element_text(size = 14)) +  # Increase legend text size
  guides(fill = "none")

# Plot 3: Uncaught Late Stage
p3 <- ggplot(plot_data_uncaught_late_stage, aes(x = factor(Year), y = Mean / 1000, group = Batch, color = factor(Batch), fill = factor(Batch))) +
  geom_line(aes(color = factor(Batch)), size = 1) +
  geom_point(aes(color = factor(Batch)), size = 3) +
  geom_segment(aes(xend = factor(Year), yend = Lower_Bound / 1000), size = 1) +
  geom_segment(aes(xend = factor(Year), yend = Upper_Bound / 1000), size = 1) +
  scale_color_manual(values = colors, labels = scenario_labels) +
  scale_fill_manual(values = fills, labels = scenario_labels) +
  labs(x = "Year", y = "Uncaught Late Stage (Thousands)", color = "Scenarios", fill = "Scenarios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.title.x = element_text(face = "bold", size = 18),  # Bold and increase x-axis title font
        axis.title.y = element_text(face = "bold", size = 18),  # Bold and increase y-axis title font
        axis.text = element_text(face = "bold", size = 16),  # Bold and increase axis text font
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major = element_blank(),  # Remove major grid lines inside the graph
        panel.grid.minor = element_blank(),  # Remove minor grid lines inside the graph
        axis.line = element_line(color = "black", size = 0.5)) +  # Add axis lines back
  theme(legend.position = "right",  # Move legend to the right
        legend.title = element_text(face = "bold", size = 16),  # Increase legend title size
        legend.text = element_text(size = 14)) +  # Increase legend text size
  guides(fill = "none")

p1 <- p1 + labs(title = "(a)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))

p2 <- p2 + labs(title = "(b)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))

p3 <- p3 + labs(title = "(c)") + 
  theme(plot.title = element_text(hjust = 0, size = 18, face = "bold"))
# Combine the three plots horizontally
combined_plot <- grid.arrange(
  p1, p2, p3, 
  ncol = 3
)

# Save the combined plot with a wide and short aspect ratio
ggsave("horizontal_panel_plot.png", combined_plot, width = 24, height = 8, dpi = 300)
