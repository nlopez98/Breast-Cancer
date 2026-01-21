  library(dplyr)
  library(actuar)
  
  # ---------------------------
  # Data & constants
  # ---------------------------
  n_patients <- 42533
  set.seed(888)
  
  # Year buckets and probabilities
  year_buckets <- seq(1968, 2017, by = 5)
  bucket_probs <- c(0.016,0.021,0.029,0.043,0.062,0.092,0.130,0.159,0.198,0.250)
  
  # Ethnicity and stage probabilities
  ethnicity_prob <- c(0.82,0.06,0.1,0.02)
  stage_prob <- c(0.33,0.39,0.18,0.1)
  
  # ---------------------------
  # Sample patient characteristics
  # ---------------------------
  # Diagnosis year
  year_bucket_indices <- sample(1:length(bucket_probs), n_patients, replace = TRUE, prob = bucket_probs)
  year_buckets_selected <- year_buckets[year_bucket_indices]
  diagnosis_year <- year_buckets_selected + sample(0:4, n_patients, replace = TRUE)
  
  # Stage and ethnicity
  stages <- sample(1:4, n_patients, replace = TRUE, prob = stage_prob)
  ethnicity <- sample(1:4, n_patients, replace = TRUE, prob = ethnicity_prob)
  
  # ---------------------------
  # Age at diagnosis
  # ---------------------------
  
  ####replace with the correct distributions from fitnrdo.R
  age_generators <- list(
    function(n) rgamma(n, shape = 18.45, rate = 0.33),    # Stage 1: Gamma
    function(n) rlnorm(n, meanlog = 4.01, sdlog = 0.23), # Stage 2: Lnorm
    function(n) rgamma(n, shape = 19.40, rate = 0.34),   # Stage 3: Gamma
    function(n) rnorm(n, mean = 59.10, sd = 12.50)       # Stage 4: Norm
  )
  age_at_diagnosis <- numeric(n_patients)
  for (s in 1:4) {
    idx <- which(stages == s)
    if(length(idx) > 0) {
      age_at_diagnosis[idx] <- age_generators[[s]](length(idx))
    }
  }
  age_at_diagnosis <- as.integer(pmin(pmax(age_at_diagnosis,10),120))
  birth_year <- diagnosis_year - round(age_at_diagnosis)
  
  simulated_data <- data.frame(
    Cohort = birth_year,
    Age    = age_at_diagnosis,
    Stage  = stages,
    Race   = factor(ethnicity, labels=c("CN","IN","MY","XX")),
    DiagnosisYear = diagnosis_year
  )
  
  data_list <- data.frame(
    Age = simulated_data$Age,
    Cohort = simulated_data$Cohort,
    Stage = simulated_data$Stage,
    DiagnosisYear = simulated_data$DiagnosisYear,
    N = nrow(simulated_data)
  )
  
  # ---------------------------
  # Constants
  # ---------------------------
  p_capture_5y <- c(0.04,0.12,0.30,0.71)
  p_capture <- 1 - (1 - p_capture_5y)^(1/5)
  
  # Time-varying multipliers by diagnosis year
  get_capture_factor <- function(diag_year){
    if(diag_year >= 2013){
      return(1)
    } else if(diag_year >= 2003 && diag_year <= 2012){
      return((1-0.797)/(1-0.817))
    } else {
      return((1-0.755)/(1-0.817))
    }
  }
  
  actual_deaths_last5 <- c(1281, 1755, 2209, 2853, 3536)
  
  # ---------------------------
  # Prior helpers (transitions)
  # ---------------------------
  dprior_trans <- function(t){
    if(length(t) != 3) return(0)
    if(any(is.na(t)) || any(t < 1) || any(t > 10)) return(0)
    if(!(t[1] >t[2] && t[2] > t[3])) return(0)
    d <- dnorm(t[1], mean=4, sd=4) * dnorm(t[2], mean=3, sd=4) * dnorm(t[3], mean=2, sd=4)
    return(d)
  }
  
  # ---------------------------
  # Simulator (stage-specific, caught only)
  # ---------------------------
  simulate_stage_counts <- function(trans_candidate){
    stage_age <- matrix(NA, nrow=n_patients, ncol=4)
    years_in_stage <- matrix(NA, nrow=n_patients, ncol=4)
    
    for(i in 1:n_patients){
      s <- data_list$Stage[i]
      age <- data_list$Age[i]
      
      # Age at start of each stage
      stage_age[i,1] <- age - sum(trans_candidate[1:(s-1)]) * (s>1)
      if(s >= 2) stage_age[i,2] <- stage_age[i,1] + trans_candidate[1]
      if(s >= 3) stage_age[i,3] <- stage_age[i,2] + trans_candidate[2]
      stage_age[i,4] <- if(s==4) age else if(!is.na(stage_age[i,3])) stage_age[i,3] + trans_candidate[3] else NA
      
      # Years spent in each stage
      for(j in 1:4){
        if(!is.na(stage_age[i,j])){
          yrs_mean_vec <- c(trans_candidate, trans_candidate[3])
          years_in_stage[i,j] <- max(1, rnorm(1, mean=yrs_mean_vec[j], sd=1))
        }
      }
    }
    
    death_year_sim <- numeric(n_patients)
    
    for(i in 1:n_patients){
      s <- data_list$Stage[i]
      ages <- stage_age[i,]
      yrs  <- years_in_stage[i,]
      current_age <- ages[1]
      
      # Adjusted capture probability based on diagnosis year
      diag_year <- data_list$DiagnosisYear[i]
      factor <- get_capture_factor(diag_year)
      p_adj <- min(0.999, p_capture[s] * factor)
      
      for(j in 1:4){
        if(is.na(current_age)) next
        if(j < s){
          current_age <- current_age + yrs[j]  # just advance age
        } else {
          d <- rgeom(1, prob=p_adj) + 1
          death_year_sim[i] <- round(data_list$Cohort[i] + current_age + d)
          break
        }
      }
    }
    
    counts <- sapply(seq(1993,2017,by=5), function(start){
      sum(death_year_sim >= start & death_year_sim <= start+4)
    })
    return(counts)
  }
  
  # ---------------------------
  # MCMC settings
  # ---------------------------
  n_iter <- 2000
  prop_sd_t <- c(4,4,4)
  likelihood_sigma <- 100
  chain_trans <- matrix(NA, nrow=n_iter, ncol=3)
  accept_count <- 0
  
  sample_trans_init <- function(){
    repeat {
      t <- rnorm(3, mean=c(4,3,2), sd=4)
      t <- pmax(t,1)
      t <- pmin(t, 10)
      if(t[1] > t[2] && t[2] > t[3]) return(t)
    }
  }
  
  current_t <- sample_trans_init()
  current_sim_counts <- simulate_stage_counts(current_t)
  rss_current <- sum((current_sim_counts - actual_deaths_last5)^2)
  loglik_current <- - rss_current / (2 * likelihood_sigma^2)
  logprior_current <- log(dprior_trans(current_t) + 1e-300)
  
  # ---------------------------
  # MH loop with print every 50 iters
  # ---------------------------
  set.seed(999)
  for(iter in 1:n_iter){
    if(iter %% 50 == 0){
      cat("iter:", iter, "accepted:", accept_count, "\ncurrent_t:", round(current_t,2), 
          "sim:", current_sim_counts, "\n")
    }
    
    proposed_t <- current_t + rnorm(3, mean=0, sd=prop_sd_t)
    proposed_t <- pmax(proposed_t,1)
    proposed_t <- sort(proposed_t, decreasing=TRUE)
    
    if(dprior_trans(proposed_t)==0){
      chain_trans[iter,] <- current_t
      next
    }
    
    proposed_sim_counts <- simulate_stage_counts(proposed_t)
    rss_proposed <- sum((proposed_sim_counts - actual_deaths_last5)^2)
    loglik_proposed <- - rss_proposed / (2 * likelihood_sigma^2)
    logprior_proposed <- log(dprior_trans(proposed_t) + 1e-300)
    
    log_accept_ratio <- (loglik_proposed + logprior_proposed) - (loglik_current + logprior_current)
    
    if(log(runif(1)) < log_accept_ratio){
      current_t <- proposed_t
      current_sim_counts <- proposed_sim_counts
      rss_current <- rss_proposed
      loglik_current <- loglik_proposed
      logprior_current <- logprior_proposed
      accept_count <- accept_count + 1
    }
    
    chain_trans[iter,] <- current_t
  }
  
  cat("Acceptance rate:", accept_count / n_iter, "\n")
  
  # ---------------------------
  # Posterior summaries
  # ---------------------------
  burnin <- floor(n_iter * 0.2)
  post_t <- chain_trans[(burnin+1):n_iter, , drop=FALSE]
  
  summary_t <- data.frame(
    mean = colMeans(post_t),
    sd   = apply(post_t, 2, sd),
    q2.5 = apply(post_t, 2, quantile, probs=0.025),
    q50  = apply(post_t, 2, quantile, probs=0.5),
    q97.5= apply(post_t, 2, quantile, probs=0.975)
  )
  summary_t
  saveRDS(summary_t, "mcmc_transition.rds")
