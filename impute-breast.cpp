#include <Rcpp.h>
using namespace Rcpp;

//Generate OR with CI for race
NumericVector generate_OR_race() {
  // Define the log-transformed CI bounds for each entry
  double log_lower_bound1 = log(1.17);
  double log_upper_bound1 = log(3.62);
  
  double log_lower_bound2 = log(1.00);
  double log_upper_bound2 = log(3.81);
  
  double log_lower_bound3 = log(0.90);
  double log_upper_bound3 = log(3.48);
  
  // Sample from a uniform distribution within the log-transformed bounds
  double log_sample1 = R::runif(log_lower_bound1, log_upper_bound1);
  double log_sample2 = R::runif(log_lower_bound2, log_upper_bound2);
  double log_sample3 = R::runif(log_lower_bound3, log_upper_bound3);
  
  // Exponentiate to get the OR in the original scale
  double OR_sample1 = exp(log_sample1);
  double OR_sample2 = exp(log_sample2);
  double OR_sample3 = exp(log_sample3);
  
  // Directly create the OR_race vector with the generated OR values
  NumericVector OR_race = {OR_sample1, 1, OR_sample2, OR_sample3};
  
  return OR_race;
}

//Generate OR with CI for children
NumericVector generate_OR_children() {
  // Define the log-transformed CI bounds for each entry
  double log_lower_bound1 = log(1.04);
  double log_upper_bound1 = log(2);
  
  double log_lower_bound2 = log(0.75);
  double log_upper_bound2 = log(1.26);
  
  double log_lower_bound3 = log(0.53);
  double log_upper_bound3 = log(0.93);
  
  // Sample from a uniform distribution within the log-transformed bounds
  double log_sample1 = R::runif(log_lower_bound1, log_upper_bound1);
  double log_sample2 = R::runif(log_lower_bound2, log_upper_bound2);
  double log_sample3 = R::runif(log_lower_bound3, log_upper_bound3);
  
  // Exponentiate to get the OR in the original scale
  double OR_sample1 = exp(log_sample1);
  double OR_sample2 = exp(log_sample2);
  double OR_sample3 = exp(log_sample3);
  
  // Directly create the OR_children vector with the generated OR values
  NumericVector OR_children = {OR_sample1, OR_sample2, OR_sample3};
  
  return OR_children;
}

//Generate OR with CI for breast density
NumericVector generate_OR_density() {
  // Define the log-transformed CI bounds for each entry
  double log_lower_bound1 = log(1.09);
  double log_upper_bound1 = log(2.21);
  
  double log_lower_bound2 = log(1.72);
  double log_upper_bound2 = log(3.39);
  
  double log_lower_bound3 = log(2.41);
  double log_upper_bound3 = log(4.77);
  
  // Sample from a uniform distribution within the log-transformed bounds
  double log_sample1 = R::runif(log_lower_bound1, log_upper_bound1);
  double log_sample2 = R::runif(log_lower_bound2, log_upper_bound2);
  double log_sample3 = R::runif(log_lower_bound3, log_upper_bound3);
  
  // Exponentiate to get the OR in the original scale
  double OR_sample1 = exp(log_sample1);
  double OR_sample2 = exp(log_sample2);
  double OR_sample3 = exp(log_sample3);
  
  // Directly create the OR_density vector with the generated OR values
  NumericVector OR_density = {OR_sample1, OR_sample2, OR_sample3};
  
  return OR_density;
}

//Generate OR with CI for bmi
NumericVector generate_OR_bmi() {
  // Define the log-transformed CI bounds for each entry
  double log_lower_bound1 = log(0.32);
  double log_upper_bound1 = log(0.92);
  
  double log_lower_bound2 = log(1.18);
  double log_upper_bound2 = log(1.78);
  
  double log_lower_bound3 = log(1.30);
  double log_upper_bound3 = log(2.55);
  
  double log_lower_bound4 = log(0.87);
  double log_upper_bound4 = log(3.44);
  // Sample from a uniform distribution within the log-transformed bounds
  double log_sample1 = R::runif(log_lower_bound1, log_upper_bound1);
  double log_sample2 = R::runif(log_lower_bound2, log_upper_bound2);
  double log_sample3 = R::runif(log_lower_bound3, log_upper_bound3);
  double log_sample4 = R::runif(log_lower_bound4, log_upper_bound4);
  // Exponentiate to get the OR in the original scale
  double OR_sample1 = exp(log_sample1);
  double OR_sample2 = exp(log_sample2);
  double OR_sample3 = exp(log_sample3);
  double OR_sample4 = exp(log_sample4);
  // Directly create the OR_bmi vector with the generated OR values
  NumericVector OR_bmi = {OR_sample1, OR_sample2, OR_sample3, OR_sample4};
  
  return OR_bmi;
}

// [[Rcpp::export]]
int schedule_breastcancer(int index, NumericVector bmi_trajectory, int breast_density, int age, int year_now, int age_first_child, int number_children, int breastcancer)
{
  int race = index / 2;
  double absrisk;
  double alpha_screen = 0.35;
  double sensitivity = 0.65;
  int year_breastcancer = breastcancer;
  // double mammogram_sensitivity = 0.8;
  int cohort = year_now - age; //birthyear
  
  // Mean and SD for disease onset
  double mean_onset_age = 47.4809643;
  double sd_onset_age = 9.568645;
  
  // OR by race
  
  NumericVector OR_race = generate_OR_race();
  
  NumericVector Transitionyears = NumericVector::create( 0.1447161,  0.2072905,  0.2645585,  0.9053595);
  
  if (year_breastcancer == 0) {
    // Calculate the absolute yearly risk based on the normal distribution
    double cdf_age = R::pnorm(age, mean_onset_age, sd_onset_age, 1, 0);
    double cdf_next_age = R::pnorm(age + 1, mean_onset_age, sd_onset_age, 1, 0);
    absrisk = cdf_next_age - cdf_age;
    
    // Scaling factor to adjust the absolute risk to 0.08% at age 50
    double baseline_risk_50 = 0.0008; // Baseline 1-year risk at age 50
    double cdf_50 = R::pnorm(50, mean_onset_age, sd_onset_age, 1, 0);
    double cdf_51 = R::pnorm(51, mean_onset_age, sd_onset_age, 1, 0);
    double baseline_pdf_50 = cdf_51 - cdf_50;
    double scaling_factor = baseline_risk_50 / baseline_pdf_50;
    // Adjust the initial absrisk
    absrisk = absrisk * scaling_factor;
    // Convert absolute risk to odds
    double odds = absrisk / (1 - absrisk);
    
    // Adjust odds based on race (using OR)
    odds = odds * OR_race[race]; 
    
    // // Adjust absrisk based on other factors
    // if (age > 60 && age < 80){
    //   absrisk = absrisk * 7 / 6;
    // } else if (age < 45 && age > 39){
    //   absrisk = absrisk * 11 / 18;
    // } else if (age < 40 && age > 34){
    //   absrisk = absrisk * 1 / 3;
    // } else if (age < 35 && age > 29){
    //   absrisk = absrisk * 0.12;
    // } else if (age < 30 && age > 24){
    //   absrisk = absrisk * 0.03;
    // } else if (age < 25 && age > 19){
    //   absrisk = absrisk * 0.01;
    // } else if (age < 20){
    //   absrisk = absrisk * 0.001;
    // } else if (age > 84){
    //   absrisk = absrisk * 5 / 6;
    // }
    
    if (age_first_child > 30 || age_first_child == 0) {
      // absrisk = absrisk * 1.51;
      double log_lower_bound = log(0.84);
      double log_upper_bound = log(1.59);
      
      // Exponentiate to get the sampled odds ratio in the original scale
      double OR_agechild = exp(R::runif(log_lower_bound, log_upper_bound));
      
      // Adjust the odds by the sampled odds ratio
      odds = odds * OR_agechild;
    }
    NumericVector OR_children = generate_OR_children();
    if (number_children == 0){
      // absrisk = absrisk*1.53;
      odds = odds*OR_children[0];
    } else if (number_children == 3 || number_children == 4) {
      // absrisk = absrisk * 0.88;
      odds = odds*OR_children[1];
    } else if (number_children >= 5) {
      // absrisk = absrisk * 0.54;
      odds = odds*OR_children[2];
    } 
    NumericVector OR_density = generate_OR_density();
    if (breast_density < 12.3) {
      odds = odds;
    } else if (breast_density < 18.24) {
      odds = (odds) * OR_density[0];
    } else if (breast_density < 26.03) {
      odds = (odds) * OR_density[1];
    } else {
      odds = (odds) * OR_density[2];
    }
    
    // 
    double bmi_now = bmi_trajectory[age];
    NumericVector OR_bmi = generate_OR_bmi();
    if (bmi_now < 18.5){
      // absrisk = absrisk * 0.73;
      odds = odds*OR_bmi[0];
    } else if (bmi_now < 30 && bmi_now > 24.9){
      // absrisk = absrisk * 1.13;
      odds = odds*OR_bmi[1];
    } else if (bmi_now > 35){
      // absrisk = absrisk * 1.11;
      odds = odds*OR_bmi[2];
    } else if (bmi_now > 30 && bmi_now < 35){
      // absrisk = absrisk * 1.23;
      odds = odds*OR_bmi[3];
    }
    
    absrisk = odds / (1 + odds);
    // Check if breast cancer occurs this year based on calculated absrisk
    if (Rcpp::runif(1)[0] < absrisk) {
      // if (Rcpp::runif(1)[0] < alpha_screen*sensitivity) {
      year_breastcancer = year_now; //update year
      // }
    }
  }
  return year_breastcancer;
}

// [[Rcpp::export]]
int schedule_stage1(int year_now) {
  // Transition probabilities for each stage
  Rcpp::NumericVector Transitionyears = Rcpp::NumericVector::create(0.1447161,  0.2072905,  0.2645585,  0.9053595);
  int year_stage1 = 0;
  double random_prob_1 = Rcpp::runif(1)[0];
  double random_prob_2 = Rcpp::runif(1)[0];
  double alpha_screen = 0.35;
  double sensitivity = 0.65;
  // Check if transition should occur
  if (random_prob_1 < Transitionyears[0]) {
    // if (random_prob_2 < alpha_screen*sensitivity){
    year_stage1 = year_now; //replace this with a placeholder where it is hidden but still progresses.
    // } else {
    //   year_stage1 = std::stoi(std::to_string(year_now) + "0");
    // }
  }
  return year_stage1;
}

// [[Rcpp::export]]
int schedule_stage2(int year_now, int stage1) {
  // Transition probabilities for each stage
  Rcpp::NumericVector Transitionyears = Rcpp::NumericVector::create(0.1447161,  0.2072905,  0.2645585,  0.9053595);
  int year_stage2 = 0;
  double random_prob_1 = Rcpp::runif(1)[0];
  double random_prob_2 = Rcpp::runif(1)[0];
  double alpha_screen = 0.35;
  double sensitivity = 0.73;
  // Check if transition should occur
  if (random_prob_1 < Transitionyears[1]) {
    // if (std::to_string(stage1).length() > 4){ //undetected
    //   // if (random_prob_2 < alpha_screen*sensitivity){
    //     year_stage2 = year_now;
    //   } else {
    //     year_stage2 = std::stoi(std::to_string(year_now) + "0");
    //   }
    // } else{
    year_stage2 = year_now;
    //     }
  }
  return year_stage2;
}

// [[Rcpp::export]]
int schedule_stage3(int year_now, int stage2) {
  // Transition probabilities for each stage
  Rcpp::NumericVector Transitionyears = Rcpp::NumericVector::create(0.1447161,  0.2072905,  0.2645585,  0.9053595);
  int year_stage3 = 0;
  double random_prob_1 = Rcpp::runif(1)[0];
  double random_prob_2 = Rcpp::runif(1)[0];
  double alpha_screen = 0.35;
  double sensitivity = 0.38;
  // Check if transition should occur
  if (random_prob_1 < Transitionyears[2]) {
    // if (std::to_string(stage2).length() > 4){ //undetected
    //   if (random_prob_2 < alpha_screen*sensitivity){
    //     year_stage3 = year_now;
    //   } else {
    //     year_stage3 = std::stoi(std::to_string(year_now) + "0");
    //   }
    // } else{
    year_stage3 = year_now;
    // }
  }
  return year_stage3;
}

// [[Rcpp::export]]
int schedule_stage4(int year_now, int stage3) {
  // Transition probabilities for each stage
  Rcpp::NumericVector Transitionyears = Rcpp::NumericVector::create(0.1447161,  0.2072905,  0.2645585,  0.9053595);
  int year_stage4 = 0;
  double random_prob_1 = Rcpp::runif(1)[0];
  double random_prob_2 = Rcpp::runif(1)[0];
  double alpha_screen = 0.35;
  double sensitivity = 0.23;
  // Check if transition should occur
  if (random_prob_1 < Transitionyears[3]) {
    // if (std::to_string(stage3).length() > 4){ //undetected
    //   if (random_prob_2 < alpha_screen*sensitivity){
    //     year_stage4 = year_now;
    //   } else {
    //     year_stage4 = std::stoi(std::to_string(year_now) + "0");
    //   }
    // } else{
    year_stage4 = year_now;}
  return year_stage4;
}
