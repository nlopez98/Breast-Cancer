#include <Rcpp.h>
using namespace Rcpp;


// New values for spline_OR
std::vector<double> spline_OR = {
  0.6294134, 0.6897204, 0.7500274, 0.8103344, 0.8706414, 0.9309485, 0.9912555, 1.0491533, 1.1019013, 1.1518142, 1.2012845,
  1.2527047, 1.3084672, 1.3709644, 1.4425890, 1.5257332, 1.6272523, 1.7544204, 1.8972140, 2.0448757, 2.1866478, 2.3117730,
  2.4094938, 2.4831695, 2.5516597, 2.6154597, 2.6747542, 2.7297281, 2.7805660, 2.8274526, 2.8705728, 2.9101112, 2.9462526,
  2.9791818, 3.0090834, 3.0361421, 3.0605428, 3.0824702, 3.1021090, 3.1196440, 3.1352598, 3.1491412, 3.1614731, 3.1724400,
  3.1822267, 3.1910180, 3.1989986, 3.2063533, 3.2132668, 3.2199237, 3.2265090, 3.2332072, 3.2402032, 3.2476816, 3.2558273,
  3.2648249, 3.2748592, 3.2861149, 3.2987768, 3.3130295, 3.3290579, 3.3470467, 3.3671806, 3.3896444, 3.4133721, 3.4371000,
  3.4608280, 3.4845560, 3.5082840, 3.5320120, 3.5557400, 3.5794680, 3.6031959, 3.6269239, 3.6506519, 3.6743799, 3.6981079,
  3.7218359, 3.7455638, 3.7692918, 3.7930198, 3.8167478, 3.8404758, 3.8642038, 3.8879318, 3.9116597, 3.9353877, 3.9591157,
  3.9828437, 4.0065717, 4.0302997, 4.0540276, 4.0777556, 4.1014836, 4.1252116, 4.1489396, 4.1726676, 4.1963955, 4.2201235,
  4.2438515, 4.2675795
};

// New values for lower bound
std::vector<double> spline_lower = {
  0.9652985, 0.9709456, 0.9765927, 0.9822398, 0.9878869, 0.9935341, 0.9991812, 1.0040791, 1.0075681, 1.0107031, 1.0145639,
  1.0202307, 1.0287835, 1.0413024, 1.0588673, 1.0825584, 1.1233122, 1.2006382, 1.3032598, 1.4184574, 1.5335115, 1.6357026,
  1.7123113, 1.7656066, 1.8150313, 1.8610459, 1.9037847, 1.9433818, 1.9799718, 2.0136889, 2.0446674, 2.0730417, 2.0989460,
  2.1225147, 2.1438821, 2.1631826, 2.1805504, 2.1961199, 2.2100253, 2.2224011, 2.2333815, 2.2431009, 2.2516935, 2.2592938,
  2.2660359, 2.2720543, 2.2774832, 2.2824571, 2.2871101, 2.2915766, 2.2959910, 2.3004875, 2.3052005, 2.3102643, 2.3158133,
  2.3219816, 2.3289038, 2.3367140, 2.3455466, 2.3555360, 2.3668164, 2.3795221, 2.3937876, 2.4097470, 2.4266255, 2.4435042,
  2.4603828, 2.4772615, 2.4941402, 2.5110188, 2.5278975, 2.5447762, 2.5616548, 2.5785335, 2.5954122, 2.6122909, 2.6291695,
  2.6460482, 2.6629269, 2.6798055, 2.6966842, 2.7135629, 2.7304415, 2.7473202, 2.7641989, 2.7810775, 2.7979562, 2.8148349,
  2.8317136, 2.8485922, 2.8654709, 2.8823496, 2.8992282, 2.9161069, 2.9329856, 2.9498642, 2.9667429, 2.9836216, 3.0005002,
  3.0173789, 3.0342576
};

// New values for upper bound
std::vector<double> spline_upper = {
  0.1847094, 0.3173849, 0.4500603, 0.5827357, 0.7154112, 0.8480866, 0.9807621, 1.1120179, 1.2402391, 1.3667897, 1.4930795,
  1.6205183, 1.7505158, 1.8844819, 2.0238263, 2.1699587, 2.3296351, 2.5116614, 2.7056244, 2.9002676, 3.0843346, 3.2465689,
  3.3757142, 3.4763876, 3.5701721, 3.6576668, 3.7391197, 3.8147784, 3.8848908, 3.9497044, 4.0094671, 4.0644265, 4.1148305,
  4.1609268, 4.2029631, 4.2411871, 4.2758466, 4.3071893, 4.3354629, 4.3609152, 4.3837940, 4.4043469, 4.4228218, 4.4394662,
  4.4545280, 4.4682550, 4.4808948, 4.4926951, 4.5039038, 4.5147686, 4.5255371, 4.5364572, 4.5477766, 4.5597429, 4.5726040,
  4.5866076, 4.6020014, 4.6190332, 4.6379506, 4.6590015, 4.6824335, 4.7084945, 4.7374321, 4.7694940, 4.8032510, 4.8370083,
  4.8707657, 4.9045230, 4.9382803, 4.9720377, 5.0057950, 5.0395523, 5.0733097, 5.1070670, 5.1408244, 5.1745817, 5.2083390,
  5.2420964, 5.2758537, 5.3096111, 5.3433684, 5.3771257, 5.4108831, 5.4446404, 5.4783977, 5.5121551, 5.5459124, 5.5796698, 
  5.6134271, 5.6471844, 5.6809418, 5.7146991, 5.7484565, 5.7822138, 5.8159711, 5.8497285, 5.8834858, 5.9172432, 5.9510005, 
  5.9847578, 6.0185152
  
};

// Generate RR with CI for race
NumericVector generate_RR_race() {
  // Malay RR CI: 0.54–1.01
  double log_lower_bound_malay = log(0.54);
  double log_upper_bound_malay = log(1.01);
  
  // Indian RR CI: 0.63–1.4
  double log_lower_bound_indian = log(0.63);
  double log_upper_bound_indian = log(1.40);
  
  // Sample from uniform distribution within log-transformed bounds
  double log_sample_malay = R::runif(log_lower_bound_malay, log_upper_bound_malay);
  double log_sample_indian = R::runif(log_lower_bound_indian, log_upper_bound_indian);
  
  // Exponentiate to get RR values
  double RR_malay = exp(log_sample_malay);
  double RR_indian = exp(log_sample_indian);
  
  // Base ethnicity Chinese = 1.0
  NumericVector RR_race = {1.0/RR_malay, 1.0, RR_indian/RR_malay};
  return RR_race;
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

double generate_OR_density(double density) {
  // Round the density value to nearest integer
  int index = std::round(density);
  
  
  
  // Get log-transformed bounds
  double log_lower = std::log(spline_lower[index]);
  double log_upper = std::log(spline_upper[index]);

  
  // Sample uniformly in log-space between lower and upper
  double log_sample = R::runif(log_lower, log_upper);
  
  // Return sample in original scale
  return std::exp(log_sample);
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
  double shape_age = 18.45;
  double rate_age =  0.33;
  
  // OR by race
  
  NumericVector RR_race = generate_RR_race();
  
  
  //NumericVector Transitionyears = NumericVector::create(0.127, 0.143, 0.159, 0.99);
  
  if (year_breastcancer == 0) {
    // Calculate the absolute yearly risk based on the normal distribution
    double cdf_age = R::pgamma(age, shape_age, 1/rate_age, 1, 0);
    double cdf_next_age = R::pgamma(age + 1, shape_age, 1/rate_age, 1, 0);
    absrisk = cdf_next_age - cdf_age;
    
    // Scaling factor to adjust the absolute risk to 0.08% at age 50
    double baseline_risk_50 = 1-pow(1-0.008, 1.0/10); // Baseline 1-year risk at age 50, no longer can use paper, 1 guess
    double cdf_50 = R::pgamma(50, shape_age, 1/rate_age, 1, 0);
    double cdf_51 = R::pgamma(51, shape_age, 1/rate_age, 1, 0);
    double baseline_pdf_50 = cdf_51 - cdf_50;
    double scaling_factor = baseline_risk_50 / baseline_pdf_50;
    // Adjust the initial absrisk
    absrisk = absrisk * scaling_factor;
    
    // Adjust absolute risk based on race (using RR first)
    absrisk = absrisk * RR_race[race];
    
    // Convert adjusted absolute risk to odds
    double odds = absrisk / (1 - absrisk);
    
    
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
    
    //if (year_now > 1990){
      if (age > 30){
        if (age_first_child > 30) {
          // absrisk = absrisk * 1.51;
          double log_lower_bound = log(0.84);
          double log_upper_bound = log(1.59);
          
          // Exponentiate to get the sampled odds ratio in the original scale
          double OR_agechild = exp(R::runif(log_lower_bound, log_upper_bound));
          
          // Adjust the odds by the sampled odds ratio
          odds = odds * OR_agechild;
      }
      }
   // }
    
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
    
    
    double OR_density = generate_OR_density(breast_density);
    odds = odds*OR_density;
    
    // 
    double bmi_now = bmi_trajectory[age];
    
    NumericVector OR_bmi = generate_OR_bmi();
    if (bmi_now < 18.5){
      // absrisk = absrisk * 0.73;
      odds = odds*OR_bmi[0];
    } else if (bmi_now < 30 && bmi_now > 24.9){
      // absrisk = absrisk * 1.13;
      odds = odds*OR_bmi[1];
    } else if (bmi_now > 30 && bmi_now < 35){
      // absrisk = absrisk * 1.11;
      odds = odds*OR_bmi[2];
    } else if (bmi_now >= 35){
      // absrisk = absrisk * 1.23;
      odds = odds*OR_bmi[3];
    }
    
    absrisk = odds / (1 + odds);

    // Check if breast cancer occurs this year based on calculated absrisk
    if (Rcpp::runif(1)[0] < absrisk) {
      // if (Rcpp::runif(1)[0] < alpha_screen*sensitivity) {
      year_breastcancer = year_now; //update year
      if (age <20){Rcout << absrisk << std::endl;}

      // }
    }
  }
  return year_breastcancer;
}

// [[Rcpp::export]]
IntegerVector schedule_all_stages(int year_now, int age_now, int stage1_year, int index) { 
  IntegerVector stage_years(4);
  int symptom_year = 0;
  int current_year = stage1_year;
  int race = index / 2;

  // --- Race-specific symptom probabilities ---
  double p1, p2, p3, p4;
  if (race == 0) {
    p1 = 0.75; p2 = 0.92; p3 = 1.0; p4 = 1.0;
  } else if (race == 1) {
    p1 = 0.79; p2 = 0.93; p3 = 0.96; p4 = 1.0;
  } else { // race 3 or 4
    p1 = 0.71; p2 = 0.91; p3 = 0.95; p4 = 1.0;
  }

  // --- Stage 1 → 2 ---
  int stage2_year;
  if (year_now > 2017) { //adjustments due to change in coding,
  // Validation of the AJCC 8th prognostic system for breast cancer in an Asian healthcare setting
    stage2_year = current_year + std::max(1.0,
      std::round(Rcpp::rnorm(1, 1 / (1 / 4.896386 * (1 - 0.242)), 1.8114970 )[0]));
  } else {
    stage2_year = current_year + std::max(1.0,
      std::round(Rcpp::rnorm(1, 4.896386, 1.8114970)[0]));
  }
  stage_years[0] = stage2_year;

  // --- Stage 2 → 3 ---
  int stage3_year;
  if (year_now > 2017) {
    stage3_year = stage2_year + std::max(1.0,
      std::round(Rcpp::rnorm(1, 1 / (1 / 2.608200 * (1 - 0.052)), 1.2430230)[0]));
  } else {
    stage3_year = stage2_year + std::max(1.0,
      std::round(Rcpp::rnorm(1, 2.608200, 1.2430230)[0]));
  }
  stage_years[1] = stage3_year;

  // --- Stage 3 → 4 ---
  int stage4_year = stage3_year +
    std::max(1.0, std::round(Rcpp::rnorm(1, 1.410677, 0.7317032 )[0]));
  stage_years[2] = stage4_year;

  // --- Stage 4 persists beyond ---
  stage_years[3] = 0; // placeholder for symptom_year

  // --- Symptom timing (uniform within stage duration) ---
  double u = R::runif(0, 1);
  if (u < p1) {
    // Symptomatic during Stage 1
    symptom_year = current_year +
      std::round(R::runif(0, stage2_year - current_year));
  } else if (u < p2) {
    // Symptomatic during Stage 2
    symptom_year = stage2_year +
      std::round(R::runif(0, stage3_year - stage2_year));
  } else if (u < p3) {
    // Symptomatic during Stage 3
    symptom_year = stage3_year +
      std::round(R::runif(0, stage4_year - stage3_year));
  } else {
    // Symptomatic during Stage 4 (or after)
    symptom_year = stage4_year;
  }

  stage_years[3] = symptom_year;
  return stage_years;
}
