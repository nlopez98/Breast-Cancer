#include <Rcpp.h>
#include <random>
using namespace Rcpp;
using namespace std;

// Global variable to store if death year was due to breast cancer
int year_bc_death = 0;

// [[Rcpp::export]]
int schedule_death(NumericVector individualdata, int age, int year_now, 
                   double basemortality, 
                   NumericMatrix amimort, NumericMatrix strokemort, NumericMatrix ckd5mort,
                   int index, int year_diabetes, int year_ami, int year_stroke, int year_emigrated) 
{
  int year_of_death = 0;
  year_bc_death = individualdata[207];
  int gender, race;
  double deathprob = R::runif(0.0, 1.0);
  double nonAMImortratio = 1.0;
  NumericVector mortalityrates(10);
  NumericVector HRt2dm = NumericVector::create(1.31, 3.01, 4.27, 1.31);

  // -------------------------------
  // Year factor
  // -------------------------------
  double year_factor;
  if (year_now >= 2013) {
    year_factor = 1.0;
  } else if (year_now >= 2003 && year_now <= 2012) {
    year_factor = (1.0 - 0.797) / (1.0 - 0.817);
  } else {
    year_factor = (1.0 - 0.755) / (1.0 - 0.817);
  }

  // -------------------------------
  // Prop factor (5-year buckets)
  // -------------------------------
  double prop_factor;
  if (year_now <= 1972) {
    prop_factor = 1.0 - 0.774395;
  } else if (year_now <= 1977) {
    prop_factor = 1.0 - 0.758405;
  } else if (year_now <= 1982) {
    prop_factor = 1.0 - 0.704844;
  } else if (year_now <= 1987) {
    prop_factor = 1.0 - 0.691202;
  } else if (year_now <= 1992) {
    prop_factor = 1.0 - 0.611708;
  } else {
    prop_factor = 1.0 - 0.611708; // copy last factor for >2017
  }

  // --- Base mortality
  if(age >= 40 && individualdata[4] == 0) {
    mortalityrates[0] = nonAMImortratio * basemortality;
  } else {
    mortalityrates[0] = basemortality;
  }
  
  // --- Diabetes mortality
  if(individualdata[4] > 0) {
    race = int(index/2);
    mortalityrates[1] = 1 - pow(1 - basemortality, HRt2dm[race]);
  }
  
  // --- Breast cancer mortality (stages)
  if(individualdata[199] > 0) {
    if(individualdata[202] > 0 && individualdata[202] <= year_now){
      if(individualdata[203] > 0 && individualdata[203] <= year_now){
        if(individualdata[204] > 0 && individualdata[204] <= year_now){
          if(individualdata[205] > 0 && individualdata[205] <= year_now){
            mortalityrates[2] = 1 - exp(log(0.29) / 5.0);   // stage 4
          //  mortalityrates[0] = mortalityrates[2] * prop_factor * year_factor;
          } else {
            mortalityrates[2] = 1 - exp(log(0.7) / 5.0);    // stage 3
           // mortalityrates[0] = mortalityrates[2] * prop_factor * year_factor;
          }
        } else {
          mortalityrates[2] = 1 - exp(log(0.88) / 5.0);     // stage 2
         // mortalityrates[0] = mortalityrates[2] * prop_factor * year_factor;
        }
      } else {
        mortalityrates[2] = 1 - exp(log(0.96) / 5.0);       // stage 1
      //  mortalityrates[0] = mortalityrates[2] * prop_factor * year_factor;
      }
    } else {
      mortalityrates[2] = 0;  // stage 0
    }
  } else {
    mortalityrates[2] = 0.0;  // no BC mortality
  }
  
  // --- Determine year_of_death (only from mortalityrates[0:1]) ---
  double max_mort_01 = std::max(mortalityrates[0], mortalityrates[1]);
  if (deathprob < max_mort_01) {
    year_of_death = year_now;
  }
  if (year_bc_death==0){
    
    // --- Determine year_bc_death (from mortalityrates[0:2]) ---
    double max_mort_012 = std::max({mortalityrates[0], mortalityrates[1], mortalityrates[2]});
    if (deathprob < max_mort_012) {
      year_bc_death = year_now;
    }
  }
  
  return year_of_death;
}

// [[Rcpp::export]]
int get_year_bc_death() {
  return year_bc_death;
}
