#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <impute-ami.cpp>
#include <impute-bmi.cpp>
#include <nsc_impact.cpp>
#include <impute-pa.cpp>
#include <impute-bp.cpp>
#include <impute-stroke.cpp>
#include <impute-esrd.cpp>
#include <impute-smoking.cpp>
#include <impute-diabetes.cpp>
#include <impute-cancer.cpp>
#include <impute-frailty.cpp>
//#include <impute-complications.cpp>
#include <impute-breast.cpp>
#include <impute-mortality.cpp>
#include <impute-fertility.cpp>
#include <calculate-econometrics.cpp>
#include <mortmod.cpp>
#include <impute-normal_hypertension.cpp>
#include <impute-highnormal_hypertension.cpp>
#include <impute-hypertensive_hypertension.cpp>
#include <random>
#include <omp.h>
using namespace Rcpp;

/*n_cond is the number of risk factor outputs
 0. YEAR OF DEATH
 1. Year of nomral hypertension
 2. Year_highnormal_hypertension
 3. Year_hypertensive_hypertension
 4. Year got diabetes
 5. Year got AMI
 6. Year got stroke
 7. Year got ESRD
 8 - 11 Reserved for hyperlipidemia
 12-72. BMI for years 1990 - 2050
 73-133. SBP for years 1990 - 2050
 134-194. DBP for years 1990 - 2050
 195-199. BRCA
 */

// [[Rcpp::export]]
NumericVector schedule_individual(int index, int yearborn, int yearimmigrated, int yearemigrated, int n_cond, 
                                  IntegerMatrix& birthmatrix, NumericMatrix& fertility_rates_new, arma::cube& mortality_cube, 
                                  NumericMatrix& bmitable, 
                                  NumericMatrix& bptable, 
                                  NumericMatrix& heightbmi,
                                  NumericMatrix& prefrailty,NumericMatrix& frailty,
                                  NumericVector& amiinc, NumericVector& strokeinc, 								
                                  NumericMatrix& amimort, NumericMatrix& strokemort, NumericMatrix& ckd5mort,
                                  bool nsc,
                                  int AGE_MAX,
                                  int YEAR_UNTIL
) 
  // Using reference types can be more efficient than copying large data structures. 
{
  
  
  
  //output for each individual
  int year_now, year_of_death, year_diagnosed_dm, year_treatment_dm, year_got_microalb, year_screendiag;
  int race, gender, fertilityindex, ageatdeath, temprace, agegrp;
  int number_children = 0;
  int age_first_child = 0;
  int have_child;
  double basemortality, basefertility;
  NumericVector individualdata(n_cond);
  
  
  //error term for diabetes calculation
  double dmerr = rnorm(1)[0];
  
  List bp;
  List pa;
  
  //calculation of BMI trajectory
  NumericVector bmi_trajectory(AGE_MAX+1);
  
  
  //calculation of Systolic Blood Pressure trajectory
  NumericVector sbp_trajectory(AGE_MAX+1);
  //calculation of Diastolic Blood Pressure trajectory
  NumericVector dbp_trajectory(AGE_MAX+1);
  
  
  //  male_chinese female_chinese     male_malay   female_malay    male_indian  female_indian     male_other   female_other 
  //         0              1              2              3              4              5              6              7 
  race = int(index/2); //0:chinese, 1:malay, 2:indian, 3:other
  gender = index % 2; //0:males, 1:females    /* Likely uses the result of the division. */
  
  
  bmi_trajectory = individual_bmi(bmitable, yearborn, index, AGE_MAX);
  
  
  if (nsc) {
    // NSC plug-in
    NumericVector steps_trajectory(AGE_MAX+1);
    NumericVector mvpa_trajectory(AGE_MAX+1);
    
    // NSC plug-in
    pa = individual_pa(yearborn, index, AGE_MAX);
    steps_trajectory = pa["steps_trajectory"];
    mvpa_trajectory = pa["mvpa_trajectory"] ; 
    bmi_trajectory = bmi_trajectory + nsc_impact(bmi_trajectory, steps_trajectory, mvpa_trajectory);
  }
  
  bp = individual_bp(bmi_trajectory, bptable, yearborn, index, AGE_MAX);
  sbp_trajectory = bp["sbp_trajectory"];
  dbp_trajectory = bp["dbp_trajectory"] ;    
  
  
  
  
  // write bmi trajectory
  /*for(int index=0; index!=120; ++index){
   cout<<bmi_trajectory[index]<<", ";
  }
   cout<<endl;*/
  
  //From this point on, need to operate on a yearly scale to account for mortality
  for(int age = 0; age < AGE_MAX; ++age)
  {
    year_now = yearborn + age;
    // modify this to change the year to project to
    if(year_now > YEAR_UNTIL) break;
    
    //allow for development of chronic diseases past age 18
    if(age >= 18)
    {
      // hypertension 
      // currently is a standalone module; does not interact with other healthoutcomes
      if(individualdata[1] == 0 && individualdata[2] == 0 && individualdata[3] == 0)
      {
        individualdata[1] = schedule_normal_hypertension(sbp_trajectory, dbp_trajectory, year_now, age, index);
      }
      
      if(individualdata[2] == 0 && individualdata[3] == 0)
      {
        individualdata[2] = schedule_highnormal_hypertension(sbp_trajectory, dbp_trajectory, year_now, age, index);
      }
      
      
      if(individualdata[3] == 0)
      {
        individualdata[3] = schedule_hypertensive_hypertension(sbp_trajectory, dbp_trajectory, year_now, age, index);
      } 
      
      //diabetes
      if(individualdata[4] == 0)
      {
        individualdata[4] = schedule_diabetes(bmi_trajectory, year_now, age, index, dmerr);
      }
      
      // currently is a standalone module; does not interact with other healthoutcomes
      
      int year_got_dm = individualdata[4];
      
      if(individualdata[5]==0){
        individualdata[5]=schedule_ami(year_got_dm, yearborn, year_now, index, amiinc);
      }
      
      
      if(individualdata[6]==0){
        individualdata[6]=schedule_stroke(year_got_dm, yearborn, year_now, index, strokeinc);
      }
      
      if(individualdata[8]==0){
        individualdata[8]=schedule_prefrailty(prefrailty,year_got_dm, yearborn, year_now, index );
      }
      
      int yearPF = individualdata[8];
      
      if(individualdata[9]==0){
        individualdata[9]=schedule_frailty(frailty,year_got_dm, yearborn, year_now, index, yearPF);
      }
      
      if (gender == 1){
        std::default_random_engine gen(std::random_device{}());
        std::normal_distribution<double> menopause_age(49.0, (0.43 / 1.96) * std::sqrt(656)); //from paper
        if (individualdata[197] == 0 && std::round(menopause_age(gen)) == age) {
          individualdata[197] = 1;
          individualdata[206] = individualdata[198];
          if (individualdata[197] == 1){
           // individualdata[198] = Rcpp::rnorm(1, 18.85, 10.19)[0];
            // double sd;
            // switch(race)
            // {
            // case 0:
            //   sd = 0.5*(25.5-21.1)/1.96*sqrt(85);
            //   individualdata[198] = Rcpp::rnorm(1, 23.3, sd)[0];
            //   break;
            // case 1:
            //   sd = 0.5*(22.5-16.5)/1.96*sqrt(42);
            //   individualdata[198] = Rcpp::rnorm(1, 19.5, sd)[0];
            //   break;
            // case 2: 
            //   sd = 0.5*(21.9-18)/1.96*sqrt(101);
            //   individualdata[198] = Rcpp::rnorm(1, 20, sd)[0];
            //   break;
            // case 3:
            //   sd = 0.5*(21.9-18)/1.96*sqrt(101);
            //   individualdata[198] = Rcpp::rnorm(1, 20, sd)[0];
            //   break;
            // }
          }
          
        }
        if (individualdata[198] == 0){
          // if (individualdata[197] == 0){
          //   double sd;
          //   switch(race)
          //   {
          //   case 0: 
          //     sd = 0.5*(34.3-30.3)/1.96*sqrt(120);
          //     individualdata[198] = Rcpp::rnorm(1, 32.3, sd)[0];
          //     individualdata[206] = individualdata[198];
          //     break;
          //   case 1:
          //     sd = 0.5*(29.8-25.5)/1.96*sqrt(90);
          //     individualdata[198] = Rcpp::rnorm(1, 27.6, sd)[0];
          //     individualdata[206] = individualdata[198];
          //     break;
          //   case 2: 
          //     sd = 0.5*(29.4-25.2)/1.96*sqrt(98);
          //     individualdata[198] = Rcpp::rnorm(1, 27.3, sd)[0];
          //     individualdata[206] = individualdata[198];
          //     break;
          //   case 3:
          //     sd = 0.5*(29.4-25.2)/1.96*sqrt(98);
          //     individualdata[198] = Rcpp::rnorm(1, 27.3, sd)[0];
          //     individualdata[206] = individualdata[198];
          //     break;
          //   }
          // }
          individualdata[198] = Rcpp::rnorm(1, 18.85, 10.19)[0];
        }
        
        
        
        if(individualdata[199]==0){individualdata[199]=schedule_breastcancer(index, bmi_trajectory, individualdata[198], age, year_now, individualdata[200], individualdata[201], individualdata[199]);
          ;}
        if(individualdata[199] > 0 && individualdata[202] == 0){
          IntegerVector stage_years = schedule_all_stages(year_now, age, individualdata[199], index);
          individualdata[202] = individualdata[199];
          individualdata[203] = stage_years[0]; // Stage 2 year
          individualdata[204] = stage_years[1]; // Stage 3 year
          individualdata[205] = stage_years[2]; // Stage 4 year
          individualdata[208] = stage_years[3]
;        }
        
        
      }
    } 
    // end of if(age >= 18)
    
    //only allow immigrants to die/make children after they have immigrated
    if(year_now >= yearimmigrated && year_now <= yearemigrated)
    {
      //After 1990 (for ciitizen) or year of immigration (for immigrants), allow individuals to die + make children
      // min of year immmigrated is 1991 for immigrants, but for citizens it's 0, thus necessitating the checking of year_now >= 1990                    
      if (year_now >= 1991){
        basemortality = mortality_cube(index, yearborn-1870, year_now-1990); // min mortality_rates$year_of_birth = 1870, 
        // min mortality_rates$year_of_death = 1990
        basemortality = mortmod(basemortality, individualdata[5], individualdata[6], individualdata[7], year_now, index);
      } else{basemortality= 0;}
          //calculate annual mortality rate starting from 1990 to check if person is alive
         
          year_of_death = schedule_death(individualdata, age, year_now, basemortality, amimort, strokemort, ckd5mort, index, individualdata[4], individualdata[5], individualdata[6],yearemigrated);
          individualdata[0] = year_of_death;
          individualdata[207] = get_year_bc_death();

          if(year_of_death > 0) 
          {
            break;}
    
        if(year_now >= 1960)
        {
          if (year_now > yearimmigrated && year_now >= 1961) {   
          //check if this individual can make a child, and update the yearborn and index vectors accordingly
          //gender = abs(remainder(index,2));
          
          // allow actual birth from 1991 to 2050, 
          // facilitated by the fact that R indices starts from 1 where as C indices start from 0
          if(gender > 0 && age < 50 && age>=15)
          {
            //compute which value of fertility to use based on race(0-3), parity(0-6), current year(1990-2051) and mothers age(15-49)
            race = int(index/2);
            int col_index = (age-15)/5; // age is integer, so col_index will be as well
            
            basefertility = fertility_rates_new((year_now-1960), col_index);
            
            
            have_child = schedule_child(basefertility, year_now, race); 
            
            //if individual has a child, update the birthmatrix based on race and gender of child
            if(have_child > 0)
            {
              
              switch(have_child)
              {
                //daughter
              case(1): birthmatrix(year_now - 1961, (race * 2) + 1) += 1;  // Right now only the first 60 rows are populated
                break;
                //son
              case(2): birthmatrix(year_now - 1961, (race * 2)) += 1;      // Right now only the first 60 rows are populated
                break;
              }
              if (number_children == 0)
              {
                age_first_child = age;
              }
              ++number_children;
              if (individualdata[200] == 0){
                individualdata[200] = age;
              }
              individualdata[201] = number_children;
            }
          }
        }   
      }
    }
    //end of one year cycle
  }
  
  
  // reach here either age = 120, or year_now > 2050, or year_of_death > 0
  
  
  // recast the trajectories
  NumericVector bmioveryears(YEAR_UNTIL - 1990 + 1);  // Initialize vectors with zeros
  NumericVector sbpoveryears(YEAR_UNTIL - 1990 + 1);
  NumericVector dbpoveryears(YEAR_UNTIL - 1990 + 1);
  
  for (int age = 18; age <= 74; ++age) {
    int year = yearborn + age;
    
    // Check if the calculated year is within the range 1990-2050
    if (year >= 1990 && year <= YEAR_UNTIL) {
      int index_in_trajectory = year - 1990;  // Adjust index for the trajectory vectors
      
      // Record BMI, SBP, and DBP values in the corresponding vectors
      bmioveryears[index_in_trajectory] = bmi_trajectory[age];
      sbpoveryears[index_in_trajectory] = sbp_trajectory[age];
      dbpoveryears[index_in_trajectory] = dbp_trajectory[age];
    }
  }
  
  // Now, bmioveryears, sbpoveryears, and dbpoveryears contain the desired values.
  
  
  // Copy BMI values from bmioveryears to individualdata
  std::copy(bmioveryears.begin(), bmioveryears.end(), individualdata.begin() + 12);
  
  // Copy SBP values from sbpoveryears to individualdata
  std::copy(sbpoveryears.begin(), sbpoveryears.end(), individualdata.begin() + 12 + (YEAR_UNTIL - 1990 + 1));
  
  // Copy DBP values from dbpoveryears to individualdata
  std::copy(dbpoveryears.begin(), dbpoveryears.end(), individualdata.begin() + 12 + 2 * (YEAR_UNTIL - 1990 + 1));
  
  return individualdata;
}


// [[Rcpp::export]]
// this function creates birth, daly, obese matrices and returns scheduled population info
NumericMatrix schedule_population(int ncol, int begin, int end, bool nsc, int AGE_MAX, int YEAR_UNTIL)
{
  //populationmatrix contains the years that individuals get conditions, thisindividual is one row of that matrix
  NumericMatrix populationmatrix(end-begin, ncol);
  
  //get baseline population parameters from R environment
  Environment global = Environment::global_env();
  NumericMatrix bmitable = global["bmitable"];
  NumericMatrix bptable = global["bptable"];
  NumericMatrix heightbmi = global["heightbmi"];
  NumericMatrix prefrailty = global["prefrailty"];
  NumericMatrix frailty = global["frailty"];
  // NumericMatrix dmtable = global["dmtable"];
  
  NumericMatrix fertility_rates_new = global["fertility_rates_new"];
  arma::cube mortality_cube = global["mortality_cube"];
  
  NumericMatrix amimort = global["amimort"];
  NumericMatrix strokemort = global["strokemort"];
  NumericMatrix ckd5mort = global["ckd5mort"];
  
  NumericVector amiinc = global["amiinc"];
  NumericVector strokeinc = global["strokeinc"];
  
  
  
  // double deltaLDL = global["deltaLDL"];
  // double deltaHDL = global["deltaHDL"];
  
  
  IntegerVector index = global["index"];
  IntegerVector yearborn = global["yearborn"];
  IntegerVector yearimmigrated = global["yearimmigrated"];
  IntegerVector yearemigrated = global["yearemigrated"];
  
  
  
  //birthmatrix is the birth counts by [yearborn(1991-2051), ethgen]
  IntegerMatrix birthmatrix(91,8); // index
  
  //Matrices of the number of overweight and obese individuals [yearnow(1990-2050) * age(18-100), Ethgen(8)]
  // initial values are 0 so no need to pass from R environment, also these matrices are computed in Cpp to be returned to R
  // IntegerMatrix overweightmatrix(83*61, 10); // year, age, index
  // IntegerMatrix obesematrix(83*61, 10); // year, age, index
  // IntegerMatrix dalymatrix(61, 10);
  
  
  
  
  //Proceed to simulate for all individuals
  
  // using for_each and multiprocessing to speed up the procedure
  // fill a index vector from begin to end
  // std::vector<int> indiv(end-begin);
  // std::iota(std::begin(indiv), std::end(indiv), begin);
  // for_each(indiv.begin(), indiv.end(), [&](int& indiv){
  //     thisindividual = schedule_individual(index[indiv], yearborn[indiv], yearimmigrated[indiv], ncol, birthmatrix, overweightmatrix, obesematrix);
  //     for(int col=0; col<ncol; ++col){
  //         populationmatrix(indiv-begin,col) = thisindividual[col];
  //     }
  //     if(remainder(indiv,10000) == 0) cout << indiv << endl;
  // });
  
  
  for(int indiv=begin; indiv < end; ++indiv)
  {
    if(indiv % 50000 == 0) cout << indiv << endl;
    NumericVector thisindividual(ncol);
    thisindividual = schedule_individual(index[indiv], yearborn[indiv], yearimmigrated[indiv], yearemigrated[indiv], ncol, 
                                         birthmatrix,  fertility_rates_new, mortality_cube,
                                         bmitable, bptable, heightbmi,prefrailty,frailty, amiinc, strokeinc, amimort, strokemort, ckd5mort, nsc, AGE_MAX, YEAR_UNTIL);
    for(int col=0; col<ncol; ++col)
    {
      populationmatrix(indiv-begin,col) = thisindividual[col];
    }
  }
  global["birthmatrix"] = birthmatrix;
  
  return populationmatrix;
}
