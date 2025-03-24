#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
int schedule_death(NumericVector individualdata, int age, int year_now, 
                   double basemortality, 
                   NumericMatrix amimort, NumericMatrix strokemort, NumericMatrix ckd5mort,
                   int index) 
{
  //mortality parameters, hazard ratios
  int year_of_death = 0;
  int gender, race, agecate, column, duration;
  double deathprob = runif(1)[0];
  double nonAMImortratio = 1;//was 0.65
  NumericVector mortalityrates(10);
  NumericVector HRt2dm = NumericVector::create(1.31, 3.01, 4.27, 1.31);
  
  //default mortality rate comes from the mortality cube [ethgen, year_of_birth (minimum = 1870), current year(minimum = 1990)]
  //mortality rates: 0:default 1:diabetes 2:smoking 3-6:cancers 7:AMI 8:Stroke 9:CKD5 10:UKPDS complications
  if(age >= 40 && individualdata[4] == 0)
  {
    mortalityrates[0] = nonAMImortratio * basemortality;
  }
  else
  {
    mortalityrates[0] = basemortality;
  }
  
  //diabetes
  if(individualdata[4] > 0)
  {
    race = int(index/2);
    mortalityrates[1] = 1- pow(1-basemortality, HRt2dm[race]);
  }
  
  //brca https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8075208/
  if(individualdata[200] > 0){
    if(individualdata[203] > 0){
      if(individualdata[204] > 0){
        if(individualdata[205] > 0){
          if(individualdata[206] > 0){
            mortalityrates[2] = 0.268; //stage 4
          } else {
            mortalityrates[2] = 0.083; //stage 3
          }
        } else {
          mortalityrates[2] = 0.044; //stage 2
      } 
      } else {
        mortalityrates[2] = 0.02; //stage 1
      }
    } else {
      mortalityrates[2] = 0.01; //stage 0
    }
      
     
  }
  
  
  //UKPDS 8-9:AMI 10-11:Stroke 12:CKD5 13:Ulcers 14:IHD 15:CHF 16:Blindness 17-18:Amputations
  //AMI
  /*
   if(individualdata[8] <= year_now && individualdata[8] > 0 && individualdata[3] == 0)
   {
   gender = abs(remainder(index,2));
   agecate = 0;
   if(age > 59) agecate = 1;
   if(age > 69) agecate = 2;
   if(age > 79) agecate = 3;
   column = 2 * agecate + gender;
   duration =  year_now - individualdata[8];
   if(duration > 10) duration = 10;
   mortalityrates[7] = amimort(duration,column);
   // //non-diabetics
   // if(individualdata[3] == 0)
   // {
   //     mortalityrates[7] = amimort(duration,column);
   // }
   // //diabetics
   // else
   // {
   //     mortalityrates[7] = amimort(duration,column+8);
   // }
   }
   
   //Stroke
   if(individualdata[10] <= year_now && individualdata[10] > 0 && individualdata[3] == 0)
   {
   gender = abs(remainder(index,2));
   agecate = 0;
   if(age > 65) agecate = 1;
   column = 2 * gender + agecate;
   duration =  year_now - individualdata[10];
   if(duration > 20) duration = 20;
   mortalityrates[8] = strokemort(duration,column);
   }
   
   //ckd5 This seems to be using ckd5 and esrd interchangeably
   if(individualdata[12] >= year_now && individualdata[12] > 0 && individualdata[3] == 0)
   {
   duration =  year_now - individualdata[12];
   if(duration > 14) duration = 13;
   mortalityrates[9] = ckd5mort(duration,2);
   }
   
   */
  
  //evaluate if the individual died this year
  if(deathprob < max(mortalityrates))
  {
    year_of_death = year_now;
  }
  
  return year_of_death;
}
