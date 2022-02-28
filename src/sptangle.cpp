#include <Rcpp.h>
#include <iostream>
#include <utility> // for pair
#include <cmath> // for math functions
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
double sptangle(double x1, double y1, double x2, double y2, double x3, double y3) {
  
  //b and c are vectors of the angle
  double b = (y2-y1)/(x2-x1);
  double c = (y3-y1)/(x3-x1);
  
  // get angles
  double alpha = atan(b) - atan(c);
  alpha = abs(alpha) * 180 / PI;
  
  //Intersection point of the medians
  
  double intmedians = (y1+y2+y3)/3;
  double difference = y2 - intmedians;
  
  if(difference < 0) {
    alpha = -(alpha/180);
    
  } else if(difference > 0) {
    alpha = alpha/180;
    
  } else if(difference == 0) {
    alpha = 0;
  }
  
  return alpha;
  
}