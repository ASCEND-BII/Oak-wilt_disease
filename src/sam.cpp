#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double sam(NumericVector& x, NumericVector& y) {
  
  const double conv = 180/3.141593;
  
  double radians;
  
  double normEM = sum(pow(y, 2));
  
  radians = acos(sum(x * y) / sqrt(sum(pow(x,2)) * normEM));
  
  double angle = 1 - (radians * conv);
  
  return angle;
}