#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double sam(NumericVector& x, NumericVector& y) {
  
  const double conv = 180/3.141593;
  
  double normEM = sum(pow(y, 2));
  
  double radians = acos(sum(x * y) / sqrt(sum(pow(x,2)) * normEM));
  
  double angle = (radians * conv);
  
  double simiarity = (90 - angle) / 90;
  
  return simiarity;
  
}