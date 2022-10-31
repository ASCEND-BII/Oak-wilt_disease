#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector normalize(NumericVector ts, int vi_min, int vi_max) {
  
  //Function to normalize the time series

  NumericVector normalized = (ts - vi_min) / (vi_max - vi_min);
  
  return normalized;
}

// [[Rcpp::export]]
NumericVector back_normalize(NumericVector ts, int vi_min, int vi_max) {
  
  //Function to normalize the time series
  
  NumericVector normalized = (ts - vi_min) / (vi_max - vi_min);
  
  return normalized;
}

// [[Rcpp::export]]
NumericVector doy2rad(NumericVector doy) {
  
  //Function to transfrom doy to radians
  
  static const double pi = 3.14159265;
  
  NumericVector r = (doy/365)*(2*pi);
  
  return r;
}

// [[Rcpp::export]]
NumericVector pcx(NumericVector normalized, NumericVector r) {
  
  //Functions to get the polar coordinates of X
  
  NumericVector vx = normalized * cos(r);
  
  return vx;
}

// [[Rcpp::export]]
NumericVector pcy(NumericVector normalized, NumericVector r) {
  
  //Functions to get the polar coordinates of Y
  
  NumericVector vy = normalized * sin(r);
  
  return vy;
}

// [[Rcpp::export]]
double xmean(NumericVector vx) {
  
  //Get the mean of PCX, be wise if you need it.
  
  double xav = mean(vx);
  
  return xav;
}

// [[Rcpp::export]]
double ymean(NumericVector vy) {
  
  //Get the mean of PCY, be wise if you need it.
  
  double yav = mean(vy);
  
  return yav;
}

// [[Rcpp::export]]
double displacement(double yav, double xav) {
  
  //Estimate the angle of displacement (angle of peak)
  
  static const double pi = 3.14159265;
  
  double r = atan2(yav, xav);
  
  if(r <= 0) {
    r = r + (2*pi);
  }
  
  return r;
}

// [[Rcpp::export]]
double distance(double yav, double xav) {
  
  //Estimate the magnitude of displacemente (distance of peak)
  
  double v = sqrt((yav*yav) + (xav*xav));
  
  return v;
}

// [[Rcpp::export]]
int rad2doy(double r) {
  
  //Transform from radians to doy
  
  static const double pi = 3.14159265;
  
  double sdoy = (365 * r) / (2 * pi);
  
  return ceil(sdoy);
  
}

// [[Rcpp::export]]
double lest_activity_angle(double r) {
  
  //Estimate the angle of lest activity
  
  static const double pi = 3.14159265;
  
  double ang;
  
  if(r < pi) {
    ang = r + pi;
    
  } else {
    ang = r - pi;
    
  }
  
  return ang;
}

// [[Rcpp::export]]
NumericVector season(NumericVector doy, int doy_lest){
  
  //Compute the phenological year
  
  int s = 1; 
  NumericVector difference = doy - doy_lest;
  
  int n_observations = doy.length();
  NumericVector pheno_year(n_observations);
  pheno_year[0] = 1;
  
  for(int j = 1; j < n_observations; j++) {
    
    if(difference[j] >= 0 & difference[j-1] < 0) {
      s = s + 1;
      pheno_year[j] = s;
      
    } else {
      pheno_year[j] = s;
    
    }
  }
  
  return pheno_year;
}

// [[Rcpp::export]]
NumericVector cumulative_season(NumericVector ts, int year, NumericVector season){
  
  //Compute the normalize accumulative values per season
  
  NumericVector col_interest = std::find(season.begin(), season.end(), year) != season.end();
  
  return col_interest;
}

// [[Rcpp::export]]
NumericMatrix phenoloical_years(NumericMatrix ts, NumericVector doy, int threads = 1){
  
  int col = ts.cols();
  int row = ts.rows();
  
  NumericMatrix output(row, col);
  
#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif
    
#pragma omp parallel for
  for(int i = 0; i < row; i++) {
    
    NumericVector sample = ts.row(i);
    NumericVector n = normalize(sample, min(sample), max(sample)); 
    NumericVector r = doy2rad(doy);
    NumericVector vx = pcx(n, r);
    NumericVector vy = pcy(n, r);
    double xav = xmean(vx);
    double yav = ymean(vy);
    double rv_ang = displacement(yav, xav);
    double ang_lest = lest_activity_angle(rv_ang);
    double doy_lest = rad2doy(ang_lest);
    NumericVector phenological = season(doy, doy_lest);
    
    output.row(i) = phenological;
  }
  
  return output;
  
}