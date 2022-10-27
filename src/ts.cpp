#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo"]]
#include <RcppArmadillo.h>

using namespace arma;

// [[Rcpp::export]]
arma::mat pheno_rcpp(arma::mat ts, arma::vec doy, int threads = 1) {

#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
  }
#endif
  
  static const double pi = 3.14159265;
  
  //Transform doy to radiance
  arma::vec r = (doy/365)*(2*pi);
  
  //n pixels
  int npixels = ts.n_rows;
  
  //collector
  arma::mat normalized(npixels, 2);
  
#pragma omp parallel for
  for (int i = 0; i < npixels; i++) {
    
    //Get the time series
    arma::rowvec vi_pixel = ts.row(i);
    
    //min and max
    int vi_min = min(vi_pixel);
    int vi_max = max(vi_pixel);
    
    //Normalize
    arma::vec vi = (vi_pixel - vi_max)/(vi_max - vi_min);
    
    //Get coordinates
    arma::vec px = vi * cos(r);
    arma::vec py = vi * sin(r);
    
    //Average vector
    double xv = mean(px); 
    double yv = mean(py);
    
    //Displacement
    double rv = atan2(yv, xv);
    
    if(rv < 0 ) {
      rv = rv + (2*pi);
    }
    
    //Distance
    double vv = sqrt(((xv*xv) + (yv*yv)));
    
    
    
    
    
    
  }
  
  return normalized;
  
}