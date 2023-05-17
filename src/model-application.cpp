#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;

// [[Rcpp::export]]
arma::mat product(arma::mat multiplication, arma::vec intercept) {
  
  int nr = multiplication.n_rows;
  int nc = multiplication.n_cols;
  
  arma:vec ans(nr);
  
  for (int r = 0; r < nr; r++) {
    
    double value = intercept[r];
    
    for (int c = 0; c < nc; c++) {
      value += multiplication(r, c);
    }
    
    ans[r] = exp(value);
    
  }
  
  return ans;
  
}

// [[Rcpp::export]]
arma::mat probability(arma::vec pro_phealthy, 
                      arma::vec pro_psymptomatic,
                      arma::vec pro_pdead) {
  
  //N of elements
  int elements = pro_phealthy.n_elem;
  
  //Product sum 
  arma::vec total = pro_phealthy + pro_psymptomatic + pro_pdead;
  
  arma::mat prob(elements, 3);
  
  prob.col(0) = pro_phealthy/total;
  prob.col(1) = pro_psymptomatic/total;
  prob.col(2) = pro_pdead/total;
  
  return prob;
  
}

// [[Rcpp::export]]
arma::mat predicted_prob(arma::mat pixels, 
                         arma::mat healthy, 
                         arma::mat symptomatic,
                         arma::mat dead) {
  
  //Dim
  int an = pixels.n_rows; //n pixels
  int nmodels = healthy.n_elem; //n models
  
  //Root of nmodels for IC
  double n_root = sqrt(nmodels);
  
  //Mat to return
  arma::mat result(an, 6);
  
  //Get coefficients
  arma::mat coef_healthy = healthy.cols(1, 3);
  arma::mat coef_symptomatic = symptomatic.cols(1, 3);
  arma::mat coef_dead = dead.cols(1, 3);
  
  for(int i = 0; i < an; i++) {
    
    //Pixel of interest
    arma::rowvec ipixel = pixels.row(i);
    
    //N models multiplication (m)
    arma::mat mhealthy = coef_healthy.each_row() % ipixel;
    arma::mat msymptomatic = coef_symptomatic.each_row() % ipixel;
    arma::mat mdead = coef_dead.each_row() % ipixel;
    
    //Get product (pro_)
    arma::vec pro_phealthy = product(mhealthy, healthy.col(0));
    arma::vec pro_psymptomatic = product(msymptomatic, symptomatic.col(0));
    arma::vec pro_pdead = product(mdead, dead.col(0));
    
    //Get probability 
    arma::mat prob = probability(pro_phealthy, pro_psymptomatic, pro_pdead);
    
    //Get mean
    double mean_healthy = mean(prob.col(0));
    double mean_symptomatic = mean(prob.col(1));
    double mean_dead = mean(prob.col(2));
    
    //Get sd
    double sd_healthy = stddev(prob.col(0));
    double sd_symptomatic = stddev(prob.col(1));
    double sd_dead = stddev(prob.col(2));
    
    //Get limits
    double z_healthy = 1.96*(sd_healthy/n_root);
    double z_symptomatic = 1.96*(sd_symptomatic/n_root);
    double z_dead = 1.96*(sd_dead/n_root);
    
    //Difference
    double amplitude_healthy = (mean_healthy + z_healthy) - (mean_healthy - z_healthy);
    double amplitude_symptomatic = (mean_symptomatic + z_symptomatic) - (mean_symptomatic - z_symptomatic);
    double amplitude_dead = (mean_dead + z_dead) - (mean_dead - z_dead);
    
    //Fill results
    result(i, 0) = round(mean_symptomatic*10000);
    result(i, 1) = round(mean_healthy*10000);
    result(i, 2) = round(mean_dead*10000);
    result(i, 3) = round(amplitude_symptomatic*10000);
    result(i, 4) = round(amplitude_healthy*10000);
    result(i, 5) = round(amplitude_dead*10000);
    
  }
  
  return result;
  
}
