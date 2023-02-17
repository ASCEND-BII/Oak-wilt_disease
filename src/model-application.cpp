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
                      arma::vec pro_psymtomatic,
                      arma::vec pro_pdead) {
  
  //N of elements
  int elements = pro_phealthy.n_elem;
  
  //Product sum 
  arma::vec total = pro_phealthy + pro_psymtomatic + pro_pdead;
  
  arma::mat prob(elements, 3);
  
  prob.col(0) = pro_phealthy/total;
  prob.col(1) = pro_psymtomatic/total;
  prob.col(2) = pro_pdead/total;
  
  return prob;
  
}

// [[Rcpp::export]]
arma::mat predicted_prob(arma::mat pixels, 
                         arma::mat healthy, 
                         arma::mat symtomatic,
                         arma::mat dead) {
  
  //Dim
  int an = pixels.n_rows; //n pixels
  int nmodels = healthy.n_elem; //n models
  
  //Mat to return
  arma::mat result(an, 6);
  
  //Get coefficients
  arma::mat coef_healthy = healthy.cols(1, 3);
  arma::mat coef_symtomatic = symtomatic.cols(1, 3);
  arma::mat coef_dead = dead.cols(1, 3);
  
  for(int i = 0; i < an; i++) {
    
    //Pixel of interest
    arma::rowvec ipixel = pixels.row(i);
    
    //N models multiplication (m)
    arma::mat mhealthy = coef_healthy.each_row() % ipixel;
    arma::mat msymtomatic = coef_symtomatic.each_row() % ipixel;
    arma::mat mdead = coef_dead.each_row() % ipixel;
    
    //Get product (pro_)
    arma::vec pro_phealthy = product(mhealthy, healthy.col(0));
    arma::vec pro_psymtomatic = product(msymtomatic, symtomatic.col(0));
    arma::vec pro_pdead = product(mdead, dead.col(0));
    
    //Get probability 
    arma::mat prob = probability(pro_phealthy, pro_psymtomatic, pro_pdead);
    
    //Get mean
    result(i, 0) = round(mean(prob.col(0))*10000);
    result(i, 1) = round(mean(prob.col(1))*10000);
    result(i, 2) = round(mean(prob.col(2))*10000);
    
    //Get sd
    result(i, 3) = round(stddev(prob.col(0))*10000);
    result(i, 4) = round(stddev(prob.col(1))*10000);
    result(i, 5) = round(stddev(prob.col(2))*10000);
    
  }
  
  return result;
  
}
