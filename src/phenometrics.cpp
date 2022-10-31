#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;

// [[Rcpp::export]]
arma::vec vi_year(arma::rowvec ts, double val, arma::rowvec season) {
  
  //Get the vi of interest for the phenological year

  arma::vec observations = ts.elem(find(season == val));
  
  return observations;

}

// [[Rcpp::export]]
arma::vec doy_year(arma::rowvec doy, double val, arma::rowvec season) {
  
  //Get the doy of interest for the phenological year
  
  arma::vec observations = doy.elem(find(season == val));
  
  return observations;
  
}

// [[Rcpp::export]]
arma::vec normalize(arma::vec observations, int vi_min, int vi_max) {
  
  //Function to normalize the time series
  
  arma::vec normalize = (observations - vi_min) / (vi_max - vi_min);
  
  return normalize*1000;
  
}

// [[Rcpp::export]]
arma::vec cumulative_observations(arma::vec normalize) {

  //Get the cumsum of the normalize time series
  
  arma::vec cumulative = cumsum(normalize);
    
  int max_val = cumulative.max();
  
  arma::vec divided = cumulative/max_val;
  
  return divided;
  
}

// [[Rcpp::export]]
arma::vec doy_corrected(arma::vec doy_observed, int doy_lest, int doy_delta = 16) {
  
  //Get the interpolation of cumulative values for detail on days
  
  arma::vec new_days = doy_observed - doy_lest;
  
  int observations = doy_observed.n_elem;
  
  arma::vec doy_complete = floor(regspace(new_days[0], doy_delta, observations*doy_delta)); 
  
  return doy_complete;
  
}

// [[Rcpp::export]]
arma::vec interpolation_cumulative(arma::vec cumulative, arma::vec doy_observed, arma::vec doy_complete) {
  
  //Get the interpolation of cumulative values for detail on days
  
  arma::vec new_cumulative;
  
  interp1(doy_observed, cumulative*100, doy_complete, new_cumulative);
  
  //new_cumulative.replace(datum::nan, 0); //in case

  return new_cumulative;
  
}

// [[Rcpp::export]]
arma::vec interpolation_vi(arma::vec vi, arma::vec doy_observed, arma::vec doy_complete) {
  
  //Get the interpolation of cumulative values for detail on days
  
  arma::vec new_vi;
  
  interp1(doy_observed, vi, doy_complete, new_vi);
  
  //new_cumulative.replace(datum::nan, 0); //in case
  
  return new_vi;
  
}

// [[Rcpp::export]]
arma::vec peak_activity(arma::rowvec normalized, arma::vec doy) {
  
  //Function to get the doy and value of peak of activity
  
  static const double pi = 3.14159265;
  
  arma::vec rad = (doy/365)*(2*pi); //doy to radians
  arma::vec vx = normalized * cos(rad); //X coordinate
  arma::vec vy = normalized * sin(rad); //Y coordinate
  double xav = mean(vx); //average vector X
  double yav = mean(vy); //average vector Y
  double r = atan2(yav, xav); //displacement
  
  if(r <= 0) {
    r = r + (2*pi);
  }
  
  double v = sqrt((yav*yav) + (xav*xav)); //distance
  double sdoy = ceil((365 * r) / (2 * pi)); //doy
  
  arma::vec out(3);
  
  out(0) = r;
  out(1) = v;
  out(2) = sdoy;
  
  return out;
  
}

// [[Rcpp::export]]
arma::vec key_doys(arma::vec vi_cumsum, double min_threshold, double max_threshold, int peak = 16) {
  
  //Get key days for the phenometric
  
  arma::vec ind(4);
  
  ind(0) = abs(vi_cumsum - min_threshold).index_min();
  ind(1) = abs(vi_cumsum - 50).index_min();
  ind(2) = abs(vi_cumsum - max_threshold).index_min();
  ind(3) = floor(peak);
  
  return ind;
  
}

// [[Rcpp::export]]
arma::vec get_slopes(arma::vec vi_cumsum, arma::vec vi, arma::vec key_doy) {
  
  //Get slopes for the phenometric
  
  arma::vec slope(7);
  
  //Create doy vector
  arma::vec FGS_doy = regspace(key_doy(0), 1, key_doy(2));
  arma::vec EGS_doy = regspace(key_doy(0), 1, key_doy(1));
  arma::vec LGS_doy = regspace(key_doy(1), 1, key_doy(2));
  arma::vec PGS_doy = regspace(key_doy(3), 1, key_doy(2));
  
  //Get cumulative value
  arma::vec FGS_cum = vi_cumsum(span(key_doy(0), key_doy(2))); 
  arma::vec EGS_cum = vi_cumsum(span(key_doy(0), key_doy(1))); 
  arma::vec LGS_cum = vi_cumsum(span(key_doy(1), key_doy(2))); 
  arma::vec PGS_cum = vi_cumsum(span(key_doy(3), key_doy(2))); 
  
  //Get vi value
  arma::vec EGS_vi = vi(span(key_doy(0), key_doy(1))); 
  arma::vec LGS_vi = vi(span(key_doy(1), key_doy(2))); 
  arma::vec PGS_vi = vi(span(key_doy(3), key_doy(2))); 
  
  //Solve
  arma::vec FGS = arma::solve(FGS_doy, FGS_cum); //Full GS
  arma::vec EGS = arma::solve(EGS_doy, EGS_cum); //Early GS
  arma::vec LGS = arma::solve(LGS_doy, LGS_cum); //Late GS
  arma::vec PGS = arma::solve(PGS_doy, PGS_cum); //After peak GS
  
  arma::vec RGU = arma::solve(EGS_doy, EGS_vi); //Early GS
  arma::vec RGS = arma::solve(LGS_doy, LGS_vi); //Late GS
  arma::vec RPS = arma::solve(PGS_doy, PGS_vi); //After peak GS
  
  slope(0) = FGS(0)*10000;
  slope(1) = EGS(0)*10000;
  slope(2) = LGS(0)*10000;
  slope(3) = PGS(0)*10000;
  slope(4) = RGU(0)*1000;
  slope(5) = RGS(0)*1000;
  slope(6) = RPS(0)*1000;
  
  return ceil(slope);
  
}

// [[Rcpp::export]]
arma::vec get_values(arma::vec key_doy, arma::vec vi, arma::vec doy_shifted, arma::vec doy_complete) {
  
  //Get slopes for the phenometric
  
  //Interpolate vi
  arma::vec new_vi;
  interp1(doy_shifted, vi, doy_complete, new_vi);
  
  //Export values
  arma::vec values(9);
  
  values(0) = new_vi(key_doy(0)); //Early value
  values(1) = new_vi(key_doy(1)); //Mid value
  values(2) = new_vi(key_doy(2)); //Late value
  values(3) = new_vi(key_doy(3)); //Peak value
  
  //Basal value
  arma::vec basal = new_vi(span(0, 29));
  values(4) = mean(basal.elem(find_finite(basal))); //VBL
  
  arma::vec mid = new_vi(span(key_doy(0), key_doy(2)));
  values(5) = mean(mid.elem(find_finite(mid))); //VGM
  values(6) = stddev(mid.elem(find_finite(mid))); //VGV
  values(7) = values(6)/values(5)*10000; //VCV
  
  arma::vec late = new_vi(span(334, 364));
  values(8) = mean(late.elem(find_finite(late))); //VGM
    
  return ceil(values);
  
}


