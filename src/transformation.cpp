#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
#include <sptangle.cpp>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix tsangle(NumericMatrix spectra, NumericVector wavelength, int threads = 1) {
  
  int row = spectra.nrow();
  int col = spectra.ncol();
  int new_col = col - 2;
  
  //object to allocate
  NumericMatrix transformed(row, new_col);
  
  //Set threads
#ifdef _OPENMP
  if ( threads > 0 ) {
    omp_set_num_threads( threads );
    REprintf("Number of threads=%i\n", omp_get_max_threads());
  }
#endif

#pragma omp parallel for
  for (int i = 0; i < row; i++) {
    for (int j = 0; j < new_col; j++) {
      
      transformed(i, j) = sptangle(wavelength[(j)],
                                   spectra(i, (j)),
                                   wavelength[(j+1)],
                                   spectra(i, (j+1)),
                                   wavelength[(j+2)],
                                   spectra(i, (j+2)));
    }
  }
  
  return transformed;
  
}