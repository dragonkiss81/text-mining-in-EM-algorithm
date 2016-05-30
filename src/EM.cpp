#include <Rcpp.h>
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins("cpp11")]]

double inner_product(NumericVector a, NumericVector b){
  if( a.size() != b.size() ){
    puts( "Error a's size not equal to b's size" ) ;
    return -1 ;
  }

  double product = 0;
  for(int i = 0; i <= a.size()-1; i++)
    product += a[i]*b[i];
  
  return product;
}

// [[Rcpp::export]]
NumericVector EM_AlgorithmCpp(NumericVector word_freq, NumericVector tau, NumericMatrix tdm){
  NumericMatrix TPWM(tdm.nrow(), tdm.ncol());
  NumericVector denom(tdm.nrow());
  
  for(int i=0; i<8; i++){
    for(int j=0; j<tdm.nrow(); j++){
      double inner = inner_product(tau, tdm(j,_));
      denom(j) = (inner==0)? 1:inner;
    }

    for(int j=0; j<tdm.ncol(); j++){
      for(int k=0; k<tdm.nrow(); k++){
        TPWM(k,j) = tau(j)*tdm(k,j) / denom(k);
      }
    }
    
    int sum_word_freq = sum(word_freq);
    for(int j=0; j<tdm.ncol(); j++){
      tau(j) = (sum_word_freq==0)? 1:inner_product(word_freq, TPWM(_,j)) / sum_word_freq;
    }
  }
  return tau;
}

