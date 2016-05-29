#include <Rcpp.h>
using namespace Rcpp;

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
  
  // for(int i=0; i<8; i++){
  //   
  //   for(int j=0; j<tdm.nrow(); j++){
  //     denom = inner_product(tau, tdm(j,_));
  //   }
  //   
  //   for(int j=0; j<tdm.ncol(); j++){
  //     for(int k=0; k<tdm.nrow(); k++){
  //       TPWM(k,j) = tau(j)*tdm(k,j) / denom(k);
  //     }
  //   }
  //   
  //   int sum_word_freq = sum(word_freq);
  //   for(int j=0; j<tdm.ncol(); j++){
  //     tau(j) = inner_product(word_freq, TPWM(_,j)) / sum_word_freq;
  //     
  //   }
  //   
  // }
  
  return tau;
}

// R Compiler 套件：加速 R 程式碼的執行速度
// http://blogger.gtwang.org/2011/08/r-compiler-r-r-compiler-package-speed.html
// https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-quickref.pdf

