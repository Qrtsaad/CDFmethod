#include <Rcpp.h>
using namespace Rcpp;

double cost_bernoullicpp2(NumericVector v)
{
  double res = 0, n = v.length(), a = sum(v), b = n - sum(v);
  
  if(mean(v) == 0 | 1.0 - mean(v)==0 ) {
    res = 0;
  } 
  else {
    res = -(a*log(a/n) + b*log(1.0-a/n));
  }
  return res;
}

// [[Rcpp::export]]
List myPELTcpp(NumericVector data) {
  unsigned short int n = data.length();
  NumericVector Q(n);
  IntegerVector cp(n), P(0), R(0);
  double val_min = 0, a = 0;
  unsigned int length_r = 0, arg_min = 0, v = 0;
  
  double beta = 2.0*var(data)*log(double(n));
  
  
  for (int t=1; t < n; t++) {
    val_min = cost_bernoullicpp2(data[Range(0,t)]);
    arg_min = 0;
    R.push_back(t);
    
    length_r = R.length();
    
    for (int s=0; s < length_r; s++)
    {
      a = Q[R[s]-1] + cost_bernoullicpp2(data[Range(R[s],t)]) + beta;
      if (a < val_min) {
        val_min = a;
        arg_min = R[s] - 1;
      }
    }
    
    Q[t] = val_min;
    cp[t] = arg_min;
    
  }
  // backtracking
  v = cp[n-1];
  P.push_back(cp[n-1]);
  
  while (v > 0) {
    P.push_back(cp[v]);
    v = cp[v];
  }
  
  std::reverse(P.begin(),P.end());
  P.erase(P.begin());
  
  List L = List::create(Named("changepoints") = P , _["globalCost"] = Q[n-1] - double(P.length())*beta);
  
  return L;
}

// [[Rcpp::export]]
List onechangePELTcpp(NumericVector data) {
  unsigned short int n = data.length();
  NumericVector Q(n);
  IntegerVector cp(n), P(0), R(0);
  double val_min = 0, a = 0;
  unsigned int length_r = 0, arg_min = 0, v = 0;
  
  double beta = 2.0*var(data)*log(double(n));
  
  
  for (int t=1; t < n; t++) {
    val_min = cost_bernoullicpp2(data[Range(0,t)]);
    arg_min = 0;
    R.push_back(t);
    
    length_r = R.length();
    
    for (int s=0; s < length_r; s++)
    {
      a = Q[R[s]-1] + cost_bernoullicpp2(data[Range(R[s],t)]) + beta;
      if (a < val_min) {
        val_min = a;
        arg_min = R[s] - 1;
      }
    }
    
    Q[t] = val_min;
    cp[t] = arg_min;
    
  }
  // backtracking
  v = cp[n-1];
  P.push_back(cp[n-1]);
  
  while (v > 0) {
    P.push_back(cp[v]);
    v = cp[v];
  }
  
  std::reverse(P.begin(),P.end());
  P.erase(P.begin());
  
  val_min = R_PosInf;
  for (int i = 0; i < P.length(); i++) {
    a = cost_bernoullicpp2( data[Range(0,P[i])] ) + cost_bernoullicpp2( data[Range(P[i]+1,n-1)] ) + beta;
    if (a < val_min) {
      val_min = a;
      arg_min = P[i];
    }
  }
  
  int rupt = NA_INTEGER;
  if(arg_min == 2) {rupt = NA_INTEGER;}
  else {rupt = arg_min;} 
  //Si on detecte le min en 2 alors c'est des données sans ruptures car ca impliquerait que l'on ai un saut en 1 or c'est la premiere donnée
  
  
  List L = List::create(Named("tau") = rupt , _["globalCost"] = Q[n-1] - beta);
  
  return L;
}

/*** R
library(purrr)
data <- dataSeries(K=4,n=100)
data=as.numeric(data)
K1 = myPELT(data)

K2 = myPELTcpp(data)

#cost_bernoulli(data)

*/
