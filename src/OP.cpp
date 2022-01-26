#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cost_bernoullicpp(NumericVector v)
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
List myOPcpp(NumericVector data) {
  
  unsigned short int n = data.length();
  NumericVector Q(n);
  IntegerVector cp(n), P(0);
  double val_min = 0, a = 0;
  int arg_min = 0, v = 0;
  
  double beta = 2.0*var(data)*log(double(n));
  
  for(int t=1;t < n;t++) {
    //Range id(0,t);
    val_min = cost_bernoullicpp(data[Range(0,t)]);
    arg_min = 0;
    
    for (int s=1;s < t;s++) {
      //Range id(s,t);
      a = Q[s-1] + cost_bernoullicpp( data[Range(s,t)] ) + beta;
      
      if (a < val_min) {
        val_min = a;
        arg_min = s-1;
      }
    }
    Q[t] = val_min;
    cp[t] = arg_min;
  }
  // backtracking
  v = cp[n-1];
  P.push_back(cp[n-1]);
  
  while (v > 0)
  {
    P.push_back(cp[v]);
    v = cp[v];
  }
  std::reverse(P.begin(),P.end());
  P.erase(P.begin());
  
  
  List L = List::create(Named("changepoints") = P , _["globalCost"] = Q[int(n)] - double(P.length())*beta);
  return L;
}

/*** R
library(purrr)
data <- dataSeries(K=4,n=100)
data=as.numeric(data)
K1 = myOP(data)

K2 = myOPcpp(data)

#cost_bernoulli(data)

*/