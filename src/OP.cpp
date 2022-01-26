#include <Rcpp.h>
using namespace Rcpp;

double cost_bernoulli(NumericVector v)
{
  unsigned short int n = v.length();
  int a = sum(v);
  int b = sum(1-v);
  double res = 0;
  
  if(mean(v) == 0 | 1-mean(v)==0 ) {
    res = 0;
    } 
  else {
    res = -(a*log(a/n) + b*log(1-a/n));
    }
  return res;
}


// [[Rcpp::export]]
List myOPcpp(NumericVector data, double beta) {
    
  unsigned short int n = data.length();
  NumericVector Q(n);
  IntegerVector cp(n), P(0);
  double val_min = 0, a = 0;
  int arg_min = 0, v = 0;
  
  
  for(int t=2 - 1;t <= n - 1;t++) {
    Range idx(1,t);
    val_min = cost_bernoulli(data[idx]);
    arg_min = 0;
    for (int s=2 - 1;s <= t - 1;s++)
    {
      Range idx(s,t);
      a = Q[s-1] + cost_bernoulli(data[idx]) + beta;
      if (a < val_min)
      {
        val_min = a;
        arg_min = s - 1;
      }
    }
    Q[t] = val_min;
    cp[t] = arg_min;
  }
// backtracking
  v = cp[n];
  P.push_front(cp[n]);
  
  while (v > 0)
  {
    v = cp[v];
    P.push_front(cp[v]);
    
  }
  std::reverse(P.begin(),P.end());
  P.erase(P.end());
  
  List L = List::create(Named("changepoints") = P , _["globalCost"] = Q[n] - P.length()*beta);
  return L;
}

/*** R

*/
