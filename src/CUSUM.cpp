#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
List cusumcpp(NumericVector X, double a) {
  double l = 9.0/100.0,h = 91.0/100.0;  // borne à définir (hypothèse : tau se trouve dans [l*n,h*n])
  double pi = 3.14159265358979323846264; // 338327950288419716939937510582
  unsigned int n = X.length();
  short unsigned int y_pred = 0;
  NumericVector S(n),  sigma_e(n);
  
  for(int t=0; t<n; t++) {
    S[t] = sum(X[Range(0,t)]) - double(t)/double(n) * sum(X);
    sigma_e[t] =  double(t)/double(n) * (1.0 - double(t)/double(n));
  }
    NumericVector curveCSM = S/sqrt(double(n));
    
    NumericVector Tt = curveCSM[Range(0,curveCSM.length()-1)] / sigma_e[Range(0,sigma_e.length()-1)];
  
    NumericVector CSMmax = abs(curveCSM) / sd(X);
    unsigned int tau = which_max(CSMmax);
    
    double T_stat = max( pow(Tt[  Range(round(l*double(n)),round(h*double(n)))  ], 2 ) );
    double p_value = sqrt((T_stat*exp(-T_stat)/(2.0*pi))) * ((1.0-1.0/T_stat) * log((1.0-l)*h/(l*(1.0-h))) + 4.0/T_stat);
    
    if(ISNAN(p_value)) {
      y_pred = NA_INTEGER;
      Rcout << "############# Erreur NaN p_value #############" << std::endl;
    }
    else if(p_value <= a & p_value > 0) {y_pred = 1;}
    else {y_pred = 0;}
    
    List L = List::create(Named("tau") = tau , _["y_pred"] = y_pred);
    
    return L;
}


/*** R
nb_test = 500
p1 = runif(nb_test,min=0.1,max=0.9)
p2 = runif(nb_test,min=0.1,max=0.9)
id = rbinom(nb_test,size=1,prob=0.5)
p2[id==0] = p1[id==0]
tau = rep(1000,nb_test)

Y_true = !(p1 == p2)
Y_pred = rep(0,nb_test)

n1 = 1000
n2 = 1000

x1 = rbinom(n1,1,p1)
x2 = rbinom(n2,1,p2)

mat_X = constuct_data(n1+n2,tau,p1,p2)


#K1 = multi_cusum(X,a=0.05,CPP=FALSE)
#K2 = multi_cusum(X,a=0.5,CPP=TRUE)


#print(compute_mf(Y_true,K1$y_pred,score="accuracy"))
#print(compute_mf(Y_true,K2$y_pred,score="accuracy"))

  */
