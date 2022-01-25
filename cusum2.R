source(file = "scorefct.R")
library(tictoc)
library(GuessCompx)

cusum = function(X,a=0.05) {
  n = length(X)
  l = 1/10
  h = 9/10  # borne à définir (hypothèse : tau se trouve dans [l*n,h*n])
  
  S = rep(0,n)
  sigma_e = rep(0,n)
  
  for(t in 1:n) {
    S[t] = sum(X[1:t]) - t/n * sum(X)
    sigma_e[t] =  t/n * (1 - t/n)
  }
  
  curveCSM = S/sqrt(n)
  
  Tt = curveCSM[-n] / sigma_e[-n]
  
  CSMmax = abs(curveCSM) / sd(X)
  tau = which.max(CSMmax)
  
  T_stat = max(Tt[round(l*n):round(h*n)]^2)
  p_value = sqrt((T_stat*exp(-T_stat)/(2*pi))) * ((1-1/T_stat) * log((1-l)*h/(l*(1-h))) + 4/T_stat)
  
  if(is.nan(p_value)) y_pred = NaN
  else if(p_value <= a && p_value > 0) y_pred = 1
  else y_pred = 0

  return(list('Tt'=Tt,'curveCSM'=curveCSM,'tau'=tau,'CSMmax'=CSMmax,'sigma_e'=sigma_e,
              'T_stat'=T_stat,'p_value'=p_value,'y_pred'=y_pred))
}

p1 = runif(1) 
p2 = runif(1)

n1 = 500
n2 = 500
x1 = rbinom(n1,1,p1)
x2 = rbinom(n2,1,p2)
X = c(x1,x2)

CSM = cusum(X)
plot(CSM$CSMmax,type='l')
abline(v=CSM$tau,col='red')
print(c(p1,p2,"diff",abs(p1-p2)))
print(c("p_value = ", CSM$p_value))
print(c("tau = ",CSM$tau))
print(c("class = ",CSM$y_pred))

nb_test = 1000
Y_true = rbinom(nb_test,1,0.5)
Y_pred = rep(0,nb_test)
n1 = 500
n2 = 500

tic("sleeping")

for(i in 1:nb_test) {
  p1 = runif(1) 
  p2 = runif(1)
  if(Y_true[i] == 0) p2=p1
  x1 = rbinom(n1,1,p1)
  x2 = rbinom(n2,1,p2)
  X = c(x1,x2)
 
  CSM = cusum(X)
  Y_pred[i] = CSM$y_pred
  if(is.nan(Y_pred[i])) {
    print(c("Erreur NaN ##########################################",p1,p2))
    break
  }
}

toc()

print(compute_mf(Y_true,Y_pred,score="accuracy"))

