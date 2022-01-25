# cusum = function(X,h) {
#   n = length(X)
#   S = rep(0,n)
#   G = rep(0,n)
#   m0 = sum(X)
#   p0 = mean(X)
#   m1 = cumsum(X)[-1]
#   m2 = m0 - m1
#   p1 = m1 / tau
#   p2 = m2 / (n-tau+1)
#   
#   r = 0.5
#     
#   for (k in 2:n) {
#    # s[i] = m1*np.log(p1) + (tau-m1)*np.log(1-p1) + m2*np.log(p2) 
#     #+ (n-tau-m2)*np.log(1-p2) - m0*np.log(p0) - (n-m0)*np.log(1-p0)
#   S[k] = S[k-1] + X[k] - r
#   G[k] = max(0,G[k-1] + )
#   if (G[k] > h) {
#     nd = k
#     nc = argmin(S[nd-1])
#   }
#     
#   }
# }

cusum = function(X,h){
  n = length(x)
  B = rep(0,n)
  B[1] = 0
  r = 0.0 #todo estimate instant likelihood
  for(t in 2:n) {
    B[t] = max(0,B[t-1] + X[t]-r)
  }
  return (B)
}


cusum2 = function(X) {
  n = length(X)
  l = round(1/10 * n)
  h = round(9/10 * n) # bond to define
  
  S = rep(0,n)
  sigma_e = rep(0,n)
  p_e = mean(X)
  
  for(t in 1:n) {
    S[t] = sum(X[1:t]) - t/n * sum(X)
    sigma_e[t] =  t/n * (1 - t/n)
  }

  curveCSM = S/sqrt(n)
  
  Tt = curveCSM[-n] / sigma_e[-n]
  
  CSMmax = abs(curveCSM) / sd(X)
  tau = which.max(CSMmax)
  
  T_stat = max(Tt[l:h]^2)
  p_value = sqrt((T_stat*exp(-T_stat)/(2*pi))) * ((1-1/T_stat) * log((1-l)*h/(l*(1-h))) + 4/T_stat)
  #print(sqrt((Tt*exp(-Tt)/(2*pi))))
  #print(((1-1/Tt) * log((1-l)*h/(l*(1-h))) + 4/Tt))
  
  if(p_value <= 0.5 && p_value > 0) y_pred = 1
  else y_pred = 0
    
  return(list(Tt,curveCSM,tau,CSMmax,sigma_e,T_stat,Tt,p_value,y_pred))
}

p1 = runif(1) 
p2 = runif(1)
#p2 = p1
#p1 = 0.2
#p2 = 0.8
n1 = 100
n2 = 100
x1 = rbinom(n1,1,p1)
x2 = rbinom(n2,1,p2)
X = c(x1,x2)

CSM = cusum2(X)
plot(unlist(CSM[4]),type='l')
abline(v=CSM[3],col='red')
plot(unlist(CSM[5]),type='l')
plot(unlist(CSM[7]),type='l')
print(c(p1,p2,"diff",abs(p1-p2)))
print(c("p_value = ", unlist(CSM[8])))
print(c("tau = ",unlist(CSM[3])))
#plot(unlist(cusum2(X)[3]),type='l')








cumsum3 = function(n1,n2,p1,p2,r=0.1,h=1,same=FALSE) {
  n = n1+n2
  p1 = runif(1)
  p2 = runif(1)
  
  if(same) p2=p1
  
  x1 = rbinom(n1,1,p1)
  x2 = rbinom(n2,1,p2)
  X = c(x1,x2)
  s = rep(0,n)
  Gp = rep(0,n)
  Gm = rep(0,n)
  
  for(t in 2:n) {
    s[t] = X[t] #- X[t-1]
    Gp[t] = max(0, Gp[t-1]+s[t] - r)
    Gm[t] = max(0, Gm[t-1]-s[t] - r)
    
    if(Gp[t] > h || Gm[t] > h) {
      tau = t
      return (list(tau,Gp,Gm,X))
    }
  }
  return (list(-1,Gp,Gm,X))
}

CSM = cumsum3(500,500,0.2,0.8,r=0.05,h=0.1)
plot(unlist(CSM[2]),type='l')
plot(unlist(CSM[3]),type='l')
plot(unlist(CSM[4]),type='l')



test_cusum = function(CSM,h) {
  if(sum(CSM > h) > 0) return(TRUE)
  else return(FALSE)
}

n = 500
nb_test = 300
Y_true = seq(1,nb_test,by=1)
Y_true = rbinom(n=nb_test,size=1,p=0.5)
Y_pred = rep(0,nb_test)
h=0.1

for(i in 1:nb_test) {
  p1 = runif(1)
  p2 = runif(1)
  if(Y_true[i] == 0) p2 = p1

p = c(p1,p2)
x1 = rbinom(n,1,p[1])
x2 = rbinom(n,1,p[2])

X = c(x1,x2)
plot(X)
#CSM = cusum(X)
CSM = cusum2(X)
# 
# plot(CSM,type='l')
# plot(CSM2$curveCSM,type='l')
# 
# h = seq(5,10,by=0.01)
# for (i in 1:length(h)) {
#   if(test_cusum(CSM,h[i])) {
#     print(h[i])
#     break
#   }
# }
if(p1 < p2) tau = which.min(CSM)
else tau = which.max(CSM)

p1e = mean(X[1:tau])
p2e = mean(X[(tau):n])

if(abs(p1e - p2e) >= h) Y_pred[i] = 1

}
plot(X)
abline(v=n,col='green')
plot(CSM,type='l')
abline(v=n,col='green')
if(Y_pred[nb_test] == 1) abline(v=tau,col='red')





  