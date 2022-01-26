source(file = "scorefct.R")
library(tictoc)
library(GuessCompx)

DIM = function(x) if(is.null(dim(x))) length(x) else dim(x)[1]

constuct_data = function(n,tau,p1,p2) {
  nb_test = length(p1)
  mat_X = matrix(data=NA,nrow = nb_test,ncol=n)
  for(i in 1:nb_test) {
    x1 = rbinom(n=tau[i],size=1,prob=p1[i])
    x2 = rbinom(n=n-tau[i],size=1,prob=p2[i])
    mat_X[i,] = c(x1, x2)
  }
  return(mat_X)
}


cusum = function(X,a=0.05) {
  TIMER = as.numeric(Sys.time()) ####################
  
  l = 1/10
  h = 9/10  # borne à définir (hypothèse : tau se trouve dans [l*n,h*n])
  
    n = length(X)
    S = sigma_e = rep(0,n)
    
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
    
    if(is.nan(p_value)) {
      y_pred = NaN
      print(c("############# Erreur NaN p_value #############"))
    }
    else if(p_value <= a && p_value > 0) y_pred = 1
    else y_pred = 0
  
  TIMER = as.numeric(Sys.time() - TIMER) #######################
  
  return(list('Tt'=Tt,'curveCSM'=curveCSM,'tau'=tau,'CSMmax'=CSMmax,'sigma_e'=sigma_e,
              'T_stat'=T_stat,'p_value'=p_value,'y_pred'=y_pred,'time'= TIMER))
}

mini_bench = function(n_seq,tau=-42) {
  n_bench = length(n_seq)
  times = rep(NA,n_bench)
  
  for(i in 1:n_bench) {
    
    p1 = runif(1,min=0.1,max=0.9)
    p2 = runif(1,min=0.1,max=0.9)
    
    if(rbinom(1,size=1,prob=0.5)) p2 = p1
    
    if(tau == -42) tau2 = sample(1:n_seq[i],1)
    else tau2 = tau[i]
    
    x1 = rbinom(n=tau2,size=1,prob=p1)
    x2 = rbinom(n=n_seq[i]-tau2,size=1,prob=p2)
    
    X = t(matrix(c(x1,x2)))
    CSM = cusum(X)
    
    times[i] = CSM$time
  }
  
  plot(n_seq,times,type="l",col='black')
  # points(n_seq,rep(1,n_bench),type="l",col='red')
  # points(n_seq,n_seq,type="l",col='blue')
  # points(n_seq,log(n_seq),type="l",col='yellow')
  # points(n_seq,n_seq*log(n_seq),type="l",col='brown')
  # points(n_seq,n_seq^0.5,type="l",col="green")
  # points(n_seq,n_seq^2,type="l",col="pink")
  # points(n_seq,n_seq^3,type="l",col="orange")
  
  return(times)
}

p1 = runif(1) 
p2 = runif(1)

n1 = 500
n2 = 500
x1 = rbinom(n1,1,p1)
x2 = rbinom(n2,1,p2)
X = c(x1,x2)
X = t(as.matrix(X))

CSM = cusum(X)
plot(CSM$CSMmax,type='l')
abline(v=CSM$tau,col='red')
abline(v=500,col='green')
print(c(p1,p2,"diff",abs(p1-p2)))
print(c("p_value = ", CSM$p_value))
print(c("tau = ",CSM$tau))
print(c("class = ",CSM$y_pred))



nb_test = 500
p1 = runif(nb_test,min=0.1,max=0.9)
p2 = runif(nb_test,min=0.1,max=0.9)
id = rbinom(nb_test,size=1,prob=0.5)
p2[id==0] = p1[id==0]
tau = rep(1000,nb_test)
Y_true = !(p1 == p2)
Y_pred = rep(0,nb_test)
n = 2000

mat_X = constuct_data(n,tau,p1,p2)

TIMER = Sys.time()
Y_pred = multi_cusum(mat_X)$y_pred
TIMER = Sys.time() - TIMER

print(compute_mf(Y_true,Y_pred,score="accuracy"))

#CompEst(mat_X,multi_cusum)


#n_seq = seq(from=200,to=20000,by=200)
#mini_bench(n_seq)

library(randomForest)

set.seed(100)
train = sample(nrow(mat_X), 0.7*nrow(mat_X), replace = FALSE)
TrainSet = mat_X[train,]
ValidSet = mat_X[-train,]

Y_train = Y_true[train]
Y_test = Y_true[-train]
RDM_forest = randomForest(as.factor(Y_train) ~. ,data=TrainSet,ntree=500)

Y_pred_RF = predict(RDM_forest,ValidSet,type="class")

print(compute_mf(Y_test,Y_pred_RF,score="accuracy"))

 
