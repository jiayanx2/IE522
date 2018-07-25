claim<-function(m,q,alpha,beta,H,S,F){
    N= rbinom(500,1,0.2)
    n=sum(N)
    g=vector(length=sum(N))
    X=rgamma(n,alpha,1/beta)
    for (i in 1:n) {g[i]=max(0,S/H*(X[i]-F))}
     s_1=sum(g)
     return(s_1)
 }

trial<-function(m,p){
    S=vector(length=m)
    for (i in 1:m) {S[i]=claim (500,0.2,2,3,35,15,6)}
    result<-c(sum(S)/m,var(S))
    return(result)
}

CVaR_S_P<- function(m,p){
  S_P=vector(length=m)
  for (i in 1:m) {S_P[i]=claim (500,0.2,2,3,35,15,6)}
  s_p=quantile(S_P,p)
  S_P_cond= vector(length=m)
  for (i in 1:m) {S_P_cond[i]=max(0, s_p)}
  result<-c(sum(S_P_cond)/m)
  return(result)
}

CVaR_S<- function(m,p)
{
S=vector(length=m)
for (i in 1:m) {S[i]=claim (500,0.2,2,3,35,35,0)}
s_p=quantile(S,p)
S_cond= vector(length=m)
for (i in 1:m) {S_cond[i]=max(0, s_p)}
result<-c(sum(S_cond)/m)
return(result)
}


EN=m*q
DN=m*q*(1-q)
     EX=alpha*beta
ES=EN*EX
K=CVaR-EN
     p_F= length(which(g>0))/n
E_p_N= p_F*(1- p_F)*EN+ p_F* p_F*DN
