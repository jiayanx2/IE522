put_price<-function(n){
  max=vector(length=n)
  r=0.02
  q=0.01
  sigma=0.2
  T=1
  S0=K=100
  BT=rnorm(n,0,1)
  ST=S0*exp((r-q-0.5*sigma*sigma)*T+sigma*BT)
  for(i in 1:n){max[i]=max(0,K-ST[i])}
  mean=mean(exp(-r*T)*max)
  sd=sd(exp(-r*T)*max)
  se=sd/sqrt(n)
  z=-qnorm(0.025)
  b=mean(exp(-r*T)*max)+se*z
  a=mean(exp(-r*T)*max)-se*z
  c<-c(mean, se,a,b)
  return(c)
}
