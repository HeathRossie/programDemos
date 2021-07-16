#-----豊田本7.4-----#
# 幾何分布を用いた推測

N<-6
x <- c(19, 34, 11, 26, 22, 30)

hist(x)
code <- '
data{
  int<lower=0> N;
  int<lower=0> x[N];
}

transformed data{
  int<lower=0> n[N];
  for(i in 1:N)
    n[i] <- x[i]-1;
}

parameters{
  real<lower=0> beta;
}

transformed parameters{
  real<lower=0, upper=1> theta;
  theta <- beta/(beta+1);
}

model{
  n ~ neg_binomial(1,beta);
}
'

dat <- list(N=N, x=x)
fit <- stan(model_code = code, data = dat)