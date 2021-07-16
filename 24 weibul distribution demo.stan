data {
  int<lower=0> N; 
  real<lower=0> Sec[N];
  int N_Dyad;
  int Dyad[N];
  int Exp[N];
}

parameters {
  real<lower=0.0001> m;
  real beta;
  real<lower=0.0001,upper=1000000> eta;
}

model {
  for(i in 1:N)
    Sec[i] ~ weibull(m, eta);
}
