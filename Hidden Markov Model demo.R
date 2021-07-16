library(depmixS4)
library(tidyverse)

# data generation
state = 3
MEAN = runif(state, -100,100)
REP = rpois(20, 100)
SIG = 30

data = NULL
for(i in 1:length(REP)){
  data = c(data, rnorm(REP[i], MEAN[sample(1:state,1)], SIG))
}


# visualize data
time = 1:length(data)
ggplot() + 
  geom_line(aes(x=time, y=data))


# fitting Hidden Markov Model
d = data.frame(val = data)
mod = depmix(val ~ 1, data = d, nstates = 3, family = gaussian())
fit.mod = fit(mod)
state = posterior(fit.mod, type="viterbi")$state
d$state = as.factor(state)

# visualize result
ggplot(d) + 
  geom_line(aes(x=time, y=val, colour=state, group=NA))


# comparison
BICs = NULL
for(i in 1:10){
  temp = depmix(val ~ 1, data = d, nstates = i, family = gaussian()) %>% fit %>% BIC
  BICs = c(BICs, temp)
}

plot(BICs, type="b")


# vis
mod = depmix(val ~ 1, data = d, nstates = which.min(BICs), family = gaussian())
fit.mod = fit(mod)
state = posterior(fit.mod, type="viterbi")$state
d$state = as.factor(state)

# visualize result
ggplot(d) + 
  geom_line(aes(x=time, y=val, colour=state, group=NA))
