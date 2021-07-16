library(ggplot2)
library(gridExtra)
library(Rwave)
#-----data generation------#
Fs = 30            # Sampling frequency                    
T = 1/Fs             # Sampling period       
L = 2000             # Length of signal
t = (0:L-1)*T        # Time vector
ff = 1:length(t) * (length(t) / Fs)		#周波数
S = 0.7 * sin(t) + 1.5 * sin(1/10 * t) 
X = S + rnorm(length(t), sd = .1)

init <- sample(1:(length(X)-100), 5)
mark <- NULL
for(i in 1:length(init)){
  mark <- c(mark, init[i]:(init[i]+100))
}

init_end <- NULL
for(i in 1:length(init)){
  init_end <- c(init_end, init[i], init[i]+100)
}

X[mark] <- X[mark] + 1.3 * sin(5 * X[mark]) 
marker <- rep(0, length(X))
marker[mark] <- 1

# take difference?
# X <- X[2:length(X)] - X[1:(length(X)-1)]

ggplot() + 
  geom_rect(aes(xmin=t[init], xmax=t[init+100], ymin=min(X), ymax=max(X)), fill="red", alpha=.3) + 
  geom_line(aes(x=t,y=X, group=NA)) +
  theme(legend.position = "none")

#-----wavelet transformation-----#
Y <- cwt(X, noctave=20, nvoice=20)

wave <- as.data.frame(Y)
wave <- abs(wave)

d <- reshape2::melt(wave)
d$freq <- as.integer(d$variable)
d$time <- rep(t, nrow(d)/length(t))
ggplot(d) + 
  geom_tile(aes(x = time, y=freq, fill=value)) + 
  scale_fill_gradient2(low="blue", mid="black", high="red", midpoint=quantile(d$value, .5))

#-----principle component analysis-----#
pc.res <-  prcomp(as.matrix(wave))

grid.arrange(
  ggplot() + 
    geom_point(aes(x=pc.res$x[,1], pc.res$x[,2], colour=as.factor(marker))) + theme(legend.position = "none"),
  ggplot() + 
    geom_point(aes(x=pc.res$x[,3], pc.res$x[,4], colour=as.factor(marker))) + theme(legend.position = "none"),
  ggplot() + 
    geom_point(aes(x=pc.res$x[,5], pc.res$x[,6], colour=as.factor(marker))) + theme(legend.position = "none"),
  ggplot() + 
    geom_point(aes(x=pc.res$x[,7], pc.res$x[,8], colour=as.factor(marker))) + theme(legend.position = "none"),
  nrow=2
)

#-----clustering with gaussian mixture model-----#
library(mclust)
gmm.res <- Mclust(pc.res$x[,1:2], G=3)
# here ICA analysis
# library(fastICA)
# gmm.res <- Mclust(fastICA(as.matrix(wave), n.comp=3)$X, G=3)

ggplot() + 
  geom_rect(aes(xmin=t[init], xmax=t[init+100], ymin=min(X), ymax=max(X)), fill="red", alpha=.3) + 
  geom_line(aes(x=t,y=X,colour=factor(gmm.res$classification), group=NA)) +
  theme(legend.position = "none")

# correct  
ans <- rep(0, length(X))
ans[gmm.res$classification==1] <- 1
sum(ans == marker)/length(X)

