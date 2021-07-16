# http://ryamada22.hatenablog.jp/entry/20150409/1428549068
# source("https://bioconductor.org/biocLite.R")
# biocLite()
# source("http://bioconductor.org/biocLite.R")
# library(BiocInstaller)
# biocLite("pcaMethods")
library(pcaMethods)
library(dplyr)
library(ggplot2)
library(MASS)

# list of methods avaiable
listPcaMethods() 
data(iris)

##  Usually some kind of scaling is appropriate
# method="svd" peforms the ordinary PCA (same as prcomp() function)
res.pca = pca(iris, method="svd", nPcs=4)
res.pca@completeObs %>% str
str(res.pca)

# PC scores
res.pca@scores

## Use Probabilistic PCA for data with NA
numNA = 10
iris2 = iris
NAcol = sample(1:4, numNA, replace = TRUE)
NArow = sample(1:150, numNA, replace = FALSE)

for(i in 1:numNA){
  iris2[NArow[i], NAcol[i]] = NA
}

res.pca2 = pca(iris2, method="ppca", nPcs=2)

ggplot() + 
  geom_point(aes(x=res.pca2@scores[,1], y=res.pca2@scores[,2], colour=iris$Species))


## simulation to examine the robustness to the number of missing
sim = function(numNA, method="ppca"){
  iris2 = iris
  
  remove = 0
  while(remove != numNA){
    NAcol = sample(1:4, 1, replace = TRUE)
    NArow = sample(1:150, 1, replace = TRUE)
    
    if( sum(is.na(iris2[NArow,])) < 3 )
      iris2[NArow, NAcol] = NA
    
    remove = remove + 1
  }
  
  res.pca2 = pca(iris2, method=method, nPcs=2)
  mod = lda(iris$Species ~ res.pca2@scores[,1] + res.pca2@scores[,2] ) 
  
  rate=sum(predict(mod)$class == iris$Species)/nrow(iris)
  return(rate)
}

sim2 = function(numNA, method="ppca"){
  
  e = try( sim(numNA, method),silent=FALSE)
  if( class(e) == "try-error") {
    e = NA
  }
  return(e)
}

N = c(0, 10, 50, 100, 150, 300)

m1 = NULL
SD1 = NULL
m2 = NULL
SD2 = NULL
m3 = NULL
SD3 = NULL
m4 = NULL
SD4 = NULL

num_repli =100
for(i in 1:6){
  res1 = replicate( num_repli, sim2(N[i], "ppca")) # probabilistic PCA
  res2 = replicate( num_repli, sim2(N[i], "bpca")) # bayesian PCA
  res3 = replicate( num_repli, sim2(N[i], "svdImpute")) # linear imputation
  res4 = replicate( num_repli, sim2(N[i], "nipals"))  # non-linear imputation
  m1 = c(m1, mean(res1,na.rm=TRUE))
  SD1 = c(SD1, sd(res1,na.rm=TRUE))
  m2 = c(m2, mean(res2,na.rm=TRUE))
  SD2 = c(SD2, sd(res2,na.rm=TRUE))
  m3 = c(m3, mean(res3,na.rm=TRUE))
  SD3 = c(SD3, sd(res3,na.rm=TRUE))
  m4 = c(m4, mean(res4,na.rm=TRUE))
  SD4 = c(SD4, sd(res4,na.rm=TRUE))
}

rate = data.frame(mean = c(m1, m2, m3, m4),
                  SD  = c(SD1, SD2, SD3, SD4),
                  method = rep(c("prob", "bayes", "svdImpute", "nipals"), each = length(N)),
                  N = rep(N,4)
                  )

ggplot(rate) + 
  geom_errorbar(aes(x=as.factor(N), ymin=mean-SD, ymax=mean+SD), width=0,
                position = position_dodge(width=100))  + 
  geom_line(aes(x=as.factor(N), y=mean, colour=method, group=method), 
             position = position_dodge(width=.5))  +
  geom_point(aes(x=as.factor(N), y=mean, colour=method), size=3,
             position = position_dodge(width=.5)) 


pos <- position_dodge(.5)
lim <- aes(ymin=mean-SD, ymax=mean+SD)

rate = data.frame(mean=sim2(0, method = "svd"), SD=0, method="svd", N=0) %>% 
  rbind(., rate)

ggplot(rate) + 
  geom_errorbar(data=rate,aes(x=as.factor(N), ymin=mean-SD, ymax=mean+SD, group=method),
                position = pos, width=0) + 
  geom_line(aes(x=as.factor(N), y=mean, colour=method, group=method),
            position = position_dodge(width=.5),) +
  geom_point(aes(x=as.factor(N), y=mean, colour=method, group=method), size=3,
             position = position_dodge(width=.5)) +
  xlab("the number of removed data") + ylab("percent of correct") 

