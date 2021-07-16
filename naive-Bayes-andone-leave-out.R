library(klaR)
library(ggplot2)

df <- iris

ggplot(data.tra, aes(x=Petal.Width, fill=Species)) + 
  geom_histogram(aes(y=..density..), position="identity", binwidth=0.08)+
  geom_density(alpha=.5)  # Overlay with transparent density plot

pred <- 0

for(i in 1:nrow(df)){
  data.tra <-iris[-i,] 
  data.test<-iris[i,] 
  
  
  NB.Model <- NaiveBayes(Species~., data=data.tra)
  
  NB.Pred <- predict(NB.Model,data.test[,1:4]) 
  result <-(table(data.test[,5],NB.Pred$class))
  result
  
  pred <- pred + sum(diag(result))/nrow(data.test)
  
}

pred/nrow(df)
