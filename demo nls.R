
ggplot() + 
  geom_point(data=iris, aes(x=Sepal.Width, y=Sepal.Length, colour=Species))

name = levels(iris$Species)
iris$name1 = as.integer(iris$Species == name[1])
iris$name2 = as.integer(iris$Species == name[2])
iris$name3 = as.integer(iris$Species == name[3])

res = nls(Sepal.Length ~ 
            a1*exp(b1*Sepal.Width) * name1 + 
            a2*exp(b2*Sepal.Width) * name2 + 
            a3*exp(b3*Sepal.Width) * name3 ,
          data=iris, start=list(a1=0.1, b1=0.1, a2=0.1, b2=0.1, a3=0.1, b3=0.1))

xx = seq(2.0, 4.5, length=100)
new_data_frame = data.frame( Sepal.Width = rep(xx, 3), 
                             name1 = c(rep(1, 100), rep(0, 200)),
                             name2 = c(rep(0,100), rep(1, 100), rep(0, 100)),
                             name3 = c(rep(0, 200), rep(1, 100)),
                             Species = rep(levels(iris$Species), each=100)
)
new_data_frame$pred = predict(res, new_data_frame)

ggplot() + 
  geom_point(data=iris, aes(x=Sepal.Width, y=Sepal.Length, colour=Species)) + 
  geom_line(data=new_data_frame, aes(x=Sepal.Width, y=pred, colour=Species))

# AIC
AIC(res)

# BIC
BIC(res)

# R- squared
Rsq = 1-sum( ( summary(res)$resid^2 ) ) / (nrow(iris)*var(iris$Sepal.Length)) 
Rsq 

# adjusted R-squared
p = summary(res)$df[1] # the number of parameters
1 - sum(summary(res)$resid^2/ (nrow(iris) * p - 1)) / sum(summary(res)$resid^2 / (nrow(iris) -1) )

