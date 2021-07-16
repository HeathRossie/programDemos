# according to https://stats.stackexchange.com/questions/229092/how-to-reverse-pca-and-reconstruct-original-variables-from-several-principal-com
library(ggplot2)
library(dplyr)
library(gridExtra)

X = iris[,1:4]
mu = colMeans(X)

Xpca = prcomp(X)


grid.arrange(
  ggplot( as.data.frame(Xpca$x[,1:nComp] )) + 
    geom_point(aes(x=PC1, y=PC2, colour=iris$Species)),
  ggplot( as.data.frame(Xpca$x[,1:nComp] )) + 
    geom_point(aes(x=PC3, y=PC4, colour=iris$Species))
)

nComp = 4
Xhat = Xpca$x[,1:nComp] %*% t(Xpca$rotation[,1:nComp])
Xhat = scale(Xhat, center = -mu, scale = FALSE)

# complete back projection
X[1,]
Xhat[1,]


# back project to the most typical features for each species
PC = as.data.frame(Xpca$x)
PC$Species = iris$Species

mat = cbind(tapply(PC$PC1, PC$Species, mean),
            tapply(PC$PC2, PC$Species, mean),
            tapply(PC$PC3, PC$Species, mean),
            tapply(PC$PC4, PC$Species, mean)
)

Xhat = mat %*% t(Xpca$rotation[,1:nComp])
Xhat = scale(Xhat, center = -mu, scale = FALSE)

Xhat
tapply(iris$Sepal.Length, iris$Species, mean)
