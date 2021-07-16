#-----ネットワーク分析のdemo-----#
library(igraph)
setwd("~/Desktop/netwk")
dec <- read.csv('dec.csv')
feb <- read.csv('feb.csv')
march <- read.csv('march.csv')
april <- read.csv('april.csv')
may <- read.csv('may.csv')

rownames(dec) <- colnames(dec)
dec[is.na(dec)] <- 0
rownames(feb) <- colnames(feb)
feb[is.na(feb)] <- 0
rownames(march) <- colnames(march)
march[is.na(march)] <- 0
rownames(april) <- colnames(april)
april[is.na(april)] <- 0
rownames(may) <- colnames(may)
may[is.na(may)] <- 0


#-----データの生成-----#
cat <- c("A","B","C","D","E","F","G","H")
# cat <- c("A","B","C")

catdat <- data.frame(sample(cat, 200, replace = TRUE),
                     sample(cat, 200, replace = TRUE))

# catdat <- data.frame("A","B")

#-----分析-----#
g <- graph.data.frame(catdat) 
g <- graph.data.frame(ants[1:100,1:2])
g <- graph.adjacency(table(catdat), weighted=TRUE)

g <- graph.adjacency(as.table(as.matrix(dec)), weighted=TRUE)
g <- graph.adjacency(as.table(as.matrix(feb)), weighted=TRUE)
g <- graph.adjacency(as.table(as.matrix(march)), weighted=TRUE)
g <- graph.adjacency(as.table(as.matrix(april)), weighted=TRUE)
g <- graph.adjacency(as.table(as.matrix(may)), weighted=TRUE)

plot.igraph(g, layout=layout.fruchterman.reingold,
            edge.width=E(g)$weight/2)



#-----指標の算出-----#
betweenness(g)
degree(g)
closeness(g)

d <- stack(closeness(g))
d$month <- 12

month = rep(2, length(closeness(g)))
d <- rbind(d, cbind(stack(closeness(g)), month))

month = rep(3, length(closeness(g)))
d <- rbind(d, cbind(stack(closeness(g)), month))

month = rep(4, length(closeness(g)))
d <- rbind(d, cbind(stack(closeness(g)), month))

month = rep(5, length(closeness(g)))
d <- rbind(d, cbind(stack(closeness(g)), month))


d[d$month==12,]$month <- 0

library(ggplot2)
ggplot(d, aes(x=month, y=values, colour=ind)) +
  geom_line(size=2, position = position_dodge(0.3)) + geom_point(size=5, position = position_dodge(0.3)) 



#------動的ネットワーク-----#
# http://www.benjaminblonder.org/timeorderednetworks/

library(timeordered)

# アリのデータらしい
data(ants)
str(ants)
?ants
head(ants)

# interactionを時系列で描く
g <- generatetonetwork(ants)
plottonet(g,edgecolor="black")

# time-aggregated ネットワーク
td100 <- generatetimedeltas(0,1500,50)
ns100 <- generatenetworkslices(g, td100)
plotnetworkslices(ns100, td100)

td500 <- generatetimedeltas(0,1500,500)
ns500 <- generatenetworkslices(g, td500)
plotnetworkslices(ns500, td500)
# ネットワークの尺度を時系列で表示
md100 <- applynetworkfunction(ns100, function(x) {diameter(x)})
plot(midpoints(td100),unlist(md100),type="l",col="blue",xlab="Time",ylab="Network diameter")
md500 <- applynetworkfunction(ns500, function(x) {diameter(x)})
lines(midpoints(td500),unlist(md500),col="red")
legend("topleft",c("100 window size", "500 window size"),bty="n",lwd=2,col=c("blue","red"))

tl100 <- generatetimelags(0,1500,100)
nl100 <- generatenetworkslices(g, tl100)
ml100 <- applynetworkfunction(nl100, function(x){diameter(x)})
plot(maxpoints(tl100),unlist(ml100),type="l",xlab="Aggregation time",ylab="Diameter",col="blue")
