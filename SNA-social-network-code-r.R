library(igraph)
library(RColorBrewer)

## Data + Networks -------------

data <- read.csv("hero-network.csv")
netBig <- graph.data.frame(data, directed=F)

dataSmall <- data[1:10000,]
net <- graph.data.frame(dataSmall, directed=F)

plot(net,
     vertex.label.cex = 0.01,
     vertex.size = 2,
     vertex.color = rainbow(674),
     layout=layout.fruchterman.reingold)

## Small Scale High Degree --------------

V(net)
E(net)
degree(net)

degrees <- degree(net)
names <- names(degree(net))
degreeDF <- as.data.frame(degrees, names)
degreeDF$names <- names

greaterThanNames <- degreeDF[degreeDF$degree > 100,]$names

hero1_higherDegree <- dataSmall[(dataSmall$hero1) == greaterThanNames,]
hero2_higherDegree <- dataSmall[(dataSmall$hero2) == greaterThanNames,]

highDegreeData <- rbind(hero1_higherDegree, hero2_higherDegree)

highDegreeNet <- graph.data.frame(highDegreeData, directed = F)

plot(highDegreeNet,
     vertex.label.cex = 0.5,
     vertex.size = 5,
     vertex.color = 'red',
     layout=layout.fruchterman.reingold)


## Full Scale High degree -----------

data <- read.csv("hero-network.csv")
netBig <- graph.data.frame(data, directed=F)

head(data)
head(netBig)

V(netBig)
E(netBig)
degree(netBig)


degrees2 <- degree(netBig)
names2 <- names(degree(netBig))
degreeDF2 <- as.data.frame(degrees2, names2)
degreeDF2$names <- names2

greaterThanNames2 <- degreeDF2[degreeDF2$degree > 3000,]$names

hero1_higherDegree2 <- data[data$hero1 == greaterThanNames2,]
hero2_higherDegree2 <- data[data$hero2 == greaterThanNames2,]

highDegreeData2 <- rbind(hero1_higherDegree2, hero2_higherDegree2)

highDegreeNet2 <- graph.data.frame(highDegreeData2, directed = F)

plot(highDegreeNet2,
     vertex.label.cex = degreeDF2$degrees2 * .00005,
     vertex.label.dist= 1,
     vertex.label.font = 1,
     vertex.label.color = 'red',
     vertex.size = degreeDF2$degrees2 * .001,
     vertex.color = 'grey',
     edge.color = 'black',
     #layout=layout_in_circle,
     layout=layout_randomly)


## Ironman Social Network ---------

head(data)

IM1 <- data[data$hero1 == "IRON MAN/TONY STARK ",]
IM2 <- data[data$hero2 == "IRON MAN/TONY STARK ",]

IMdata <- rbind(IM1, IM2)
IMdata$hero1 <- gsub("IRON MAN/TONY STARK ", "Ironman", IMdata$hero1)
IMdata$hero2 <- gsub("IRON MAN/TONY STARK ", "Ironman", IMdata$hero2)
IMnet <- graph.data.frame(IMdata, directed=F)

V(IMnet)
degree(IMnet)

IMdegrees <- degree(IMnet)
IMnames <- names(degree(IMnet))
IMdegreeDF <- as.data.frame(IMdegrees, IMnames)
IMdegreeDF$names <- IMnames

IMgreaterThanNames <- IMdegreeDF[IMdegreeDF$IMdegrees > 10 ,]$names

IMhero1_higherDegree <- IMdata[IMdata$hero1 == IMgreaterThanNames,]
IMhero2_higherDegree <- IMdata[IMdata$hero2 == IMgreaterThanNames,]

IMhighDegreeData <- rbind(IMhero1_higherDegree, IMhero1_higherDegree)
IMhighDegreeNet <- graph.data.frame(IMhighDegreeData, directed = F)

plot(IMhighDegreeNet,
     vertex.label.cex = (degree(IMnet)) * .001,
     vertex.label.dist= 1,
     vertex.label.font = 1,
     vertex.label.color = 'black',
     vertex.size = degree(IMnet) * .01,
     vertex.color = 'red',
     edge.color = 'grey',
     #layout=layout_randomly
     #layout=layout_components
     #layout=layout_in_circle
     layout=layout_with_kk
     )




