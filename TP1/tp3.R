# Génération des Points

# Tracer kes différents classes 
# 1. Génération du nuage de points

# a. Génération de x et de y uniformes sur [0,1]
x_u <- runif(100)
y_u <- runif(100)
#loi_u <- runif(100, 0,1)
plot(x_u, y_u, col="red")

# b. Génération de x et y gaussienne de variance 1, x de moyen 4 et y centrée
x_g1=dnorm(runif(100), 4, 1 )
y_g1=scale(runif(100),center=TRUE, scale=FALSE)
plot(x_g1,y_g1,col="blue")

# c. Génération de x et y gaussienne de variance 2, x de moyen 0.5 et y moyen 6
x_g2=dnorm(runif(100), 0.5, 2 )
y_g2=dnorm(runif(100), 6, 2 )
plot(x_g2,y_g2, col="green")

##########################################################"""

# 2. Classification non supervisée

# calculer la distance Euclidean

matrix<-rbind(x_u, y_u)
umatrix<-rbind(x_g2,y_g2)

known_data <- cbind(x_u,y_u)
unknown_data <- cbind(x_g1,y_g1)
#euclidean_distance <- function(p,q){
#  for(i in 1:100)
#    sqrt(sum((p - q)^2))
#}
# distance euclidean 
euclidean_dist <- function(k,unk) {
  # la distance de vecteur
  #distance <- rep(0, ncol(k))
  distance <- rep(0, nrow(k))
  
  #for(i in 1:ncol(k))
  for(i in 1:nrow(k))
    # Changer unk[,1][i] à unk[1,1] et similaire pour unk[,2][i]
    distance[i] <- sqrt((k[,1][i] - unk[1,1])^2 + (k[,2][i] - unk[1,2])^2)
  
  return(distance)
} 
euclidean_dist(matrix, umatrix)
#euclidean_dist(known_data, unknown_data)

# vérifier
nuage<-cbind(x_u,y_u)
#nuage<-rbind(x_u,y_u)
D <- dist(matrix, method="euclidean")
plot(D)

# la fonction de classification ascendante hiérarchique

# vérifier
AscHierarchique <- hclust(D, method="complete") # complete ou ward

# visualisation par Dendrogramme

#vérifier
plot(AscHierarchique, cex=0.6, hang=-1)

# récupérer les résultats

# vérifier
cluster = cutree(AscHierarchique,3)
