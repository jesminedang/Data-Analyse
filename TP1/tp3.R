# Génération des Points

# Tracer kes différents classes 
# 1. Génération du nuage de points

# a. Génération de x et de y uniformes sur [0,1]
x_u <- runif(100, 0, 1)
y_u <- runif(100, 0, 1)
u<-cbind(x_u,y_u)
#plot(x_u, y_u, col="red")

# b. Génération de x et y gaussienne de variance 1, x de moyen 4 et y centrée
x_g1=rnorm(runif(100), 4, 1 )
y_g1=scale(runif(100),center=TRUE, scale=FALSE)
#plot(x_g1,y_g1,col="green")

# c. Génération de x et y gaussienne de variance 2, x de moyen 0.5 et y moyen 6
x_g2=rnorm(runif(100), 0.5, sqrt(2) )
y_g2=rnorm(runif(100), 6, sqrt(2) )
#plot(x_g2,y_g2, col="blue")

# Créer une matrice de 300 lignes et 2 colonnes qui contient 300 points generes
M<-matrix(c(x_u,x_g1,x_g2,y_u,y_g1,y_g2),nrow=300,ncol=2)
M2<-M

# Tracer l'ensemble des 300 points:
col<-matrix(data=NA,300,1)
col[1:100]<-"red"
col[101:200]<-"green"
col[201:300]<-"blue"
plot(M,col=col)

##########################################################

# 2. Classification non supervisée

# Génerer une matrice identite
id_Matrice<-diag(x=1,nrow=300,ncol=300)

# La fonction pour calculer la distance euclidean 
euclidean_dist <-function(M){
  A<-c()
  n<-nrow(M)
  for(i in 1:n){
    for(j in 1:n){
      #d=dist_euclidean(M[i,],M[j,])
      d=sqrt((M[i,1] - M[j,1])^2 + (M[i,2] - M[j,2])^2)
      A<-c(A,d)
    }
  }
  matrice<-matrix(c(A),nrow=n,ncol=n)
  return (matrice)
}
euclidean_dist(M)
#euclidean_dist <- function(k,unk) {
  # la distance de vecteur
#  distance <- rep(0, nrow(k))

#  for(i in 1:nrow(k))
    # Changer unk[,1][i] à unk[1,1] et similaire pour unk[,2][i]
#    distance[i] <- sqrt((k[,1][i] - unk[1,1])^2 + (k[,2][i] - unk[1,2])^2)
  
#  return(distance)
# } 
# euclidean_dist(matrix, umatrix)

# Vérifier la matrice de distance
#nuage<-M
D <- dist(M, method="euclidean")

# La fonction de classification ascendante hiérarchique

# La classification jusqu'a ce qu'il reste K classes
K=4
while(nrow(M)>K){
  #calculer la matrice de distance
  distance_matrice=euclidean_dist(M)
  
  #chercher la plus petite distance
  tmp<-distance_matrice
  for(i in 1:nrow(tmp)){
    tmp[i,i]=Inf
  }
  min_distance<-min(tmp)
  
  # chercher deux points qui ont la distance minimale
  indice<-which(distance_matrice==min_distance,arr.ind = TRUE)
  
  # calculer les coordonnees du barycentre de ces 2 points
  newX<-(M[indice[1,1],1]+M[indice[1,2],1])/2
  newY<-(M[indice[1,1],2]+M[indice[1,2],2])/2
  
  # mettre a jour la matrice des points
  M[indice[1,1],1]<-newX
  M[indice[1,1],2]<-newY
  M<-M[-indice[1,2],]
  
  # mettre a jour la matrice identite
  id_Matrice[index[1,1],] <-id_Matrice[indice[1,1],] + id_Matrice[index[1,2],]
  id_Matrice <- id_Matrice[-indice[1,2],]
  
  #recalculer la matrice de distance avec la nouvelle classe
  
}

# Vérifier
AscHierarchique <- hclust(D, method="complete") # complete ou ward

# Visualisation par Dendrogramme

# Vérifier
plot(AscHierarchique, cex=0.6, hang=-1)

# Récupérer les résultats

# Vérifier
cluster = cutree(AscHierarchique,3)

##########################################################

# 3.Colorer en fonction des index des classes:
col<-matrix(data=M,300,1)
for(i in 1:ncol(id_Matrice)){
  if(id_Matrice[1,i]==1){
    col[i]="red"
  }
  else if(id_Matrice[2,i]==1){
    col[i]="green"
  }
  else if(id_Matrice[3,i]==1){
    col[i]="blue"
  }
  else{
    col[i]="yellow"
  }
}
#
# Representer le nuage de points
plot(M2,col=col)
# Tracer le centre de chaque classe
points(M,cex=1.5,col="pink",pch=10)
