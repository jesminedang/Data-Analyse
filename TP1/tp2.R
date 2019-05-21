# TP2: L'analyse en composantes principales
data<-read.table(file="D:/Documents/DataAnalysis-master/data1TP2.txt", header=TRUE)

# question 1
# Tracer en dimension 3 le nuage de 10 points
x=data$Stature
y=data$Poids
z=data$Taille
A<-cbind(x, y, z)
install.packages("plot3D")
library("plot3D")
scatter3D(x, y, z, colvar = NULL, col = "blue", pch = 19, cex = 0.5)
scatter3D(x, y, z, clab = c("", ""))

#################################################################

# question 2
# Ecrire le tableau centré B
B<-scale(A,scale=FALSE)
#B<-cbind(xB, yB, zB)
# la matrice de covariance V
#V<-cor(A)
V<-cov(A)

################################################################

# question 3
# la représentation spectrale
S<-eigen(V)
#eigen() decomposition
Eval=S$values
Evec=S$vectors
#$`values`
#[1] 97.084062 22.183157  6.343892

#$vectors
#[,1]       [,2]       [,3]
#[1,] -0.7046461 -0.4385058  0.5578410
#[2,] -0.6999582  0.3006869 -0.6478008
#[3,] -0.1163290  0.8469357  0.5188135

###############################################################

# question 4
# les axes principaux dans l'ordre

###############################################################

# question 5
# Générer le tableau C
C1<-B %*% Evec
# vérifier C
C2<-princomp(A)$scores
# C1=C2
#[,1]        [,2]       [,3]
#[1,]  -2.441048 -2.29074657  0.4937234
#[2,]  -4.327017 -6.03932803  2.3352611
#[3,]   7.979499 -0.04057249  1.0844150
#[4,]   2.007407  1.20976619  3.4866580
#[5,]  12.909318  7.07070278  2.1458520
#[6,] -18.240682 -1.26594783  1.0606056
#[7,]   1.893781 -1.98503279 -0.9608524
#[8,] -13.292112  8.80209807 -2.7005248
#[9,]   9.042507 -0.66448847 -3.4411503
#[10,]   4.468348 -4.79645085 -3.5039877

###############################################################

# question 6
# Tracer le premier axe principal
scatter3D(x=c(0,-300*Evec[1,1]), y=c(0,-300*Evec[2,1]), z=c(0,-300*Evec[3,1]), add=TRUE, type="l")
scatter3D(x=c(0, 100*Evec[1,2]), y=c(0, 100*Evec[2,2]), z=c(0, 100*Evec[3,2]), add=TRUE, type="l")
scatter3D(x=c(0, 100*Evec[1,3]), y=c(0, 100*Evec[2,3]), z=c(0, 100*Evec[3,3]), add=TRUE, type="l")
plotrgl()

# question 7
# Représenter le nuage de points en dimension 2, projetés des points de départ sur le plan formé
# des deux premiers axes principaux
plot(C1[,1], C2[,2])
