data1<-read.table(file="D:/Documents/DataAnalysis-master/data1TP1.txt", header=TRUE)

# question 1
# Tracer le nuage de 15 points pour chaque variable
op <- par(mfrow=c(2,3)) 
plot(x=data1$A, data1$Y) # Y réduit relativement linéaire
plot(x=data1$B, data1$Y) # Y augmente relativement linéaire
plot(x=data1$C, data1$Y) # Y est au hasard
plot(x=data1$D, data1$Y) # Y est diverge
plot(x=data1$E, data1$Y) # Y a une forme hyperbolique

#######################################################

# question 2

# créer fonction basée sur la formule
x=data1$A
y=data1$Y
rA<-cov(x,y)/(sd(x)*sd(y)) # rA=-0.9722452

# vérifier avec la fonction
rA1=cor(x, y, method="pearson") # rA1=-0.9722452

# créer fonction basée sur la formule
x=data1$B
y=data1$Y
rB<-cov(x,y)/(sd(x)*sd(y)) # rB= 0.9815886

# vérifier avec la fonction
rB1=cor(x, y, method="pearson") # rB1= 0.9815886

# créer fonction basée sur la formule
x=data1$C
y=data1$Y
rC<-cov(x,y)/(sd(x)*sd(y)) # rC=0.4119462

# vérifier avec la fonction
rC1=cor(x, y, method="pearson") # rC1= 0.4119462

# créer fonction basée sur la formule
x=data1$D
y=data1$Y
rD<-cov(x,y)/(sd(x)*sd(y)) # rD=0.7513686

# vérifier avec la fonction
rD1=cor(x, y, method="pearson") # rD1= 0.7513686

# créer fonction basée sur la formule
x=data1$E
y=data1$Y
rE<-cov(x,y)/(sd(x)*sd(y)) # rD=0.210302

# vérifier avec la fonction
rE1=cor(x, y, method="pearson") # rD1=0.210302

# Deux résultats sont égals.

####################################################

# question 3

# créer fonction basée sur la formule
x=data1$A
y=data1$Y
n=length(x)
xR=rank(x)
yR=rank(y)
k=sum((xR-yR)^2, na.rm = FALSE)
pA<-(1-6*k/(n^3-n)) # pA=-0.9973214

# vérifier avec la fonction
pA1=cor(x, y, method="spearman") # pA1=-0.9991067

# créer fonction basée sur la formule
x=data1$B
y=data1$Y
n=length(x)
xR=rank(x)
yR=rank(y)
k=sum((xR-yR)^2, na.rm = FALSE)
pB<-(1-6*k/(n^3-n)) # pB=0.9982143

# vérifier avec la fonction
pB1=cor(x, y, method="spearman") # pB1=0.9982127

# créer fonction basée sur la formule
x=data1$C
y=data1$Y
n=length(x)
xR=rank(x)
yR=rank(y)
k=sum((xR-yR)^2, na.rm = FALSE)
pC<-(1-6*k/(n^3-n)) # pC=0.4169643

# vérifier avec la fonction
pC1=cor(x, y, method="spearman") # pC1=0.4164434

# créer fonction basée sur la formule
x=data1$D
y=data1$Y
n=length(x)
xR=rank(x)
yR=rank(y)
k=sum((xR-yR)^2, na.rm = FALSE)
pD<-(1-6*k/(n^3-n)) # pD=1

# vérifier avec la fonction
pD1=cor(x, y, method="spearman") # pD1=1

# créer fonction basée sur la formule
x=data1$E
y=data1$Y
n=length(x)
xR=rank(x)
yR=rank(y)
k=sum((xR-yR)^2, na.rm = FALSE)
pE<-(1-6*k/(n^3-n)) # pE=0.3419643

# vérifier avec la fonction
pE1=cor(x, y, method="spearman") # pE1=0.3413764

# comparer deux fonctions, les résultats sont approches.

# Comparer deux résultats, on peut trouver que le coefficient de Spearman a la distance plus que le coefficient de Pearson

# question 4
# La relation entre E et Y n'est pas de la forme Y=aE+b, mais de type différent (comme un parabole).
# Le nuage de point présente une forme complexe avec des courbures.
# Donc la relation entre E et Y est une relation est non-linéaire.
# Les deux coefficients de corrélation (Pearson, Spearman) sont approches à nuls (calculé au dessus), 
# mais pourtant il existe bel et bien une relation (non-linéaire et non-monotone) entre les deux variables E et Y.
# Donc il faut calculer les deux coefficients de corrélation qui sont nuls.