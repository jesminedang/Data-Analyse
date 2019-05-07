# Test Non Paramétrique 

# question 7
# Test d'indépendance pour une variable qualitative

# Entrer des valeurs des respectives phénotypes de ratio génétique
R<-c(9, 3, 3, 1)
# Entrer des valeurs des respectives phénotypes de observé
O<-c(1528, 106, 117, ?81)

phenotype<-data.frame(R,O)
# calculer la valeur théorique de chaque catégorie de phénotype
sum1=sum(R)
sum2=sum(O)
sum3=phenotype$R+phenotype$O
sum=sum(R,O)
E1=sum1*sum3/sum
E2=sum2*sum3/sum
E<-data.frame(E1,E2)
# Créer une fonction du Khi deux (X2)
K?sum((O-E2)^2/E2)
