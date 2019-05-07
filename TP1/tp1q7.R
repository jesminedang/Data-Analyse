# Test Non Param�trique 

# question 7
# Test d'ind�pendance pour une variable qualitative

# Entrer des valeurs des respectives ph�notypes de ratio g�n�tique
R<-c(9, 3, 3, 1)
# Entrer des valeurs des respectives ph�notypes de observ�
O<-c(1528, 106, 117, ?81)

phenotype<-data.frame(R,O)
# calculer la valeur th�orique de chaque cat�gorie de ph�notype
sum1=sum(R)
sum2=sum(O)
sum3=phenotype$R+phenotype$O
sum=sum(R,O)
E1=sum1*sum3/sum
E2=sum2*sum3/sum
E<-data.frame(E1,E2)
# Cr�er une fonction du Khi deux (X2)
K?sum((O-E2)^2/E2)