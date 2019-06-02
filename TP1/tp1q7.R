# Test Non Paramétrique 

# question 7
# Test d'indépendance pour une variable qualitative

# Entrer des valeurs des respectives phénotypes de ratio génétique
R<-c(9, 3, 3, 1)
# Entrer des valeurs des respectives phénotypes de observé
O<-c(1528, 106, 117, 381)

#phenotype<-data.frame(R,O)
# calculer la valeur théorique de chaque catégorie de phénotype
# l'hypothèse Ho: 9:3:3:1
# calculer les valeurs théorique
sum1=sum(R)
sum2=sum(O)
E<-R*(sum2/sum1)
#E
#[1] 1199.25  399.75  399.75  133.25

# Créer une fonction du Khi deux (X2)
K=sum((O-E)^2/E)
# [1] 966.61

# Avec α=0.05, degré de liberté = k - 1 = 4-1 = 3, DF=3, K = 7.81
# Sa valeur observée est K= 966.61 < 7.81
# Région de rejet de H0 associée à α = 5%
# Sous H0, on s'attend à observer une valeur de Q2 proche de 0. 
# Plus la valeur de K est grande et plus elle est en faveur de H1.
# La région de rejet est située à l'extrémité droite du domaine. 
# Elle contient les 5% de valeurs les plus grandes de K.

##############################################################

# question 8
# Test d'indépendance pour les variables qualitatives
# Test de forme
# Test de couleur: light brown

##############################################################

# question 9
# Le test de Student/t est classé comme paramétrique parce que 
# ce test paramétrique repose sur des comparaisons de moyennes.
# pour comparer deux échantillons indépendants et/ou appariés
# et concerne des données quantitatives, mesurées sur une échelle d’intervalle ou de rapport
# Certains tests statistiques ne sont valables que sous certaines
# conditions concernant la distribution de la ou les variable(s).
# Certains tests ont pour but de montrer une égalité sur certains
# paramètres : ce sont les tests paramétriques.
# Le test du Khi Deux est classé comme non paramétrique parce que
# Certains tests statistiques ne sont valables que sous certaines
# conditions concernant la distribution de la ou les variable(s).
# C’est le cas du test du Khi-deux, du test des signes, ou du test
# du coefficient de Spearman.


#############################################################
# question 10
# Nous appliquer le coefficient de Pearson et le coefficient de Spearman aux données qualitatives
