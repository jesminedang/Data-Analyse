# Test Paramétrique 
data2<-read.table(file="D:/Documents/DataAnalysis-master/data2TP1.txt", header=TRUE)
# question 5
# Test d'indépendance pour une variable quantitative.
# calculer le score de t
x=data2$Marseille
n=length(x)
t=abs(19-mean(x))/(sd(x)/sqr?(n)) # t=2.177369
# ??=0.05, Level of Signi???cance = 0.025 (1 tailed) et Con???dence Level = 95%
# df=n-1=14, t(??)=2.145 < t
# Vérifier t
t.test(x, mu=19) 
# One Sample t-test

# data:  x
# t = 2.1774, df = 14, p-value = 0.04705
# alternative hypothesis: true mean is not equal to 19
# 95 percent confidence interval:
#   19.04090 24.42577
# sample estimates:
#  mean o? x 
# 21.73333 

##########################################################

# question 6
# Test d'indépendance pour deux variables quantitatives.
# calculer le score de t
x1=data2$Marseille
x2=data2$Aix
n1=length(x1)
n2=length(x2)
tt=abs(mean(x1)-mean(x2)?/sqrt(sd(x1)^2/n1+sd(x2)^2/n2) # tt=2.321494
# ??=0.05, Level of Signi???cance = 0.05 (2 tailed) et Con???dence Level = 95%
# df=n1+n2-2=28, t(??)=2.048 < tt
# Vérifier t selon la fonction
t.test(x1, x2) 
# Two Sample t-test

# data:  x1 and x2
# t = -2.3215, df = 27.156, p-value = 0.028
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -7.157?913 -0.4423087
# sample estimates:
#  mean of x mean of y 
#  21.73333  25.53333

# p=0.02, il montre la différence entre x1 (Marseille) et x2(Aix) 
# c'est statistiquement significatif car p<0.05
#########################################################
