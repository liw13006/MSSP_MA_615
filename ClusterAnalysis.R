pacman::p_load(tidyverse,rattle,psych,GPArotation,magrittr)
wine <- force(wine)
factorwine = select(wine,Alcohol:Proline)
summary(wine)
corwine = cor(select(wine,Alcohol:Proline))
parallel = fa.parallel(select(wine,Alcohol:Proline),fm = "minres",fa = "fa")
zscore = function(DF){
  
}
factorwine = mutate_all(factorwine,funs())
threefactor = fa(factorwine,nfactors = 3,rotate = "oblimin",fm = "minres")    
threefactor$weights
wine3 = kmeans(factorwine,3)
print(wine3)
print(mean(wine[,1] == wine3$cluster))

wine3$cluster
wine[,1]
