pacman::p_load(psych,GPArotation)
link = "https://www.promptcloud.com/wp-content/uploads/2017/02/EFA.csv"
EFAdata = read_csv(link)
##correlation matrix
cormat = cor(EFAdata)

identitymat = diag(x = 1, nrow = 14)
##Barlett's test
cortest.bartlett(cormat,n = 90)
#cortest.bartlett(identitymat,n = 90)
eigen(cormat)
set.seed(2019)
parallel = fa.parallel(EFAdata,fm = "minres",fa = "fa")

threefactor = fa(EFAdata,nfactors = 3,rotate = "oblimin",fm = "minres")

print(threefactor)

fourfactor = fa(EFAdata,nfactors = 4,rotate = "oblimin",fm = "minres")

print(fourfactor)
