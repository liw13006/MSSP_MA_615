


##################################### HW 1, prob 1

## solve Ax=b
## note Matrix
## 1 2 3 4 5
## 2 1 2 3 4
## 3 2 1 2 3
## 4 3 2 1 2
## 5 4 3 2 1

b <- c(7, -1, -3, 5, 15)

dim(b) <- c(5, 1)

A <-  matrix(data=0, nrow = 5, ncol=5)

## or

A1 <- rep(0,25)
dim(A1) <- c(5,5)

col(A)
row(A)

## so,

abs(col(A) - row(A))

A <- abs(col(A) - row(A)) + 1


## x = inv(A)b


A_inv <- solve(A)

AA <- A%*%A_inv

options(scipen=999, digits = 2)
round(AA,digits = 1)

A_inv%*%b

############################################## Prob 2

set.seed(75)

aMat <- matrix(sample(10, size=60, replace=TRUE), nrow=6)

aMat
############################################# a

apply(aMat, 1, function(x){sum(x>4)})



############################################# b

apply(aMat,1, function(x){sum(x==7)==2})

which(apply(aMat,1, function(x){sum(x==7)==2}))

############  -- in a loop  ####################  debug

seven_ct <- function(aMat){
  out = 1:dim(aMat)[1]
for(i in 1:dim(aMat)[1]){
  cnt7 = sum(aMat[i,]==7)
  if(cnt7!=2){out=out[-which(out==i)]}
}
  return(out)
}

blah <- seven_ct(aMat)

############################################# c

aMatColSums <- colSums(aMat)

aMatColSums

outer(aMatColSums, aMatColSums, "+")

vals <- outer(aMatColSums, aMatColSums, "+")

pick <- (vals>75)
pick

rep(1:10, rep(10,10))
rep(1:10, 10)

r1 <- rep(1:10, rep(10,10))
r2 <- rep(1:10, 10)

length(r1)
length(r2)


cbind(r1, r2)

rc <- cbind(r1, r2)

pick <- (vals>75)

col_select <- rc[pick]

matrix(col_select, ncol=2, byrow = FALSE)



##############################################  in one line
cbind(rep(1:10, rep(10,10)), rep(1:10,10))[outer(aMatColSums, aMatColSums, "+")>75,]

#############################################prob 3

###################################### a

sum((1:20)^4) * sum(1/(4:8))


## or

sum(outer((1:20)^4, 4:8, "/"))


############################################# b


sum((1:20)^4/(3 + outer(1:20, 1:5, "*")))

############################################# c

sum( outer(1:10, 1:10, function(i,j){ (i >= j)*i^4/(3+i*j)}))



######################################################## prob 3

##################################### a

testLoop <- function(n){
  if(!is.vector(n)){stop("yVec is not a vector")}
  if(length(n)>1){stop("testLoop only takes single values")}
  xVec <- rep(NA, n-1)
  xVec[1]=1
  xVec[2]=2
  for(j in 3:(n-1)) xVec[j] <- xVec[j-1] + 2/xVec[j-1]
  xVec
}


testLoop(5)
############################################## b

testLoop2 <- function(yVec){
  n = length(yVec)
  sum(exp(seq(along=yVec)))
}

