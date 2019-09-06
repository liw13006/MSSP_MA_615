###  apply

## repetitive application of a function to slices of data from
## matrices, arrays, lists, dataframes

## apply:   used for arrays
## lapply:  used for lists, vectors, data frames, 
## sapply:  used for lists -- but simplfies the output
## tapply:  used for ragged arrays
## 


## start with a 3x5 array
##
set.seed(2019)
norm_a <- matrix(round(rnorm(15),1), nrow=3, byrow = TRUE)

## now find the means on the margins
## there are plenty of ways to do this

rowSums(norm_a)
dim(norm_a)
rowSums(norm_a)/dim(norm_a)[2]


apply(norm_a,1,FUN = mean)
apply(norm_a,2,FUN = mean)

## or mayby you have your own function -- 
## the values are weighted by 
## their positions in the row or col being averaged

pos_wt <- function(A){
  wt = 1:length(A)
  wt_vec = A*wt
  wt_sum = sum(wt_vec)
  wt_avg = wt_sum/length(A)
  return(wt_avg)
}

test <- c(3,6,1,2)
pos_wt(test)

apply(norm_a,2,pos_wt)



###   tapply

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
           "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa",
           "sa", "act", "nsw", "vic", "vic", "act")





incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)


length(state)
length(incomes)

statef <- factor(state)
incomesf = factor(incomes)
tapply(incomes, statef, length)
tapply(state,incomesf,length)

count(incomes)


incmeans <- tapply(incomes, statef, mean)
incmeans
