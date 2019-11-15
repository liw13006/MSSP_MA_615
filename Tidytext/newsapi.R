
library(tidyverse)
library(httr)

url <- "https://newsapi.org/v2/everything?q=microsoft&apiKey=104a7c8531f14da4897bb3f6ac13e061"



j1 <- GET(url)
j1


j3 <- GET(url = "https://newsapi.org/v2/everything?q=microsoft", 
          apikey = "e81cda2325c34f2b89d520b4553c7330")



j4 <- GET(url = "https://newsapi.org/v2/everything?q=microsoft&apikey=e81cda2325c34f2b89d520b4553c7330")


j5 <- content(j4)


j5[[1]]

j5$articles[[1]]

j5$articles[[1]]$description

j5$articles[[1]]$content

j5$articles[[1]]$url

j5$articles[[1]]$content[1]

cat(j5$articles[[1]]$content[1])

print(j5$articles[[1]]$content[1])

#################################################
j4


validate(j4)

j4$all_headers



j6 <- content(j4, as = "text")


validate(j6)

library(jsonlite)

j7 <- fromJSON(j6)

j7[3]$articles$description[1]



j8 <- fromJSON(j6, flatten = TRUE)

j8$articles$description[1]

j9 <- fromJSON(j6, simplifyDataFrame = TRUE, flatten=TRUE)

is.data.frame(j9)


j9$articles


is.data.frame(j9$articles)

colnames(j9$articles)

a <- rchisq(1000000,3)
a.sd <- sd(a)
b <- a
for (i in 1:length(a)){
  b[i] <- rbinom(1,round(a[i]),prob = runif(1,min = 0,max = 1))
}
c <- a
for (i in 1:length(a)){
  c[i] <- rnorm(1,mean = b[i],sd = a.sd)
}

ggplot()+aes(x = c,fill = "random distribution",color = "random distribution")+geom_histogram(aes(y = ..density..),bins = 30,alpha = .4)+geom_density(fill = NA)+theme(legend.position="bottom")
