library(sparklyr)
sparkversion <- spark_installed_versions()$spark

sc <- spark_connect(master = "local",version = sparkversion)
install.packages("rJava")
library(rJava)
