## Assignment
## 1. Acquire a simple dataset
## load the dataset into sparklyr
## clean/wrangle/visualize the data using dplyr and dbplot
## define a pipline consisting of transformers and estimators
## fir a model and evaluate performance
## share with partner

pacman::p_load(sparklyr,DBI,dplyr,magrittr)
sparkversion <- spark_installed_versions()$spark



sc <- spark_connect(master = "local", version = sparkversion)

CO2 <- CO2
rm(CO2)
#write.csv2(x = CO2,file = "co2.csv",sep = ",")
#?CO2
#co2 <- copy_to(sc,CO2)

co2 <- spark_read_csv(sc,"co2.csv")
loan <- spark_read_csv(sc,"loan.csv")

DBI::dbGetQuery(sc, "SELECT Type FROM co2 ")

dplyr::select(co2,Type)%>%dplyr::distinct()%>%dplyr::show_query()


dplyr::count(co2) %>%
  dplyr::show_query()

DBI::dbGetQuery(sc,"SELECT DISTINCT `Type` FROM co2")

select(co2, conc, uptake)%>%show_query() %>%
  collect() %>%
  plot()

co2model <- ml_linear_regression(co2,conc~uptake)                
coef(co2model)
colnames(loan)
DBI::dbGetQuery(sc,"SELECT * FROM `loan` LIMIT 5")
spark_disconnect(sc)
