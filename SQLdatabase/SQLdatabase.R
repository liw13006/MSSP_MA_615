pacman::p_load(tidyverse,RSQLite,DBI)
# setwd(paste0(getwd(),"/SQLdatabase"))
# getwd()
# 
# 
# con <- dbConnect(SQLite(),"../SQLdatabase/chinook.db")
# USAcustomers <- dbGetQuery(con,
#            "select firstname, lastname
#            from customers
#            where country = 'USA';")
# 
# 
# dbListTables(con)

donars_des <- readxl::read_excel("./SQLdatabase/Top MA Donors 2016-2020.xlsx",sheet = 1)
contrib_all <- readxl::read_excel("./SQLdatabase/Top MA Donors 2016-2020.xlsx",sheet = 2)
JFC <- readxl::read_excel("./SQLdatabase/Top MA Donors 2016-2020.xlsx",sheet = 3)

contributors <- select(contrib_all,
                       contribid,fam,contrib,City,State,Zip,Fecoccemp,orgname,lastname)%>%distinct()

orgs <- select(contrib_all,orgname,ultorg)%>%distinct()

contribution <- select(contrib_all,
                       date,amount,type,fectransid,Fecoccemp,contribid,fam,recipient,cycle,cmteid)%>%distinct()
recipients <- select(contrib_all,
                     recipient,recipid,party,recipcode,cmteid)%>%distinct()

mydb = dbConnect(SQLite(),"./SQLdatabase/Political-Contribution.db")
dbWriteTable(conn = mydb,value = contributors,name = "contributors",overwrite = TRUE)
dbWriteTable(conn = mydb,value = orgs,name = "orgs",overwrite = TRUE)
dbWriteTable(conn = mydb,value = contribution,name = "contribution",overwrite = TRUE)
dbWriteTable(conn = mydb,value = recipients,name = "recipients",overwrite = TRUE)

test1 <- full_join(contribution,recipients,by = c("recipient","cmteid"))
test2 <- full_join(contributors,test1,by = c("contribid","contrib","Zip"))
left_join(test2,orgs,by = "orgname")%>%distinct()
