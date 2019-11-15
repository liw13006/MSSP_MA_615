library(odbc,DBI)


con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 17 for SQL Server",
                      Server   = "bu-rstudio-connect.bu.edu",
                      UID      = "liw13006",
                      PWD      = "Liw13006327725",
                      Port     = 1433)

#login.list = list(Driver   = "ODBC Driver 17 for SQL Server",
               #Server   = "bu-rstudio-connect.bu.edu",
               #UID      = "liw13006",
               #PWD      = "Liw13006327725",
               #Port     = 1433)

#con <- DBI::dbConnect(odbc::odbc(),"./login.ini")

courses <- read.csv2("courses.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
persons <- read.csv2("persons.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
enrollment <- read.csv2("enrollment.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
sessions <- read.csv2("sessions.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#dbRemoveTable(con,"school2_liw13006_courses")
dbWriteTable(con,"school2_liw13006_courses",courses)
dbWriteTable(con,"school2_liw13006_persons",persons)
dbWriteTable(con,"school2_liw13006_enrollment",enrollment)
dbWriteTable(con,"school2_liw13006_sessions",sessions)



dbGetQuery(con,"select * from school2_liw13006_persons")

data <- dbSendQuery(con,"select * from school2_lauraww where CDUR = 5")

d <- dbFetch(data)



dbListTables(con)

dbDisconnect(con)


