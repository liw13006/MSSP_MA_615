pacman::p_load(tidyverse,rattle,psych,GPArotation,magrittr,reshape2)
ClimateRaw1 = readxl::read_xls("Climate_Change.xls",sheet = 1,na = "..",col_types = "guess")
ClimateRaw2 = readxl::read_xls("Climate_Change.xls",sheet = 2,na = "..",col_types = "guess")
ClimateRaw3 = readxl::read_xls("Climate_Change.xls",sheet = 3,na = "..",col_types = "guess")

##pull out Region codes only
Regioncode = dplyr::filter(ClimateRaw2,Region == "Aggregates")[-grep("income",ClimateRaw2$`Country name`),] 
Regioncode = Regioncode %>% select(`Country code`) %>% pull()
##pull out income codes only
Incomecode = dplyr::filter(ClimateRaw2,Region == "Aggregates")[grep("income",ClimateRaw2$`Country name`),] 
Incomecode = Incomecode %>% select(`Country code`)%>%pull()

Relatedvariables = dplyr::select(ClimateRaw3,`Series code`) %>% slice(1:4,14:15,41:43)%>%pull()


RegionClimate = dplyr::filter(ClimateRaw1,`Country code` %in% Regioncode,`Series code` %in% Relatedvariables)
unique(select(RegionClimate,`Series code`))
IncomeClimate = dplyr::filter(ClimateRaw1,`Country code` %in% Incomecode,`Series code` %in% Relatedvariables)

#ggplot(t(select(slice(IncomeClimate,1:6),`Country code`,`1990`:`2008`))) +aes(x = HIC)+ geom_bar()
#

## Self-defined transpose function for tibble
transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
  return(t_df)
}

## cleanning the Income table
IncomeCo2Tot = select(slice(IncomeClimate,1:6),`Country code`,`1990`:`2008`)%>%column_to_rownames(var = "Country code")%>%transpose_df()%>%rename(year = "rowname")%>%mutate(year = as.numeric(year))

IncomeCo2perGDP = select(slice(IncomeClimate,13:18),`Country code`,`1990`:`2008`)%>%column_to_rownames(var = "Country code")%>%transpose_df()%>%rename(year = "rowname")%>%mutate(year = as.numeric(year))

IncomeCo2perCap = select(slice(IncomeClimate,7:12),`Country code`,`1990`:`2008`)%>%column_to_rownames(var = "Country code")%>%transpose_df()%>%rename(year = "rowname")%>%mutate(year = as.numeric(year))

IncomePop = select(slice(IncomeClimate,37:42),`Country code`,`1990`:`2008`)%>%column_to_rownames(var = "Country code")%>%transpose_df()%>%rename(year = "rowname")%>%mutate(year = as.numeric(year))

IncomeGDP = select(slice(IncomeClimate,19:24),`Country code`,`1990`:`2008`)%>%column_to_rownames(var = "Country code")%>%transpose_df()%>%rename(year = "rowname")%>%mutate(year = as.numeric(year))



##plot them and see what happens
ggplot(IncomeCo2Tot)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+
  geom_line(mapping = aes(x = year,y = LMC,color = "Lower Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = UMC,color = "Upper Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = HIC,color = "High Income Countries"))+ylab("Total Co2 emission per class")

ggplot(IncomeCo2perCap)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+
  geom_line(mapping = aes(x = year,y = LMC,color = "Lower Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = UMC,color = "Upper Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = HIC,color = "High Income Countries"))+ylab("Co2 emission per Capita by class")

ggplot(IncomeCo2perGDP)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+
  geom_line(mapping = aes(x = year,y = LMC,color = "Lower Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = UMC,color = "Upper Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = HIC,color = "High Income Countries"))+ylab("Co2 emission per GDP")

ggplot(IncomeGDP)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+
  geom_line(mapping = aes(x = year,y = LMC,color = "Lower Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = UMC,color = "Upper Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = HIC,color = "High Income Countries"))+ylab("GDP by class")

ggplot(IncomePop)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+
  geom_line(mapping = aes(x = year,y = LMC,color = "Lower Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = UMC,color = "Upper Middle Income Countries"))+
  geom_line(mapping = aes(x = year,y = HIC,color = "High Income Countries"))+ylab("Population by class")

## take notice on the Co2 emmision per gdp and the GDP by class. there's a sudden drop on co2 per gdp but no much has changed in gdp it self. for a closer look. separate plot will be drawn below

ggplot(IncomeGDP)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+ylab("GDP by class")
ggplot(IncomeCo2perGDP)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+ylab("Co2 emission per GDP")
ggplot(IncomeCo2Tot)+geom_line(mapping = aes(x = year,y = LIC,color = "Low Income Countries"))+ylab("Total Co2 emission per class")

## looks like there is some issue on total low income countries' co2 emission
##
## Extract low income countries
LICcode = filter(ClimateRaw2,`Income group` == "Low income")%>%select(`Country code`)%>%pull()

LICClimate = dplyr::filter(ClimateRaw1,`Country code` %in% LICcode,`Series code` %in% Relatedvariables)

LICTotCo2 = filter(LICClimate,`Series code`=="EN.ATM.CO2E.KT")%>%select(`Country code`,`1990`:`2008`)%>%melt(id.vars = "Country code",variable.name = "year",value.name = "tot co2 emmision")

##Turns out that PRK:People's Republic of Korea, contributed most to 
ggplot(filter(LICTotCo2,`Country code`=="PRK"),aes(year,`tot co2 emmision`))+geom_point(aes(color=`Country code`))
