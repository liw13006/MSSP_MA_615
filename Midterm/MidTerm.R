pacman::p_load(tidyverse,rattle,psych,GPArotation,magrittr,reshape2,plotly,gridExtra)
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

## Filtered Climate Data
Climate <-  dplyr::filter(ClimateRaw1,`Series code` %in% Relatedvariables)#,!`Country code` %in% Regioncode,!`Country code` %in% Incomecode)

## Assigning categorical data to the original dataframe
Climate <- left_join(Climate,select(ClimateRaw2,`Country code`,`Income group`,Region)) %>% melt(measure.vars = 7:28)%>%dplyr::rename(year = variable)

#Data READY TO PLAY!!!
#
# 1st the series code explanation table:

SeriesCode = ClimateRaw3%>%select(`Series code`,`Series name`,Definition)%>%dplyr::filter(`Series code` %in% Relatedvariables)

IncomeTable = ClimateRaw2%>%select(`Country code`,`Country name`)%>%filter(`Country code` %in% Incomecode)#%>%dplyr::arrange(`Country code` = c("HIC","UMC","MIC","LMC","LIC","LMY"))
IncomeTable = IncomeTable[c(1,6,5,3,2,4),]# re-order it

RegionTable = ClimateRaw2%>%select(`Country code`,`Country name`)%>%filter(`Country code` %in% Regioncode)

# Check NA values in world data, Region data and also Income group data

Climate%>%dplyr::filter(`Country code`=="WLD",is.na(value))%>%dplyr::group_by(year)%>%dplyr::select(`Series code`,year,value)%>%summary()
Climate%>%dplyr::filter(`Country code`%in% Incomecode,is.na(value))%>%dplyr::group_by(year)%>%dplyr::select(`Series code`,year,value)%>%summary()
Climate%>%dplyr::filter(`Country code`%in% Regioncode,is.na(value))%>%dplyr::group_by(year)%>%dplyr::select(`Series code`,year,value)%>%summary()

# Because we are using World data as a Base line, we need to filter out years which has absolutely no world data, in this case is: 2011
Climate <- Climate%>%dplyr::filter(year != 2011)
# Check again
Climate%>%dplyr::filter(`Country code`=="WLD",is.na(value))%>%dplyr::group_by(year)%>%dplyr::select(`Series code`,year,value)%>%summary()
Climate%>%dplyr::filter(`Country code`%in% Incomecode,is.na(value))%>%dplyr::group_by(year)%>%dplyr::select(`Series code`,year,value)%>%summary()
Climate%>%dplyr::filter(`Country code`%in% Regioncode,is.na(value))%>%dplyr::group_by(year)%>%dplyr::select(`Series code`,year,value)%>%summary()


# Plot our data 
print(Relatedvariables)
p <- ggplot(Climate%>%dplyr::filter(`Series code` == Relatedvariables[7],`Country code` %in% Regioncode))+aes(x = as.numeric(as.character(year)),y = value,color = `Country name`)+geom_line()
p
plotly_build(p)%>%layout(legend = list(orientation = 'h',x = 0, y = 1.2))
# found north america missing. need to recode 

Regioncode_new = dplyr::filter(ClimateRaw2,Region != "Aggregates")%>%select(Region)%>%pull()%>%unique()

# Check if NAS has been occupied

Climate%>%dplyr::filter(`Country code`=="NAS")
#Checked

View(Climate%>%dplyr::filter(Region == "North America"))


# Total CO2 emission
D1 <- Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[9])%>%dplyr::select(1:8)%>%dplyr::slice(1)
D1 <- D1%>%as.data.frame()
D1[1,c(1:2,7:8)] = c("NAS","North America","Aggregates","Aggregates")
D2 = Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[9])%>%summarise_at(.vars = 9:29,.funs = sum)
D = dplyr::bind_cols(D1,D2)

# Total Population
D1 <- Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[1])%>%dplyr::select(1:8)%>%dplyr::slice(1)
D1 <- D1%>%as.data.frame()
D1[1,c(1:2,7:8)] = c("NAS","North America","Aggregates","Aggregates")
D2 = Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[1])%>%summarise_at(.vars = 9:29,.funs = sum)
D <- dplyr::bind_rows(D, dplyr::bind_cols(D1,D2))

# GDP
D1 <- Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[3])%>%dplyr::select(1:8)%>%dplyr::slice(1)
D1 <- D1%>%as.data.frame()
D1[1,c(1:2,7:8)] = c("NAS","North America","Aggregates","Aggregates")
D2 = Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[3])%>%summarise_at(.vars = 9:29,.funs = sum)
D <- dplyr::bind_rows(D, dplyr::bind_cols(D1,D2))

# Urban Population
D1 <- Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[5])%>%dplyr::select(1:8)%>%dplyr::slice(1)
D1 <- D1%>%as.data.frame()
D1[1,c(1:2,7:8)] = c("NAS","North America","Aggregates","Aggregates")
D2 = Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Relatedvariables[5])%>%summarise_at(.vars = 9:29,.funs = sum)
D <- dplyr::bind_rows(D, dplyr::bind_cols(D1,D2))

# Calculate CO2 per capita, CO2 per unit PPP
# Bermuda's PPP factor is missing from the world bank data, the cloest one I could find is a data given by federal reserve bank: https://fred.stlouisfed.org/series/PLGDPEBMA670NRUG

pppBer <- 1.98021
pppCan <- 1.21
D_Temp <- D%>%dplyr::select(`Series name`,`1990`:`2010`)%>%
  column_to_rownames(var = "Series name")%>%
  transpose_df()
Temp_Colnames <- colnames(D_Temp)
colnames(D_Temp) <- c("year","CO2","Pop","GDP","Urban")
# Calculate ppp in 2005 for North america
ppp2005 <- Climate%>%dplyr::filter(Region == "North America")%>%
  pivot_wider(names_from = year,values_from = value)%>%
  dplyr::filter(`Series code` == Relatedvariables[3])%>%
  dplyr::select(`Country code`,`2005`)%>%
  column_to_rownames(var = "Country code")%>%
  transpose_df()%>%dplyr::transmute(`North America` = BMU*pppBer + CAN * pppCan + USA)%>%
  pull()

D_Temp = D_Temp%>%dplyr::mutate(CO2percap = CO2*1000/Pop,CO2perGDP = CO2*1000000000/ppp2005)

# Adjust comparing variables exclude Pop growth and Urban growth. (For simplicity)

SeriesCode <- SeriesCode%>%slice(-2,-4,-6)
# update related Variables
Related_variables <- SeriesCode%>%dplyr::select(`Series code`)%>%pull()
Temp_Colnames <- SeriesCode%>%dplyr::select(`Series name`)%>%pull()
#set adequate colname order and rename the D_Temp cols
colnames(D_Temp) <- c("year",Temp_Colnames[c(6,1:5)])
# Transpose back
D_Temp%>%column_to_rownames(var = "year")%>%transpose_df()
colnames(D_Temp)[colnames(D_Temp)=="rowname"] <- "Series name"

# Reconstruct the other part of climate data
D_ = data.frame()

for (i in 1:length(Related_variables)){
  D1 <- Climate%>%dplyr::filter(Region == "North America")%>%pivot_wider(names_from = year,values_from = value)%>%dplyr::filter(`Series code` == Related_variables[i])%>%dplyr::select(1:8)%>%dplyr::slice(1)
  D1 <- D1%>%as.data.frame()
  D1[1,c(1:2,7:8)] = c("NAS","North America","Aggregates","Aggregates")
  D_ <- bind_rows(D_,D1)
  print(i)
}
# Combine D_ and D_Temp

D <- left_join(D_,D_Temp,by = "Series name")%>%pivot_longer(cols = 9:29,names_to = "year",values_to = "value")

# add these to the original Climate table and we keep the related variables
Climate <- bind_rows(Climate,D)
Climate <- Climate%>%dplyr::filter(`Series code` %in% Related_variables)
# update Regioncode
Regioncode <- append(Regioncode,"NAS")
# Replot the previous plot 
p <- ggplot(Climate%>%dplyr::filter(`Series code` == Related_variables[1],`Country code` %in% Regioncode))+aes(x = as.numeric(as.character(year)),y = value,color = `Country name`)+geom_line()+xlab("year")+ggtitle(Related_variables[1])
plotly_build(p)%>%layout(legend = list(orientation = 'h',x = 0, y = -0))
# success!
# 
# Series code is not user friendly, we would use Series name instead
SeriesName = SeriesCode%>%dplyr::select(`Series name`)%>%pull()
# try plot
p <- ggplot(Climate%>%dplyr::filter(`Series name` == SeriesName[5],`Country code` %in% Incomecode))+aes(x = as.numeric(as.character(year)),y = value,color = `Country name`)+geom_line()+xlab("year")+ggtitle(SeriesName[5])
plotly_build(p)%>%layout(legend = list(orientation = 'h',x = 0, y = -0.2))

#we shall see how the data is distributed

p1 <- ggplot(Climate%>%dplyr::filter(`Series name` == SeriesName[4],!`Country code` %in% Incomecode & !`Country code` %in% Regioncode,year ==1993 ))+aes(x =log(value),color = `Region`,fill = `Region`)+geom_histogram(bins = 30,aes(y = ..density..),position = "dodge",alpha = .2,show.legend = FALSE)+ggtitle(paste0(SeriesName[4]," of 1992"))+geom_density(alpha = .3,show.legend = FALSE)+ylab("1992")#+xlim(c(0,400000000))

p2 <- ggplot(Climate%>%dplyr::filter(`Series name` == SeriesName[4],!`Country code` %in% Incomecode & !`Country code` %in% Regioncode,year ==2008 ))+aes(x =log(value),color = `Region`,fill = `Region`)+geom_histogram(bins = 30,aes(y = ..density..),position = "dodge",alpha = .2)+ggtitle(paste0(SeriesName[4]," 1993 vs 2008"))+geom_density(alpha = .3)+xlab(c("log ",SeriesName[4]))+ylab("2008")+ theme(legend.position = "bottom")
#plotly_build(grid.arrange(p1,p2,nrow = 2))
p <- subplot(plotly_build(p1),plotly_build(p2),nrows = 2,shareX = TRUE)
p%>%layout(legend = list(orientation = 'h',x = 0, y = -0.2))

plotly_build(p)%>%layout(legend = list(orientation = 'h',x = 0, y = -0.5))

# try pivot series code,log all input

Climate1 <- Climate%>%dplyr::select(1:3,7:10)%>%pivot_wider(names_from = `Series code`,values_from = value)%>%dplyr::mutate_at(.vars = 6:11,.funs = log)

p3 <- ggplot(Climate1%>%dplyr::filter(!`Country code` %in% Incomecode & !`Country code` %in% Regioncode,year ==2005 ))+aes(y = `EN.ATM.CO2E.KT`,x = `SP.POP.TOTL`)+geom_point(aes(size = `NY.GDP.MKTP.CD`,color = `Region`))+scale_size_continuous(range = c(0,4))

plotly_build(p3)%>%layout(legend = list(orientation = 'h',x = 0, y = -0.2))

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

## Turns out that PRK:People's Republic of Korea, contributed most to 
ggplot(filter(LICTotCo2,`Country code`=="PRK"),aes(year,`tot co2 emmision`))+geom_point(aes(color=`Country code`))


