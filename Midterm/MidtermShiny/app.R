#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny,tidyverse,reshape2,plotly,DT,htmlwidgets,kableExtra)

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
## Compute data table
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


# Because we are using World data as a Base line, we need to filter out years which has absolutely no world data, in this case is: 2011
Climate <- Climate%>%dplyr::filter(year != 2011)

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
D_Temp <- D_Temp%>%column_to_rownames(var = "year")%>%transpose_df()
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
# success!
# 
# Series code is not user friendly, we would use Series name instead
SeriesName = SeriesCode%>%dplyr::select(`Series name`)%>%pull()


# try pivot series code,log all input

Climate1 <- Climate%>%dplyr::select(1:3,7:10)%>%pivot_wider(names_from = `Series code`,values_from = value)%>%dplyr::mutate_at(.vars = 6:11,.funs = log)





# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel(
    tabPanel("Climate Data vs Year",
             titlePanel("Income groups or Region data vs year"),
             
             
             sidebarLayout(
                 sidebarPanel(
                     selectInput("seriesname",label = "Series Name",choices = SeriesName),
                     selectInput("Income",label = "Income or Region",c("Income" = TRUE,"Region" = FALSE))
                 ),
                 mainPanel(
                     plotlyOutput("linePlot1")
                     #dataTableOutput("tablePlot")
                 )
             )
             
             ),
    
    tabPanel("Comparing distribution of variables",
             titlePanel("Climate Related Data 1992 vs 2008"),
             
             
             sidebarLayout(
                 sidebarPanel(
                     selectInput("seriesname1",label = "Series Name",choices = SeriesName)
                 ),
                 mainPanel(
                     plotlyOutput("distPlot"),
                     plotlyOutput("distPlot1")
                     
                 )
             )
             
    ),
    
    
    tabPanel("Plot Variables as scatter plot",
             titlePanel("Interactions between variables"),
             flowLayout(
                      selectInput("year",label = "Year Interested",choices = 1992:2008)
                      
                      ),
             splitLayout(
                 plotlyOutput("scatterPlot"),
                 plotlyOutput("scatterPlot1")
             )
             )
             
    )
)






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #bins computation
    compute_bins <- function(x, n) {
        list(
            start = min(x),
            end = max(x),
            size = (max(x) - min(x)) / n
        )
    }
    ## dataframe for 3rd tab
    DF2 <- reactive({
        DF2 <- Climate1%>%
            dplyr::filter(!`Country code` %in% Incomecode & !`Country code` %in% Regioncode,
                          year %in% input$year )
        DF2
    })
    
    output$scatterPlot <- renderPlotly({
        
        p3 <- ggplot(DF2())+
            aes(y = `EN.ATM.CO2E.KT`,x = `SP.POP.TOTL`)+
            geom_point(aes(size = `NY.GDP.MKTP.CD`,color = `Income group`))+
            scale_size_continuous(range = c(0,4))+
            xlab("Total Population, size = GDP")+
            ylab("Total CO2 Emission")+
            ggtitle(paste0("year of ",input$year))
        
        plotly_build(p3)%>%layout(legend = list(orientation = 'h',x = 0, y = -0.3))
        
    })
    
    output$scatterPlot1 <- renderPlotly({
        
        p4 <- ggplot(DF2())+
            aes(y = `EN.ATM.CO2E.PC`,x = `SP.POP.TOTL`)+
            geom_point(aes(size = `NY.GDP.MKTP.CD`,color = `Income group`))+
            scale_size_continuous(range = c(0,4))+
            xlab("Total Population, size = GDP")+
            ylab("Total CO2 Emission per CAP")+
            ggtitle(paste0("year of ",input$year))
        
        plotly_build(p4)%>%layout(legend = list(orientation = 'h',x = 0, y = -0.3))
        
    })
    
    ## dataframe for 2nd tab
    DF1 <- reactive({
        DF1 <- Climate%>%
            dplyr::filter(`Series name` == input$seriesname1,
                          !`Country code` %in% Incomecode & !`Country code` %in% Regioncode,
                          year %in% c(1992,2008))
        DF1
    })
        
    
    ## list for the 1st tab
    DFlist <- reactive({if(input$Income == TRUE){
        Dlist <- Incomecode
    } else {
        Dlist <- Regioncode
    }
        Dlist
        })
    
    ## dataframe for the 1st tab
    DF <- reactive({
        DF <- Climate%>%
            dplyr::filter(`Series name` == input$seriesname,`Country code` %in% DFlist())%>%
            dplyr::mutate(year = as.numeric(as.character(year)))
        DF
        })

    output$distPlot <- renderPlotly({
        p1 <- ggplot(DF1())+
            aes(x =log(value),color = `Income group`,fill = `Income group`)+
            facet_wrap(as.factor(DF1()$year),nrow = 2)+
            geom_histogram(bins = 30,aes(y = ..density..),position = "dodge",alpha = .2)+
            geom_density(alpha = .3)+
            ggtitle(paste0("log ",input$seriesname1))+ theme(axis.title.x=element_blank(),
                                                            axis.title.y=element_blank())
        
        
        plotly_build(p1)
        
    })
    
    output$distPlot1 <- renderPlotly({
        p2 <- ggplot(DF1())+
            aes(x =log(value),color = `Region`,fill = `Region`)+
            facet_wrap(as.factor(DF1()$year),nrow = 2)+
            geom_histogram(bins = 30,aes(y = ..density..),position = "dodge",alpha = .2)+
            geom_density(alpha = .3)+
            ggtitle(paste0("log ",input$seriesname1))+ theme(axis.title.x=element_blank(),
                                                             axis.title.y=element_blank())


        plotly_build(p2)
    })
    
    output$tablePlot <- renderDataTable(DF1())
    
    
    output$linePlot1 <- renderPlotly({
        plotly_build(ggplot(DF())+
                         aes(x = year,y = value,color = `Country name`)+
                         geom_line()+
                         ggtitle(input$seriesname))%>%
            layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    })
    
    
}
shinyApp(ui = ui, server = server)


