#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny,tmap,maps,magrittr,dplyr,leaflet)

library(readr)

Parking_Meters <- read_csv("Parking_Meters.csv")
datasites <- Parking_Meters%>%
    filter(SPACE_STATE == "ACTIVE"&METER_STATE == "ACTIVE"&LONGITUDE < -10)%>%
    select(BLK_NO,STREET,LONGITUDE,LATITUDE)

datasites1 <- Parking_Meters%>%
    filter(SPACE_STATE == "ACTIVE"&METER_STATE == "ACTIVE"&LONGITUDE < -10)%>%
    select(BLK_NO,STREET,LONGITUDE,LATITUDE)

MA <- map('county', 'Massachusetts', fill=TRUE, plot=FALSE,boundary = TRUE)

icons1 <- awesomeIcons(
    icon = 'cog',
    iconColor = 'rgba',
    library = 'fa', 
    markerColor = 'beige',
    squareMarker = F,
    spin=T
)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Meter in Boston"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("zone","Please select zones",unique(Parking_Meters$BLK_NO),multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput(outputId = "mymap"),
            width = 12,
            height = 12
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    DF <- reactive({
        if(length(input$zone)==0){
            datasites1%>%sample_frac(.01)
        } else{
            datasites%>%filter(BLK_NO %in% input$zone)
        }
    })

    output$mymap <- renderLeaflet({
        leaflet(DF()) %>% 
            setView(-71.098849, 42.350434, zoom = 13) %>% 
            addProviderTiles("CartoDB.Positron", group = "Map") %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite")%>%
            addProviderTiles("OpenStreetMap", group = "Mapnik")%>%
            addAwesomeMarkers(~LONGITUDE, 
                              ~LATITUDE, 
                              label = ~STREET, 
                              group = "Meter",
                              icon = icons1) %>%
            addPolygons(data=MA, group="States", weight=2, fillOpacity = 0) %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("Mapnik","Map", "Satellite" ),
                overlayGroups = c("Meter", "States"),
                options = layersControlOptions(collapsed = TRUE)
            )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
