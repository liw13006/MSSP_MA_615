#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(htmltools)
library(tidyverse)
library(geosphere)
library(shiny)
library(shinydashboard)
library(DT)
library(magrittr)

## Data Import, this import includes:
## 1. Four interactive maps of both Boston and San Francisco, raw and clustered
## 2. Four Gaussian Mixture models of spatial distribution, 2 in Boston, 2 in San Francisco
## 3. Restaurant coordinates which can be used to generate user defined clusters. 
##    But not able to recluster the maps and plots for now. It can be done with more time
##    and with reactive shiny elements.
## 4. Original Boston & San Francisco yelp Data. In city or in city.genre sub-tables.
## 5. ggplots of ratings, clusters and distance relationships
source("EDA.R")

#library(plotly)
#library(DT)
##

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Restaurant Spatial Distribution",titleWidth = 300),
    
    dashboardSidebar(
      sidebarMenu(
        id = "sidebarmenu",
        menuItem("Introduction", tabName = "intro", icon = icon("trello")),
        menuItem("Raw Data on Map", tabName = "raw_map",icon = icon("map-marker-alt"),
                 menuItem("Boston",tabName = "raw_bos",icon = icon("map")),
                 menuItem("San Francisco",tabName = "raw_sf",icon = icon("map"))),
        menuItem("EDA",tabName = "eda",icon = icon("edit"),
                 menuItem("Distribution of Ratings",tabName = "eda_rat",icon = icon("edit")),
                 menuItem("Distribution of Distance",tabName = "eda_dist",icon = icon("edit"))),
        menuItem("Clustering",tabName = "clus_map",icon = icon("map-marker-alt"),
                 menuItem("Gaussian Mixture Clustering",tabName ="gmm",icon = icon("connectdevelop") ),
                 menuItem("Boston",tabName = "clus_bos",icon = icon("map")),
                 menuItem("San Francisco",tabName = "clus_sf",icon = icon("map"))),
        menuItem("Analysis",tabName = "a",icon = icon("chart-bar"),
                 menuItem("Rating of different clusters",
                          tabName = "rating_plot", 
                          icon = icon("star-half-alt"),
                          menuItem("Boston",tabName = "rat_plot_bos",icon = icon("cogs")),
                          menuItem("San Francisco",tabName = "rat_plot_sf",icon = icon("cogs"))),
                 menuItem("Distance Distribution of clusters",
                          tabName = "dist_plot",
                          icon = icon("route"),
                          menuItem("Boston",tabName = "dist_plot_bos",icon = icon("cogs")),
                          menuItem("San Francisco",tabName = "dist_plot_sf",icon = icon("cogs")))),
        menuItem("Conclusions",tabName = "conclusion",icon = icon("book"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "intro",
                fluidRow(
                  column(width = 12,
                         box(width = 10, solidHeader = T,status = "info",
                             title = "Restaurant Clustering in Cities?",
                             style = "font-family: 'Roboto'",
                             h5("Do restaurants exhibit a geospatial clustering by genre?",
                                style = "font-family: 'Roboto'"),
                             h5("For example, Chinese cuisine clustered in allston and chinatown. Italian in North End, Vietnamese in Dorchester.",style = "font-family: 'Roboto'"),
                             h5("Does the rating of restaurants also clustered geospatially?",style = "font-family: 'Roboto'"),
                             h5("When conditioned in one attribute(genre or rating), do this two clusterings have a strong interaction?(Maybe the restaurant in it's genre cluster are likely to associated with higher ratings or not?)",style = "font-family: 'Roboto'" )),
                         box(width = 5, solidHeader = T, status = "info",
                             title = "Data Scope & Data Sources",
                             style = "font-family: 'Roboto'",
                             h5("For this project, ",
                                strong("Boston"),
                                " & ",
                                strong("San Francisco"),
                                " data was observed.",style = "font-family: 'Roboto'"),
                             h5(a(href = "https://www.yelp.com/developers/documentation/v3/business_search","Yelp.com"),
                                "Data was collected from "," via yelp Fusion api",
                                style = "font-family: 'Roboto'")),
                         box(width = 5, solidHeader = T, status = "info",
                             title = "Method Used",
                             style = "font-family: 'Roboto'",
                             h5(strong(a(href = "https://towardsdatascience.com/gaussian-mixture-models-explained-6986aaf5a95",
                                         "Gaussian Mixture Model"))," was used to perform geospatial clustering of the restaurants",
                                style = "font-family: 'Roboto'")),
                         box(width = 10,solidHeader = T,status = "success",
                             title = "Raw Data from Yelp",
                             style = "font-family: 'Roboto'",
                             h5("The Raw data contains 640 restaurants from Boston & 1440 restaurants from the City of San Francisco.",style = "font-family: 'Roboto'"),
                             strong("Boston Yelp RawData: "),br(),
                             DT::DTOutput("raw_data_bs"),br(),
                             strong("San Francisco Yelp RawData: "),br(),
                             DT::DTOutput("raw_data_sf"),
                             collapsible = T,collapsed = T))
                )
        ),
        
        tabItem(tabName = "raw_bos",
                fluidRow(
                  column(width = 12,
                         box(width = 8,solidHeader = T,status = "success",
                             title = "Unclustered Restaurant in Boston",
                             style = "font-family: 'Roboto'",
                             leafletOutput("bos_raw",height = 500)
                             ),
                         box(width = 4, solidHeader = T,status = "info",
                             h4("From a glance, there is not much to see from the raw output.",
                                style = "font-family: 'Roboto'"),
                             br(),
                             h4(" Without zooming in, it is hard to detect any clustering. However, if zoomed in, we could clearly see some clustering(Chinese food have a High concentration at Chinatown and Italian in North End)",
                                style = "font-family: 'Roboto'"))
                         )
                )),
        tabItem(tabName = "raw_sf",
                fluidRow(
                  column(width = 12,
                         box(width = 8,solidHeader = T,status = "success",
                             title = "Unclustered Restaurant in San Francisco",
                             style = "font-family: 'Roboto'",
                             leafletOutput("sf_raw",height = 500)
                         ),
                         box(width = 4, solidHeader = T,status = "info",
                             h4("From a glance, there is not much to see from the raw output.",
                                style = "font-family: 'Roboto'"),
                             br(),
                             h4(" Without zooming in, it is hard to detect any clustering. However, if zoomed in, we could clearly see some clustering(Chinese food have a High concentration at Chinatown and Italian in North End)",
                                style = "font-family: 'Roboto'"))
                  )
                )),
        tabItem(tabName = "eda_rat",
                fluidRow(
                  column(width = 12,
                         box(width = 10,solidHeader = T,status = "success",
                             title = "Distribution of Ratings by Genre",
                             style = "font-family: 'Roboto'",
                             h5("Started our from comparing the distribution of ratings within each city of the two genre interested. We found that the overall distribution is similar between the two Cities.", style = "font-family: 'Roboto'"),br(),
                                h5("Overall, Italian restaurants rated a little bit higher than Chinese restaurants",
                                style = "font-family: 'Roboto'")
                             ),
                         box(width = 10, solidHeader = T,status = "success",
                             plotOutput("bs_rating_hist")),
                         box(width = 10,solidHeader = T,status = "success",
                             plotOutput("sf_rating_hist")))
                )),
        tabItem(tabName = "eda_dist",
                fluidRow(
                  column(width = 12,
                         box(width = 10,solidHeader = T,status = "success",
                             title = "The Distribution of Distance to City Center conditioned by Rating",
                             style = "font-family: 'Roboto'",
                             h5("As we can see from the map. When conditioned on distance to the city center, the two cities showed a different trend",
                                style = "font-family: 'Roboto'"),br(),
                             h5("In Boston, for both Italian and Chinese Restaurants, when approaching to the city center, food tends to be ",strong("Okay"),". When moving away from the city center, it becames harder to determine whether it is actually good or not.",
                                style = "font-family: 'Roboto'"),br(),
                             h5("In San Francisco, Chinese food seems to get better when you approaching towards the city center while Italian food's quality stays roughly the same",
                                style = "font-family: 'Roboto'")
                         ),
                         box(width = 10,solidHeader = T,status = "success",
                             plotOutput("bs_rat_dist_vio")),
                         box(width = 10,solidHeader = T,status = "success",
                             plotOutput("sf_rat_dist_vio")))
                )),
        tabItem(tabName = "gmm",
                fluidRow(
                  column(width = 12,
                         box(width = 10,solidHeader = T,status = "info",
                             title = "Gaussian Mixture",
                             style = "font-family: 'Roboto'",
                             h5("Gaussian Mixture model was widely used to do clustering analysis. Not only can it provide a soft boundary assignment but can also detect a highly concentrated anomaly with in a cluster.",
                                style = "font-family: 'Roboto'"),br(),
                             h5("Below are an example output of a gaussian mixture map from the package: ",strong("mclust"),
                                style = "font-family: 'Roboto'"),br(),
                             strong("Boston Chinese Restaurant Clustering"),br(),
                             plotOutput("mclust_bs_cn",width = "50%"),br(),
                             strong("Boston Italian Restaurant Clustering"),br(),
                             plotOutput("mclust_bs_it",width = "50%"),br(),
                             strong("San Francisco Chinese Restaurant Clustering"),br(),
                             plotOutput("mclust_sf_cn",width = "50%"),br(),
                             strong("San Francisco Italian Restaurant Clustering"),br(),
                             plotOutput("mclust_sf_it",width = "50%"),br(),
                             h5("As can be seen in the plots, there are highly concentrated areas with in the cluster, The small blue cluster in the Boston Chinese Restaurant map is actually the Chinatown area.",
                                style = "font-family: 'Roboto'"),br(),
                             h5("For the next two tabs, user can explore the map to see what area are these clusters actually showing.",
                                style = "font-family: 'Roboto'")
                         ))
                )),
        tabItem(tabName = "clus_bos",
                fluidRow(
                  column(width = 12,
                         box(width = 10,solidHeader = T,status = "success",
                             h4(strong("Boston City Clustering"),
                                style = "font-family: 'Roboto'"),br(),
                             h6("Restaurants are clickable, toggle base layer to 'None' to observe the cluster"),
                             leafletOutput("bos_clust",height = 500)))
                )),
        tabItem(tabName = "clus_sf",
                fluidRow(
                  column(width = 12,
                         box(width = 10,solidHeader = T,status = "success",
                             h4(strong("San Francisco City Clustering"),
                                style = "font-family: 'Roboto'"),br(),
                             h6("Restaurants are clickable, toggle base layer to 'None' to observe the cluster"),
                             leafletOutput("sf_clust",height = 500)))
                )),
        tabItem(tabName = "rat_plot_bos",
                fluidRow(
                           column(width = 12,
                                  box(width = 5,solidHeader = T,status = "warning",
                                      plotOutput("rat_plot_bs_cn"),br(),
                                      plotOutput("rat_plot_bs_it")),
                                  box(width = 5,solidHeader = T,status = "info",
                                      leafletOutput("bos_clust_1",height = 818))
                            
                  ))
                ),
        tabItem(tabName = "rat_plot_sf",
                fluidRow(
                column(width = 12,
                       box(width = 5,solidHeader = T,status = "warning",
                           plotOutput("rat_plot_sf_cn"),br(),
                           plotOutput("rat_plot_sf_it")),
                       
                       box(width = 5,solidHeader = T,status = "info",
                           leafletOutput("sf_clust_1",height = 818))
                       
                ))
                  
                ),
        tabItem(tabName = "dist_plot_bos",
                fluidRow(
                  column(width = 12,
                         box(width = 5,solidHeader = T,status = "warning",
                             plotOutput("dist_plot_bs_cn"),br(),
                             plotOutput("dist_plot_bs_it")),
                         
                         box(width = 5,solidHeader = T,status = "info",
                             leafletOutput("bos_clust_2",height = 818))
                         
                  ))
                
        ),
        tabItem(tabName = "dist_plot_sf",
                fluidRow(
                  column(width = 12,
                         box(width = 5,solidHeader = T,status = "warning",
                             plotOutput("dist_plot_sf_cn"),br(),
                             plotOutput("dist_plot_sf_it")),
                         
                         box(width = 5,solidHeader = T,status = "info",
                             leafletOutput("sf_clust_2",height = 818))
                         
                  ))
                
        ),
        tabItem(tabName = "conclusion",
                fluidRow(
                  column(width = 12,
                         box(width = 10,solidHeader = T,status = "success",
                             title = "Conclusion",
                             style = "font-family: 'Roboto'",
                             h5("As we can see from the map and plots, restaurants are highly clustered in both city: Boston and San Francisco. However, unlike what people normally thought, the best place for the authentic cuisine might not always be at the place where the specific demographic group lives.(In boston, turns out that Allston has the best Chinese food when Chinatown on the other hand, has the lowest ratings).",
                                style = "font-family: 'Roboto'"),br(),
                             h5("One speculation is that, the demand for authentic food might actually driven by the concentration of international population in the city. Also take Boston as an Example. The highest concentration of Chinese population(excluding American born Chinese) might actually be at Allston because of the Boston University! Chinese student population here might actually be the driven force of the authenticity of foods which resembles their home on the other side of the Earth.", style = "font-family: 'Roboto'")))
                ))
      )
      )
)

server <- function(input, output) {
  output$raw_data_bs <- DT::renderDataTable({DT::datatable(bs_clean,
                                                           options =list(searching = TRUE, 
                                                                         pageLength = 20, 
                                                                         scrollX = T,
                                                                         scrollY = "550px"))%>%
      DT::formatRound(columns = c("latitude","longitude"),digits = 2)%>%
      DT::formatStyle(columns = colnames(.), fontSize = '20%')
    })
  
  output$raw_data_sf <- DT::renderDataTable({DT::datatable(sf_clean,
                                                           options =list(searching = TRUE, 
                                                                         pageLength = 20, 
                                                                         scrollX = T,
                                                                         scrollY = "550px"))%>%
      DT::formatRound(columns = c("latitude","longitude"),digits = 2)%>%
      DT::formatStyle(columns = colnames(.), fontSize = '20%')
  })
  
  output$bos_raw <- renderLeaflet(map_bs_unclus)
  
  output$sf_raw <- renderLeaflet(map_sf_unclus)
  
  output$bs_rating_hist <- renderPlot(hist_bs_rating)
  
  output$sf_rating_hist <- renderPlot(hist_sf_rating)
  
  output$bs_rat_dist_vio <- renderPlot(hist_bs_rat_dist)
  
  output$sf_rat_dist_vio <- renderPlot(hist_sf_rat_dist)
  
  output$mclust_bs_cn <- renderPlot({mclust::mclust2Dplot(data = coor_bs_cn,parameters = boston_gm_cn$parameters,z = boston_gm_cn$z,classification = boston_gm_cn$classification,what = "classification",main = F)})
  
  output$mclust_bs_it <- renderPlot({mclust::mclust2Dplot(data = coor_bs_it,parameters = boston_gm_it$parameters,z = boston_gm_it$z,classification = boston_gm_it$classification,what = "classification",main = F)})
  
  output$mclust_sf_cn <- renderPlot({mclust::mclust2Dplot(data = coor_sf_cn,parameters = sf_gm_cn$parameters,z = sf_gm_cn$z,classification = sf_gm_cn$classification,what = "classification",main = F)})
  
  output$mclust_sf_it <- renderPlot({mclust::mclust2Dplot(data = coor_sf_it,parameters = sf_gm_it$parameters,z = sf_gm_it$z,classification = sf_gm_it$classification,what = "classification",main = F)})
  
  output$bos_clust <- renderLeaflet(bs_map_test)
  
  output$sf_clust <- renderLeaflet(sf_map_test)
  
  output$rat_plot_bs_cn <- renderPlot(bs_rating_cn)
  output$rat_plot_bs_it <- renderPlot({bs_rating_it+theme(plot.title = element_blank())})
  
  output$rat_plot_sf_cn <- renderPlot(sf_rating_cn)
  output$rat_plot_sf_it <- renderPlot({sf_rating_it+theme(plot.title = element_blank())})
  
  output$bos_clust_1 <- renderLeaflet({bs_map_test})
  
  output$sf_clust_1 <- renderLeaflet(sf_map_test)
  
  output$dist_plot_bs_cn <- renderPlot(bs_dist_cn)
  output$dist_plot_bs_it <- renderPlot(bs_dist_it)
  output$dist_plot_sf_cn <- renderPlot(sf_dist_cn)
  output$dist_plot_sf_it <- renderPlot(sf_dist_it)
  
  output$bos_clust_2 <- renderLeaflet(bs_map_test)
  
  output$sf_clust_2 <- renderLeaflet(sf_map_test)
}

shinyApp(ui = ui, server = server)
