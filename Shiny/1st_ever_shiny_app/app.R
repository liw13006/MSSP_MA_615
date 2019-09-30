#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
pacman::p_load('HistData')


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
#shinyApp(ui = ui, server = server)

ui1 <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server1 <- function(input, output, session) {
  product = reactive({input$x * input$y})
  f1 = 5
  output$product <- renderText({ 
   # product <- input$x * input$y
    product()
  })
  output$product_plus5 <- renderText({ 
    #product <- input$x * input$y
    product() + f1
  })
  output$product_plus10 <- renderText({ 
    #product <- input$x * input$y
    product() + f1+f1
  })
}



ui1 <- fluidPage(
  selectInput("dataset", "Dataset", choices = ls("package:HistData")),
  verbatimTextOutput("summary"),
  tableOutput("plot")
)

server1 <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:HistData")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderTable({
    dataset()
  })
}
shinyApp(ui1, server1)