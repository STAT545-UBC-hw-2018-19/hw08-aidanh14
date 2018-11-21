library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  
  tags$head(tags$style(type="text/css", "#image img {max-width: 100%; width: auto; height: auto}")),
  
  sidebarLayout(
    sidebarPanel(
    	imageOutput("image"),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
    	radioButtons("plotType", "Plot type",
    							 choices = c("Histogram", "Density")),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      dataTableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) + {
    	if (input$plotType == "Histogram")
    		geom_histogram()
    	else
    		geom_density(fill="grey")
    }
  })

  output$results <- renderDataTable({
    filtered()
  })
  
  output$image <- renderImage({
  	list(src = "www/bcl_pic2.jpg",
  			 contentType = "image/jpg")},
  	deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)
