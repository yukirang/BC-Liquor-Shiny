library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  # add an image
  img(src = "liquor.jpeg"), 
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      # mutiple selection with checkbox
      checkboxGroupInput("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      # download button for downloading the filtered data
      downloadButton('downloadData', 'Download'),
      # DT package to make the table interactive
      DT::dataTableOutput  ("results")
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
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  # render a interactive table with "DT::renderDataTable"
  output$results <- DT::renderDataTable({
    filtered()
  })
  # download the filtered data and save it as .csv file
  output$downloadData <- downloadHandler(
      filename = function() {
        # specifiy the file name
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        # write the .csv file with the filtered data
        write.csv(filtered(), con)
      }
    )
}

shinyApp(ui = ui, server = server)


