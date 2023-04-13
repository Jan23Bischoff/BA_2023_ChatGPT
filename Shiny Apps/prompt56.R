# Load necessary libraries
library(shiny)
library(DT)

# Shiny UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv"))
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  output$table <- renderDataTable({
    data()
  }, editable = TRUE, options = list(autoWidth = TRUE))
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
