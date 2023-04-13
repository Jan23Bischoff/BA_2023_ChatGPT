# Load required libraries
library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("CSV File Upload and Display"),
  
  # Create a sidebar layout
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")),
      tags$hr(),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Data", DTOutput("data_table"))
      ),
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to read uploaded CSV file
  dataset <- reactive({
    req(input$file_upload)
    read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
  })
  
  # Render datatable
  output$data_table <- renderDT({
    datatable(dataset())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
