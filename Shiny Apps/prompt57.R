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
    datatable(data(), editable = 'cell', rownames = FALSE)
  })
  
  proxy <- dataTableProxy("table")
  
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    row <- info$row
    col <- info$col + 1  # Column index adjustment
    value <- info$value
    
    data <- isolate(data())  # Retrieve the current dataset
    
    if (value == "NA") {
      value <- NA  # Convert "NA" string to actual NA value
    }
    
    data[row, col] <<- as.numeric(value)  # Update the value in the dataset
    replaceData(proxy, data, resetPaging = FALSE)  # Update the table with the new data
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
