# Load necessary libraries
library(shiny)
library(DT)

# Shiny UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      actionButton("update_data", "Update Data")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  data <- reactiveVal()  # Create an empty reactive value to store the data
  
  observeEvent(input$file, {
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    dataset <- read.csv(inFile$datapath)
    data(dataset)  # Store the dataset in the reactive value
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
    
    current_data <- data()  # Retrieve the current dataset
    
    if (value == "NA") {
      value <- NA  # Convert "NA" string to actual NA value
    }
    
    current_data[row, col] <- as.numeric(value)  # Update the value in the dataset
    data(current_data)  # Update the reactive value with the modified dataset
  })
  
  observeEvent(input$update_data, {
    replaceData(proxy, data(), resetPaging = FALSE)  # Update the table with the new data
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
