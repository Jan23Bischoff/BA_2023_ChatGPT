# Load necessary libraries
library(shiny)
library(DT)

# Shiny UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      actionButton("save_changes", "Save Changes")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  rv <- reactiveValues(original_data = NULL, edited_data = NULL)
  
  observeEvent(input$file, {
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    dataset <- read.csv(inFile$datapath)
    rv$original_data <- dataset
  })
  
  output$table <- renderDataTable({
    datatable(rv$original_data, editable = 'cell', rownames = FALSE)
  })
  
  proxy <- dataTableProxy("table")
  
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    row <- info$row
    col <- info$col + 1  # Column index adjustment
    value <- info$value
    
    current_data <- if (is.null(rv$edited_data)) {
      rv$original_data
    } else {
      rv$edited_data
    }
    
    if (value == "NA") {
      value <- NA  # Convert "NA" string to actual NA value
    }
    
    current_data[row, col] <- as.numeric(value)  # Update the value in the dataset
    rv$edited_data <- current_data  # Store the modified dataset in reactiveValues
  })
  
  observeEvent(input$save_changes, {
    if (!is.null(rv$edited_data)) {
      rv$original_data <- rv$edited_data
      replaceData(proxy, rv$original_data, resetPaging = FALSE)
      rv$edited_data <- NULL
    }
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
