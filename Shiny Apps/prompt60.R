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
  original_data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  edited_data <- reactiveValues(data = NULL)
  
  output$table <- renderDataTable({
    datatable(original_data(), editable = 'cell', rownames = FALSE)
  })
  
  proxy <- dataTableProxy("table")
  
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    row <- info$row
    col <- info$col + 1  # Column index adjustment
    value <- info$value
    
    current_data <- if (is.null(edited_data$data)) {
      original_data()
    } else {
      edited_data$data
    }
    
    if (value == "NA") {
      value <- NA  # Convert "NA" string to actual NA value
    }
    
    current_data[row, col] <- as.numeric(value)  # Update the value in the dataset
    edited_data$data <- current_data  # Store the modified dataset in reactiveValues
  })
  
  observeEvent(input$save_changes, {
    if (!is.null(edited_data$data)) {
      replaceData(proxy, edited_data$data, resetPaging = FALSE)
      original_data(isolate(edited_data$data))
      edited_data$data <- NULL
    }
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
