# Load necessary libraries
library(shiny)
library(DT)
library(dplyr)

# Shiny UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Data Import and Table Display",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload CSV File", accept = c(".csv")),
                 actionButton("save_changes", "Save Changes")
               ),
               mainPanel(
                 DTOutput("table")
               )
             )
    ),
    tabPanel("Missing Values and Summary Statistics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("column", "Select Column", choices = NULL),
                 selectInput("imputation_strategy", "Imputation Strategy", choices = c("Mean", "Mode", "Median")),
                 actionButton("impute", "Impute Missing Values"),
                 actionButton("summary_stats", "Calculate Summary Statistics")
               ),
               mainPanel(
                 verbatimTextOutput("summary_output")
               )
             )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Code for the first page (Data Import and Table Display) remains the same
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
  
  observeEvent(rv$original_data, {
    updateSelectInput(session, "column", choices = colnames(rv$original_data))
  })
  
  observeEvent(input$impute, {
    req(rv$original_data)
    column <- input$column
    strategy <- input$imputation_strategy
    
    if (!is.null(column) && !is.null(strategy)) {
      imputed_data <- rv$original_data
      
      if (strategy == "Mean") {
        imputed_value <- mean(imputed_data[[column]], na.rm = TRUE)
      } else if (strategy == "Mode") {
        imputed_value <- as.numeric(names(which.max(table(imputed_data[[column]], useNA = "no"))))
      } else if (strategy == "Median") {
        imputed_value <- median(imputed_data[[column]], na.rm = TRUE)
      }
      
      imputed_data[[column]][is.na(imputed_data[[column]])] <- imputed_value
      rv$original_data <- imputed_data
      replaceData(proxy, rv$original_data, resetPaging = FALSE)
    }
  })
  
  output$summary_output <- renderPrint({
    req(rv$original_data)
    input$summary_stats
    summary(rv$original_data)
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)