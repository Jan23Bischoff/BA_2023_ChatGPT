# Load necessary libraries
library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)

# Define UI
ui <- fluidPage(
  titlePanel("Regression Analysis Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      uiOutput("variable_selectors"),
      actionButton("run_regression", "Run Regression"),
      hr(),
      uiOutput("prediction_inputs"),
      actionButton("predict", "Predict"),
      hr()
    ),
    mainPanel(
      tableOutput("regression_results"),
      h4("Prediction:"),
      textOutput("prediction_output")
    )
  )
)

# Define server
server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  regression_model <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    dataset(read_csv(input$file$datapath))
  })
  
  output$variable_selectors <- renderUI({
    req(dataset())
    
    independent_vars <- setdiff(names(dataset()), "realSum")
    selectInput("dependent_variable", "Dependent Variable:", choices = "realSum", selected = "realSum", multiple = FALSE)
    selectInput("independent_variables", "Independent Variables:", choices = independent_vars, selected = independent_vars, multiple = TRUE)
  })
  
  observeEvent(input$run_regression, {
    req(dataset())
    
    fmla <- as.formula(paste(input$dependent_variable, paste(input$independent_variables, collapse = " + "), sep = " ~ "))
    regression_model(lm(fmla, data = dataset()))
    output$regression_results <- renderTable(tidy(regression_model()))
  })
  
  output$prediction_inputs <- renderUI({
    req(regression_model())
    
    input_fields <- lapply(input$independent_variables, function(var) {
      numericInput(var, label = paste("Enter", var, ":"), value = 0)
    })
    return(input_fields)
  })
  
  observeEvent(input$predict, {
    req(regression_model())
    
    new_values <- data.frame(matrix(ncol = length(input$independent_variables), nrow = 1))
    colnames(new_values) <- input$independent_variables
    
    for (var in input$independent_variables) {
      new_values[[var]] <- as.numeric(input[[var]])
    }
    
    prediction <- predict(regression_model(), newdata = new_values)
    output$prediction_output <- renderText(round(prediction, 2))
  })
}

# Run the application
shinyApp(ui, server)
