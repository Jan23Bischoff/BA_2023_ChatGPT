# Load libraries
library(shiny)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("CSV Regression Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File", accept = c(".csv")),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Regression Analysis",
                 h3("Instructions"),
                 p("Upload a CSV file, select dependent and independent variables, and run the regression analysis."),
                 selectInput("dependent", "Dependent Variable:", choices = NULL),
                 checkboxGroupInput("independent", "Independent Variables:", choices = NULL),
                 actionButton("regression", "Run Regression Analysis"),
                 verbatimTextOutput("regression_output"),
                 h3("Predict New Value"),
                 uiOutput("predict_inputs"),
                 actionButton("predict", "Predict"),
                 textOutput("prediction")
        )
      ),
      width = 10
    )
  )
)

# Define server
server <- function(input, output, session) {
  dataset <- reactive({
    if (is.null(input$file1)) return(NULL)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  observeEvent(dataset(), {
    updateSelectInput(session, "dependent", choices = names(dataset()))
    updateCheckboxGroupInput(session, "independent", choices = names(dataset()))
  })
  
  output$regression_output <- renderPrint({
    if (is.null(input$regression)) return(NULL)
    
    if (is.null(input$dependent) || length(input$independent) == 0) {
      return("Please select dependent and independent variables.")
    }
    
    data <- dataset()
    formula_string <- paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    model <- lm(as.formula(formula_string), data = data)
    summary(model)
  })
  
  output$predict_inputs <- renderUI({
    if (is.null(input$regression)) return(NULL)
    
    if (is.null(input$dependent) || length(input$independent) == 0) {
      return("Please select dependent and independent variables.")
    }
    
    lapply(input$independent, function(var) {
      tagList(
        numericInput(paste0("predict_", var), label = paste("Value for", var, ":"), value = 0),
        br()
      )
    })
  })
  
  prediction_data <- reactive({
    if (is.null(input$predict)) return(NULL)
    if (is.null(input$dependent) || length(input$independent) == 0) return(NULL)
    
    sapply(input$independent, function(var) {
      input[[paste0("predict_", var)]]
    })
  })
  
  observeEvent(prediction_data(), {
    req(prediction_data())
    data <- dataset()
    formula_string <- paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    model <- lm(as.formula(formula_string), data = data)
    
    new_data <- data.frame(t(prediction_data()))
    colnames(new_data) <- input$independent
    
    pred <- predict(model, newdata = new_data)
    
    output$prediction <- renderText({
      paste("Predicted value for", input$dependent, ":", round(pred, 2))
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
