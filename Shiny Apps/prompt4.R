# Load libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("CSV Data Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File", accept = c(".csv")),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DT::dataTableOutput("table1")),
        tabPanel("Data Visualization",
                 selectInput("variable1", "Variable 1:", choices = NULL),
                 selectInput("variable2", "Variable 2:", choices = NULL),
                 selectInput("plottype", "Plot Type:",
                             choices = c("Scatterplot", "Histogram", "Linechart", "Box-plot", "Barchart")),
                 plotOutput("plot1")
        ),
        tabPanel("Regression Analysis",
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
    read.csv(input$file1$datapath)
  })
  
  observeEvent(dataset(), {
    updateSelectInput(session, "variable1", choices = names(dataset()))
    updateSelectInput(session, "variable2", choices = names(dataset()))
    updateSelectInput(session, "dependent", choices = names(dataset()))
    updateCheckboxGroupInput(session, "independent", choices = names(dataset()))
  })
  
  output$table1 <- DT::renderDataTable(dataset())
  
  output$plot1 <- renderPlot({
    if (is.null(dataset())) return(NULL)
    if (is.null(input$variable1) || is.null(input$variable2)) return(NULL)
    
    data <- dataset()
    var1 <- data[[input$variable1]]
    var2 <- data[[input$variable2]]
    
    ggplot(data, aes_string(x = input$variable1, y = input$variable2)) +
      geom_point() +
      theme_minimal()
  })
  
  output$regression_output <- renderPrint({
    if (is.null(input$regression)) return(NULL)
    
    if (is.null(input$dependent) || length(input$independent) == 0) {
      return("Please select dependent and independent variables.")
    }
    
    formula_string <- paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    model <- lm(as.formula(formula_string), data = dataset())
    summary(model)
  })
  
  output$predict_inputs <- renderUI({
    if (is.null(input$regression)) return(NULL)
    
    if (is.null(input$dependent) || length(input$independent) == 
        0) {
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
    formula_string <- paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    model <- lm(as.formula(formula_string), data = dataset())
    
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


