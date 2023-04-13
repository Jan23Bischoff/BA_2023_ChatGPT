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
        tabPanel("Data",
                 h3("Instructions"),
                 p("Upload a CSV file to view the dataset in a table format."),
                 DT::dataTableOutput("table1")
        ),
        tabPanel("Data Visualization",
                 h3("Instructions"),
                 p("Choose variables to visualize and select a plot type."),
                 selectInput("variable1", "Variable 1:", choices = NULL),
                 selectInput("variable2", "Variable 2:", choices = NULL),
                 selectInput("plottype", "Plot Type:",
                             choices = c("Scatterplot", "Histogram", "Linechart", "Box-plot", "Barchart")),
                 plotOutput("plot1")
        ),
        tabPanel("Regression Analysis",
                 h3("Instructions"),
                 p("Select dependent and independent variables, then run the regression analysis and make predictions."),
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
    updateSelectInput(session, "variable1", choices = names(dataset()))
    updateSelectInput(session, "variable2", choices = names(dataset()))
    updateSelectInput(session, "dependent", choices = names(dataset()))
    updateCheckboxGroupInput(session, "independent", choices = names(dataset()))
  })
  
  output$table1 <- DT::renderDataTable(dataset())
  
  output$plot1 <- renderPlot({
    if (is.null(dataset())) return(NULL)
    if (is.null(input$variable1)) return(NULL)
    
    data <- dataset()
    var1 <- data[[input$variable1]]
    
    if (input$plottype == "Histogram") {
      ggplot(data, aes_string(x = input$variable1)) +
        geom_histogram() +
        theme_minimal()
    } else {
      if (is.null(input$variable2)) return(NULL)
      var2 <- data[[input$variable2]]
      
      if (input$plottype == "Scatterplot") {
        ggplot(data, aes_string(x = input$variable1, y = input$variable2)) +
          geom_point() +
          theme_minimal()
      } else if (input$plottype == "Linechart") {
        ggplot(data, aes_string(x = input$variable1, y = input$variable2, group = 1)) +
          geom_line() +
          theme_minimal()
      } else if (input$plottype == "Box-plot") {
        ggplot(data, aes_string(x = input$variable1, y = input$variable2)) +
          geom_boxplot() +
          theme_minimal()
      } else if (input$plottype == "Barchart") {
        ggplot(data, aes_string(x = input$variable1, y = "..count..")) +
          geom_bar() +
          theme_minimal()
      }
    }
  })

  output$regression_output <- renderPrint({
    if (is.null(input$regression)) return(NULL)

    if (is.null(input$dependent) || length(input$independent) == 0) {
      return("Please select dependent and independent variables.")
    }

    data <- dataset()
    data[] <- lapply(data, function(x) {
      if (is.factor(x)) as.numeric(as.factor(x)) else x
    })

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
    data[] <- lapply(data, function(x) {
      if (is.factor(x)) as.numeric(as.factor(x)) else x
    })

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
