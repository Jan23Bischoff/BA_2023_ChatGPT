# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(esquisse)
library(plotly)
library(reshape2)
library(dplyr)
library(readr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Science Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import Data", tabName = "import", icon = icon("file-import")),
      menuItem("Data Visualization", tabName = "visualize", icon = icon("chart-bar")),
      menuItem("Regression Analysis", tabName = "regression", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Import Data
      tabItem(tabName = "import",
              fileInput("file", "Choose CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              h5("Instructions:"),
              p("1. Upload your CSV file using the file input above."),
              p("2. The dataset will be displayed in a table below."),
              DT::dataTableOutput("dataTable")
      ),
      # Data Visualization
      tabItem(tabName = "visualize",
              h5("Instructions:"),
              p("1. Use the esquisse panel to create and customize your data visualizations."),
              p("2. Choose variables and visualization types."),
              esquisserUI(
                id = "esquisse",
                header = FALSE,
                container = esquisseContainer(
                  height = "800px",
                  width = "100%"
                )
              )
      ),
      # Regression Analysis
      tabItem(tabName = "regression",
              h5("Instructions:"),
              p("1. Select the dependent and independent variables for the regression analysis."),
              p("2. View the regression results."),
              p("3. Upload a CSV file with new observations to predict values."),
              p("4. View the final result based on the new values."),
              uiOutput("regression_ui"),
              tableOutput("regression_results"),
              fileInput("predict_file", "Choose CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              tableOutput("prediction_results")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load and display dataset
  dataset <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  output$dataTable <- DT::renderDataTable({
    dataset()
  })
  
  # Regression UI
  output$regression_ui <- renderUI({
    req(dataset())
    var_choices <- colnames(dataset())
    fluidRow(
      column(4, wellPanel(selectInput("dependent", "Dependent Variable", choices = var_choices))),
      column(4, wellPanel(selectInput("independent", "Independent Variables", choices = var_choices, multiple = TRUE))),
      column(4, wellPanel(actionButton("run_regression", "Run Regression")))
    )
  })
  
  # Run regression and display results
  regression_results <- eventReactive(input$run_regression, {
    req(input$dependent, input$independent)
    formula_string <- paste(input$dependent, "~", paste(input$independent, collapse = "+"))
    model <- lm(formula_string, data = dataset())
    summary(model)
  })
  
  output$regression_results <- renderTable({
    req(regression_results())
    as.data.frame(summary(regression_results())$coefficients)
  })
  
  # Predict values based on new observations and display results
  prediction_data <- reactive({
    req(input$predict_file)
    read_csv(input$predict_file$datapath)
  })
  
  predictions <- reactive({
    req(regression_results(), prediction_data())
    predict(regression_results(), newdata = prediction_data())
  })
  
  output$prediction_results <- renderTable({
    req(predictions())
    data.frame(Predicted = predictions())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

             