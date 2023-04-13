# Load required libraries
library(shiny)
library(DT)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("CSV File Upload, Visualization, and Regression Analysis"),
  
  # Create a sidebar layout
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")),
      tags$hr(),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Data",p("Upload a CSV file to view the data in a tabular format."), DTOutput("data_table")),
        tabPanel("Visualization",
                 p("Choose variables for the X and Y axes and select the type of plot to visualize your dataset."),
                 selectInput("x_axis", "X-axis:", choices = NULL),
                 selectInput("y_axis", "Y-axis:", choices = NULL),
                 selectInput("plot_type", "Plot Type:", 
                             choices = c("Scatterplot", "Histogram", "Boxplot", "Linechart", "Barchart")),
                 plotOutput("plot")
        ),
        tabPanel("Regression",
                 p("Select dependent and independent variables for regression analysis, and upload a CSV file to make predictions based on the generated model."),
                 selectInput("dependent_var", "Dependent Variable:", choices = NULL),
                 selectInput("independent_var", "Independent Variables:", choices = NULL, multiple = TRUE),
                 
                 actionButton("run_regression", "Run Regression"),
                 verbatimTextOutput("regression_output"),
                 fileInput("prediction_file", "Upload CSV File for Prediction", accept = c(".csv")),
                 actionButton("run_prediction", "Run Prediction"),
                 tableOutput("prediction_output")
        )
      ),
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to read uploaded CSV file
  dataset <- reactive({
    req(input$file_upload)
    read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
  })
  
  # Update choices for select inputs based on uploaded dataset
  observeEvent(dataset(), {
    updateSelectInput(session, "x_axis", choices = names(dataset()))
    updateSelectInput(session, "y_axis", choices = names(dataset()))
    updateSelectInput(session, "dependent_var", choices = names(dataset()))
    updateSelectInput(session, "independent_var", choices = names(dataset()))
  })
  
  # Render datatable
  output$data_table <- renderDT({
    datatable(dataset())
  })
  
  # Render plot
  output$plot <- renderPlot({
    req(input$x_axis, input$y_axis, input$plot_type)
    
    switch(input$plot_type,
           "Scatterplot" = print(ggplot(dataset(), aes_string(x = input$x_axis, y = input$y_axis)) + geom_point()),
           "Histogram" = print(ggplot(dataset(), aes_string(x = input$x_axis)) + geom_histogram(binwidth = 1)),
           "Boxplot" = print(ggplot(dataset(), aes_string(x = input$x_axis, y = input$y_axis)) + geom_boxplot()),
           "Linechart" = print(ggplot(dataset(), aes_string(x = input$x_axis, y = input$y_axis)) + geom_line()),
           "Barchart" = print(ggplot(dataset(), aes_string(x = input$x_axis)) + geom_bar())
    )
  })
  
  # Run regression
  run_regression <- eventReactive(input$run_regression, {
    req(input$dependent_var, input$independent_var)
    independent_vars <- paste(input$independent_var, collapse = " + ")
    formula <- as.formula(paste(input$dependent_var, "~", independent_vars))
    lm(formula, data = dataset())
  })
  
  # Display regression output
  output$regression_output <- renderPrint({
    req(run_regression())
    summary(run_regression())
  })
  
  # Read uploaded prediction file
  prediction_dataset <- reactive({
    req(input$prediction_file)
    read.csv(input$prediction_file$datapath, stringsAsFactors = FALSE)
  })
  
  # Run prediction
  run_prediction <- eventReactive(input$run_prediction, {
    req(run_regression(), prediction_dataset())
    predict(run_regression(), newdata = prediction_dataset(), se.fit = FALSE)
  })
  
  # Display prediction output
  output$prediction_output <- renderTable({
    req(run_prediction())
    cbind(prediction_dataset(), Prediction = run_prediction())
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)