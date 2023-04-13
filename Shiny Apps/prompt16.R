
# Load required packages
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

# Shiny app UI
ui <- fluidPage(
  titlePanel("Dataset Exploration and Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("var_selection"),
      actionButton("plot_data", "Plot Data"),
      checkboxGroupInput("independent_variables",
                         "Select Independent Variables for Regression:",
                         choices = NULL),
      selectInput("dependent_variable",
                  "Select Dependent Variable for Regression:",
                  choices = NULL),
      actionButton("run_regression", "Run Regression")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DT::dataTableOutput("data_table")),
        tabPanel("Data Visualization", plotOutput("data_plot")),
        tabPanel("Regression Results", verbatimTextOutput("regression_results")),
        tabPanel("Predict New Value",
                 uiOutput("input_boxes"),
                 verbatimTextOutput("prediction_results")),
        tabPanel("How to Use", h4("How to use this Decision Support System (DSS):"),
                 ol(
                   li("Upload a CSV file."),
                   li("View and format individual values in the data table."),
                   li("Explore variables with data visualizations by manually choosing the variables."),
                   li("Select variables to run a regression analysis."),
                   li("View the regression results."),
                   li("Input new values for variables included in the regression analysis."),
                   li("View the final result based on the new values.")
                 ))
      )
    )
  )
)

# Shiny app server
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  observeEvent(dataset(), {
    updateCheckboxGroupInput(session, "independent_variables",
                             "Select Independent Variables for Regression:",
                             choices = colnames(dataset()))
    updateSelectInput(session, "dependent_variable",
                      "Select Dependent Variable for Regression:",
                      choices = colnames(dataset()))
  })
  
  output$data_table <- DT::renderDataTable({
    req(dataset())
    dataset()
  }, editable = TRUE)
  
  output$var_selection <- renderUI({
    req(dataset())
    selectInput("variable", "Select Variable to Plot:",
                choices = colnames(dataset()))
  })
  
  output$data_plot <- renderPlot({
    req(input$variable, dataset())
    selected_var <- dataset()[[input$variable]]
    
    plot_types <- c("Scatterplot", "Histogram", "Linechart", "Boxplot", "Barchart")
    plot_type <- selectInput("plot_type", "Select Plot Type:", choices = plot_types)
    
    switch(input$plot_type,
           "Scatterplot" = {
             ggplot(dataset(), aes_string(x = input$variable, y = input$dependent_variable)) +
               geom_point
             # Install required packages if not already installed
             if (!require("shiny")) install.packages("shiny")
             if (!require("readr")) install.packages("readr")
             if (!require("dplyr")) install.packages("dplyr")
             if (!require("ggplot2")) install.packages("ggplot2")
             if (!require("DT")) install.packages("DT")
             
             # Load required packages
             library(shiny)
             library(readr)
             library(dplyr)
             library(ggplot2)
             library(DT)
             
             # Shiny app UI
             ui <- fluidPage(
               titlePanel("Dataset Exploration and Regression Analysis"),
               sidebarLayout(
                 sidebarPanel(
                   fileInput("file", "Upload CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   uiOutput("var_selection"),
                   selectInput("plot_type", "Select Plot Type:", 
                               choices = c("Scatterplot", "Histogram", "Linechart", "Boxplot", "Barchart")),
                   actionButton("plot_data", "Plot Data"),
                   checkboxGroupInput("independent_variables",
                                      "Select Independent Variables for Regression:",
                                      choices = NULL),
                   selectInput("dependent_variable",
                               "Select Dependent Variable for Regression:",
                               choices = NULL),
                   actionButton("run_regression", "Run Regression")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Data Table", DT::dataTableOutput("data_table")),
                     tabPanel("Data Visualization", plotOutput("data_plot")),
                     tabPanel("Regression Results", verbatimTextOutput("regression_results")),
                     tabPanel("Predict New Value",
                              uiOutput("input_boxes"),
                              verbatimTextOutput("prediction_results")),
                     tabPanel("How to Use", h4("How to use this Decision Support System (DSS):"),
                              ol(
                                li("Upload a CSV file."),
                                li("View and format individual values in the data table."),
                                li("Explore variables with data visualizations by manually choosing the variables."),
                                li("Select variables to run a regression analysis."),
                                li("View the regression results."),
                                li("Input new values for variables included in the regression analysis."),
                                li("View the final result based on the new values.")
                              ))
                   )
                 )
               )
             )
             
             # Shiny app server
             server <- function(input, output, session) {
               dataset <- reactive({
                 req(input$file)
                 read_csv(input$file$datapath)
               })
               
               observeEvent(dataset(), {
                 updateCheckboxGroupInput(session, "independent_variables",
                                          "Select Independent Variables for Regression:",
                                          choices = colnames(dataset()))
                 updateSelectInput(session, "dependent_variable",
                                   "Select Dependent Variable for Regression:",
                                   choices = colnames(dataset()))
               })
               
               output$data_table <- DT::renderDataTable({
                 req(dataset())
                 dataset()
               }, editable = TRUE)
               
               output$var_selection <- renderUI({
                 req(dataset())
                 selectInput("variable", "Select Variable to Plot:",
                             choices = colnames(dataset()))
               })
               
               output$data_plot <- renderPlot({
                 req(input$variable, dataset())
                 selected_var <- dataset()[[input$variable]]
                 
                 switch(input$plot_type,
                        "Scatterplot" = {
                          ggplot(dataset(), aes_string(x = input$variable, y = input$dependent_variable)) +
                            geom_point()
                        },
                        "Histogram" = {
                          ggplot(dataset(), aes_string(x = input$variable)) +
                            geom_histogram()
                        },
                        "Linechart" = {
                          ggplot(dataset(), aes_string(x = input$variable, y = input$dependent_variable)) +
                            geom_line()
                        },
                        "Boxplot" = {
                          ggplot(dataset(), aes_string(x = input$variable, y = input$dependent_variable)) +
                            geom_boxplot()
                        },
                        "Barchart" = {
                          ggplot(dataset(), aes_string(x = input$variable, y = input$dependent_variable)) +
                            geom_bar(stat = "identity")
                        }
                 )
               })
               
               regression_model <- eventReactive(input$run_regression, {
                 req(input$independent_variables)
                 formula_text <- paste("realSum ~", paste(input$independent_variables, collapse = " + "))
                 formula_obj <- as.formula(formula_text)
                 lm(formula_obj, data = dataset())
               })
               
               output$regression_results <- renderPrint({
                 req(regression_model())
                 summary(regression_model())
               })
               
               output$input_boxes <- renderUI({
                 req(input$independent_variables)
                 lapply(input$independent_variables, function(var) {
                   numericInput(paste0("input_", var), label = paste("Enter value for", var), value = 0)
                 })
               })
               
               observeEvent(input$independent_variables, {
                 lapply(input$independent_variables, function(var) {
                   updateNumericInput(session, paste0("input_", var), label = paste("Enter value for", var), value = 0)
                 })
               })
               
               output$prediction_results <- renderPrint({
                 req(regression_model())
                 new_data <- data.frame(lapply(input$independent_variables, function(var) input[[paste0("input_", var)]]))
                 prediction <- predict(regression_model(), newdata = new_data)
                 cat("Predicted value of realSum:", prediction)
               })
             }
             
             # Run the app
             shinyApp(ui = ui, server = server)