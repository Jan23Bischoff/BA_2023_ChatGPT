# Load required packages
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Shiny app UI
ui <- fluidPage(
  titlePanel("CSV Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxGroupInput("independent_variables",
                         "Select Independent Variables:",
                         choices = c("room_type",
                                     "person_capacity",
                                     "host_is_superhost",
                                     "cleanliness_rating",
                                     "guest_satisfaction_overall",
                                     "dist")),
      actionButton("run_regression", "Run Regression")
    ),
    mainPanel(
      verbatimTextOutput("regression_results"),
      uiOutput("input_boxes"),
      verbatimTextOutput("prediction_results")
    )
  )
)

# Shiny app server
server <- function(input, output) {
  dataset <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
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
