# Load required packages
library(shiny)
library(readr)
library(dplyr)

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
      verbatimTextOutput("regression_results")
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
}

# Run the app
shinyApp(ui = ui, server = server)
