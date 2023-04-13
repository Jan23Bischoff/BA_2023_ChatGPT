# Load required packages
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  titlePanel("Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      pickerInput("plot_type", "Choose plot type:", choices = c("Scatterplot", "Histogram", "Line Chart", "Box Plot", "Bar Chart"), multiple = FALSE),
      uiOutput("variables"),
      uiOutput("regression_variables")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", dataTableOutput("table")),
        tabPanel("Visualizations", plotlyOutput("plot")),
        tabPanel("Regression", verbatimTextOutput("regression_results"), uiOutput("input_panel"), textOutput("predicted_value"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  output$table <- renderDataTable({
    data()
  })
  
  output$variables <- renderUI({
    req(data())
    pickerInput("vars", "Choose variables:", choices = colnames(data()), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10))
  })
  
  output$plot <- renderPlotly({
    req(input$vars, input$plot_type)
    plt <- data() %>%
      select(input$vars) %>%
      ggplot() +
      theme_bw()
    
    switch(input$plot_type,
           "Scatterplot" = {plt <- plt + geom_point(aes_string(x = input$vars[1], y = input$vars[2]))},
           "Histogram" = {plt <- plt + geom_histogram(aes_string(x = input$vars[1]), bins = 30)},
           "Line Chart" = {plt <- plt + geom_line(aes_string(x = input$vars[1], y = input$vars[2]))},
           "Box Plot" = {plt <- plt + geom_boxplot(aes_string(x = input$vars[1], y = input$vars[2]))},
           "Bar Chart" = {plt <- plt + geom_bar(aes_string(x = input$vars[1], y = input$vars[2]), stat = "identity")}
    )
    ggplotly(plt)
  })
  
  output$regression_variables <- renderUI({
    req(data())
    tagList(
      pickerInput("dependent_var", "Choose dependent variable:", choices = colnames(data()), multiple = FALSE),
      pickerInput("independent_vars", "Choose independent variables:", choices = colnames(data()), multiple = TRUE, options = list(`actions-box` = TRUE, size = 10))
    )
  })
  
  regression_model <- reactive({
    req(input$dependent_var, input$independent_vars)
    formula <- paste(input$dependent_var, "~", paste(input$independent_vars, collapse = "+"))
    lm(formula, data = data())
  })
  
  output$regression_results <- renderPrint({
    summary(regression_model())
  })
  
  output$input_panel <- renderUI({
    req(regression_model())
    input_vars <- input$independent_vars
    input_panel <- tagList()
    for (var in input_vars) {
      input_panel <- tagList(input_panel, numericInput(paste0("input_", var), paste("Enter value for", var, ":"), value = 0))
    }
    input_panel
  })
  
  new_data <- reactive({
    req(regression_model())
    input_vars <- input$independent_vars
    new_data_list <- list()
    for (var in input_vars) {
      new_data_list[[var]] <- input[[paste0("input_", var)]]
    }
    as.data.frame(list(new_data_list))
  })
  
  output$predicted_value <- renderText({
    req(new_data(), regression_model())
    predicted_val <- predict(regression_model(), new_data())
    paste("Predicted value for the dependent variable:", round(predicted_val, 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

                                                                                                                                     