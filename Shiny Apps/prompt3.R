# Load required libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Dataset Explorer & Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values", "text/tab-separated-values", "text/plain", ".csv", ".tsv")),
      uiOutput("variable_selection"),
      actionButton("run_regression", "Run Regression")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tab",
        tabPanel("Data", tableOutput("data_table")),
        tabPanel("Visualization", plotlyOutput("data_plot")),
        tabPanel("Regression Analysis", verbatimTextOutput("regression_results"), uiOutput("prediction_inputs"), verbatimTextOutput("prediction_results"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    data(read_csv(input$file1$datapath))
  })
  
  output$data_table <- renderTable({
    req(data())
    data()
  })
  
  output$variable_selection <- renderUI({
    req(data())
    selectInput("variable", "Select Variable", choices = colnames(data()), multiple = TRUE)
  })
  
  output$data_plot <- renderPlotly({
    req(input$variable)
    
    plot_data <- data() %>% select(all_of(input$variable))
    plot_data_melted <- plot_data %>% mutate(id = row_number()) %>% tidyr::gather(key = "Variable", value = "Value", -id)
    
    plot <- ggplot(plot_data_melted, aes(x = Value, fill = Variable)) +
      geom_histogram(position = "identity", alpha = 0.5) +
      theme_minimal() +
      labs(title = "Histogram", x = "", y = "Frequency") +
      facet_wrap(~ Variable, scales = "free_x")
    
    ggplotly(plot)
  })
  
  regression <- reactiveVal(NULL)
  
  observeEvent(input$run_regression, {
    req(input$variable)
    formula_str <- paste0(input$variable[[1]], " ~ ", paste(input$variable[-1], collapse = " + "))
    regression(lm(as.formula(formula_str), data = data()))
  })
  
  output$regression_results <- renderPrint({
    req(regression())
    summary(regression())
  })
  
  output$prediction_inputs <- renderUI({
    req(regression())
    input_names <- names(coef(regression()))[-1]
    tagList(
      lapply(input_names, function(x) {
        tagList(
          tags$label(x),
          textInput(paste0("input_", x), label = NULL, value = 0)
        )
      }),
      actionButton("make_prediction", "Make Prediction")
    )
  })
  
  prediction <- eventReactive(input$make_prediction, {
    req(regression())
    input_values <- sapply(names(coef(regression()))[-1], function(x) as.numeric(input[[
      paste0("input_", x)]]))
    new_data <- as.data.frame(t(input_values))
    colnames(new_data) <- names(input_values)
    predict(regression(), newdata = new_data)
  })
  
  output$prediction_results <- renderPrint({
    req(prediction())
    cat("Prediction:", prediction())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

      