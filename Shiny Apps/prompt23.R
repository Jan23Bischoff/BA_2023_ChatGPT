# Load required packages
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  navbarPage("Shiny App",
             tabPanel("Upload Data",
                      fileInput("file_upload", "Upload CSV file", accept = c(".csv")),
                      tableOutput("data_table")
             ),
             tabPanel("Visualize Data",
                      selectInput("x_var", "X Variable", ""),
                      selectInput("y_var", "Y Variable", ""),
                      selectInput("plot_type", "Plot Type", c("Boxplot", "Histogram", "Line Chart", "Bar Chart", "Scatterplot")),
                      plotOutput("plot")
             ),
             tabPanel("Regression Analysis",
                      selectInput("dep_var", "Dependent Variable", ""),
                      selectInput("ind_vars", "Independent Variables", "", multiple = TRUE),
                      actionButton("run_regression", "Run Regression"),
                      tableOutput("regression_summary"),
                      fileInput("prediction_upload", "Upload CSV file for prediction", accept = c(".csv")),
                      actionButton("run_prediction", "Run Prediction"),
                      tableOutput("predictions")
             )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file_upload, {
    tmp_data <- read.csv(input$file_upload$datapath)
    data(tmp_data)
    updateSelectInput(session, "x_var", choices = names(tmp_data))
    updateSelectInput(session, "y_var", choices = names(tmp_data))
    updateSelectInput(session, "dep_var", choices = names(tmp_data))
    updateSelectInput(session, "ind_vars", choices = names(tmp_data))
  })
  
  output$data_table <- renderTable({
    req(data())
  })
  
  observeEvent(c(input$x_var, input$y_var, input$plot_type), {
    req(data())
    if (input$plot_type == "Histogram") {
      p <- ggplot(data(), aes_string(x = input$x_var)) + geom_histogram()
    } else if (input$plot_type == "Boxplot") {
      p <- ggplot(data(), aes_string(x = input$x_var, y = input$y_var)) + geom_boxplot()
    } # Add other plot types as needed
    output$plot <- renderPlot(p)
  })
  
  observeEvent(input$run_regression, {
    req(data())
    fit <- lm(as.formula(paste(input$dep_var, "~", paste(input$ind_vars, collapse = " + "))), data = data())
    output$regression_summary <- renderTable(summary(fit)$coefficients)
  })
  
  observeEvent(input$run_prediction, {
    req(data())
    fit <- lm(as.formula(paste(input$dep_var, "~", paste(input$ind_vars, collapse = " + "))), data = data())
    new_data <- read.csv(input$prediction_upload$datapath)
    preds <- predict(fit, newdata = new_data)
    output$predictions <- renderTable(data.frame(Predictions = round(preds, 2)))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
``
