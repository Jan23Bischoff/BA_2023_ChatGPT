# Load required packages
library(shiny)
library(readr)
library(DT)
library(plotly)
library(ggplot2)
library(tidyverse)

# Define the UI
ui <- fluidPage(
  titlePanel("Shiny App for Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      helpText("Note: The dataset should be in CSV format."),
      actionButton("submit", "Submit"),
      tags$hr(),
      helpText("Select the page you want to use:"),
      navlistPanel(
        id = "tabs",
        tabPanel("Page 1", DT::dataTableOutput("datatable")),
        tabPanel("Page 2", plotlyOutput("plot"),
                 selectInput("x", "Select X-axis variable:", ""),
                 selectInput("y", "Select Y-axis variable:", ""),
                 selectInput("plot_type", "Select plot type:",
                             choices = c("Scatterplot" = "scatter",
                                         "Histogram" = "hist",
                                         "Linechart" = "line",
                                         "Box-plot" = "box",
                                         "Barchart" = "bar"))),
        tabPanel("Page 3", verbatimTextOutput("regression_results"),
                 fileInput("pred_file", "Choose CSV File for Prediction",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 actionButton("predict", "Predict"),
                 DT::dataTableOutput("prediction_results"))
      )
    ),
    mainPanel()
  )
)

# Define the server
server <- function(input, output) {
  df <- reactiveVal()
  
  observeEvent(input$submit, {
    req(input$file)
    df(read_csv(input$file$datapath))
  })
  
  output$datatable <- DT::renderDataTable({
    req(df())
    DT::datatable(df(), extensions = "Buttons", options = list(dom = "Bfrtip",
                                                               buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                               paging = TRUE, searching = TRUE),
                  rownames = FALSE)
  })
  
  observeEvent(df(), {
    updateSelectInput(session, "x", choices = colnames(df()))
    updateSelectInput(session, "y", choices = colnames(df()))
  })
  
  output$plot <- renderPlotly({
    req(input$x, input$y, df())
    plot_data <- df()
    plot_type <- input$plot_type
    
    p <- ggplot(plot_data, aes_string(x = input$x, y = input$y))
    
    if (plot_type == "scatter") {
      p <- p + geom_point()
    } else if (plot_type == "hist") {
      p <- p + geom_histogram(binwidth = 30)
    } else if (plot_type == "line") {
      p <- p + geom_line()
    } else if (plot_type == "box") {
      p <- p + geom_boxplot()
    } else if (plot_type == "bar") {
      p <- p + geom_bar(stat = "identity")
    }
    
    p <- p + theme_minimal()
    
    ggplotly(p)
  })
  
  output$regression_results <- renderPrint({
    req(input$x, input$y, df())
    model <- lm(as.formula(paste(input$y, "~", input$x)), data = df())
    summary(model)
  })
  
  
  pred_df <- reactiveVal()
  
  observeEvent(input$predict, {
    req(input$pred_file)
    pred_df(read_csv(input$pred_file$datapath))
  })
  
  output$prediction_results <- DT::renderDataTable({
    req(pred_df(), input$x, input$y)
    model <- lm(as.formula(paste(input$y, "~", input$x)), data = df())
    predictions <- predict(model, pred_df())
    results <- cbind(pred_df(), Prediction = predictions)
    DT::datatable(results, rownames = FALSE)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
