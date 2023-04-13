library(shiny)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Data Analysis Shiny App"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabset1 == 'page1'",
                       fileInput("file", "Upload CSV File"),
                       helpText("Your CSV file must include a header.")
      ),
      conditionalPanel(condition = "input.tabset1 == 'page2'",
                       selectInput("xvar", "X-axis Variable:", NULL),
                       selectInput("yvar", "Y-axis Variable:", NULL),
                       selectInput("plottype", "Plot Type:", c("scatterplot", "histogram", "linechart", "box-plot", "barchart"))
      ),
      conditionalPanel(condition = "input.tabset1 == 'page3'",
                       selectInput("dependent", "Dependent Variable:", NULL),
                       selectInput("independent", "Independent Variable:", NULL),
                       actionButton("run_reg", "Run Regression"),
                       fileInput("predict_file", "Upload CSV File to Predict"),
                       actionButton("predict_values", "Predict Values")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabset1",
                  tabPanel("Page 1", 
                           DTOutput("data_table"),
                           helpText("Page 1: Use the file input to upload a CSV file. The data will be displayed in a table format.")
                  ),
                  tabPanel("Page 2", 
                           plotlyOutput("plot"),
                           helpText("Page 2: Select the variables to visualize and the plot type. Supported visualizations: Scatterplot, histogram, linechart, box-plot, barchart.")
                  ),
                  tabPanel("Page 3", 
                           verbatimTextOutput("regression_results"),
                           DTOutput("prediction_table"),
                           helpText("Page 3: Select the dependent and independent variables for regression analysis. Upload a CSV file to predict new values based on the uploaded observations. The final result will be displayed in a table.")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  output$data_table <- renderDT({
    data()
  }, editable = TRUE)
  
  observe({
    updateSelectInput(session, "xvar", choices = names(data()))
    updateSelectInput(session, "yvar", choices = names(data()))
    updateSelectInput(session, "dependent", choices = names(data()))
    updateSelectInput(session, "independent", choices = names(data()))
  })
  
  output$plot <- renderPlotly({
    req(input$xvar, input$yvar, input$plottype)
    plot_data <- data()
    
    if (input$plottype == "scatterplot") {
      p <- ggplot(plot_data, aes_string(x = input$xvar, y = input$yvar)) + geom_point() + theme_minimal()
    } else if (input$plottype == "histogram") {
      p <- ggplot(plot_data, aes_string(x = input$xvar)) + geom_histogram() + theme_minimal()
    } else if (input$plottype == "linechart") {
      p <- ggplot(plot_data, aes_string(x = input$xvar, y = input$yvar)) + geom_line() + theme_minimal()
    } else if (input$plottype == "box-plot") {
      p <- ggplot(plot_data, aes_string(x = input$xvar, y = input$yvar)) + geom_boxplot() + theme_minimal()
    } else if (input$plottype == "barchart") {
      p <- ggplot(plot_data, aes_string(x = input$xvar, y = input$yvar)) + geom_bar(stat = "identity") + theme_minimal()
    }
    
    ggplotly(p)
  })
  
  reg_model <- reactive({
    req(input$run_reg)
    isolate({
      lm(as.formula(paste(input$dependent, "~", input$independent)), data())
    })
  })
  
  output$regression_results <- renderPrint({
    req(input$run_reg)
    summary(reg_model())
  })
  
  prediction_data <- reactive({
    req(input$predict_file)
    read_csv(input$predict_file$datapath)
  })
  
  observeEvent(input$predict_values, {
    req(reg_model(), prediction_data())
    new_data <- cbind(prediction_data(), Prediction = predict(reg_model(), prediction_data()))
    output$prediction_table <- renderDT({
      new_data
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
