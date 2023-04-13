library(shiny)
library(readr)
library(DT)
library(ggplot2)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Upload CSV",
             fileInput("file", "Choose CSV File"),
             tableOutput("table")
    ),
    tabPanel("Visualize",
             selectInput("x_axis", "X-axis:", NULL),
             selectInput("y_axis", "Y-axis:", NULL),
             selectInput("plot_type", "Plot Type:", c("scatter", "line")),
             plotOutput("plot")
    ),
    tabPanel("Regression",
             uiOutput("input_columns"),
             actionButton("run_regression", "Run Regression"),
             verbatimTextOutput("regression_result")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    data(read_csv(input$file$datapath))
    updateSelectInput(session, "x_axis", choices = colnames(data()))
    updateSelectInput(session, "y_axis", choices = colnames(data()))
  })
  
  output$table <- renderTable({ data() })
  
  output$plot <- renderPlot({
    if (is.null(data())) return(NULL)
    
    if (input$plot_type == "scatter") {
      ggplot(data(), aes_string(x = input$x_axis, y = input$y_axis)) + geom_point()
    } else {
      ggplot(data(), aes_string(x = input$x_axis, y = input$y_axis)) + geom_line()
    }
  })
  
  output$input_columns <- renderUI({
    if (is.null(data())) return(NULL)
    
    checkboxGroupInput("selected_columns", "Select Columns for Regression:", choices = colnames(data()))
  })
  
  output$regression_result <- renderPrint({
    req(input$run_regression)
    
    lm_formula <- as.formula(paste("y ~", paste(input$selected_columns, collapse = " + ")))
    model <- lm(lm_formula, data())
    summary(model)
  })
}

shinyApp(ui, server)
