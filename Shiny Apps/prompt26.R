# Load required libraries
library(shiny)
library(DT)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("CSV File Upload and Visual Analysis"),
  
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
        tabPanel("Data", DTOutput("data_table")),
        tabPanel("Visual Analysis",
                 fluidRow(
                   column(4,
                          selectInput("x_var", "X-axis Variable", choices = NULL),
                          selectInput("y_var", "Y-axis Variable", choices = NULL, multiple = FALSE),
                          selectInput("plot_type", "Plot Type", choices = c("Scatterplot", "Boxplot", "Histogram", "Linechart", "Barchart"))
                   ),
                   column(8,
                          plotOutput("visualization")
                   )
                 )
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
  
  # Render datatable
  output$data_table <- renderDT({
    datatable(dataset())
  })
  
  # Update variable options for x and y axes
  observe({
    updateSelectInput(session, "x_var", choices = colnames(dataset()))
    updateSelectInput(session, "y_var", choices = colnames(dataset()))
  })
  
  # Render visualization
  output$visualization <- renderPlot({
    req(input$x_var)
    plot_type <- input$plot_type
    x_var <- dataset()[, input$x_var]
    
    # Create plot based on selected plot type
    plot <- NULL
    if (plot_type == "Scatterplot") {
      req(input$y_var)
      y_var <- dataset()[, input$y_var]
      plot <- ggplot(dataset(), aes(x = x_var, y = y_var)) + geom_point()
    } else if (plot_type == "Boxplot") {
      req(input$y_var)
      y_var <- dataset()[, input$y_var]
      plot <- ggplot(dataset(), aes(x = x_var, y = y_var)) + geom_boxplot()
    } else if (plot_type == "Histogram") {
      plot <- ggplot(dataset(), aes(x = x_var)) + geom_histogram()
    } else if (plot_type == "Linechart") {
      req(input$y_var)
      y_var <- dataset()[, input$y_var]
      plot <- ggplot(dataset(), aes(x = x_var, y = y_var)) + geom_line()
    } else if (plot_type == "Barchart") {
      plot <- ggplot(dataset(), aes(x = x_var)) + geom_bar()
    }
    
    print(plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)