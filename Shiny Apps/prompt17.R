library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Define UI for app
ui <- fluidPage(
  
  # App title
  titlePanel("Regression Analysis App"),
  
  # Sidebar with file input and variable selection
  sidebarLayout(
    sidebarPanel(
      
      # File input
      fileInput("file", "Choose CSV File",
                accept = c(".csv")),
      
      # Variable selection for visualizations and regression analysis
      selectInput("xvar", "Choose X variable for visualization:",
                  choices = NULL, selected = NULL),
      
      selectInput("yvar", "Choose Y variable for visualization:",
                  choices = NULL, selected = NULL),
      
      selectInput("depvar", "Choose dependent variable for regression analysis:",
                  choices = NULL, selected = NULL),
      
      selectInput("indvar", "Choose independent variable for regression analysis:",
                  choices = NULL, selected = NULL),
      
      # Input fields for regression analysis
      numericInput("xval", "Input X value:", value = NULL),
      numericInput("yval", "Input Y value:", value = NULL),
      numericInput("depval", "Input dependent variable value:", value = NULL),
      numericInput("indval", "Input independent variable value:", value = NULL),
      br(),
      actionButton("regbutton", "Run Regression Analysis"),
      br(),
      textOutput("regoutput")
    ),
    
    # Main panel with visualizations and table
    mainPanel(
      
      # Data table
      dataTableOutput("table"),
      
      # Scatterplot
      plotOutput("scatterplot"),
      
      # Histogram
      plotOutput("histogram"),
      
      # Line chart
      plotOutput("linechart"),
      
      # Box plot
      plotOutput("boxplot"),
      
      # Bar chart
      plotOutput("barchart")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Import CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update variable choices for visualizations and regression analysis based on imported data
  observe({
    updateSelectInput(session, "xvar", choices = colnames(data()))
    updateSelectInput(session, "yvar", choices = colnames(data()))
    updateSelectInput(session, "depvar", choices = colnames(data()))
    updateSelectInput(session, "indvar", choices = colnames(data()))
  })
  
  # Create data table
  output$table <- renderDataTable({
    data()
  })
  
  # Create scatterplot based on selected variables
  output$scatterplot <- renderPlot({
    req(input$xvar, input$yvar)
    ggplot(data(), aes(x = data()[,input$xvar], y = data()[,input$yvar])) +
      geom_point()
  })
  
  # Create histogram based on selected variable
  output$histogram <- renderPlot({
    req(input$xvar)
    ggplot(data(), aes(x = data()[,input$xvar])) +
      geom_histogram()
  })
  
  # Create line chart based on selected variables
  output$linechart <- renderPlot({
    req(input$xvar, input$yvar)
    ggplot(data(), aes(x = data()[,input$xvar], y = data()[,input$yvar])) +
      geom_line()
  })
  
  # Create box plot based on selected variable
  output$boxplot <- renderPlot({
    req(input$xvar)
    ggplot(data(), aes(x = factor(1), y = data()[,input$xvar])) +
    geom_boxplot()
  })

# Create bar chart based on selected variable
output$barchart <- renderPlot({
  req(input$xvar)
  ggplot(data(), aes(x = data()[,input$xvar])) +
    geom_bar()
})

# Run linear regression based on selected variables and display results
reg <- reactive({
  req(input$depvar, input$indvar)
  lm(data()[,input$depvar] ~ data()[,input$indvar], data = data())
})

output$regoutput <- renderText({
  if(input$regbutton > 0){
    summary(reg())$coefficients
  }
})

# Calculate predicted value based on inputted values and display result
pred <- reactive({
  req(input$xval, input$yval, input$depval, input$indval)
  data.frame(
    xval = input$xval,
    yval = input$yval,
    depval = input$depval,
    indval = input$indval,
    predval = predict(reg(), data.frame(data()[,input$indvar] = input$indval)))
})

}

# Run the app
shinyApp(ui, server)

    