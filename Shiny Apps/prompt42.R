library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

ui <- dashboardPage(
  dashboardHeader(title = "Data Science Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Page 1: Data Upload and Table", tabName = "page1", icon = icon("table")),
      menuItem("Page 2: Data Visualization", tabName = "page2", icon = icon("chart-bar")),
      menuItem("Page 3: Regression Analysis", tabName = "page3", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Page 1: Data Upload and Table
      tabItem(tabName = "page1",
              h3("Page 1: Data Upload and Table"),
              p("On this page, you can upload a CSV file to view and edit the dataset. Click the 'Upload CSV File' button to choose a file, and the data will be displayed in a table below. To edit a cell, click on it, enter the new value, and press Enter. Click 'Save changes' to save your edits."),
              fileInput("file1", "Upload CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              DTOutput("dataTable"),
              br(),
              actionButton("save_changes", "Save changes")
      ),
      # Page 2: Data Visualization
      tabItem(tabName = "page2",
              h3("Page 2: Data Visualization"),
              p("On this page, you can visualize your dataset using different plot types. Choose a plot type from the dropdown menu and select the appropriate variables for each axis. For scatterplots, you can also select a variable for coloring the points."),
              sidebarLayout(
                sidebarPanel(
                  selectInput("plot_type", "Choose a plot type",
                              choices = c("Scatterplot", "Histogram", "Linechart", "Box-plot", "Barchart")),
                  uiOutput("x_var"),
                  uiOutput("y_var"),
                  uiOutput("color_var")
                ),
                mainPanel(
                  plotlyOutput("plot")
                )
              )
      ),
      # Page 3: Regression Analysis
      tabItem(tabName = "page3",
              h3("Page 3: Regression Analysis"),
              p("On this page, you can perform regression analysis on your dataset. Select the dependent variable and one or more independent variables, then click 'Run Regression' to view the results. You can also upload a new CSV file to make predictions using the regression model."),
              sidebarLayout(
                sidebarPanel(
                  selectInput("dependent_var", "Dependent Variable", choices = NULL),
                  checkboxGroupInput("independent_vars", "Independent Variables", choices = NULL),
                  actionButton("run_regression", "Run Regression"),
                  fileInput("file2", "Upload CSV File for Prediction", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                ),
                mainPanel(
                  verbatimTextOutput("regression_summary"),
                  DTOutput("prediction_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    read.csv(inFile$datapath)
  })
  
  output$dataTable <- renderDT({
    dataset()
  }, editable = TRUE)
  
  observeEvent(input$save_changes, {
    req(input$dataTable_cell_edit)
    cell <- input$dataTable_cell_edit
    i <- cell$row
    j <- cell$col
    v <- cell$value
    
    if (is.logical(dataset()[i, j])) {
      v <- as.logical(v)
    } else if (is.numeric(dataset()[i, j])) {
      v <- as.numeric(v)
    } else if (is.factor(dataset()[i, j])) {
      v <- as.factor(v)
    } else if (is.character(dataset()[i, j])) {
      v <- as.character(v)
    }
    
    dataset()[i, j] <- v
    dataset() <<- dataset()
  })
  
  observeEvent(dataset(), {
    updateSelectInput(session, "dependent_var", choices = colnames(dataset()))
    updateCheckboxGroupInput(session, "independent_vars", choices = colnames(dataset()))
  })
  
  output$x_var <- renderUI({
    req(dataset())
    switch(input$plot_type,
           "Scatterplot" = selectInput("x_var", "X Variable", choices = colnames(dataset())),
           "Histogram" = selectInput("x_var", "X Variable", choices = colnames(dataset())),
           "Linechart" = selectInput("x_var", "X Variable", choices = colnames(dataset())),
           "Box-plot" = selectInput("x_var", "X Variable", choices = colnames(dataset())),
           "Barchart" = selectInput("x_var", "X Variable", choices = colnames(dataset()))
    )
  })
  
  output$y_var <- renderUI({
    req(dataset())
    if (input$plot_type %in% c("Scatterplot", "Linechart", "Barchart")) {
      selectInput("y_var", "Y Variable", choices = colnames(dataset()))
    } else {
      NULL
    }
  })
  
  output$color_var <- renderUI({
    req(dataset())
    if (input$plot_type == "Scatterplot") {
      selectInput("color_var", "Color Variable", choices = c("None" = "", colnames(dataset())))
    } else {
      NULL
    }
  })
  
  output$plot <- renderPlotly({
    req(input$x_var, dataset())
    
    p <- switch(input$plot_type,
                "Scatterplot" = ggplot(dataset(), aes_string(x = input$x_var, y = input$y_var, color = ifelse(input$color_var == "", NA, input$color_var))) + geom_point(),
                "Histogram" = ggplot(dataset(), aes_string(x = input$x_var)) + geom_histogram(),
                "Linechart" = ggplot(dataset(), aes_string(x = input$x_var, y = input$y_var)) + geom_line(),
                "Box-plot" = ggplot(dataset(), aes_string(x = input$x_var, y = input$y_var)) + geom_boxplot(),
                "Barchart" = ggplot(dataset(), aes_string(x = input$x_var, y = input$y_var)) + geom_bar(stat = "identity")
    )
    
    ggplotly(p)
  })
  
  lm_model <- reactiveVal(NULL)
  
  observeEvent(input$run_regression, {
    req(dataset(), input$dependent_var, input$independent_vars)
    lm_formula <- as.formula(paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + ")))
    lm_model(lm(lm_formula, data = dataset()))
  })
  
  output$regression_summary <- renderPrint({
    req(lm_model())
    summary(lm_model())
  })
  
  prediction_data <- reactive({
    inFile <- input$file2
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    read.csv(inFile$datapath)
  })
  
  output$prediction_table <- renderDT({
    req(lm_model(), prediction_data())
    predict_df <- data.frame(Predicted = predict(lm_model(), newdata = prediction_data()))
    cbind(prediction_data(), predict_df)
  }, options = list(scrollX = TRUE))
}

shinyApp(ui, server)