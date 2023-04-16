library(shiny)
library(DT)
library(ggplot2)
library(broom)
library(esquisse)

ui <- fluidPage(
  titlePanel("CSV Data Viewer, Editor, Visualizer, and Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("editable", "Enable table editing", FALSE),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Editor",
                 helpText("On this page, you can:",
                          tags$ul(tags$li("Upload a csv Dataset"),
                                  tags$li("Edit the individual values in the table")),
                    
                          "To Upload the csv file, click the Button on the left side and then choose the file you want to upload",
                          "To edit values, you have to enable editing by clicking the box on the left side. Afterwards you can edit values by double clicking the cell."),
                 
                 DTOutput("dataTable")),
        tabPanel("Data Visualizer",
                 helpText("On this page, you can:",
                          tags$ul(tags$li("Visualize your Dataset"),
                                  tags$li("Download visualizations")),
                          "You can select and try out multiple graphs by drag and drop the different columns.",
                          "You can download the visualizations you like by clicking the downloa button on the top right corner."),
                 esquisse_ui(
                   id = "esquisse", 
                   header = FALSE)),
        tabPanel("Regression Analysis",
                 helpText("On this page, you can:",
                          tags$ul(tags$li("Select the dependent and independent variables for regression analysis"),
                                  tags$li("Run a regression analysis and view the results"),
                                  tags$li("Upload a new CSV file to predict values based on the regression model"),
                                  tags$li("View the predictions along with the original values")),
                          "To run a regression analysis, select the dependent variable and one or more independent variables, then click 'Run Regression Analysis'.",
                          "The p-values in the regression results can help you determine the statistical significance of each independent variable."),
                 fluidRow(
                   column(4, selectInput("dependentVar", "Dependent Variable", choices = NULL)),
                   column(4, checkboxGroupInput("independentVars", "Independent Variables", choices = NULL))
                 ),
                 actionButton("runRegression", "Run Regression Analysis"),
                 tableOutput("regressionResults"),
                 fileInput("predictFile", "Upload CSV File for Prediction",
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 DTOutput("predictionResults")
        )
      ), width = 10
    )
  )
)


server <- function(input, output, session) {
  v <- reactiveValues(data=data.frame(matrix(ncol = 0, nrow = 0)))
  
  observeEvent(input$fileUpload, {
    v$data <- read.csv(input$fileUpload$datapath)
    v$name <- "data"
    updateSelectInput(session, "dependentVar", choices = names(v$data))
    updateCheckboxGroupInput(session, "independentVars", choices = names(v$data))
  })
  
  output$dataTable <- renderDT({
    datatable(v$data, editable = input$editable, rownames = FALSE)
  })
  
  proxy <- dataTableProxy("dataTable")
  
  observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    i <- info$row
    j <- info$col + 1
    k <- info$value
    
    isolate({
      v$data[i, j] <- DT::coerceValue(k, v$data[i, j])
      replaceData(proxy, v$data, resetPaging = FALSE, rownames = FALSE)
    })
  })
  
  
  results <- esquisse_server(
    id = "esquisse",
    data_rv = v
  )
  
  # Regression Analysis
  output$regressionResults <- renderTable({
    req(input$runRegression)
    isolate({
      formula_str <- paste(input$dependentVar, "~", paste(input$independentVars, collapse = " + "))
      model <- lm(as.formula(formula_str), data = v$data)
      tidy_results <- tidy(model)
      tidy_results
    })
  })
  
  observeEvent(input$predictFile, {
    v$predictData <- read.csv(input$predictFile$datapath)
  })
  
  output$predictionResults <- renderDT({
    req(v$predictData, input$runRegression)
    isolate({
      formula_str <- paste(input$dependentVar, "~", paste(input$independentVars, collapse = " + "))
      model <- lm(as.formula(formula_str), data = v$data)
      predictions <- predict(model, newdata = v$predictData)
      prediction_df <- data.frame(v$predictData, Predicted_Value = predictions)
      prediction_df
    })
  })
}

shinyApp(ui = ui, server = server)

