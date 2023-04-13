library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("CSV Data Viewer, Editor and Visualizer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("editable", "Enable table editing", FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Editor", DTOutput("dataTable")),
        tabPanel("Data Visualizer",
                 fluidRow(
                   column(4, selectInput("chartType", "Select Chart Type",
                                         choices = c("Scatterplot", "Histogram", "Line Chart", "Box Plot", "Bar Chart"))),
                   column(4, uiOutput("xAxis")),
                   column(4, uiOutput("yAxis")),
                   column(4, uiOutput("color"))
                 ),
                 plotOutput("plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$fileUpload, {
    v$data <- read.csv(input$fileUpload$datapath)
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
  
  output$xAxis <- renderUI({
    req(v$data)
    selectInput("xAxisVar", "X-Axis Variable", choices = names(v$data))
  })
  
  output$yAxis <- renderUI({
    req(v$data)
    chartType <- input$chartType
    
    if (chartType %in% c("Scatterplot", "Line Chart", "Box Plot", "Bar Chart")) {
      selectInput("yAxisVar", "Y-Axis Variable", choices = names(v$data))
    }
  })
  
  output$color <- renderUI({
    req(v$data)
    selectInput("colorVar", "Color Variable (Optional)", c("", names(v$data)))
  })
  
  output$plot <- renderPlot({
    req(v$data)
    chartType <- input$chartType
    xVar <- input$xAxisVar
    yVar <- input$yAxisVar
    colorVar <- input$colorVar
    
    p <- ggplot(v$data)
    
    if (chartType == "Scatterplot") {
      p <- p + geom_point(aes_string(x = xVar, y = yVar, color = colorVar))
    } else if (chartType == "Histogram") {
      p <- p + geom_histogram(aes_string(x = xVar, fill = colorVar), binwidth = 30, alpha = 0.5)
    } else if (chartType == "Line Chart") {
      p <- p + geom_line(aes_string(x = xVar, y = yVar, color = colorVar))
    } else if (chartType == "Box Plot") {
      p <- p + geom_boxplot(aes_string(x = xVar, y = yVar, fill = colorVar))
    } else if (chartType == "Bar Chart") {
      p <- p + geom_bar(aes_string(x = xVar, y = yVar, fill = colorVar), stat = "identity")
    }
    
    p <- p + theme_minimal() + labs(x = xVar, y = yVar)
    print(p)
  })
}

shinyApp(ui, server)
  