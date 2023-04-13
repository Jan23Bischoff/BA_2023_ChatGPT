library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("CSV Data Viewer and Editor"),
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
      DTOutput("dataTable")
    )
  )
)

ui <- fluidPage(
  titlePanel("CSV Data Viewer and Editor"),
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
      DTOutput("dataTable")
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
}



shinyApp(ui, server)