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
  uploadedData <- reactiveVal()
  
  observeEvent(input$fileUpload, {
    uploadedData(read.csv(input$fileUpload$datapath))
  })
  
  output$dataTable <- renderDT({
    datatable(uploadedData(), editable = input$editable, rownames = FALSE)
  })
  
  proxy <- dataTableProxy("dataTable")
  
  observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value
    
    currentData <- isolate(uploadedData())
    currentData[i, j] <<- v
    replaceData(proxy, currentData, resetPaging = FALSE, rownames = FALSE)
  })
}


shinyApp(ui, server)


