library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Choose a CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Import", id = "data_import"),
        tabPanel("Page 2", id = "page_2")
      )
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$datafile) # Ensure the file is uploaded before reading it
    read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
  })
}

shinyApp(ui = ui, server = server)

