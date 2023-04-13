library(shiny)
library(esquisse)

ui <- fluidPage(
  titlePanel("Interactive Data Visualization with Esquisse"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Choose a CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Esquisse Plot", uiOutput("esquisse_ui"))
      )
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$datafile) # Ensure the file is uploaded before reading it
    read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
  })
  
  output$esquisse_ui <- renderUI({
    req(data())
    data_r <- data()
    colnames(data_r) <- make.names(colnames(data_r))
    
    esquisser(
      data = data_r,
      viewer = "inline",
      width = "100%",
      height = "600px"
    )
  })
}

shinyApp(ui = ui, server = server)
