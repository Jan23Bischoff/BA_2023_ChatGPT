library(shiny)
library(esquisse)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .shiny-output-error { display: none; }
    .shiny-output-error:before { content: ' '; }
  "))),
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
        tabPanel("Esquisse Plot", div(style = "height: 600px;", uiOutput("esquisse_ui")))
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
      viewer = "inline"
    )
  })
}

shinyApp(ui = ui, server = server)
