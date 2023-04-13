library(shiny)
library(esquisse)

ui <- fluidPage(
  titlePanel("Interactive Data Visualization with Esquisse"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Choose a CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("esquisse_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Esquisse Plot", esquisseOutput("esquisse_plot"))
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
    esquisseUI(
      id = "esquisse",
      header = FALSE,
      container = tags$div(style = "height: 600px;")
    )
  })
  
  observeEvent(input$esquisse$data, {
    data_r <- data()
    colnames(data_r) <- make.names(colnames(data_r))
    esquisse_proxy("esquisse") %>%
      update_data(data = data_r)
  })
  
  output$esquisse_plot <- renderEsquisse({
    req(input$esquisse$plot)
    input$esquisse$plot
  })
}

shinyApp(ui = ui, server = server)
