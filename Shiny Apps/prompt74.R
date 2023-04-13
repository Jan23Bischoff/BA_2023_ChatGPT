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
        tabPanel("Esquisse Plot", div(style = "height: 600px;", esquisseUI(id = "esquisse"))),
        tabPanel("Plot Output", plotOutput("esquisse_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$datafile) # Ensure the file is uploaded before reading it
    read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
  })
  
  observeEvent(input$datafile, {
    data_r <- data()
    colnames(data_r) <- make.names(colnames(data_r))
    updateEsquisseData(proxyId = "esquisse", data = data_r)
  })
  
  output$esquisse_plot <- renderPlot({
    req(input$esquisse$ggplot_code)
    eval(parse(text = input$esquisse$ggplot_code))
  })
}

shinyApp(ui = ui, server = server)
