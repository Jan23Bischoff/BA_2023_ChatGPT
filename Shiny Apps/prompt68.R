library(shiny)
library(ggplot2)

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
        tabPanel("Data Visualization",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("chart_type", "Chart Type",
                                 choices = c("Histogram", "Boxplot", "Scatterplot", "Linechart", "Barchart")),
                     uiOutput("x_var"),
                     uiOutput("y_var"),
                     uiOutput("color_var")
                   ),
                   mainPanel(
                     plotOutput("data_plot")
                   )
                 ))
      )
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$datafile) # Ensure the file is uploaded before reading it
    read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
  })
  
  output$x_var <- renderUI({
    req(data())
    selectInput("x_var_input", "X Variable", choices = names(data()), selected = NULL)
  })
  
  output$y_var <- renderUI({
    req(data())
    selectInput("y_var_input", "Y Variable", choices = names(data()), selected = NULL)
  })
  
  output$color_var <- renderUI({
    req(data())
    selectInput("color_var_input", "Color Variable", choices = c("", names(data())), selected = "")
  })
  
  output$data_plot <- renderPlot({
    req(data(), input$x_var_input, input$y_var_input, input$chart_type)
    
    chart <- switch(input$chart_type,
                    "Histogram" = ggplot(data(), aes_string(x = input$x_var_input, fill = input$color_var_input)) +
                      geom_histogram(),
                    "Boxplot" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, fill = input$color_var_input)) +
                      geom_boxplot(),
                    "Scatterplot" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, color = input$color_var_input)) +
                      geom_point(),
                    "Linechart" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, color = input$color_var_input)) +
                      geom_line(),
                    "Barchart" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, fill = input$color_var_input)) +
                      geom_bar(stat = "identity"))
    
    chart + theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
