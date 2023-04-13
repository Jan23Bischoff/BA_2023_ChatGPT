library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Choose a CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectInput("chart_type", "Chart Type",
                  choices = c("Histogram", "Boxplot", "Scatterplot", "Linechart", "Barchart", "Density", "Violin")),
      uiOutput("x_var"),
      uiOutput("y_var"),
      uiOutput("color_var"),
      uiOutput("facet_var"),
      uiOutput("bin_width"),
      textInput("x_label", "X-axis Label", ""),
      textInput("y_label", "Y-axis Label", ""),
      textInput("plot_title", "Plot Title", ""),
      selectInput("theme_choice", "Choose a Theme", choices = c("theme_minimal", "theme_bw", "theme_classic", "theme_light", "theme_dark")),
      downloadButton("download_plot", "Save plot as PNG")
    ),
    mainPanel(
      plotOutput("data_plot")
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
    selectInput("y_var_input", "Y Variable", choices = c("", names(data())), selected = NULL)
  })
  
  output$color_var <- renderUI({
    req(data())
    selectInput("color_var_input", "Color Variable", choices = c("", names(data())), selected = "")
  })
  
  output$facet_var <- renderUI({
    req(data())
    selectInput("facet_var_input", "Facet Variable", choices = c("", names(data())), selected = "")
  })
  
  output$bin_width <- renderUI({
    req(data())
    if (input$chart_type == "Histogram") {
      numericInput("bin_width_input", "Bin Width", value = 30, min = 1, step = 1)
    } else {
      NULL
    }
  })
  
  output$data_plot <- renderPlot({
    req(data(), input$x_var_input, input$y_var_input, input$chart_type)
    
    chart <- switch(input$chart_type,
                    "Histogram" = ggplot(data(), aes_string(x = input$x_var_input, fill = input$color_var_input)) +
                      geom_histogram(binwidth = input$bin_width_input),
                    "Boxplot" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, fill = input$color_var_input)) +
                      geom_boxplot(),
                    "Scatterplot" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, color = input$color_var_input)) +
                      geom_point(),
                    "Linechart" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, color = input$color_var_input)) +
                      geom_line(),
                    "Barchart" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, fill = input$color_var_input)) +
                      geom_bar(stat = "identity"),
                    "Density" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, color = input$color_var_input)) +
                      geom_density(),
                    "Violin" = ggplot(data(), aes_string(x = input$x_var_input, y = input$y_var_input, fill = input$color_var_input)) +
                      geom_violin())
    
    chart <- chart + labs(x = input$x_label, y = input$y_label, title = input$plot_title)
    
    if (input$facet_var_input != "") {
      chart <- chart + facet_wrap(as.formula(paste("~", input$facet_var_input)))
    }
    
    chart + do.call(input$theme_choice, list())
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = output$data_plot(), device = "png")
    },
    contentType = "image/png"
  )
}

shinyApp(ui = ui, server = server)

                    