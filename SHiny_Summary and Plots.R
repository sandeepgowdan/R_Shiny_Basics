library(shiny)
library(readr)
library(dplyr)



# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Data Summary"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      uiOutput("column_select"),
      downloadButton("download_summary", "Download Summary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", tableOutput("summary_table")),
        tabPanel("Plots",
                 uiOutput("boxplot_ui"),
                 uiOutput("histogram_ui")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Read data from uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Select columns for summary table
  output$column_select <- renderUI({
    req(data())
    selectInput("columns", "Select Columns", choices = colnames(data()), multiple = TRUE)
  })
  
  # Transpose and render summary table
  output$summary_table <- renderTable({
    req(data())
    req(input$columns)
    
    summary_table <- lapply(input$columns, function(col) {
      s <- summary(data()[[col]])
      s <- as.data.frame(t(s))
      colnames(s) <- col
      s
    })
    summary_table <- do.call(cbind, summary_table)
    summary_table
  }, rownames = TRUE)
  
  # Render boxplot for each selected column
  output$boxplot_ui <- renderUI({
    req(input$columns)
    plot_list <- lapply(input$columns, function(col) {
      plotOutput(paste0("boxplot_", col))
    })
    do.call(tagList, plot_list)
  })
  
  # Render histogram for each selected column
  output$histogram_ui <- renderUI({
    req(input$columns)
    plot_list <- lapply(input$columns, function(col) {
      plotOutput(paste0("histogram_", col))
    })
    do.call(tagList, plot_list)
  })
  
  # Generate boxplot for each selected column
  observe({
    req(input$columns)
    
    lapply(input$columns, function(col) {
      output[[paste0("boxplot_", col)]] <- renderPlot({
        req(data())
        ggplot(data(), aes_string(y = col)) +
          geom_boxplot(fill = "lightgreen", color = "black") +
          labs(title = paste("Box Plot of", col))
      })
    })
  })
  
  # Generate histogram for each selected column
  observe({
    req(input$columns)
    
    lapply(input$columns, function(col) {
      output[[paste0("histogram_", col)]] <- renderPlot({
        req(data())
        ggplot(data(), aes_string(x = col)) +
          geom_histogram(fill = "skyblue", color = "black", bins = 20) +
          labs(title = paste("Histogram of", col))
      })
    })
  })
  
  # Download summary table as CSV
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("summary_table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summary_table(), file, row.names = TRUE)
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)
