library(shiny)
library(readr)
library(ggplot2)
library(corrplot)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Correlation Analysis"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      selectInput("plot_type", "Select Plot Type",
                  choices = c("Pairplot", "Correlation Matrix", "Correlogram")),
      uiOutput("column_select"),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      plotOutput("correlation_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read data from uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Select columns for correlation
  output$column_select <- renderUI({
    req(data())
    selectInput("columns", "Select Columns", choices = colnames(data()), multiple = TRUE)
  })
  
  # Perform correlation analysis
  observeEvent(input$submit, {
    req(data())
    req(input$columns)
    
    # Subset data based on selected columns
    selected_data <- data()[, input$columns]
    
    # Perform plot based on selected plot type
    if (input$plot_type == "Pairplot") {
      output$correlation_plot <- renderPlot({
        pairs(selected_data)
      })
    } else if (input$plot_type == "Correlation Matrix") {
      output$correlation_plot <- renderPlot({
        corr <- cor(selected_data)
        corrplot(corr, method = "circle", type = "upper", tl.cex = 0.7)
      })
    } else if (input$plot_type == "Correlogram") {
      output$correlation_plot <- renderPlot({
        corr <- cor(selected_data)
        ggcorrplot(corr, type = "lower", lab = TRUE)
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

