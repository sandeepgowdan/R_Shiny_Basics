library(shiny)
library(readr)
library(dplyr)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("ANOVA Analysis"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      verbatimTextOutput("anova_results")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Read data from uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Perform ANOVA analysis
  output$anova_results <- renderPrint({
    req(input$submit)
    req(data())
    
    # Assuming "yield" is the dependent variable and "Variety" is the independent variable
    model <- aov(yield ~ Variety, data = data())
    summary(model)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
