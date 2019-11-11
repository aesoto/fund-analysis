library(shiny)

shinyLibraries <- c('colorspace', 'plyr', 'labeling', 'munsell', 'RColorBrewer', 'gtable', 'reshape2', 'scales', 'viridisLite', 'withr','yaml',
                    'lazyeval','ggplot2','htmlwidgets','crosstalk','DT','shiny')
lapply(shinyLibraries, require, character.only=TRUE)

#fields <- c("name","age","height","weight")
fields <- c("US Equity Schema","Intl Equity Schema","Bond Schema")
ui <- fluidPage(
  
  # Application title
  titlePanel("Attribution Schema"),
  
  # Sidebar with reactive inputs
  sidebarLayout(
    sidebarPanel(
      selectInput("US Equity Schema","Choose US Schema",c("Default","GICS2","Market Cap")),
      selectInput("Intl Equity Schema","Choose Intl Schema",c("Default","GICS","Market Cap")),
      selectInput("Bond Schema","Choose Bond Schema",c("Default","Duration")),
      actionButton(inputId = "save",label = "Add")
      
    ),
    
    # a table of reactive outputs
    mainPanel(
      mainPanel(
        
        DT::dataTableOutput("responses", width = 500), tags$hr()
        
      )
    )
  )
)

# Define server logic 
server <- function(input, output,session) {
  
  #create a data frame called responses
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    responses <<- data
  }
  
  loadData <- function() {
   if (exists("responses")) {
     responses
    }
  }
  
  
  # Whenever a field is filled, aggregate all form data
  #formData is a reactive function
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when save is clicked)
  output$responses <- DT::renderDataTable({
    input$save
    loadData()
  })     
}


# Run the application 
shinyApp(ui = ui, server = server)
