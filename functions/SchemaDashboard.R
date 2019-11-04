#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
 getShiny<-function(){
   
  #Default schema for US Equity
  if(USschema=='GICS') {
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Equity'),
                selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='US'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='GICS_1'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='GICS_2'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='GICS_2'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value"))) 
    
    US<-list('Equity','US','GICS_1','GICS_2','GICS_2','Selection')
  } 
   
  #Alternative schema for US Equity
  if (USschema=='Mkt_Cap') {
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Equity'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='US'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='US'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='Mkt_Cap'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='Mkt_Cap'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    US<-list('Equity','US','GICS_1','GICS_2','GICS_2','Selection')
  }
  
  #Alternative schema for US Equity
  if (USschema=='Country') {
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Equity'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='US'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='US'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='GICS_1'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='GICS_2'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    US<-list('Equity','US','US','Mkt_Cap','Mkt_Cap','Selection')  
  }
  
  #Default schema for International Equity
  if(intlSchema=='Country') {
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Equity'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='International'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='DM/EM'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='Continent'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='Country'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    intl<-list('Equity','International','DM/EM','Continent','Country','Selection')
  } 
  
  #Alternative schema for International Equity
  if (intlSchema=='GICS'){
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Equity'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='International'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='DM/EM'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='GICS_1'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='GICS_2'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    intl<-list('Equity','International','DM/EM','GICS_1','GICS_2','Selection')   
  }
  
  #Alternative schema for International Equity
  if (intlSchema=='Mkt_Cap'){
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Equity'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='International'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='DM/EM'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='Mkt_Cap'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='Mkt_Cap'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    intl<-list('Equity','International','DM/EM','Mkt_Cap','Mkt_Cap','Selection')              
  }
  
  #Default schema for Fixed Income
  if(bondSchema=='AssetType') {
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Fixed Income'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='Fixed Income'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='Sec_Group'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='Sec_Type'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='Sec_Type'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    bonds<-list('Fixed Income','Fixed Income','Sec_Group','Sec_Type','Sec_Type','Selection')  
  
  }
  
  #Alternative schema for Fixed Income
  if(bondSchema=='Duration') {
    
    selectInput("select", label = h3("Select Level 1"), 
                choices = list(Level1='Fixed Income'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 2"), 
                choices = list(Level2='Fixed Income'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 3"), 
                choices = list(Level3='Dur'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))  
    
    selectInput("select", label = h3("Select Level 4"), 
                choices = list(Level4='Dur'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))   
    
    selectInput("select", label = h3("Select Level 5"), 
                choices = list(Level5='Dur'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))
    
    selectInput("select", label = h3("Select Level 6"), 
                choices = list(Level6='Selection'),selected = 1)
    hr()
    fluidRow(column(3, verbatimTextOutput("value")))    
    
    bonds<-list('Fixed Income','Fixed Income','Dur','Dur','Dur','Selection')
  }
  
  schema <- rbind(US, intl)
  schema <- rbind(schema, bonds)
 
  return(schema)
  
 }   
)

# Define server logic required to execute the user interaction outcome
server <- function(input, output) {
  
  # You can access the value of the widget with input$select, e.g.
  output$value <- renderPrint({ input$select })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



