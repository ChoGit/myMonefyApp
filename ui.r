library(shiny)
library(shinydashboard)

startDate <- as.Date(as.yearmon(Sys.Date()) - 1/12) - 3
endDate <- as.Date(as.yearmon(Sys.Date())) - 3


dashboardPage(
  dashboardHeader(title = "MyMonefy dashboard"),
  
  dashboardSidebar(
    
    dateRangeInput('dateRange',
                   label = 'Select range date',
                   start = startDate, end = endDate
    ),
    
    
    checkboxGroupInput("category", "Categoria:", choices = c()),
    
    checkboxInput("isCumulative", "Cumulative sum", value = TRUE),
    
    numericInput("thresholdPlot", "Set horizontal line:", 0, min = -100000, max = 100000),
    
    verbatimTextOutput("value")
    
  ),
  dashboardBody(
    
    tabsetPanel( id = "tabsPanel",
      
      tabPanel("Balance sheet",
               
               plotlyOutput('plotBalance')
               
               ),
      
      tabPanel("Incomes",
               
               plotOutput('plotIncomes')
               
      ),
      
      tabPanel("Outcames",
               
               plotlyOutput('plotOutcomes')
               
      ),
      
      tabPanel("Table", 
               
               tableOutput('table')
               
               )
    )
    
  ),
  title = "Dashboard example"
)