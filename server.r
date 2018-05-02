setwd("C:/MyDesktop/Personale/Bilancio/appBilancio/")
source("dashboardSourcingAndLibraries.r")

function(input, output, session) {
  
  bilancio <- loadBilancio()
  
  observeEvent(input$tabsPanel, {

    x <- input$tabsPanel
    
    if (x == "Balance sheet"){
      updateNumericInput(session, "thresholdPlot", value = 800)
    }else if(x == "Outcames"){
      updateNumericInput(session, "thresholdPlot", value = 1000)
    }
    
  })
  
  output$table <- renderTable({
    tableData <- filterBilancio(bilancio, "category", "contains", paste0(input$category, collapse = "|"))
    tableData$date <- as.character(tableData$date)
    tableData
    })
  
  updateCheckboxGroupInput(session, "category",
                           choices = c(levels(bilancio$category)),
                           selected = c()
  )
  
  output$plotBalance <- renderPlotly({
    selectedData <- filterBilancio(bilancio, "category", "contains", paste0(input$category, collapse = "|"))
    plotBilancio(bilancio, input$isCumulative, input$thresholdPlot, datesRange = input$dateRange)
  })
  
  output$plotOutcomes <- renderPlotly({
    selectedData <- filterBilancio(bilancio, "amount", "<", 0)
    selectedData <- filterBilancio(selectedData, "category", "contains", paste0(input$category, collapse = "|"))
    plotUscite(selectedData, input$isCumulative, input$thresholdPlot, datesRange = input$dateRange)
  })

}