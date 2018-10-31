library(shiny)
library(DT)
library(rpivotTable)
library(rvest)

ui <- fluidPage(
  rpivotTableOutput('RESULTS'),
  
  downloadButton("downloadData", "Download"),
  
  DT::dataTableOutput('aSummaryTable')
  
)

server<-function(input, output, session) {
  
  # Make some sample data
  qbdata <- reactive({
    expand.grid(LETTERS,1:3)
  })
  
  # Clean the html and store as reactive
  summarydf <- eventReactive(input$myData,{
    input$myData %>% 
      read_html %>% 
      html_table(fill = TRUE) %>% 
      # Turns out there are two tables in an rpivotTable, we want the second
      .[[2]]
    
  })
  
  # show df as DT::datatable
  output$aSummaryTable <- DT::renderDataTable({
    datatable(summarydf(), rownames = FALSE)
    
  })
  
  tabella<-DT::renderDataTable({
    datatable(summarydf(), rownames = FALSE)
    
  })
  
  # Whenever the config is refreshed, call back with the content of the table
  output$RESULTS <- renderRpivotTable({
    rpivotTable(
      qbdata(),
      onRefresh = 
        htmlwidgets::JS("function(config) { 
                           Shiny.onInputChange('myData', document.getElementById('RESULTS').innerHTML); 
                        }")
    )
  })
  
 
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("prova", ".csv", sep = "")
    },
    content = function(file) {
      write.csv( tabella, file, row.names = FALSE)
    }
  )
  
}
shinyApp(ui = ui, server = server) 