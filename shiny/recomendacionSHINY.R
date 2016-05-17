## Only run this example in interactive R sessions
if (interactive()) {
  # table example
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(12,
               tableOutput('table'),
               plotOutput('plot')
        )
      )
    ),
    server = function(input, output) {
      output$table <- renderTable(iris)
      output$plot <- renderPlot(iris)
    }
    

  )
  
  
  # DataTables example
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(12,
               dataTableOutput('table'),
               plotOutput('plot')
        )
      )
    ),
    server = function(input, output) {
      output$table <- renderDataTable(iris)
    
    output$plot <- renderPlot(iris)
    }

  )
}