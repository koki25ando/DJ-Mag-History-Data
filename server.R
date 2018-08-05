##----------- server script

server <- function (input, output) {
  output$HistoryChart <- renderPlot(
    DJ.Mag %>% 
      ggplot(aes(Year, Rank, colour = DJ)) +
      geom_line(show.legend = FALSE) +
      scale_y_reverse() +
      gghighlight(DJ == as.character(input$DJ_name)) +
      theme_minimal()
  )
  
  output$ranking.table <- DT::renderDataTable(
    DJ.Mag %>% 
      filter(Year == as.numeric(input$year.select))
  )
  
  output$ComparisonPlot <- renderPlot(
    DJ.Mag %>% 
      filter(Year %in% c(input$YearRange[2],input$YearRange[1])) %>% 
      arrange(Year) %>% 
      ggplot(aes(x = Rank, y = reorder(DJ, -Rank))) +
      geom_path(color = "red", 
                arrow = arrow(length = unit(1.5, "mm"), 
                              type = "closed")) +
      scale_x_reverse() +
      labs(y = "")
  )
}