library(tidyverse)
library(shiny)
library(shinydashboard)

##----------- Global script

DJ.Mag <- read.csv("DJ_Mag.csv")
DJ.Mag <- DJ.Mag %>% 
  select(Year:Change)


##----------- header

header <- dashboardHeader(
  title = "DJ Mag History",
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "Dataset will be updated ASAP after the DJ mag official announce on Oct 21st!"
    ),
    badgeStatus = "danger"
  ),
  tags$li(
    a(href = "",
      icon = "power-off"), 
    class = "dropdown"
  )
)

##----------- sidebar

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "History Chart", icon = icon("bar-chart-o"), tabName = "HistoryChart"),
    menuItem(text = "Ranking Table", icon = icon("table"), tabName = "RankingTable"),
    menuItem(text = "Compare Ranking", icon = icon("exchange"), tabName = "CompareRanking"),
    menuItem(text = "Overview Information", icon = icon("info"), tabName = "OverviewInfo")
  )
)

##----------- Body

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "HistoryChart", h2("History Chart"),
            box(title = "DJ Mag History",
                selectizeInput(inputId = "DJ_name",
                               label = "Type Your Favorite DJ Name: ",
                               selected = "Armin Van Buuren",
                               choices = unique(DJ.Mag$DJ)),
                width = 3),
            box(title = "History Chart",
                plotOutput(outputId = "HistoryChart"),
                width = 9)),
    tabItem(tabName = "RankingTable", h2("Ranking Table"),
            box(title = "Ranking Table",
                selectInput(inputId = "year.select",
                            label = "Select a Year: ",
                            selected = "2017",
                            choices = unique(DJ.Mag$Year)),
                width = 3),
            box(title = "Ranking Table",
                dataTableOutput(outputId = "ranking.table"),
                width = 9)),
    tabItem(tabName = "CompareRanking", h2("Ranking Comparison"),
            box(title = "Ranking Comparison",
                sliderInput(inputId = "YearRange",
                            label = "Select Year Range: ",
                            min = 2004, max = 2017, value = c(2010,2016),
                            format = "####"
                            ), width = 3
                ),
            box(title = "Comparison Table",
                plotOutput(outputId = "ComparisonPlot"),
                width = 9)
            ),
    tabItem(tabName = "OverviewInfo", h2("Overview Information"))
  )
)


ui <- 
  dashboardPage(header, sidebar, body)

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

##----------- Excecution
shinyApp(ui, server)