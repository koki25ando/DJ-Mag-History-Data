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
    menuItem(text = "Overview Information", icon = icon("info"), tabName = "OverviewInfo")
  )
)

##----------- Body

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "HistoryChart", h2("History Chart")),
    tabItem(tabName = "RankingTable", h2("Ranking Table")),
    tabItem(tabName = "OverviewInfo", h2("Overview Information"))
  )
)


ui <- 
  dashboardPage(header, sidebar, body)

##----------- server script

server <- function (input, output) {
  
}

##----------- Excecution
shinyApp(ui, server)