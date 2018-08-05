library(tidyverse)
library(shiny)
library(shinydashboard)
library(gghighlight)
library(DT)

##----------- Global script

DJ.Mag <- read.csv("https://s3-ap-southeast-2.amazonaws.com/koki25ando/DJ_Mag.csv")
DJ.Mag <- DJ.Mag %>% 
  select(Year:Change)