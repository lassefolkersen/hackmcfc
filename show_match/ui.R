
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("Show match"),
  beginPage(),
  
  beginPanel('1/3'),
  selectInput("match", label = "Match", 
              choices = list("02 - City vs Chelsea" = "803174", "01 - West Brom vs City" = "803165"), selected = "803174"),
  sliderInput("time", "Time-interval (minutes):", min = 0, max = 120, value = c(27,29), step= 1),
    actionButton("goButton","Run analysis"),
  endPanel(),
  
  beginPanel('2/3'),
  HTML("This module provides a show match function.<br>"),
  plotOutput("plot1"),
  endPanel(),
  
  endPage(),
  footer()
))	