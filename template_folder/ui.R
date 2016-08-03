
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("Blank template for module"),
  beginPage(),
  
  beginPanel('1/3'),
  textInput(inputId="email", label = "Email", value = ""),
  actionButton("goButton","Run analysis"),
  endPanel(),
  
  beginPanel('2/3'),
  HTML("This module provides a template. That's it.<br>"),
  plotOutput("plot1"),
  endPanel(),
  
  endPage(),
  footer()
))	