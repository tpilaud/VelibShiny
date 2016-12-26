
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  titlePanel("CSV - Classif Stations Velib"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("typeJour", "Type de jour", c("Jour de la semaine", "Mercredi", "Samedi/Dimanche"), selected="Jour de la semaine"),
      uiOutput("secondSelection"),
      textOutput("propClasseText")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cartographie", leafletOutput("carto", width = "100%", height = 600)), 
        tabPanel("Profils moyens", plotOutput("profilsPlot"))
      )
    )
  )
))