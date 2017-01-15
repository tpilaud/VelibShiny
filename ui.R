
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
      selectInput("mailleClassifSelect", "Maille de la classification", c("A la journée", "A la semaine"), selected="A la journée"),
      uiOutput("selectionTypeJour"),
      uiOutput("selectionClasse")
      ,
      uiOutput("propClasseText")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cartographie", leafletOutput("carto", width = "100%", height = 600))
        ,
        tabPanel("Profils moyens", plotOutput("profilsPlot"))
      )
    )
  )
))