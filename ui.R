
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(navbarPage("Nom appli",
                   tabPanel("Cartographie",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("jourSemaine", "Jour de la semaine", c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"), selected="lundi")
                              ),
                          
                              # Show a plot of the generated distribution
                              mainPanel(
                                leafletOutput("carto", width = "100%", height = 600)
                                )
                            )
                   ),
                   tabPanel("Profils")
))
