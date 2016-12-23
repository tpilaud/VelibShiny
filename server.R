
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(leaflet)
library(dplyr)
load("stations.RData")
# stations$cluster5 <- factor(sample(1:5, replace=T, nrow(stations)))
load("classif.station.RData")
stations <- inner_join(stations, classif.station)
cols <- rainbow(length(levels(stations$cluster5)), alpha=NULL)
stations$colors <- cols[unclass(stations$cluster5)]
stations$pop <- paste(stations$name, stations$cluster5)


shinyServer(function(input, output) {

  output$carto <- renderLeaflet({
    
    stations.tmp <- subset(stations, Daym == input$jourSemaine)
    
    if(input$classeCarto != "Toutes"){
      stations.tmp <- subset(stations.tmp, cluster5 == input$classeCarto)
    }

    leaflet(stations.tmp) %>% 
      addTiles() %>%
      addCircles(lat = ~latitude, lng = ~longitude, popup = ~ pop , color = ~ colors) %>%
      addLegend("topright", colors = unique(stations$colors), labels = unique(stations$cluster5),
                title = "Classes de stations",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1)

  })
  
  output$propClasseText <- renderText({
    if(input$classeCarto != "Toutes"){
      stations.tmp <- subset(stations, Daym == input$jourSemaine)
      prop <- round(table(stations.tmp$cluster5)[input$classeCarto]/sum(table(classif.station$cluster5))*100, 1)
      paste0("Pour le ", input$jourSemaine, ", la classe ", input$classeCarto, " représente ", prop, "% des stations")
    }
  })

})
