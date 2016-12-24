library(shiny)
library(leaflet)
library(dplyr)
require(RColorBrewer)

load("stations.RData")
load("classif.station.RData")

stations <- inner_join(stations, classif.station)
# cols <- brewer.pal(length(levels(stations$cluster5)), "Dark2")
# stations$colors <- cols[unclass(stations$cluster5)]
stations$pop <- paste(stations$name, stations$classe)

shinyServer(function(input, output) {

  output$secondSelection <- renderUI({
    selectInput("classeCarto", "Classe", choices = c("Toutes", unique(as.character(stations[stations$typeJour==input$typeJour,"classe"]))))
  })
  
  output$carto <- renderLeaflet({
    
    # Filtre sur le type de jour selectionne
    stations.tmp <- subset(stations, typeJour == input$typeJour)
    stations.tmp$numeroClasse <- dense_rank(unclass(stations.tmp$classe))
    
    # Definition des couleurs
    cols <- brewer.pal(length(unique(stations.tmp$numeroClasse)), "Dark2")
    stations.tmp$colors <- cols[stations.tmp$numeroClasse]
    
    # Filtre sur la classe selectionnee
    if(input$classeCarto != "Toutes"){
      stations.tmp <- subset(stations.tmp, classe == input$classeCarto)
    }

    # Carte interactive
    leaflet(stations.tmp) %>% 
      addTiles() %>%
      addCircles(lat = ~latitude, lng = ~longitude, popup = ~ pop , color = ~ colors, opacity = 1, fillOpacity = 1) %>%
      addLegend("topright", colors = unique(stations.tmp$colors), labels = unique(stations.tmp$classe),
                title = "Classes de stations",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1)

  })
  
  output$propClasseText <- renderText({
    if(input$classeCarto != "Toutes"){
      stations.tmp <- subset(stations, typeJour == input$typeJour)
      prop <- round(table(stations.tmp$classe)[input$classeCarto]/sum(table(stations.tmp$classe))*100, 1)
      paste0("Pour les ", input$typeJour, ", la classe ", input$classeCarto, " représente ", prop, "% des stations")
    }
  })

})
