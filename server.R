require(shiny)
require(leaflet)
require(dplyr)
require(RColorBrewer)
require(ggplot2)

load("stations.RData")
load("classif.station.RData")
load("profils.moy.RData")

stations <- inner_join(stations, classif.station)
# cols <- brewer.pal(length(levels(stations$cluster5)), "Dark2")
# stations$colors <- cols[unclass(stations$cluster5)]
stations$pop <- paste(stations$name, stations$classe)

shinyServer(function(input, output) {

  output$secondSelection <- renderUI({
    selectInput("classeCarto", "Classe", choices = c("Toutes", unique(as.character(stations[stations$typeJour==input$typeJour,"classe"]))))
  })
  
  output$secondSelection2 <- renderUI({
    selectInput("classeCarto2", "Classe", choices = c(unique(as.character(stations[stations$typeJour==input$typeJour2,"classe"]))))
  })
  
  output$carto <- renderLeaflet({
    
    # Filtre sur le type de jour selectionne
    stations.tmp <- subset(stations, typeJour == input$typeJour)
    stations.tmp$numeroClasse <- dense_rank(unclass(stations.tmp$classe))
    
    # Definition des couleurs
    cols <- brewer.pal(length(unique(stations.tmp$numeroClasse)), "Dark2")
    stations.tmp$colors <- cols[stations.tmp$numeroClasse]
    
    # Filtre sur la classe selectionnee
    if(!is.null(input$classeCarto)){
      if(input$classeCarto != "Toutes"){
        stations.tmp <- subset(stations.tmp, classe == input$classeCarto)
      }
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
    if(!is.null(input$classeCarto)){
      if(input$classeCarto != "Toutes"){
        stations.tmp <- subset(stations, typeJour == input$typeJour)
        prop <- round(table(stations.tmp$classe)[input$classeCarto]/sum(table(stations.tmp$classe))*100, 1)
        paste0("Pour les ", input$typeJour, ", la classe ", input$classeCarto, " représente ", prop, "% des stations")
      }
    }

  })
  
  output$profilsPlot <- renderPlot({
    
    profils.moy.tmp <- subset(profils.moy, typeJour == input$typeJour2)

    # Definition des couleurs
    cols <- brewer.pal(length(unique(profils.moy.tmp$classe)), "Dark2")
    classes <- unique(profils.moy.tmp$classe)
    
    profils.moy.tmp <- subset(profils.moy.tmp, classe == input$classeCarto2)
    
    ggplot(profils.moy.tmp) + 
      aes(x=time, y=yspline) +
      geom_line(size=1.5, col=cols[match(input$classeCarto2, classes)]) +
      ylim(c(0,1)) +
      xlab("Heure de la journée") +
      ylab("Taux de vélos disponibles") +
      ggtitle(paste("Profil moyen de la classe", input$classeCarto2)) +
      theme_bw() +
      theme(legend.position="none", plot.title=element_text(size = rel(1.5)))
      
  })

})
