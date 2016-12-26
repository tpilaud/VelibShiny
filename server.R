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
    selectInput("classeSelect", "Classe", choices = c("Toutes", unique(as.character(stations[stations$typeJour==input$typeJour,"classe"]))))
  })
  
  output$carto <- renderLeaflet({
    
    # Filtre sur le type de jour selectionne
    stations.tmp <- subset(stations, typeJour == input$typeJour)
    stations.tmp$numeroClasse <- dense_rank(unclass(stations.tmp$classe))
    
    # Definition des couleurs
    cols <- brewer.pal(length(unique(stations.tmp$numeroClasse)), "Dark2")
    stations.tmp$colors <- cols[stations.tmp$numeroClasse]
    
    # Filtre sur la classe selectionnee
    if(!is.null(input$classeSelect)){
      if(input$classeSelect != "Toutes"){
        stations.tmp <- subset(stations.tmp, classe == input$classeSelect)
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
    if(!is.null(input$classeSelect)){
      if(input$classeSelect != "Toutes"){
        stations.tmp <- subset(stations, typeJour == input$typeJour)
        prop <- round(table(stations.tmp$classe)[input$classeSelect]/sum(table(stations.tmp$classe))*100, 1)
        paste0("Pour les ", input$typeJour, ", la classe ", input$classeSelect, " représente ", prop, "% des stations")
      }
    }

  })
  
  output$profilsPlot <- renderPlot({
    
    # Filtre sur le type de jour selectionne
    profils.moy.tmp <- subset(profils.moy, typeJour == input$typeJour)
    profils.moy.tmp$numeroClasse <- dense_rank(unclass(profils.moy.tmp$classe))
    classes <- unique(profils.moy.tmp$classe)
    
    # Definition des couleurs
    cols <- brewer.pal(length(classes), "Dark2")

    if(!is.null(input$classeSelect)){
      if(input$classeSelect != "Toutes"){
        profils.moy.tmp <- subset(profils.moy.tmp, classe == input$classeSelect)
        numeroClasse <- unique(profils.moy.tmp$numeroClasse)
        color <- cols[numeroClasse]
        ggplot(profils.moy.tmp) + 
          aes(x=time, y=yspline) +
          geom_line(size=1.5, col=color) +
          ylim(c(0,1)) +
          xlab("Heure de la journée") +
          ylab("Taux de vélos disponibles") +
          ggtitle(paste("Profil moyen de la classe", input$classeSelect)) +
          theme_bw() +
          theme(legend.position="none", plot.title=element_text(size = rel(1.5)))
      }else{
        ggplot(profils.moy.tmp) + 
          aes(x=time, y=yspline, col=as.character(numeroClasse)) +
          geom_line(size=1.5) +
          ylim(c(0,1)) +
          xlab("Heure de la journée") +
          ylab("Taux de vélos disponibles") +
          ggtitle(paste("Profil moyen de la classe", input$classeSelect)) +
          theme_bw() +
          theme(legend.position="none", plot.title=element_text(size = rel(1.5))) +
          scale_colour_brewer(palette = "Dark2")
      }
    }
      
  })

})
