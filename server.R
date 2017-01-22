require(shiny)
require(leaflet)
require(dplyr)
require(ggplot2)
require(reshape2)
require(fmsb)

load("stations.RData")
load("shiny.classif.jour.RData")
load("shiny.classif.semaine.RData")
load("profils.moy.jour.RData")
names(profils.moy.jour)[names(profils.moy.jour)=="yspline"] <- "y"
load("profils.moy.semaine.RData")
names(profils.moy.semaine)[names(profils.moy.semaine)=="yfourier"] <- "y"
load("df.radar.RData")

id.class.jour <- unique(classif.jour$classe)
id.class.semaine <- unique(classif.semaine$classe)

# Jointure des tables stations et classif
stations.jour <- inner_join(stations, classif.jour)
stations.semaine <- inner_join(stations, classif.semaine)

# Preparation du contenu de la popup
pop.jour <- stations.jour
pop.jour$pop <- paste0(pop.jour$classe, " : ", pop.jour$nb, "/", pop.jour$nb_total)
pop.jour <- dcast(pop.jour, number + name + address + typeJour ~ rank, value.var = "pop")
pop.jour[is.na(pop.jour)] <- ""
pop.jour <- cbind.data.frame(number = pop.jour$number, typeJour = pop.jour$typeJour, 
                             pop = paste0("<font size=\"3\"><b>", pop.jour$name, "</b></font size=\"3\"><br>",
                                          "<em>", pop.jour$address, "</em><br><br>",
                                          "<b>", "Classes identifiées :", "</b><br>",
                                    pop.jour$`1`, "<br>", pop.jour$`2`, "<br>", pop.jour$`3`, "<br>", pop.jour$`4`, "<br>",
                                    pop.jour$`5`, "<br>", pop.jour$`6`, "<br>", pop.jour$`7`),
                             stringsAsFactors = F)

stations.jour <- stations.jour %>%
  filter(rank == 1) %>%
  select(-rank, -nb, -nb_total)
stations.jour <- inner_join(stations.jour, pop.jour)

pop.semaine <- stations.semaine
pop.semaine$pop <- paste0(pop.semaine$classe, " : ", pop.semaine$nb, "/", pop.semaine$nb_total)
pop.semaine <- dcast(pop.semaine, number + name + address ~ rank, value.var = "pop")
pop.semaine[is.na(pop.semaine)] <- ""
pop.semaine <- cbind.data.frame(number = pop.semaine$number, 
                             pop = paste0("<font size=\"3\"><b>", pop.semaine$name, "</b></font size=\"3\"><br>",
                                          "<em>", pop.semaine$address, "</em><br><br>",
                                          "<b>", "Classes identifiées :", "</b><br>",
                                          pop.semaine$`1`, "<br>", pop.semaine$`2`, "<br>", pop.semaine$`3`, "<br>", pop.semaine$`4`, "<br>",
                                          pop.semaine$`5`, "<br>", pop.semaine$`6`, "<br>", pop.semaine$`7`, "<br>",
                                          pop.semaine$`8`, "<br>", pop.semaine$`9`, "<br>", pop.semaine$`10`),
                             stringsAsFactors = F)

stations.semaine <- stations.semaine %>%
  filter(rank == 1) %>%
  select(-rank, -nb, -nb_total)
stations.semaine <- inner_join(stations.semaine, pop.semaine)

# Definition des couleurs
cols <- c("red", "navy", "orange", "royalblue", "limegreen", 
          "magenta", "chocolate", "darkgreen", "purple", "#000000")

shinyServer(function(input, output) {

    output$selectionTypeJour <- renderUI({
      if(input$mailleClassifSelect=="A la journée"){
        selectInput("typeSelect", "Type de jour", choices = c("Jours de la semaine", "Weekend"), selected = "Jours de la semaine")
      }
    })

  
  output$selectionClasse <- renderUI({
    if(input$mailleClassifSelect=="A la journée"){
      selectInput("classeSelect", "Classe", choices = c("Toutes", unique(as.character(stations.jour[stations.jour$typeJour==input$typeSelect,"classe"]))))
    }else{
      selectInput("classeSelect", "Classe", choices = c("Toutes", unique(as.character(stations.semaine$classe))))
    }
  })

  output$carto <- renderLeaflet({

    # Filtre sur le type de jour selectionne
    if(input$mailleClassifSelect=="A la journée"){
      stations.tmp <- subset(stations.jour, typeJour == input$typeSelect)
      stations.tmp$numeroClasse <- dense_rank(sapply(stations.tmp$classe, function(x) which(id.class.jour==x)))
    }else{
      stations.tmp <- stations.semaine
      stations.tmp$numeroClasse <- dense_rank(sapply(stations.tmp$classe, function(x) which(id.class.semaine==x)))
    }
    
    # Definition des couleurs
    cols <- cols[1:length(unique(stations.tmp$numeroClasse))]
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

  output$propClasseText <- renderUI({
    if(!is.null(input$classeSelect)){
      if(input$classeSelect != "Toutes"){
        if(input$mailleClassifSelect == "A la journée"){
          stations.tmp <- subset(stations.jour, typeJour == input$typeSelect)
          prop <- round(table(stations.tmp$classe)[input$classeSelect]/sum(table(stations.tmp$classe))*100, 1)
          HTML(paste0("Pour les ", input$typeSelect, ", la classe <i>", input$classeSelect, "</i> représente <b>", prop, "%</b> des stations"))
        }else{
          stations.tmp <- stations.semaine
          prop <- round(table(stations.tmp$classe)[input$classeSelect]/sum(table(stations.tmp$classe))*100, 1)
          HTML(paste0("La classe <i>", input$classeSelect, "</i> représente <b>", prop, "%</b> des stations"))
        }
      }
    }

  })

  output$profilsPlot <- renderPlot({

    # Filtre sur le type de jour selectionne
    if(input$mailleClassifSelect == "A la journée"){
      profils.moy.tmp <- subset(profils.moy.jour, typeJour == input$typeSelect)
      profils.moy.tmp$numeroClasse <- dense_rank(sapply(profils.moy.tmp$classe, function(x) which(id.class.jour==x)))
    }else{
      profils.moy.tmp <- profils.moy.semaine
      profils.moy.tmp$numeroClasse <- dense_rank(sapply(profils.moy.tmp$classe, function(x) which(id.class.semaine==x)))
    }

    # Definition des couleurs
    cols <- cols[1:length(unique(profils.moy.tmp$numeroClasse))]

    if(!is.null(input$classeSelect)){
      if(input$classeSelect != "Toutes"){
        profils.moy.tmp <- subset(profils.moy.tmp, classe == input$classeSelect)
        numeroClasse <- unique(profils.moy.tmp$numeroClasse)
        color <- cols[numeroClasse]
        ggplot(profils.moy.tmp) +
          aes(x=time, y=y) +
          geom_line(size=1.5, col=color) +
          xlab("Temps") +
          ylab("Taux de vélos disponibles") +
          ggtitle(paste("Profil moyen de la classe", input$classeSelect)) +
          theme_bw() +
          theme(legend.position="none", plot.title=element_text(size = rel(1.5))) +
          scale_y_continuous(labels = scales::percent, limits = c(0,1))
      }else{
        ggplot(profils.moy.tmp) +
          aes(x=time, y=y, col=as.factor(numeroClasse)) +
          geom_line(size=1.5) +
          xlab("Temps") +
          ylab("Taux de vélos disponibles") +
          ggtitle(paste("Profil moyen de la classe", input$classeSelect)) +
          theme_bw() +
          theme(legend.position="none", plot.title=element_text(size = rel(1.5))) +
          scale_colour_manual(values=cols) +
          scale_y_continuous(labels = scales::percent, limits = c(0,1))
      }
    }

  })
  
  output$radarPlot <- renderPlot({
    if(input$mailleClassifSelect == "A la journée"){
      if(input$typeSelect == "Jours de la semaine"){
        df.radar.tmp <- df.radar.j.lmmjv
      }else{
        df.radar.tmp <- df.radar.j.sd
      }
      df.radar.tmp$numeroClasse <- dense_rank(sapply(df.radar.tmp$classe, function(x) which(id.class.jour==x)))
    }else{
      df.radar.tmp <- df.radar.s
      df.radar.tmp$numeroClasse <- dense_rank(sapply(df.radar.tmp$classe, function(x) which(id.class.semaine==x)))
    }
    maxmin <- rbind.data.frame(sapply(df.radar.tmp[,!names(df.radar.tmp)%in%c("classe", "numeroClasse")], max),
                               sapply(df.radar.tmp[,!names(df.radar.tmp)%in%c("classe", "numeroClasse")], min))
    names(maxmin) <- names(df.radar.tmp)[!names(df.radar.tmp)%in%c("classe", "numeroClasse")]
    classes <- unique(df.radar.tmp$classe)
    if(input$classeSelect != "Toutes"){
      cla=input$classeSelect
      col=cols[df.radar.tmp[which(as.character(df.radar.tmp$classe)==cla),"numeroClasse"]]
      dat <- df.radar.tmp %>%
        filter(classe == cla) %>%
        select(-classe, -numeroClasse)
      dat <- rbind.data.frame(maxmin, dat)
      radarchart(dat, axistype=1, pty=16, plty=1, pdensity=50, caxislabels=rep("", 5),
                 axislabcol="grey", pcol=col, pfcol=col, na.itp=FALSE, title = paste0("Caractéristiques des stations de la classe ", cla),
                 vlabels = c("Emplois", "Habitations", "Jeunes & actifs",
                             "Accessibilite aux transports", "Enseignement"))
    }else{
      col=cols[df.radar.tmp$numeroClasse]
      dat <- df.radar.tmp %>%
        select(-classe, -numeroClasse)
      dat <- rbind.data.frame(maxmin, dat)
      radarchart(dat, axistype=1, pty=16, plty=1, pdensity=50, caxislabels=rep("", 5),
                 axislabcol="grey", pcol=col, pfcol=col, na.itp=FALSE, title = "Caractéristiques des stations",
                 vlabels = c("Emplois", "Habitations", "Jeunes & actifs",
                             "Accessibilite aux transports", "Enseignement"))
    }
  })

})
