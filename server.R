library(shiny)
library(xlsx)
library(RODBC)
library(rnaturalearth)
library(utils)
library(leaflet)
library(ggplot2)

rotas<-read.csv("Data/rotas.csv")
cruzeiros<-read.csv("Data/cruzeiros.csv")


shinyServer(function(input, output) {
  
  
  output$mymap <- renderLeaflet({
    x<- read.csv("Data/LOGs.csv")
      m<-leaflet() %>%
      addTiles()
    for (i in unique(x$ID)) {
      m<- m %>%
          addPolylines(data = x[x$ID == i, ], 
                       lng = ~Longitude, 
                       lat = ~Latitude,
                       popup = paste0(cruzeiros$Cruzeiro[unique(x$CruzeiroID)]))
        
      }
      m
  })
  
  
  
  output$tabelacruzeiro <- renderDataTable(
         datatable({
           data<-cruzeiros
     if(input$dataset != "Todos" ){
       data <- data[data$Navio == input$dataset,]
     }
      if(input$dataset2 != "Todos"){
        data <- data[rotas$CruzeiroID[rotas$Nome == input$dataset2],]
      }
          #data <- data[(data$Ano>=input$range[1]&&data$Ano<=input$range[2]),]
      data
      } 
    )
  )
  
  
  
  output$tabelarotas <- renderDataTable(
    datatable(
      rotas,rownames = FALSE,options = list(pageLength = 10, dom = 'tip'),),
    
  )
  
  
  
  output$contents <- renderDataTable({
    req(input$file1)
    tryCatch(
      {
        df <- read.xlsx(input$file1$datapath,1,header = TRUE)
        df[,2]<-format(df[,2], "%H:%M") 
        df[,3:4]<-round(df[,3:4], digits = 4)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    datatable(df,rownames = FALSE,options = list(pageLength = 10, dom = 'tip'))
  })
  
  output$calculoseg <- renderDataTable({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.xlsx(input$file1$datapath,1,header = TRUE)
        df[,2]<-format(df[,2], "%H:%M") 
        df[,3:4]<-round(df[,3:4], digits = 4)
    c = 1
    contlinha = 1
    tabelao <- data.frame()
    while (c<=nrow(df)) {
      if (c<nrow(df)) {
        dif <- as.numeric(difftime(hora[c+1], hora[c], units = 'mins')) 
        tabelao[contlinha,1:2] <- df[c,3:4] 
        fatlat <- (df[c+1,3]-df[c,3])/dif 
        fatlon <- (df[c+1,4]-df[c,4])/dif 
        cont = 1
        contlinha <- contlinha + 1
        while (cont<dif) {     
          tabelao[contlinha,1] <- df[c,3]+(cont*fatlat)
          tabelao[contlinha,2] <- df[c,4]+(cont*fatlon)
          cont <- cont+1
          contlinha <- contlinha + 1
        }
      }
      else {  
        tabelao[contlinha,1:2] <- df[c,3:4]
      }
      c <- c+1
    } 
    tsegmento <- 5 
    segmento = 1 
    tabsegmento <- data.frame() 
    posseg <- data.frame() 
    c = 1
    cont = 0
    while (c<nrow(tabelao)) {
      tabsegmento[c,1:2] <- tabelao[c,1:2]
      tabsegmento[c,3:4] <- tabelao[c+1,1:2]
      tabsegmento[c,5] <- sqrt(((tabelao[c+1,1])-(tabelao[c,1]))^2+(((tabelao[c+1,2])-(tabelao[c,2]))
                                                                    *cos((((tabelao[c+1,1])+(tabelao[c,1]))/2)*pi/180))^2)*60 
      cont <- cont + tabsegmento[c,5] 
      tabsegmento[c,6] <- cont 
      if (cont>5){  
        posseg[segmento,1] <- segmento
        posseg[segmento,2:3] <- tabsegmento[c,1:2]
        segmento <- segmento+1
        tabsegmento[c,7] <- segmento
        cont = tabsegmento[c,5]
        tabsegmento[c,6] <- cont
      } else {tabsegmento[c,7] <- segmento 
      if(c+1==nrow(tabelao)){
        posseg[segmento,1] <- segmento
        posseg[segmento,2:3] <- tabsegmento[c,3:4]
      }
      }
      c <- c+1
    }
    segmentos <- data.frame()
    c = 1
    i = 1
    cont = 0
    while (i<=nrow(tabsegmento)) {
      if (i < nrow(tabsegmento)){
        if (c == tabsegmento[i,7]){
          cont = cont+1
        }else {
          segmentos[c,1:2] <- tabsegmento[i-cont,1:2]
          segmentos[c,3:4] <- tabsegmento[i-1,3:4]
          segmentos[c,5:6] <- tabsegmento[i-1,6:7]
          segmentos[c,7]<- tabsegmento[i-1,6]*1852*0.00016
          c = c+1
          cont = 0
        }
      }else {
        segmentos[c,1:2] <- tabsegmento[i-cont,1:2]
        segmentos[c,3:4] <- tabsegmento[i,3:4]
        segmentos[c,5:6] <- tabsegmento[i,6:7]
        segmentos[c,7]<- tabsegmento[i-1,6]*1852*0.00016
      }
      i <- i+1
    }
    segmentos[,c(1:5,7)]<-round(segmentos[,c(1:5,7)], digits = 4)
    row.names(segmentos) <- segmentos[,6]
    names(segmentos) <- c("Latitude inicial", "Longitude inicial", "Lat final", "Long final", "Tamanho (nm)","Segmento", "volume (m3)" )
      },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    )
    datatable(segmentos[,c(6,1,2,5,7)],rownames = FALSE,options = list(pageLength = 10, dom = 'tip'))
  })
  
  output$plot <- renderPlot({
    df <- read.xlsx(input$file1$datapath,1,header = TRUE)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    ggplot(data = world) +
      geom_sf()+
      coord_sf(xlim = c(min(df[,4])-5, max(df[,4])+5), ylim = c(min(df[,3])-5, max(df[,3])+5), expand = FALSE)+
      geom_line(data = df, aes(x=Longitude, y=Latitude))+
      annotation_scale(location = "br", width_hint = 0.5) 
  })
  

})
