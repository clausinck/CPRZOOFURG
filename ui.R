#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(DT)

rotas<-read.csv("Data/rotas.csv")
cruzeiros<-read.csv("Data/cruzeiros.csv")
rotas<-read.csv("Data/rotas.csv")

shinyUI(fluidPage( #style="background-color:#00172D",
  navbarPage(
  title = 'CPR BRASIL',
  tabPanel(icon("home"),
          h2(p("Bem vindo ao portal de dados do CPR Brasil",style="text-align:center;color:white;background-color:#00172D;padding:20px;border-radius:20px")),
           br(),
          
          fluidPage(img(src = 'IMG_0680.jpg', height = '300px', width = '500px')),
          br(),
          br(),
           p("Through this application, it is intended to develop a learning environment for anyone who is starting in the study of statistical modeling, 
                                          specifically linear regression through the method of ordinary least squares. 
                                          In any case, we will focus on the procedure (graphics, interpretations, hypotheses, etc.) and not on the mathematical processes.", 
             strong("But do not worry!"), "you will find alternatives to learn all these technical aspects independently.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
           br(),

  ),
  
  tabPanel('Rotas',     leafletOutput("mymap")),
  
  tabPanel('Cruzeiros',  
    sidebarLayout(
    sidebarPanel(
      h3(p("Cruzeiros de coleta CPR")),
      p("Utilize os filtros para encontrar a coleta"),
        #checkboxGroupInput("show_vars", "Cruzeiros realizados:",
         #                  names(cruzeiros), selected = names(cruzeiros)),
        
        sliderInput("range", "Selecione um periodo:",
                    min = min(cruzeiros$Ano), max = max(cruzeiros$Ano),
                    value = c(min(cruzeiros$Ano), max = max(cruzeiros$Ano))),
        
        selectInput("dataset", "Selecione um navio:",
                    c("Todos",unique(as.character( cruzeiros$Navio)))),
      
      selectInput("dataset2", "Selecione uma rota:",
                  c("Todos",unique(as.character( rotas$Nome)))),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cruzeiros",dataTableOutput('tabelacruzeiro')),
        tabPanel("Rotas",dataTableOutput('tabelarotas'))
      ) ,
      )
    )
  ),
           

  
  tabPanel('Ferramentas',  
           sidebarLayout(
             sidebarPanel(
               h3(p("Calculadora de segmentos")),
               p("Para usar a calculadora de segmentos insira uma tabela do Excel com a seguinte formatacao"),
               p("Hora (hh:mm)"),
               p("Data (dd/mm/aaaa)"),
               p("Latitude (-00,0000) "),
               p("Longitude (+00,0000)"),
               p(br()),

                fileInput("file1", "Enviar arquivo .xlsx",
                         multiple = FALSE),
               p("Ou utilize um arquivo de teste clicanco aqui."),
               
               
               downloadButton("downloadData", "Download"),
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Coordenadas", dataTableOutput("contents")),
                 tabPanel("Segmentos", dataTableOutput("calculoseg")),
                 tabPanel("Plot", plotOutput("plot"))
               )
             )
           )
           
           
),
  
  
  tabPanel('Contato',       DT::dataTableOutput('ex4')),
  
  tabPanel(icon("gear"))
),
))
