library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
library(dplyr)

setwd("D:/documentos/Santander bedu/Fase 2/Repositorio/Evidencia/Programación y estadística con R Evidencias/Sesión 8")
datos<-read.csv("data/data.csv")
match<-read.csv("data/match.data.csv")



#Esta parte es el analogo al ui.R
ui <- 
  
  fluidPage(
    
    dashboardPage(
      
      dashboardHeader(title = "dashboard Momios"),
      
      dashboardSidebar(
        
        sidebarMenu(
          menuItem("Barras", tabName = "Barras", icon = icon("dashboard")),
          menuItem("Imagenes", tabName = "img", icon = icon("area-chart")),
          menuItem("Data Table", tabName = "data_table", icon = icon("table")),
          menuItem("Ganancias", tabName = "img_momio", icon = icon("file-picture-o"))
        )
        
      ),
      
      dashboardBody(
        
        tabItems(
          
          # Barras
          tabItem(tabName = "Barras",
                  fluidRow(
                    titlePanel("Grafico de barras"), 
                    selectInput("x", "Seleccione el valor de X",
                                choices = c("FTHG", "FTAG")),

                    box(plotOutput("plot1", width=650, height = 650))
                    

                  )
          ),
          
          tabItem(tabName = "img",
                  fluidRow(
                    titlePanel(h3("Imagenes correspondientes al postwork 3")),
                    img( src = "Prob_marg_golesxvisita.png", 
                         height = 350, width = 450),
                    img( src = "Prob_marg_golesxequipo.png", 
                         height = 350, width = 450),
                    img( src = "Prob_conjunta_golesanotados.png", 
                         height = 350, width = 450)
                  )
          ),
          
          tabItem(tabName = "data_table",
                  fluidRow(        
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                  )
          ), 
          
          tabItem(tabName = "img_momio",
                  fluidRow(
                    titlePanel(h3("Imagenes correspondientes a los momios de las apuestas")),
                    img( src = "Secuencia_apuestas_momios.png", 
                         height = 350, width = 550),
                    img( src = "Secuencia_apuestas_momios2.png", 
                         height = 350, width = 550)
                  )
          )
          
          
        )
        
        
      )
    )
  )

#De aqui en adelante es la parte que corresponde al server

server <- function(input, output) {
  library(ggplot2)

  
  #Grafico de Histograma

  output$plot1 <- renderPlot({
    
    x <- datos[,input$x]
    
    #summary(data)
    datos %>% ggplot(aes(x)) + 
      geom_bar() + 
      facet_wrap("AwayTeam") +
      labs(x =input$x, y = "Goles") + 
      ylim(0,50)  
    
    
  })
  
  #Data Table
  output$data_table <- renderDataTable( {match}, 
                                        options = list(aLengthMenu = c(10,25,50),
                                                       iDisplayLength = 10)
  )
  
}


shinyApp(ui, server)
