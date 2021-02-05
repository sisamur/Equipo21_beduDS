## app.R ##

## Dash board para el data set 'adverstising'



#aqui empieza el shiny


library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)

#Esta parte es el anÃÂ¡logo al ui.R
ui <- 
    
    fluidPage(
        
        dashboardPage(
            skin = "green",
            
            dashboardHeader(title = "Predicción de Ventas",
                            
                            dropdownMenu(type="notifications",
                                         
                                    notificationItem(
                                             text = "Juan Luis, José Luis, Alfonso,María Dulce",
                                             icon("users")
                                         )
                                         
                                         
                                         )
            ),
         
            
            
            dashboardSidebar(
                
                sidebarMenu(
                   # menuItem("Boxplotdinamico", tabName = "btp", icon = icon("dashboard")),
                    #menuItem("DispersiÃÂ³n", tabName = "graph", icon = icon("area-chart")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                    menuItem("Gráficas", icon = icon("bar-chart-o"),
                             menuSubItem("Boxplot", tabName = "btp"),
                             menuSubItem("Histogramas", tabName = "histo"),
                             menuSubItem("Correlación", tabName = "corre"),
                             menuSubItem("Distribución", tabName = "dist"),
                             menuSubItem("Regresión", tabName = "reg"),
                             menuSubItem("Residuos", tabName = "resi"),
                             menuSubItem("Predicción", tabName = "pred"))
                    
                    
                    
                )
                
                
            ),
            
            dashboardBody(

                
                tabItems(

                    
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Tabla de datos")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    tabItem(tabName = "btp",
                            fluidRow(
                                titlePanel("Gráfica de cajas Medios de comunicación"), 
                                plotOutput("btp1", height = 650, width=650)
                            )
                    ),
                    
                    
                    tabItem(tabName = "histo",
                            fluidRow(
                                titlePanel("Histogramas"), 
                                plotOutput("histo1", height = 650, width=650)
                            )
                    ),
                            
                            
                            
                    tabItem(tabName = "corre",
                            fluidRow(
                                titlePanel("Coeficientes de correlación"), 
                                plotOutput("corre1", height = 650, width=650)
                            )
                    ),
                    
                    tabItem(tabName = "dist",
                            fluidRow(
                                titlePanel("Distribución"), 
                                plotOutput("paranu", height = 650, width=650)
                            )
                    ),
                    
                    tabItem(tabName = "reg",
                            fluidRow(
                                titlePanel("Modelo de Regresión"), 
                                plotOutput("model1", height = 650, width=650),
                                plotOutput("model2", height = 650, width=650)
                            )
                    ),
                    
                    tabItem(tabName = "resi",
                            fluidRow(
                                titlePanel("Residuales Estandarizados"), 
                                plotOutput("residuos1", height = 650, width=650)
                            )
                    ),
                    tabItem(tabName = "pred",
                            fluidRow(
                                titlePanel("Modelo de Predicción"), 
                                plotOutput("predic1", height = 650, width=650),
                                textOutput(outputId = "mi_texto")
                                
                            )
                    )
                    
                )
            )
        )
    )







#De aquÃ en adelante es la parte que corresponde al server

server <- function(input, output) {
    

    
    ###https://www.kaggle.com/sazid28/advertising.csv
    
    ## 1. Primero cargamos las librerias correspondientes para el proyecto
    
    suppressWarnings(suppressMessages(library(reshape2)))
    suppressWarnings(suppressMessages(library(ggplot2)))
    suppressWarnings(suppressMessages(library(dplyr)))
    suppressWarnings(suppressMessages(library(gridExtra)))
    
    
    ## 2. Definimos la ruta del dataset
    
    anuncios<-read.csv("Advertising.csv") #esto esta en documentos
    
## 3. Se quiere conocer la informaciÃ³n que tenemos en el dataframe cargado, 
    #     por ello primero se elimina la primer columna que contiene los datos.
    
    head(anuncios)
    anuncios <- anuncios[,-1]
    anuncios <- rename(anuncios, Ventas = sales, periodico = newspaper)
    
    
    ##4. Para conocer un poco mÃ¡s acerca de los datos haciendo un EDA y 
    #    observando sus caracterÃ­sticas
    
    str(anuncios); dim(anuncios)
    head(anuncios); summary(anuncios)
    #View(anuncios)
    
    ## 5. Se procede a realizar unas grÃ¡ficas que permitan conocer la 
    #     distribuciÃ³n de los datos, conociendo su media y su desviaciÃ³n estandar
    
    anuncios <- na.omit(anuncios)
    
    # Medidas de tendencia central
    
    media_ventas <- mean(anuncios$Ventas)
    mediana_ventas <- median(anuncios$Ventas)
    
    media_tv <- mean(anuncios$TV)
    mediana_tv <- median(anuncios$TV)
    
    media_radio <- mean(anuncios$radio)
    mediana_radio <- median(anuncios$radio)
    
    media_perio <- mean(anuncios$periodico)
    mediana_perio <- median(anuncios$periodico)
    
    ##6. Y para conocer si exiten algunos outliers, se realiza un 
    #    boxplot de cada una de las variables
    

      #  ggplot() +
       # aes(x=variable, y=value) + 
        #geom_boxplot(aes(fill=factor(variable))) +
        #ylab("Presupuesto en Publicidad ($)") +
        #xlab("") + 
        #labs(fill = "Medio de comunicaciÃ³n")
    
    
    output$btp1 <- renderPlot({
        anuncios.m1 = melt(anuncios, id.vars = c("Ventas"),
                           measure.vars = c("TV", "radio", "periodico"))
        
        anuncios.m1 %>%
        ggplot() +
            aes(x=variable, y=value) + 
            geom_boxplot(aes(fill=factor(variable))) +
            ylab("Presupuesto en Publicidad ($)") +
            xlab("") + 
            labs(fill = "Medio de comunicación")
    })
    
    
    ## 7. histogramas
    
    
    
    ## 7. histogramas
    
    
    output$histo1 <- renderPlot({

    p1 <- anuncios %>% 
        ggplot() +
        aes(Ventas) + 
        geom_histogram( aes(y=..density..), binwidth = 2,
                        colour="black", fill="white") +
        geom_density(alpha=.2, fill="#FF6666") +
        ggtitle("Ventas totales") +
        ylab("Frecuencia") +
        xlab("Ventas") + 
        theme_light() +
        geom_vline(xintercept = media_ventas, color = "red", size=0.5) +
        geom_label(aes(x = media_ventas, y = 0.115, 
                       label = media_ventas), fill = "#FF6666")
    
    p2 <- anuncios %>%
        ggplot() +
        aes(TV) +
        geom_histogram( aes(y=..density..), binwidth = 20, 
                        colour="black", fill="white") +
        geom_density(alpha=.2, fill="#FF6666") +
        ggtitle("Presupuesto en TV") +
        ylab("Frecuencia") +
        xlab("TV") + 
        theme_light() +
        geom_vline(xintercept = media_tv, color = "red", size=0.5) +
        geom_label(aes(x = media_tv, y = 0.0074, 
                       label = media_tv), fill = "#FF6666")
    
    p3 <- anuncios %>%
        ggplot() +
        aes(radio) + 
        geom_histogram( aes(y=..density..), binwidth = 3,
                        colour="black", fill="white") +
        geom_density(alpha=.2, fill="#FF6666") +
        ggtitle("Presupuesto en Radio") +
        ylab("Frecuencia") +
        xlab("Radio") + 
        theme_light()+
        geom_vline(xintercept = media_radio, color = "red", size=0.5)+
        geom_label(aes(x = media_radio, y = 0.04, 
                       label = media_radio), fill = "#FF6666")
    
    p4 <- anuncios %>%
        ggplot() +
        aes(periodico) +
        geom_histogram( aes(y=..density..), binwidth = 7,
                        colour="black", fill="white") +
        geom_density(alpha=.2, fill="#FF6666") +
        ggtitle("Presupuesto en Periodico") +
        ylab("Frecuencia") +
        xlab("Periodico") + 
        theme_light()+
        geom_vline(xintercept = media_perio, color = "red", size=0.5)+
        geom_label(aes(x = media_perio, y = 0.031, 
                       label = media_perio), fill = "#FF6666")
    
    # multiplot hisrogramas
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
    })
    
    
    
    ## 8. Correlaciones entre los datos
    
    
    output$corre1 <- renderPlot({
        
    anun.cor <- round(cor(anuncios), 3)
    
    anun.cor1 <- melt(anun.cor, id.vars = c("Ventas"),
                      measure.vars = c("TV", "radio", "periodico"))
    
    # heatmap de los coeficientes de correlacion
    
    anun.cor1 %>%
        ggplot() +
        aes(x = Var1, y = Var2) +
        geom_tile(aes(fill = value), colour = "black") + 
        scale_fill_gradient(low = "white", high = "blue") +
        ggtitle("Coeficientes de correlación") +
        geom_text(aes(label=value),  vjust=-0.3, size=3.5) + 
        theme_minimal() +
        labs(x='',y='', fill='Coeficiente de Correlación') 
    })
    
    ## 9. Correlacion entre variables
    
    # Antes de pensar en el modelado, es interesante observar 
    # como se distribuyen los datos en una grafica con respecto a las ventas
    
    output$paranu <- renderPlot({
        
        pairs(anuncios)
        
        
    })
    
    output$model1 <- renderPlot({
    
    p5 <- anuncios.m1 %>%
        ggplot()+ 
        aes(x=value,y=Ventas, color=variable) + 
        geom_point() 
    p5
    })
    
    output$model2 <- renderPlot({
    p6 <- anuncios.m1 %>%
        ggplot() + 
        aes(x=value,y=Ventas, color=variable) + 
        geom_point() +
        #modelo de regresion lineal 
        geom_smooth(method = "lm", se = F, colour = "black") + 
        labs(x='Presupuesto', 
             y='Ventas', 
             colour='Medio de comunicación') +
        facet_wrap("variable", scales="free_x")
    p6
    })
    

   # p5
    #p6
    
    # El incremento de presupuesto en television esta aparentemente relacionado 
    # con un aumento de ventas, lo mismo ocurre con el presupuesto en radio, 
    # sin embargo, las el incremento en el presupuesto en periodico aparentemente 
    # no esta relacionado con con aunemtos de ventas. Analizaremos las regresion
    # multiple para ver su comportamiento
    
    ## 10. Regresion lineal multiple
    
    # partimos de una prueba de hipotesis donde 
    # H0: beta1 = beta2 = beta3 = 0
    # es decir 
    # Ventas = beta0 + beta1*TV + beta2*radio + beta3*periodico + e
    # contra
    # H1: H0 no es verdad
    
    attach(anuncios)
    m1 <- lm(Ventas ~ TV + radio +  periodico)
    
    summary(m1)
    anova(m1)
    
    #el modelo presenta valores de p-value < 0.05, sin embargo, los valores del
    #p-value para el presupuesto en el periodico no es estadisticamente 
    #significativo por lo que reduciremos el modelo
    # Ventas = beta0 + beta1*TV + beta2*radio + e (Modelo Reducido)
    
    m2 <- update(m1, ~.-periodico)
    summary(m2)
    
    ## 11. Analisis de covarianza
    
    # Procederemos a realizar un modelo con interaccion para poder analizar como se
    # comportan las variables si tienen inteaccion entre ellas y como se pueden
    # afectar a las ventas. Consideramos un modelo completo 
    # Ventas = beta0 + beta1*TV + beta2*radio + beta3*periodico + beta4*TV*radio
    #          + beta5*TV*Periodico + beta6*radio*periodico + e  (modelo completo)
    
    mfull <- lm(Ventas ~ (TV + radio + periodico)**2 )
    
    summary(mfull)
    
    # podemos observar que los coeficientes de la interaccion entre las variables
    # son estadisticamente significativos, excepto en el coeficiente entre
    # periodico y radio, pues no son estadisticamente significativos. Para analizar
    # como afecta el modelo completo con el modelo reducido realizamos una prueba
    # de ANOVA con respecto a las siguientes hipotesis
    
    # H0: beta4 = beta5 = beta6 = 0
    # es decir,
    # Ventas = beta0 + beta1*TV + beta2*radio + e (Modelo Reducido)
    # contra
    # H1: H0 no es verdad
    # es decir, 
    # Ventas = beta0 + beta1*TV + beta2*radio + beta3*periodico + beta4*TV*radio
    #          + beta5*TV*Periodico + beta6*radio*periodico + e  (modelo completo)
    
    anova(m2,mfull)
    
    # Dado que el p-value es menor a 0.05, rechazamos la hipotesis nula, razon por
    # la que usaremos el modelo con todas sus interacciones para poder determinar 
    # las predicciones y los residuos. A partir de aqui podremos ver de que manera 
    # invertir en nuevos anuncios.
    
    ## 12. Validacion del modelo y analisis de residuales
    
    # Obtenemos los residuos estandarizados para todos los modelos y analizaremos
    # los residuales para determinar cual modelo cumple mejor la aleatoriedad
    
    StanRes1 <- rstandard(m1)
    StanRes2 <- rstandard(m2)
    StanRes3 <- rstandard(mfull)

    output$residuos1 <- renderPlot({
        
    p7 <- ggplot()+ 
        aes(x=m1$fitted.values, y=StanRes1) + 
        geom_point(colour = "Green", show.legend = FALSE) + 
        ggtitle("Residuales Estandarizados modelo 1") +
        ylab("Residuales") +
        xlab("Valores ajustados") + 
        theme_light()
    
    p8 <- ggplot()+ 
        aes(x=m2$fitted.values, y=StanRes2) + 
        geom_point(colour = "Blue", show.legend = FALSE) + 
        ggtitle("Residuales Estandarizados modelo reducido") +
        ylab("Residuales") +
        xlab("Valores ajustados") + 
        theme_light()
    
    p9 <- ggplot()+ 
        aes(x=mfull$fitted.values, y=StanRes3) + 
        geom_point(colour = "Red", show.legend = FALSE) + 
        ggtitle("Residuales Estandarizados modelo completo") +
        ylab("Residuales") +
        xlab("Valores ajustados") + 
        theme_light()
    
    p10 <- ggplot()+ 
        aes(sample = StanRes1) + 
        stat_qq(colour = "Green") +
        stat_qq_line() +
        ggtitle("QQ plot - modelo 1") +
        ylab("Residuales") +
        xlab("Teoricos") + 
        theme_light()
    
    p11 <- ggplot()+ 
        aes(sample = StanRes2) + 
        stat_qq(colour = "Blue") +
        stat_qq_line() +
        ggtitle("QQ plot - modelo reducido") +
        ylab("Residuales") +
        xlab("Teoricos") + 
        theme_light()
    
    p12 <- ggplot()+ 
        aes(sample = StanRes3) + 
        stat_qq(colour = "Red") +
        stat_qq_line() +
        ggtitle("QQ plot - modelo completo") +
        ylab("Residuales") +
        xlab("Teoricos") + 
        theme_light()
    
    
    grid.arrange(p7, p8, p9, 
                 p10, p11, p12, nrow = 2)
    })
    
    # A partir de la grafica podemos observar que el modelo completo es el que mejor 
    # se ajusta a un modelo homocedástico.
    
    ## 13. Predicciones
    
    # A partir del modelo completo es posible realizar predicciones de las ventas
    # obtenidas de acuerdo a la inversion realizada en periodico, radio y TV.
    output$predic1 <- renderPlot({
    
    ggplot() + 
        aes(x=mfull$fitted.values,y=Ventas) + 
        geom_point(colour = "Red") +
        geom_smooth(method = "lm", se = TRUE, colour = "Red") + 
        labs(x='Valores Ajustados', 
             y='Ventas') + 
        theme_light()
    
    })
    # Finalmente hacemos prediciones
    
    #Intervalos de confianza
    
    confint(mfull,level=0.95)
    
    #damos posibles valores de presupuesto para los diferentes medios
    
    predict.at = data.frame( TV = 1500, radio = 80, periodico = 50)
    
    # Las posibles ventas obtenidas a partir de los presupuestos asignados antes,
    # esta prediccion encuentra asociada un valor de incertidumbre
    
    b <- predict(mfull, newdata = predict.at, interval="prediction",level=0.95)
    
    output$mi_texto <- renderText({
    
    paste("Si invertimos", predict.at$TV, "mil dolares en TV,", predict.at$radio, 
          "mil dolares en radio, y", predict.at$periodico, 
          "mil dolares en periodico", "obtendremos entre", round(b[2] * 1000, 0), 
          "y", round(b[3] * 1000, 0) , "unidades vendidas de X producto")

        })

    
    # Como conclusion principal, se tiene que la mejor inversion en anuncios se 
    # encuentra en la television, sin embargo, es posible modelar y estimar el 
    # numero de ventas a partir de un modelo con variables pareadas. Es por eso,
    # que para obtener un mejor numero de ventas es mejor invertir más en anuncios
    # de television
    
    # Recordemos las palabras de George Box: "Todos los modelos son erroneos, pero
    # algunos son utiles".
        
        

        
        
        
        output$data_table <- renderDataTable( {anuncios}, 
                                              options = list(aLengthMenu = c(5,25,50),
                                                             iDisplayLength = 5)
        )
        
        

    
   
}


shinyApp(ui, server)
