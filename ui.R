#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(shinycssloaders)



# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # useShinyjs(),
        # extendShinyjs("www/kobe.js"),
        theme = "style.css",
        title = "Distribución de Tiros del Jugador según tipo de Tiro",
        tags$br(),
        tags$script(src="confetti.js"),
        fluidRow(id = "contenido",
                 tableOutput("prueba2"),
                 tabsetPanel(
                     tabPanel("Player Shot Analysis", class="tab",
            column(id = "cont_principal",
                width = 7,
                column(id = "filtro",
                       width = 3,
                       selectInput(
                           inputId = "jugador",
                           label = "Player",
                           choices =  datos_final2 %>%group_by(shoot.player)%>% summarize(n = n())%>% arrange(desc(n)) %>% pull(shoot.player),
                           selected = "Russell Westbrook"
                       ),
                       selectInput(
                           inputId = "tipo_tiro",
                           label = "Shot Type",
                           choices = c("All Shot Types",unique(datos_final2$shot.type)),
                           selected = "All Shot Types"
                           
                       )
                       #radioButtons("tipo_gráfico",label = "Tipo de Gráfico",selected = "" )
                ),
                div(id = "izquierda", width = 4,
                div(class="card z-depth-4",
                    h5("Shot Heatmap"),
                    #p("Mapa de calor de tiros. Cuanto más oscuro, más tira de esa posición."),
                    depth = 4,
                    valueBoxOutput("total_tiros"),
                    withSpinner(plotOutput("prueba"), type = 6, color = "#00263e")
                    )),
                div(id = "datos_vs_global", width = 4,
                       div(id = "info_jugador",
                           h5("Average Shooting Percentage. Player vs NBA Average"),
                           withSpinner(plotOutput("radar"), type = 6, color = "#00263e"),
                           htmlOutput("imagen_jugador"),
                           htmlOutput("imagen_equipo")
                       )),
                div(id = "derecha", width = 4,
                    h5("Field Goal Percentage by position"),
                    valueBoxOutput("probabilidad_acierto"),
                    withSpinner(plotOutput("porcentaje_acierto"), type = 6, color = "#00263e")
                )
            ) 
        ), #Cerramos el primer tabset
        tabPanel(title = "Player Comparison", class="tab",
                 column(   width = 3,   #Columna de la izquierda
                     class="ficha_jugador",
                        div(
                            selectInput(
                                inputId = "jugador1",
                                label = NULL,
                                choices =  datos_final2 %>%group_by(shoot.player)%>% summarize(n = n())%>% arrange(desc(n)) %>% pull(shoot.player),
                                selected = "Russell Westbrook"
                            ),
                            div(class= "foto_jugador", #Foto del jugador
                                htmlOutput("comp_imagen_jugador1", class="comp_imagen_jugador"),
                                htmlOutput("comp_imagen_equipo1", class="comp_imagen_equipo"),
                                htmlOutput("dorsal_jugador1", class="dorsal"),
                                htmlOutput("comparativa_estadisticas1_glob1", class="total")
                            ),
                            div(class="nombre",
                                withSpinner(htmlOutput("nombre_jugador1"), type = 6, color = "#00263e")
                                ),
                            tableOutput("comparativa_estadisticas1")
                        )
                        ), # Jugador1
                 column(width=6,
                     class= "resultado_comparativa",
                     div(id = "logos",
                             htmlOutput("comp_imagen_equipo3"),
                         p("VS", class ="vs"),
                             htmlOutput("comp_imagen_equipo4"),
                         h4(id ="comparativa","Player with best shoting average")
                         ),
                     div(
                         id = "mejor_jugador",
                         htmlOutput("comparativa_global"),
                         htmlOutput("comparativa_global_dorsal")
                         )
                 )
                 ,column(width = 3, #Columna de la derecha
                 class="ficha_jugador",

                 div(
                        selectInput(
                            inputId = "jugador2",
                            label = NULL,
                            choices =  datos_final2 %>%group_by(shoot.player)%>% summarize(n = n())%>% arrange(desc(n)) %>% pull(shoot.player),
                            selected = "Stephen Curry"
                        ),
                        div(
                            class= "foto_jugador", #Foto del jugador
                            htmlOutput("comp_imagen_jugador2", class="comp_imagen_jugador"),
                            htmlOutput("comp_imagen_equipo2", class="comp_imagen_equipo"),
                            htmlOutput("dorsal_jugador2", class="dorsal"),
                            htmlOutput("comparativa_estadisticas1_glob2", class="total")
                        ),
                        div(
                            class="nombre",
                            withSpinner(htmlOutput("nombre_jugador2"), type = 6, color = "#00263e")),
                        tableOutput("comparativa_estadisticas2")
                    )
                 ), #Jugador Derecha
                 div(id = "canvas2",
                 tags$canvas(id="canvas"),
                 tags$script(src = "confetti.js"),
                 tags$script(src = "prueba.js")
                 )
                     ),  #Cerramos el segundo tabPanel
        tabPanel("Mamba Forever", 
                 tags$script(src = "kobe.js"),
                 div(id ="fondo", div(id = "transparente")),
                 div(id = "graficos",
                     selectInput("año_kobe",label ="",choices = c("All Years",unique(kobe$Año))),
                 div(id="evolucion",
                     h4("Points per Match Average Evolution"),
                 plotlyOutput("kobe_evolution",  width = "350px")
                 ),div(id = "partidos",
                       h4("Points & Minutes played per match"),
                       plotlyOutput("kobe_puntos_año",  width = "350px")
                 )),
                 div(id = "derecha2",
                     div(id= "dato1",
                     h2(textOutput("num_1")),
                     h4(textOutput("txt_1"))),
                     hr(),
                     div(id= "dato2",
                        h2(textOutput("num_2")),
                        h4(textOutput("txt_2"))) 
                     ),
                 h4(id = "pruebamamba"," 1978 -",span(id="pruebamamba2","Kobe Bryant")," - 2020")
                 ) #Cerramos el tercer tabPanel
    )#Cerramos el tabsetPanel
        ) 
    
    )
    
)
