library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(shiny)
library(rvest)
library(plotly)
library(shinyjs)
library(cowplot)


    
shinyServer(function(input, output,session) {
    # observeEvent(input$tab, {
    #     onclick("kobe",js$kobe())    
    # })
    #ns <- NS(id)
    
    datos <- read.csv("datos/datos_final.csv", stringsAsFactors = FALSE)
    kobe <- read.csv("datos/kobe2.csv", stringsAsFactors = FALSE , encoding = "latin1", sep = ";")
    logros_kobe <- read.csv("datos/logros_kobe_en.csv", stringsAsFactors = FALSE, sep = ";", encoding = "latin1")
    url <-  read_html("https://github.com/anderfernandez/Basketball-Analytics-App/blob/master/prueba.html")
    players_html <- url %>% html_nodes("#LC1") %>% html_text() %>%  paste(collapse = '') %>% read_html()
    
    #Crear Campo
    {
        circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
            tt <- seq(start*pi, end*pi, length.out=npoints)
            data.frame(
                x = center[1] + diameter / 2 * cos(tt),
                y = center[2] + diameter / 2 * sin(tt)
            )
        }
        rev_y <- function(y) {94-y}
        # From x and y coordinates for a line (represented by a polygon here),
        # a number of group and a short description
        # creates a data.frame for this line
        # in order to use it with ggplot2.
        new_coords <- function(x, y, group, descri){
            new_coords_df <- data.frame(x = x, y = y)
            new_coords_df$group <- group
            new_coords_df$side <- 1
            group <- group + 1
            
            # The same thing for the opposite side
            new_coords_df2 <- data.frame(x = x, y = rev_y(y))
            new_coords_df2$group <- group
            new_coords_df2$side <- 2
            group <<- group + 1
            
            # On reunit les donnees
            new_coords_df <- rbind(new_coords_df, new_coords_df2)
            new_coords_df$descri <- descri
            
            return(new_coords_df)
        }
        # Restricted area
        cercle_np_out <- circle_fun(center = c(25,5+3/12), diameter = (4+1/6)*2)
        cercle_np_in <- circle_fun(center = c(25,5+3/12), diameter = 4*2)
        # Three point
        cercle_3pts_out <- circle_fun(center = c(25,5+3/12), diameter = (23+9/12)*2)
        cercle_3pts_in <- circle_fun(center = c(25,5+3/12), diameter = (23+7/12)*2)
        # Hoop
        cercle_ce <- circle_fun(center = c(25,5+3/12), diameter = 1.5)
        # Free Throws
        cercle_lf_out <- circle_fun(center = c(25,19), diameter = 6*2)
        cercle_lf_in <- circle_fun(center = c(25,19), diameter = (6-1/6)*2)
        # Center Circle
        cercle_mil_out <- circle_fun(center = c(25,47), diameter = 6*2)
        cercle_mil_in <- circle_fun(center = c(25,47), diameter = (6-1/6)*2)
        # Small Center Circle
        cercle_mil_petit_out <- circle_fun(center = c(25,47), diameter = 2*2)
        cercle_mil_petit_in <- circle_fun(center = c(25,47), diameter = (2-1/6)*2)
        group <- 1
        court <- new_coords(c(0-1/6,0-1/6,50 + 1/6,50 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "ligne de fond")
        court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne gauche"))
        court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne droite"))
        court <- rbind(court, new_coords(x = c(0,0,3,3), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur gauche"))
        court <- rbind(court, new_coords(x = c(47,47,50,50), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur droite"))
        court <- rbind(court, new_coords(x = c(3,3,3+1/6,3+1/6), y = c(0,14,14,0), group = group, descri = "3pts bas gauche"))
        court <- rbind(court, new_coords(x = c(47-1/6,47-1/6,47,47), y = c(0,14,14,0), group = group, descri = "3pts bas droit"))
        court <- rbind(court, new_coords(x = c(17,17,17+1/6,17+1/6), y = c(0,19,19,0), group = group, descri = "LF bas gauche"))
        court <- rbind(court, new_coords(x = c(33-1/6,33-1/6,33,33), y = c(0,19,19,0), group = group, descri = "LF bas droit"))
        court <- rbind(court, new_coords(x = c(17,17,33,33), y = c(19-1/6,19,19,19-1/6), group = group, descri = "LF tireur"))
        court <- rbind(court, new_coords(x = c(14-1/6,14-1/6,14,14), y = c(0,1/2,1/2,0), group = group, descri = "marque fond gauche"))
        court <- rbind(court, new_coords(x = c(36,36,36+1/6,36+1/6), y = c(0,1/2,1/2,0), group = group, descri = "marque fond droit"))
        court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "LF gauche interieur"))
        court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "LF droite interieur"))
        court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "planche"))
        court <- rbind(court, new_coords(x = c(cercle_3pts_out[31:220,"x"], rev(cercle_3pts_in[31:220,"x"])),
                                         y = c(cercle_3pts_out[31:220,"y"], rev(cercle_3pts_in[31:220,"y"])), group = group, descri = "cercle 3pts"))
        court <- rbind(court, new_coords(x = c(cercle_np_out[1:250,"x"], rev(cercle_np_in[1:250,"x"])),
                                         y = c(cercle_np_out[1:250,"y"], rev(cercle_np_in[1:250,"y"])), group = group, descri = "cercle non passage en force"))
        court <- rbind(court, new_coords(x = c(20+1/6,20+1/6,20+8/12,20+8/12), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas gauche cercle LF"))
        court <- rbind(court, new_coords(x = c(30-8/12,30-8/12,30-1/6,30-1/6), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas droite cercle LF"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[1:250,"x"], rev(cercle_lf_in[1:250,"x"])),
                                         y = c(cercle_lf_out[1:250,"y"], rev(cercle_lf_in[1:250,"y"])), group = group, descri = "cercle LF haut"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[250:269,"x"], rev(cercle_lf_in[250:269,"x"])),
                                         y = c(cercle_lf_out[250:269,"y"], rev(cercle_lf_in[250:269,"y"])), group = group, descri = "cercle LF partie 1"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[288:308,"x"], rev(cercle_lf_in[288:308,"x"])),
                                         y = c(cercle_lf_out[288:308,"y"], rev(cercle_lf_in[288:308,"y"])), group = group, descri = "cercle LF partie 2"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[327:346,"x"], rev(cercle_lf_in[327:346,"x"])),
                                         y = c(cercle_lf_out[327:346,"y"], rev(cercle_lf_in[327:346,"y"])), group = group, descri = "cercle LF partie 3"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[365:385,"x"], rev(cercle_lf_in[365:385,"x"])),
                                         y = c(cercle_lf_out[365:385,"y"], rev(cercle_lf_in[365:385,"y"])), group = group, descri = "cercle LF partie 4"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[404:423,"x"], rev(cercle_lf_in[404:423,"x"])),
                                         y = c(cercle_lf_out[404:423,"y"], rev(cercle_lf_in[404:423,"y"])), group = group, descri = "cercle LF partie 5"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[442:462,"x"], rev(cercle_lf_in[442:462,"x"])),
                                         y = c(cercle_lf_out[442:462,"y"], rev(cercle_lf_in[442:462,"y"])), group = group, descri = "cercle LF partie 6"))
        court <- rbind(court, new_coords(x = c(cercle_lf_out[481:500,"x"], rev(cercle_lf_in[481:500,"x"])),
                                         y = c(cercle_lf_out[481:500,"y"], rev(cercle_lf_in[481:500,"y"])), group = group, descri = "cercle LF partie 7"))
        court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF gauche"))
        court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF gauche"))
        court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF gauche"))
        court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF gauche"))
        court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF droite"))
        court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF droite"))
        court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF droite"))
        court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF droite"))
        court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "ligne mediane"))
        court <- rbind(court, new_coords(x = c(cercle_mil_out[250:500,"x"], rev(cercle_mil_in[250:500,"x"])),
                                         y = c(cercle_mil_out[250:500,"y"], rev(cercle_mil_in[250:500,"y"])), group = group, descri = "cercle milieu grand"))
        court <- rbind(court, new_coords(x = c(cercle_mil_petit_out[250:500,"x"], rev(cercle_mil_petit_in[250:500,"x"])),
                                         y = c(cercle_mil_petit_out[250:500,"y"], rev(cercle_mil_petit_in[250:500,"y"])), group = group, descri = "cercle milieu petit"))
        court <- rbind(court, new_coords(x = cercle_ce[,"x"], y = cercle_ce[,"y"], group = group, descri = "anneau"))
        rotate_court <- function(court, theta=pi/2){
            court_r <- court
            court_r$x <- court_r$x / 180 * pi
            court_r$y <- court_r$y / 180 * pi
            matrice_r <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
            coords_r <- apply(court_r[,c("x","y")], 1, function(x) x %*% matrice_r)
            court_r$x <- coords_r[1,] ; court_r$y <- coords_r[2,]
            court_r$x <- court_r$x * 180 / pi
            court_r$y <- court_r$y * 180 / pi
            return(court_r)
        }
    } 
    
    #Crear Campo c(input$jugador,input$tipo_tiro),
    datos_jugador <- reactive({
        if(input$tipo_tiro != "All Shot Types"){
            datos <- datos %>% filter(shot.type %in% input$tipo_tiro)
        }
        
        datos %>%
            filter(shoot.player %in% input$jugador) %>%
            mutate(
                location.x = ifelse(location.x<470,location.x, 940-location.x)/10,
                location.y = ifelse(location.x<470,location.y, 500-location.y)/10,
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(location.x,location.y, tipo_tiro, shot.type) %>%
            summarize(total_tiros = n()) %>%
            arrange(desc(total_tiros)) %>%
            #select(-shot.type)%>%
            pivot_wider(names_from = tipo_tiro, values_from = total_tiros) %>%
            mutate(
                Scored = ifelse(is.na(Scored),0,Scored),
                Not_Scored = ifelse(is.na(Not_Scored),0,Not_Scored),
                total_tiros = Scored + Not_Scored,
                probabilidad_acierto = Scored/total_tiros * 100
            ) %>%
            select(location.x,location.y,probabilidad_acierto,total_tiros,Scored, shot.type)
        
        
    }) 
    output$prueba <- renderPlot({
        datos2 <- datos_jugador()
        gg <- datos2 %>%
            # expand(count = seq(1:total_tiros),shot.type = shot.type) %>%
            # select(location.x,location.y, shot.type)%>%
            ggplot() +
            #geom_hex(aes(location.y-50,location.x))+
            stat_density_2d(geom = "polygon", aes(location.y-50,location.x,  alpha = ..level.., fill = shot.type)) +
            #geom_point(aes(location.y-50,location.x, size =  total_tiros, fill  = probabilidad_acierto, alpha = 0.5))+
            scale_colour_brewer(palette="BuPu") +
            #scale_fill_gradient(low = " #8dc8e8", high = "#00263e")+ 
            geom_polygon(data = rotate_court(court[court$side==1,], theta = pi/2), aes(x = y, y = x, group = group), col = "gray") +
            xlim(-2,50) + ylim(-55,2) +
            scale_x_continuous(breaks = c(0, 23.5, 47)) +
            scale_y_continuous(breaks = c(0, -12.5, -25, -37.5, -50)) +
            xlab("") + ylab("") +theme_minimal() +theme_nothing() + theme(legend.position = "none") #+ coord_equal(clip = "off")  
        gg
    })
    
    output$porcentaje_acierto <- renderPlot({
        datos2 <- datos_jugador()
        
        gg2 <- datos2 %>%
            #mutate(probabilidad_acierto = round(probabilidad_acierto*10), count = 1)%>%
            #expand(count = seq(1:probabilidad_acierto),shot.type = shot.type) %>%
            group_by(location.x,location.y, shot.type) %>%
            mutate(probabilidad_acierto = list(seq.int(0,probabilidad_acierto*10))) %>%
            unnest() %>%
            select(location.x,location.y, shot.type)%>%
            ggplot() +
            #geom_hex(aes(location.y-50,location.x, stat = ..density..)) +
            #scale_fill_gradient(low = " #8dc8e8", high = "#00263e")
            stat_density_2d(geom = "polygon",aes(location.y-50,location.x, alpha = ..level.., fill = shot.type)) +
            scale_colour_brewer(palette="BuPu") +
            geom_polygon(data = rotate_court(court[court$side==1,], theta = pi/2), aes(x = y, y = x, group = group), col = "gray") +
            xlim(-2,50) + ylim(-55,2) +
            scale_x_continuous(breaks = c(0, 23.5, 47)) +
            scale_y_continuous(breaks = c(0, -12.5, -25, -37.5, -50)) +
            xlab("") + ylab("") +theme_minimal() + theme_nothing() + theme(legend.position = "none")
        gg2
    })
    
    
    output$probabilidad_acierto <- renderValueBox({
        datos_jug <- datos_jugador()
        prob <- datos_jug %>% 
            mutate(prueba = 1) %>%
            group_by(prueba) %>%
            summarize(total = sum(total_tiros), acertados = sum(Scored)) %>%
            mutate(probabilidad_acierto2 = acertados/total * 100) %>%
            pull(probabilidad_acierto2) 
        
        valueBox(paste0(round(prob,2),"%"),"Field Goal Percentage")
    })
    output$total_tiros <- renderValueBox({
        datos_jug <- datos_jugador()
        prob <- datos_jug %>% 
            mutate(prueba = 1) %>%
            group_by(prueba) %>%
            summarize(total = sum(total_tiros)) %>%
            pull(total) 
        
        
        valueBox(round(prob,2),"Total Shots")
    })
    output$imagen_jugador <- renderText({
         
        jugador <- input$jugador
        nodo <- paste0('a.playerList.row[title =" ',jugador,' "] .lazyload')
        img <- players_html %>% html_nodes(nodo) %>% html_attr("data-src")
        c('<img src="',img,'">')
    })
    
    output$imagen_equipo <- renderText({
        jugador <- input$jugador
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        url2 <- read_html(url) %>% html_node(".nba-detail-header__team-logo") %>% html_attr("href")
        url3 <- paste0("https://www.nba.com",url2)
        img <- read_html(url3) %>% html_node(".nba-player-index__team-image img") %>% html_attr("src")
        c('<img src="',img,'">')
    })
    
    datos_jugador2 <- eventReactive(input$jugador1, {
        #datos <- datos_jugador()
        datos %>%
            filter(shoot.player %in% input$jugador2) %>%
            mutate(
                location.x = ifelse(location.x<470,location.x, 940-location.x)/10,
                location.y = ifelse(location.x<470,location.y, 500-location.y)/10,
                tipo_tiro = ifelse(current.shot.outcome=="Scored","Scored","Not_Scored")
            ) %>%
            group_by(location.x,location.y, tipo_tiro, shot.type) %>%
            summarize(total_tiros = n()) %>%
            arrange(desc(total_tiros)) %>%
            #select(-shot.type)%>%
            pivot_wider(names_from = tipo_tiro, values_from = total_tiros) %>%
            mutate(
                Scored = ifelse(is.na(Scored),0,Scored),
                Not_Scored = ifelse(is.na(Not_Scored),0,Not_Scored),
                total_tiros = Scored + Not_Scored,
                probabilidad_acierto = Scored/total_tiros * 100
            ) %>%
            select(location.x,location.y,probabilidad_acierto,total_tiros,Scored, shot.type)
        
    })
    
    ###########
    # PARTE 2 #
    ###########
    
    #Jugador 1
    output$comparativa_estadisticas1 <- renderTable({
        #datos_jug <- datos_jugador()
        datos %>%
            filter(shoot.player %in% input$jugador1) %>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")) %>%
            group_by(shot.type,tipo_tiro) %>%
            summarize(
                tiros_posicion = n()) %>%
            mutate(
                total_tiros = sum(tiros_posicion),
                probabilidad = paste0(round(tiros_posicion/total_tiros * 100),"%")) %>%
            filter(tipo_tiro == "Scored") %>%
            select(shot.type, probabilidad)
    })
    
    output$comp_imagen_equipo1 <- renderText({
        jugador <- input$jugador1
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        url2 <- read_html(url) %>% html_node(".nba-detail-header__team-logo") %>% html_attr("href")
        url3 <- paste0("https://www.nba.com/",url2)
        img <- read_html(url3) %>% html_node(".nba-player-index__team-image img") %>% html_attr("src")
        c('<img src="',img,'">')
    })
    
    output$comp_imagen_equipo3 <- renderText({
        jugador <- input$jugador1
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        url2 <- read_html(url) %>% html_node(".nba-detail-header__team-logo") %>% html_attr("href")
        url3 <- paste0("https://www.nba.com",url2)
        img <- read_html(url3) %>% html_node(".nba-player-index__team-image img") %>% html_attr("src")
        c('<img src="',img,'">')
    })
    
    output$comp_imagen_jugador1 <- renderText({
        jugador <- input$jugador1
         
        nodo <- paste0('a.playerList.row[title =" ',jugador,' "] .lazyload')
        img <- players_html %>% html_nodes(nodo) %>% html_attr("data-src")
        #img <- read_html(url) %>% html_node(".nba-player-header__item img") %>% html_attr("src")
        c('<img src="',img,'">')
    })
    output$dorsal_jugador1 <- renderText({
        jugador <- input$jugador1
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        dorsal <- read_html(url) %>% html_node(".nba-player-header__jersey-number") %>% html_text()
        dorsal
    })
    output$nombre_jugador1 <- renderText({
        jugador <- input$jugador1
        jugador
    })
    
    output$comparativa_estadisticas1_glob1 <- renderText({
        #datos_jug <- datos_jugador()
        datos %>%
            filter(shoot.player %in% input$jugador1)%>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(tipo_tiro) %>%
            summarize(
                tiros_posicion = n(),
            ) %>%
            mutate(
                total_tiros = sum(tiros_posicion),
                probabilidad = paste0(round(tiros_posicion/total_tiros * 100,2),"%")
            ) %>%
            filter(tipo_tiro == "Scored") %>%
            pull(probabilidad)
        
    })
    
    #Jugador 2
    output$comparativa_estadisticas2 <- renderTable({
        #datos <- datos_jugador()
        datos %>%
            filter(shoot.player %in% input$jugador2) %>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(shot.type,tipo_tiro) %>%
            summarize(
                tiros_posicion = n(),
            ) %>%
            mutate(
                total_tiros = sum(tiros_posicion),
                probabilidad = paste0(round(tiros_posicion/total_tiros * 100),"%")
            ) %>%
            filter(tipo_tiro == "Scored") %>%
            select(shot.type, probabilidad)
    })
    output$comp_imagen_equipo2 <- renderText({
        jugador <- input$jugador2
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        url2 <- read_html(url) %>% html_node(".nba-detail-header__team-logo") %>% html_attr("href")
        url3 <- paste0("https://www.nba.com",url2)
        img <- read_html(url3) %>% html_node(".nba-player-index__team-image img") %>% html_attr("src")
        c('<img src="',img,'">')
    })
    
    output$comp_imagen_equipo4 <- renderText({
        jugador <- input$jugador2
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        url2 <- read_html(url) %>% html_node(".nba-detail-header__team-logo") %>% html_attr("href")
        url3 <- paste0("https://www.nba.com",url2)
        img <- read_html(url3) %>% html_node(".nba-player-index__team-image img") %>% html_attr("src")
        c('<img src="',img,'">')
    })
    
    
    output$comp_imagen_jugador2 <- renderText({
        jugador <- input$jugador2
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        img <- read_html(url) %>% html_node(".nba-player-header__item img") %>% html_attr("src")
        c('<img src="',img,'">')
    })
    output$dorsal_jugador2 <- renderText({
        jugador <- input$jugador2
         
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        dorsal <- read_html(url) %>% html_node(".nba-player-header__jersey-number") %>% html_text()
        dorsal
    })
    output$nombre_jugador2 <- renderText({
        jugador <- input$jugador2
        jugador
    })
    output$comparativa_estadisticas1_glob2 <- renderText({
        #datos <- datos_jugador()
        datos %>%
            filter(shoot.player %in% input$jugador2)%>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(tipo_tiro) %>%
            summarize(
                tiros_posicion = n(),
            ) %>%
            mutate(
                total_tiros = sum(tiros_posicion),
                probabilidad = paste0(round(tiros_posicion/total_tiros * 100,2),"%")
            ) %>%
            filter(tipo_tiro == "Scored") %>%
            pull(probabilidad)
        
    })
    
    #Comparativa entre jugadores
    output$comparativa_global <- renderText({
        #datos_jug <- datos_jugador()
        datos %>%
            filter(shoot.player %in% c(input$jugador2,input$jugador1))%>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(tipo_tiro, shoot.player) %>%
            summarize(
                tiros_posicion = n(),
            ) %>%
            pivot_wider(shoot.player, names_from = tipo_tiro, values_from = tiros_posicion) %>%
            mutate(
                total_tiros = Scored + Not_Scored,
                probabilidad = paste0(round(Scored/total_tiros * 100,2),"%")
            ) %>%
            top_n(1, probabilidad) %>%
            pull(shoot.player)
    })
    
    output$comparativa_global_dorsal <- renderText({
        #datos_jug <- datos_jugador()
        jugador <- datos %>%
            filter(shoot.player %in% c(input$jugador2,input$jugador1))%>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(tipo_tiro, shoot.player) %>%
            summarize(
                tiros_posicion = n(),
            ) %>%
            pivot_wider(shoot.player, names_from = tipo_tiro, values_from = tiros_posicion) %>%
            mutate(
                total_tiros = Scored + Not_Scored,
                probabilidad = paste0(round(Scored/total_tiros * 100,2),"%")
            ) %>%
            top_n(1, probabilidad) %>%
            pull(shoot.player)
        
        nodo <- paste0("a.playerList.row[title = ' ",jugador," '","]")
         
        url <- players_html %>% html_nodes(nodo) %>% html_attr("href")
        url <- paste0("https://www.nba.com",url)
        dorsal <- read_html(url) %>% html_node(".nba-player-header__jersey-number") %>% html_text()
        dorsal
    })
    
    output$radar <- renderPlot({
        tipo_tiro <- input$tipo_tiro
        jugador <- datos %>%
            filter(shoot.player == input$jugador)%>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(tipo_tiro,shot.type) %>%
            summarize(
                total_tiros_tipo = n()
            ) %>%
            pivot_wider(shot.type,names_from = tipo_tiro, values_from = total_tiros_tipo)%>%
            mutate(
                Scored = ifelse(is.na(Scored),0,Scored),
                Not_Scored = ifelse(is.na(Not_Scored),0,Not_Scored),
                total_tiros = Scored + Not_Scored,
                percentage = Scored/total_tiros *10,
                tipo = "Player"
            ) %>%
            select(shot.type,percentage, tipo) 
        
        media <- datos %>%
            mutate(
                tipo_tiro = ifelse(current.shot.outcome=="SCORED","Scored","Not_Scored")
            ) %>%
            group_by(tipo_tiro,shot.type) %>%
            summarize(
                total_tiros_tipo = n()
            ) %>%
            pivot_wider(shot.type,names_from = tipo_tiro, values_from = total_tiros_tipo)%>%
            mutate(
                Scored = ifelse(is.na(Scored),0,Scored),
                Not_Scored = ifelse(is.na(Not_Scored),0,Not_Scored),
                total_tiros = Scored + Not_Scored,
                percentage = Scored/total_tiros *10,
                tipo = "Global NBA Average"
            ) %>%
            select(shot.type,percentage, tipo)
        
        
        if(tipo_tiro != "All Shot Types"){
            jugador<- jugador %>%
                mutate(visb = ifelse(shot.type == tipo_tiro,1,0.2))
            
            media<- media %>%
                mutate(visb = ifelse(shot.type == tipo_tiro,1,0.2))
            
        }else{
            jugador <- jugador %>%
                mutate(visb = 1)
            media <- media %>%
                mutate(visb = 1)
        }
        
        grafico <- rbind(jugador, media)
        
        grafico%>%
            ggplot(aes(shot.type, percentage, fill= tipo, alpha = visb)) + geom_col(position = "dodge") +  
            coord_polar() +
            theme_minimal() + scale_alpha(guide = 'none') +
            theme(
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                panel.background = element_blank(),
                plot.background = element_blank(),
                legend.position = "bottom"
            ) + labs(fill = "")
        
        
    },bg="transparent")
    
    ########
    # KOBE #
    ########
    
    output$kobe_evolution <- renderPlotly({
        
        if(input$ano_kobe != "All Years"){
            kobe3<- kobe %>%
                mutate(visb = ifelse(Ano == input$ano_kobe,1,0.2),
                       visb= as.factor(visb))
        
        }else{
            kobe3<- kobe %>%
                mutate(visb = 1)
        }
        
        # kobe3$segundos <- as.numeric(gsub(".+:","",kobe3$Minutos_Jugados)) /60
        # kobe3$minutos <- as.numeric(gsub(":.+","",kobe3$Minutos_Jugados))
        
        ggkobe <- kobe3 %>%
            mutate(
                #minutos = as.numeric(substr(Minutos_Jugados, 1, 2)),
                #segundos = as.numeric(substitute(Minutos_Jugados, 4,5)),
                segundos = as.numeric(gsub(".+:","",Minutos_Jugados)) /60,
                minutos = as.numeric(gsub(":.+","",Minutos_Jugados)),
                total_minutos = segundos + minutos
            ) %>%
            group_by(Ano, visb) %>%
            summarize(casos = n(),
                      Puntos =sum(Puntos),
                      minutos = sum(total_minutos)
            ) %>%
            mutate(
                Points_per_Match = Puntos/casos,
                PPM = Puntos/minutos,
                Year = Ano
            ) %>%
            ggplot(aes(Year,Points_per_Match, alpha = visb)) + geom_col(fill= "#FDB927") +
            theme(
                panel.background = element_rect(fill = "transparent"), # bg of the panel
                plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                panel.grid.major = element_blank(), # get rid of major grid
                panel.grid.minor = element_blank(), # get rid of minor grid
                legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
                axis.text.x = element_text(angle = 90, hjust = 1,colour="white"),
                axis.text.y = element_text(colour="white")
            ) + scale_x_continuous(breaks = seq(1997,2016)) + scale_y_continuous(breaks= seq(0,35,5)) +
            labs(x = "", y = "")
        ggplotly(ggkobe) %>% layout(plot_bgcolor='transparent') %>% 
            layout(paper_bgcolor='transparent') %>% config(displayModeBar = F, showscale=FALSE )
    })#,bg="transparent")
    
    
    output$kobe_puntos_ano <- renderPlotly({
        
        if(input$ano_kobe != "All Years"){
            kobe2 <- kobe %>%
                filter(Ano == input$ano_kobe)
        }else{
            kobe2 <- kobe
        }
        
        # kobe2$segundos <- as.numeric(gsub(".+:","",kobe2$Minutos_Jugados)) /60
        # kobe2$minutos <- as.numeric(gsub(":.+","",kobe2$Minutos_Jugados))
        
        ggkobe2 <- kobe2 %>%
            mutate(
                #minutos = as.numeric(substr(Minutos_Jugados, 1, 2)),
                #segundos = as.numeric(substitute(Minutos_Jugados, 4,5)),
                segundos = as.numeric(gsub(".+:","",Minutos_Jugados)) /60,
                minutos = as.numeric(gsub(":.+","",Minutos_Jugados)),
                Minutes_Played = segundos + minutos,
                Points = Puntos,
                Year = Ano
            ) %>%
            
            ggplot(aes(Minutes_Played,Points, size = Points)) + geom_point(alpha = 0.4,col= "#FDB927") +
            theme(legend.position = "none",     panel.background = element_rect(fill = "transparent"), # bg of the panel
                  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                  panel.grid.major = element_blank(), # get rid of major grid
                  panel.grid.minor = element_blank(), # get rid of minor grid
                  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                  legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg)
                  axis.text.x = element_text(colour="white"), axis.text.y = element_text(colour="white")
            ) + scale_x_continuous(breaks = seq(0,50,10)) + labs(x="Minutos Jugados", y = "Puntos Totales") +
            scale_fill_manual()
        
        
        ggplotly(ggkobe2) %>% layout(plot_bgcolor='transparent') %>% 
            layout(paper_bgcolor='transparent') %>% config(displayModeBar = F,  showscale=FALSE )
    })
    
    ##Textos Kobe
    output$num_1 <- renderText({
        dato <- logros_kobe %>%
            filter(Ano == input$ano_kobe & Import == 1) %>%
            pull(Texto)
        paste(dato)
    })
    
    output$num_2 <- renderText({
        dato <- logros_kobe %>%
            filter(Ano == input$ano_kobe & Import == 2) %>%
            pull(Texto)
        paste(dato)
    })
    
    output$txt_1 <- renderText({
        dato <- logros_kobe %>%
            filter(Ano == input$ano_kobe & Import == 1) %>%
            pull(Texto2)
        paste(dato)
    })
    
    output$txt_2 <- renderText({
        dato <- logros_kobe %>%
            filter(Ano == input$ano_kobe & Import == 2) %>%
            pull(Texto2)
        paste(dato)
    })
    
})
