{
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(rvest)
  library(lubridate)
  library(ggplot2)
}


# Extracción de datos de datos de un partido por id de partido
pass = "MYSPORTSFEEDS"
token = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxx"
game = "20200104-DET-GSW"
season = "2019-2020"

# We extract all the matches for a seson. We will this need to get query all play-by-play data.
url <- "https://api.mysportsfeeds.com/v2.1/pull/nba/2017-2018-regular/games.json"
r <- GET(url, authenticate(user = token, password = pass))
raise <- content(r, as="text")
new <- fromJSON(raise)

lista_partidos <- new[["games"]][["schedule"]][["id"]]
away <- new[["games"]][["schedule"]][["awayTeam"]][["abbreviation"]]
home <- new[["games"]][["schedule"]][["homeTeam"]][["abbreviation"]]
fecha <- new[["games"]][["schedule"]][["startTime"]]

partidos <- cbind(home,away,fecha)
partidos <- as.data.frame(partidos)

lista_partidos <- partidos %>%
  mutate(
    fecha = as.character(as.Date(ymd_hms(partidos$fecha))),
    fecha = gsub("-","", fecha),
    partido = paste(fecha,away,home, sep="-")
  ) %>%
  pull(partido)

length(lista_partidos)

rm(partidos, away, home, fecha)


# Now we just have to create a for loop that enables to query all that for each match.
# We will first append everything on a list and the unlist it. 

season = "2017-2018"
lista_partidos
datos_2017_2018 <- list()

for(i in 1164:length(lista_partidos)){
  url = paste0("https://api.mysportsfeeds.com/v2.1/pull/nba/2018-2019-regular/games/",lista_partidos[i],"/playbyplay.json?playtype=field-goal")
  r <- GET(url, authenticate(user = token, password = pass))
  raise <- content(r, as="text")
  new <- fromJSON(raise)
  jug <- new[["plays"]][["fieldGoalAttempt"]]
  
  datos_2017_2018[[i]] <- jug %>%
      filter(!is.na(team$id))%>%
      mutate(id = shootingPlayer$id,
             partido = lista_partidos[i], 
             temporada =  season)  
  print(i)
}


i=1
datos2 = NULL
for( i in 1:length(datos)){
  if(i == 1){
    datos2 <- datos[[i]]  
  }else{
    datos2 <- rbind(datos2,datos[[i]])
  }
  print(i)
}

# We already have the NBA shot data ready.

# Now we will scrpa data for Kobe Bryant visualization. 
# We will scrap the data from "basketball-reference".
# Kobe played from 1997 to 2016.

ligas = c(1997:2016)

for (i in 1:length(ligas)){
  ano = ligas[i]
  url = paste0("https://www.basketball-reference.com/players/b/bryanko01/gamelog/",ano)
  regular <- url %>% read_html() %>% html_node("#pgl_basic") %>% html_table(fill = T)
  regular <- regular[,c(1,2,28,29,10,22)]
  regular <- regular %>% filter(G != "") %>% filter(!(PTS %in% c("Did Not Play","Did Not Dress","Inactive","PTS","Not With Team","Player Suspended")))
  regular <- cbind(regular, "Regular",ano)  
  colnames(regular) <- c("Rank","Paritdo","Puntos","Score","Minutos_Jugados","Rebotes","Tipo","Año")
  
  if(!(ano %in% c(2005,2014,2015,2016))){
    playoffs <- url %>% read_html() %>% html_nodes(xpath = '//comment()') %>% html_text()%>%
      paste(collapse='') %>% read_html() %>%  html_node("#pgl_basic_playoffs") %>% html_table(fill=T)
    
    playoffs <- cbind(playoffs[,c(1,2,28,29,10,22)], "Playoffs",ano)
    playoffs <- playoffs %>% filter(G != "") %>% filter(!(PTS %in% c("Did Not Play","Did Not Dress","Inactive","PTS","Not With Team","Player Suspended")))
    colnames(playoffs) <- c("Rank","Paritdo","Puntos","Score","Minutos_Jugados","Rebotes","Tipo","Año")
    
    if(i == 1){
      kobe <- rbind(regular,playoffs)
    }else{
      x <- rbind(regular,playoffs)
      kobe <- rbind(kobe,x)
    }  
  } else{
    kobe <- rbind(kobe, regular)
  }
  
  print(ano)
  i = i + 1
}
write.csv(kobe, "kobe.csv")



