library(tidyverse)
library(rvest)

## Busquedas basicas se pueden realizar directamente desde el script
## Busquedas avanzadas se realizar pegando en el script la url de resultados de la busqueda avanzada ya realizada

##################################### CONFIGURACION BASICA ######################################

# url del home la busqueda
home <- "https://repositoriosdigitales.mincyt.gob.ar"

# url de busqueda basica. 
searchUrl <- "https://repositoriosdigitales.mincyt.gob.ar/vufind/Search/Results?lookfor="

# opciones de filtro por campo de busqueda, elegir uno
campos <- list(todos = "type=AllFields", titulo ="type=Title", autor = "type=Author", materia ="type=Subject")

# palabras clave y operadores introducidos a la busqueda: el símbolo + une términos de búsqueda y/o operadores logicos AND, OR, NOT
# El operador %2B hace que se requiera el término a continuación.
# Así los resultados deben de figurar de ese término en algún campo del registro.
# es posible armar keysSearched con paste(x,y, sep = "&") pero me pareció que no era cómodo

############################################### TERMINOS DE BUSQUEDA ##############################################

keysSearched <- ""

# construir busqueda basica
basicSearch <- paste0(searchUrl, paste(keysSearched, campos$todos, sep = "&"))

# para busquedas complejas mejor ir al browser, armarla y copiar la url
keysSearched <- "salar*+-salar+-salares+AND+distribu*+OR+dispers*" #Todos los Campos:salar* -salares -salar OR ingreso* Y Todos los Campos:distribu* OR dispers*)
#para conocer terminos de busqueda de la url que sigue abajo 
advancedSearch<- "https://repositoriosdigitales.mincyt.gob.ar/vufind/Search/Results?sort=title&join=AND&lookfor0%5B%5D=salar*+-salares+-salar+OR+ingreso*&type0%5B%5D=AllFields&lookfor0%5B%5D=distribu*+OR+dispers*&type0%5B%5D=AllFields&bool0%5B%5D=AND&filter%5B%5D=%7Elanguage%3A%22spa%22&filter%5B%5D=%7Eeu_rights_str_mv%3A%22openAccess%22&filter%5B%5D=%7Ereponame_str%3A%22Biblioteca+Digital+%28UBA-FCE%29%22&filter%5B%5D=%7Ereponame_str%3A%22CONICET+Digital+%28CONICET%29%22&filter%5B%5D=%7Ereponame_str%3A%22Memoria+Acad%C3%A9mica+%28UNLP-FAHCE%29%22&filter%5B%5D=%7Ereponame_str%3A%22Repositorio+Digital+Universitario+%28UNC%29%22&filter%5B%5D=%7Ereponame_str%3A%22Repositorio+Institucional+%28UNSAM%29%22&filter%5B%5D=%7Ereponame_str%3A%22RIDAA+%28UNQ%29%22&filter%5B%5D=%7Ereponame_str%3A%22SEDICI+%28UNLP%29%22&daterange%5B%5D=publishDate&publishDatefrom=2000&publishDateto="

############################################# EXTRACCION ########################################################

# maxima cantidad de paginas de resultados a scrapear
max <- 20

links <- list()
titles <- list()
extra <- list() 

# en caso de querer guardar una busqueda básica y una avanzada, o muchas busquedas y dps repetir el scrapeo para cada una
# ademas es más comodo de tipear
busqueda <- list(basica = basicSearch, avanzada = advancedSearch)

# hay un if al final del loop que evita que sigas scrapeando dps del primer resultado vacío.
# no es lo óptimo creo pero lo que tengo x ahora
for (i in 1:max) {
    url <- paste(busqueda$avanzada , "&page=",
                 i, sep = "")
    htmlRepo <- read_html(url)
    links[i] <- htmlRepo %>% 
        html_nodes(".result .media .media-body .result-body .row .col-sm-12 .title") %>% 
        html_attr("href") %>% 
        list()
    titles[i] <- htmlRepo %>% 
        html_nodes(".result .media .media-body .result-body .row .col-sm-12 .title") %>% 
        html_text() %>% 
        list()
    extra[i] <- htmlRepo %>% 
        html_nodes(".result .media .media-body .result-body .row .col-xs-12") %>% 
        html_text() %>% 
        list() 
    # condicion que evalua si el resultado fue vacío
    if(is.na(links[[i]][1])) break
}

# pego la seccion de url al link obtenido para poder accederlo desde afuera del sitio
links <- unlist(links) %>% 
    paste0(home,.)

# vectorizo los titulos
titles <- unlist(titles) %>% 
    str_trim()

# transformo lista a matriz con las columnas de autores, fecha e idioma
extra <- unlist(extra) %>% 
    str_trim() %>% 
    matrix(ncol = 3, byrow = T, dimnames = list(NULL,c("autores", "fecha_de_publicacion", "idioma")))

extra[,1] <- extra[,1] %>% 
    str_remove("Autores:\n") %>% 
    str_remove_all("                    ") %>% 
    str_trim()

extra[,2] <- extra[,2] %>% 
    str_remove("Fecha de publicación:") %>% 
    str_remove("\\.") %>% 
    str_trim()

extra[,3] <- extra[,3] %>% 
    str_remove("Idioma:") %>%
    str_remove("\\.") %>% 
    str_trim()

# armo la tabla de resultados
resultados <- cbind(links, titles, extra) %>% 
    as.data.frame()

resultados$fecha_de_publicacion <-  resultados$fecha_de_publicacion %>% 
    as.character() %>% 
    as.numeric()

################################### GUARDADO DE RESULTADOS ########################################
write_csv(resultados, path = paste0(keysSearched,".csv"))
