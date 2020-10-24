library(tidyverse)
library(rvest)
#busqueda 1
#terminos clave: actividad*;salar*

#url de la home
home <- "https://repositoriosdigitales.mincyt.gob.ar"
#url de la búsqueda, copiar de browser y pegar # se podría armar un generador de url
root<- "https://repositoriosdigitales.mincyt.gob.ar/vufind/Search/Results?filter%5B%5D=~language%3A%22spa%22&filter%5B%5D=~format%3A%22article%22&filter%5B%5D=~format%3A%22workingPaper%22&filter%5B%5D=~format%3A%22report%22&filter%5B%5D=~format%3A%22bookPart%22&filter%5B%5D=~eu_rights_str_mv%3A%22openAccess%22&filter%5B%5D=~reponame_str%3A%22Biblioteca+Digital+%28UBA-FCE%29%22&filter%5B%5D=~reponame_str%3A%22CONICET+Digital+%28CONICET%29%22&filter%5B%5D=~reponame_str%3A%22Filo+Digital+%28UBA-FFyL%29%22&filter%5B%5D=~reponame_str%3A%22Memoria+Acad%C3%A9mica+%28UNLP-FAHCE%29%22&filter%5B%5D=~reponame_str%3A%22RepHipUNR+%28UNR%29%22&filter%5B%5D=~reponame_str%3A%22Repositorio+Digital+San+Andr%C3%A9s+%28UdeSa%29%22&filter%5B%5D=~reponame_str%3A%22Repositorio+Digital+Universitario+%28UNC%29%22&filter%5B%5D=~reponame_str%3A%22Repositorio+Digital+UNLaM%22&filter%5B%5D=~reponame_str%3A%22Repositorio+Institucional+%28UNSAM%29%22&filter%5B%5D=~reponame_str%3A%22RIDAA+%28UNQ%29%22&filter%5B%5D=~reponame_str%3A%22SEDICI+%28UNLP%29%22&filter%5B%5D=publishDate%3A%22%5B2000+TO+2021%5D%22&join=AND&bool0%5B%5D=AND&lookfor0%5B%5D=salar%2A&lookfor0%5B%5D=actividad%2A&type0%5B%5D=AllFields&type0%5B%5D=AllFields"
# string para definir paginas dps
tailurl <- "&page="
# pegar tail a root para rearmar url con N° de paginacion
root <- paste0(root, tailurl)
#cantidad de paginas de resultados, determinar manualmente #se podria automatizar
pages <- 1:8
#contenedores para el loop
links <- list()
titles <- list()
extra <- list()
#loop de scrapeo
for (i in pages) {
    url <- paste(root,
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
}

#pego la seccion de url al link obtenido para poder accederlo desde afuera del sitio
links <- unlist(links) %>% 
    paste0(home,.)
#vectorizo los titulos
titles <- unlist(titles)
#transformo lista a matriz con las columnas de autores, fecha e idioma
extra <- unlist(extra) %>% 
    matrix(ncol = 3, byrow = T, dimnames = list(NULL,c("autores", "fecha", "idioma")))
#armo tablar de resultados
resultados <- cbind(links, titles, extra) %>% 
    as.data.frame()
#guardar
write_csv(resultados, path = "salar*ANDactividad*.csv")
