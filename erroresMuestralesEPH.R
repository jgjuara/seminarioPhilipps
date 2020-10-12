#' tablas de error muestral EPH y funcion para encontrar el DS o CV para la estimacion realizada
#' 
#' paquete pdftools para extraer las tablas del pdf
#' install.packages("pdftools", dependencies = T) ver problema de dependencias
#library(pdftools)


error201403TotalAglom <- read.csv2("~/Descargas/EPH/error201403TotalAglom.txt", header=FALSE, sep="", stringsAsFactors=FALSE)

error201403TotalAglom$V1 <-  stringr::str_replace_all(error201403TotalAglom$V1,"\\.","")
error201403TotalAglom$V2 <-  stringr::str_replace_all(error201403TotalAglom$V2,"\\.","")

error201403TotalAglom$V1 <- as.numeric(error201403TotalAglom$V1)
error201403TotalAglom$V2 <- as.numeric(error201403TotalAglom$V2)
error201403TotalAglom$V3 <- as.numeric(error201403TotalAglom$V3)

names(error201403TotalAglom) <- c("estimacion","DS","CVpp")

write_csv(error201403TotalAglom, path = "~/Descargas/EPH/error201403TotalAglom.csv")

#ejemplo de funcion
closest <- function(valor, referencia, resultado = referencia) {
    resultado[which.min(abs(referencia-valor))]
}

closest2 <- function(valor, aglomerado, busqueda, periodo) {
    #aglomerado = #lista de aglomerados
    #busqueda = #CV o #DS
    #periodo = #2003-2do trim 2014 ó #3er trim 2014 y posterior
    tablaReferencia <- errores %>% 
        filter(AGLOM == aglomerado & periodo == periodo)
    tablaReferencia[busqueda][which.min(abs(estimacion-valor))]
}