# Proyecto de Investigación

library(eph)
library(tidyverse)
library(DescTools)

####----TABLAS DE VARIABLES EPH----
#tabla con nombres de variables eph
toybase_individual_2016_03 <- organize_labels(toybase_individual_2016_03)
variables_eph <- sapply(toybase_individual_2016_03, expss::var_lab)
tabladevariables_eph <- tibble(nombre_variable = names(toybase_individual_2016_03), descripcion_variables = variables_eph)
nivels_eph <- lapply(toybase_individual_2016_03, function(x) levels(factor(x)))
nivels_eph <- lapply(nivels_eph, function(x) glue::glue_collapse(x,sep = ";"))
tabladevariables_eph <- add_column(tabladevariables_eph, nivels_eph)
tabladevariables_eph$nivels_eph <- unlist(tabladevariables_eph$nivels_eph)
write_csv(tabladevariables_eph, path = "tabladevariables_eph.csv")


####----INFLACION----
#tabla con inflacion
url_CBTregiones <- "https://infra.datos.gob.ar/catalog/sspm/dataset/446/distribution/446.1/download/canasta-basica-total-regiones-del-pais.csv"
# consultar sobre el origen y consistencia de los datos
CBTregiones <- read_csv(url_CBTregiones)


####----BASES----
#carga de bases
eph2017[[1]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_1.RDS")
eph2017[[2]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_2.RDS")
eph2017[[3]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_3.RDS")
eph2017[[4]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_4.RDS")
#suma de bases
eph2017 <- bind_rows(eph2017[[1]][[5]],eph2017[[2]][[5]], eph2017[[3]][[5]], eph2017[[4]][[5]])

# guardo la base unificada
write_rds(eph2017, path = "eph2017completa.RDS", compress = "gz")
#creo una variable "idhogar" que une el CODUSU y el NRO de hogar en una sola linea de caracteres
eph2017completa <- eph2017completa %>% 
    mutate(idhogar = paste0(CODUSU, NRO_HOGAR))

# obtengo la lista de hogares observados una sola vez en el año 
hogaresdeunaobservacion <- eph2017completa %>% 
    select(idhogar, TRIMESTRE) %>% 
    #agrupo primero por idhogar y dps por trimestre
    pivot_wider(names_from = TRIMESTRE, values_from = TRIMESTRE, values_fn = list(TRIMESTRE = is.vector))

hogaresdeunaobservacion[2:5] <- !is.na(hogaresdeunaobservacion[2:5])

hogaresdeunaobservacion <- hogaresdeunaobservacion %>% 
    mutate( ntrims = hogaresdeunaobservacion$`1`+hogaresdeunaobservacion$`2`+hogaresdeunaobservacion$`3`+hogaresdeunaobservacion$`4`)
    
hogaresdeunaobservacion %>% 
    write_csv(., path = "tablahogaresobservadosportrimestres.csv")

tablahogaresobservadosportrimestres %>%
    group_by(ntrims) %>% 
    summarise(t1 = sum(`1`),
              t2 = sum(`2`),
              t3 = sum(`3`),
              t4 = sum(`4`)) %>% 
    t(.) %>% 
    as.data.frame() %>% 
    .[2:5,1:2] %>% 
    mutate(porcentaje = 100*V1/(V1+V2)) %>% 
    knitr::kable(., row.names = T ,digits = 2,
                 col.names = c("Hogares entrevistados una vez",
                                             "Hogares entrevistados dos veces",
                                             "Porcentaje de Hogares entrevistados una vez sobre el total"))



eph1 <- hogaresdeunaobservacion %>% 
    filter(`1` == T & ntrims == 1) %>% 
    sample_n(tbl = .,size = 2137,replace = F)

eph2 <- hogaresdeunaobservacion %>% 
    filter(`2` == T & ntrims == 1 ) %>% 
    sample_n(tbl = .,size = 2137,replace = F)

eph3 <- hogaresdeunaobservacion %>% 
    filter(`3` == T & ntrims == 1) %>% 
    sample_n(tbl = .,size = 2137,replace = F)

eph4 <- hogaresdeunaobservacion %>% 
    filter(`4` == T & ntrims == 1) %>% 
    sample_n(tbl = .,size = 2137,replace = F)

hogaresdeunaobservacion <- bind_rows(eph1,eph2,eph3,eph4)

eph2017completa <- semi_join(eph2017completa, hogaresdeunaobservacion)
    
eph2017completa %>% 
    group_by(TRIMESTRE) %>% 
    summarise(individuos = n(),
              hogares = n_distinct(idhogar))

write_rds(eph2017completa, path = "eph2017.RDS", compress = "gz")

eph2017 <- eph2017 %>% 
    organize_labels()

eph2017 <- eph2017 %>% 
    filter(CH06 >= 18 & ESTADO == 1 & CAT_OCUP == 3)
####----TABLAS----

#1 nivel educativo vs sexo

questionr::wtd.table(x = eph2017$NIVEL_ED, y = eph2017$CH04, weights = eph2017$PONDERA, digits = 2) %>% 
    PercTable(freq = F, rfrq ="111",margins = c(1,2))

educVSsex <- xtabs(PONDERA ~ CH04+NIVEL_ED, data = eph2017)
summary(educVSsex)
vcd::assocstats(educVSsex)

#2 nivel educativo vs Estado
questionr::wtd.table(x = eph2017$NIVEL_ED, y = eph2017$ESTADO, weights = eph2017$PONDERA, digits = 2) %>%
    PercTable(freq = F, rfrq ="111",margins = c(1,2))

educVSestado <- xtabs(PONDERA ~ NIVEL_ED+ ESTADO, data = eph2017)
summary(educVSestado)
vcd::assocstats(educVSestado)

#3 sexo vs ESTADO
questionr::wtd.table(x = eph2017$ESTADO, y = eph2017$CH04, weights = eph2017$PONDERA, digits = 2) %>%
    PercTable(freq = F, rfrq ="111",margins = c(1,2))
 
sexVSestado <- xtabs(PONDERA ~ ESTADO+CH04, eph2017)
summary(sexVSestado)
vcd::assocstats(sexVSestado)

#4 relacion original XY (CH04 ~ ESTADO) T (NIVEL_ED)
## ¿es necesario probar el resto de las relaciones? ¿Tiene sentido para mi trabajo?
sexVSestadoTeduc <- xtabs(PONDERA ~ CH04+ESTADO+NIVEL_ED, data = eph2017)
addmargins(sexVSestadoTeduc)
summary(sexVSestadoTeduc)
# fuerte relacion original!
vcd::assocstats(sexVSestadoTeduc)

#5 relacion original XY (NIVEL_ED ~ CAT_OCUP) T (CH04) para OCUPADOS
## ¿es necesario probar el resto de las relaciones? ¿Tiene sentido para mi trabajo?
educVScatocupTsex <- list()

educVScatocupTsex$data <- eph2017 %>% 
    #solo ocupados
    filter(ESTADO == 1)

educVScatocupTsex$resumen <- xtabs(PONDERA ~ NIVEL_ED+CAT_OCUP+CH04, data = educVScatocupTsex$data)

addmargins(educVScatocupTsex$resumen)

summary(educVScatocupTsex$resumen)
vcd::assocstats(educVScatocupTsex$resumen)

# agrego etiquetas de actividad del establecimiento
eph2017 <- eph2017 %>%
    organize_caes()

# 6. ¿vamos a considerar al sector publico? Mi definición teórica me lleva a excluirlo pero veamos igual sus frecuencias por actividad
# Hay varias ramas importantantes donde hay alta proporcion de trabajadores en unidades de produccion estatal
temp <- calculate_tabulates(eph2017, y = "PP04A", x = "caes_division_label", weights = "PONDERA", add.percentage = c("row"))
temp %>% 
    mutate(...estatal = parse_number(...estatal)) %>% 
    view()

# aunque solo el 17.6% de la poblacion total bajo estudio se desempeña en una unidad productiva de caracter estatal

# 7. relacion original XY (NIVEL_ED ~ caes_division_label) T (PP04A)
niveleducVSdivcaes<- list()

niveleducVSdivcaes$data <- eph2017 %>% 
    # solo ocupados y obreros
    filter(ESTADO == 1 & CAT_OCUP == 3)

niveleducVSdivcaes$resumen <- xtabs(PONDERA ~ NIVEL_ED+caes_division_label, data = niveleducVSdivcaes$data)

addmargins(niveleducVSdivcaes$resumen)
summary(niveleducVSdivcaes$resumen)
vcd::assocstats(niveleducVSdivcaes$resumen)
CramerV(niveleducVSdivcaes$resumen)


