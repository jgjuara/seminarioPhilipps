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
eph_2017_1 <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_1.RDS")
eph_2017_2 <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_2.RDS")
eph_2017_3 <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_3.RDS")
eph_2017_4 <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_4.RDS")
#suma de bases
eph2017 <- bind_rows(eph_2017_1[[5]],eph_2017_2[[5]], eph_2017_3[[5]], eph_2017_4[[5]])
#saco las bases individuales
rm(eph_2017_1,eph_2017_2, eph_2017_3, eph_2017_4)
#creo una variable "idhogar" que une el CODUSU y el NRO de hogar en una sola linea de caracteres
eph2017 <- eph2017 %>% 
    mutate(idhogar = paste0(CODUSU, NRO_HOGAR))

# obtengo la lista de hogares observados una sola vez en el año
hogaresdeunaobservacion <- eph2017 %>% 
    #agrupo primero por idhogar y dps por trimestre
    group_by(idhogar, TRIMESTRE) %>% 
    # armo una tabla con cada idhogar por trimestre (sin duplicados)
    summarise(observaciones = n_distinct(idhogar)) %>% 
    ungroup %>% 
    # agrupo esa tabla por idhogar ignorando trimestres
    group_by(idhogar) %>% 
    # cuento la cantidad de trimestres que apareció cada idhogar durante el año
    summarise(obs = sum(observaciones)) %>% 
    ungroup() %>% 
    # me quedo solo con los idhogar que aparecieron en un solo trimestres
    filter(obs == 1)

eph2017 <- eph2017 %>% 
    # uso la lista de idhogar que solo aparecieron en un trimestre para filtrar la base
    filter(idhogar %in% hogaresdeunaobservacion$idhogar)

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


