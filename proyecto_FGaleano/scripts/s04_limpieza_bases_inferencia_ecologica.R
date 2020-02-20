###########################################
# LIMPIEZA Y PREPROCESAMIENTO DE LAS BASES
###########################################
## OBJETIVO: LIMPIEZA Y TRANSFORMACION DE LAS BASES DE DATOS
## PARA SU PROCESAMIENTO EN EL SCRIPT SIGUIENTE


## Importamos libs y paquetes
library(janitor)
library(readxl)
library(tidyverse)


### IMPORTAR DATA
###########################################################################
## ESCRUTINIOS DEFINITIVOS DISPONIBLES EN: http://www.electoralsalta.gob.ar
###########################################################################
##PASO
salta_paso <- read_excel("data/escrutinio-definitivo-paso-2019.xlsx") %>% 
  clean_names() %>% #Funcion de Janitor para limpiar nombres de variables
  filter(cargo == "GOBERNADOR") %>% # Me quedo solo con los votos a gobernador
  mutate(mesa = as.character(mesa)) %>% 
  drop_na(mesa) %>% ##Estan cargadas como mesas las decisiones sobre votos impugnados/observados
  select(-ambito, -nº, -primaria) %>% 
  spread(key = lista, value=votos) %>% #Datos en forma tidy
  mutate(paso_votos_positivos = `CELESTE Y BLANCA` +
           `EL FUTURO ES CON TODOS` +
           `NUEVA IZQUIERDA` +
           `OLMEDO GOBERNADOR 2019` +
           `POLITICA OBRERA` +
           `SAENZ GOBERNADOR` +
           `TODOS TENEMOS FUTURO` +
           UNIDAD,
         paso_votos = paso_votos_positivos + `VOTOS EN BLANCO`) %>% 
  rename(paso_fdt_leavy = `CELESTE Y BLANCA`,
         paso_fdt_isa = `EL FUTURO ES CON TODOS`,
         paso_fit_villegas = `NUEVA IZQUIERDA`,
         paso_fit_gil = `POLITICA OBRERA`,
         paso_fit_lopez = UNIDAD,
         paso_olmedo = `OLMEDO GOBERNADOR 2019`,
         paso_saenz = `SAENZ GOBERNADOR`,
         paso_fernandez = `TODOS TENEMOS FUTURO`,
         paso_blancos = `VOTOS EN BLANCO`) %>% 
  print()

##GENERALES
salta_generales <- read_excel("data/escrutinio-definitivo-generales-2019.xlsx") %>% 
  clean_names() %>% 
  filter(categoria == "GOBERNADOR") %>%
  drop_na(mesa) %>% 
  mutate(mesa = as.character(mesa)) %>% 
  select(-ambito, -nº) %>% 
  spread(key = lista, value=votos) %>%
  mutate(grales_votos_positivos = `FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD` +
           `FRENTE DE TODOS` + `OLMEDO GOBERNADOR` + `PARTIDO FRENTE GRANDE` + `SAENZ GOBERNADOR`,
         grales_votos = grales_votos_positivos + `VOTOS EN BLANCO`) %>% 
  rename(grales_fdt_leavy = `FRENTE DE TODOS`,
         grales_fit_lopez = `FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD`,
         grales_olmedo = `OLMEDO GOBERNADOR`,
         grales_saenz = `SAENZ GOBERNADOR`,
         grales_fernandez = `PARTIDO FRENTE GRANDE`,
         grales_blancos = `VOTOS EN BLANCO`) %>% 
  print()

## LOS ESCRUTINIOS DEFINITIVOS NO PRESENTAN LA CANTIDAD DE ELECTORES POR MESA (SE UTILIZARAN LOS DEL PADRON NACIONAL)
## BREVE COMENTARIO: POSEO 1067 ELECTORES MENOS QUE LOS QUE INDICA EL PADRON PROVINCIAL (SOBRE UN TOTAL DE 1,025 MILLONES)

# ELECTORES
electores_salta <- read_csv2("data/electores_salta2019.csv") %>%
  mutate(mesa = as.character(mesa)) %>%
  print()



## CREO LA BASE DE DATOS PARA EL ANALISIS DE INFERENCIA ECOLÓGICA
## LA BASE DEBE POSEER UNA COLUMNA POR CADA LISTA
## VOTOS BLANCOS, NULOS Y AUSENTES SON CALCULADOS POR DESCARTE SEGUN EL MODELO DE KING, ROSER Y TANER.

base_inferencia_ecologica <- 
  salta_paso %>%
  select(-departamento, -municipio, -subcircuito, -establecimiento, -cargo) %>% 
  left_join(salta_generales, by = "mesa") %>% 
  rename(circuito = subcircuito) %>% 
  select(-departamento, -municipio) %>% 
  left_join(electores_salta, by= c("mesa","circuito", "establecimiento")) %>%
  select(departamento, 
         municipio,
         circuito,
         establecimiento,
         mesa,
         electores,
         paso_saenz,paso_fdt_leavy, paso_fdt_isa, paso_olmedo, 
         paso_fit_villegas, paso_fit_gil, paso_fit_lopez, paso_fernandez,
         grales_saenz, grales_fdt_leavy, grales_olmedo, grales_fit_lopez, grales_fernandez) %>% 
  print()

# write_csv
base_inferencia_ecologica %>% write_csv("data/base_inferencia_ecologica_saltaGob2019.csv")
