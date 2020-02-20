##########
## BREVE ANALISIS ELECTORAL
########################################################
### El objetivo de este script es realizar un breve analisis electoral,
## El mismo consiste en: mapas, geo_facets y barplots
#########################################################
library(bbplot)
library(geofacet) # Grillas como si fueran mapas 
library(ggrepel)
library(ggpubr)
library(janitor)
library(readxl)
library(sf) # Simple features. Para trabajar con data de información geográfica
library(tidyverse)


## base con geografia de los departamentos
mapa_deptos_salta <- read_sf("data_geo/geo_depto.shp") %>%
  group_by(codprov, coddepto) %>% 
  slice(1) %>% #hacemos esto porque tenemos 5 instancias para departamentos
  filter(codprov == 17) %>% 
  select(-codprov, -prov) %>% 
  print()


## Para joinear las bases del escrutinio con las bases geo es necesario
## limpiar las bases y transformarlas

#Base de electores
electores_salta <- read_csv2("data/electores_salta2019.csv") %>%
  mutate(mesa = as.character(mesa)) %>%
  print()

## Escrutinio definitivo del Tribunal electoral (no tiene electores)
paso <- read_excel("data/escrutinio-definitivo-paso-2019.xlsx") %>%
  clean_names() %>% #Funcion de Janitor para limpiar nombres de variables
  filter(cargo == "GOBERNADOR") %>% # Me quedo solo con los votos a gobernador
  mutate(mesa = as.character(mesa)) %>% 
  drop_na(mesa) %>% ##Estan cargadas como mesas las decisiones sobre votos impugnados/observados
  select(-ambito, -nº, -primaria, -cargo) %>% 
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
  rename(circuito = subcircuito,
         paso_fdt_leavy = `CELESTE Y BLANCA`,
         paso_fdt_isa = `EL FUTURO ES CON TODOS`,
         paso_fit_villegas = `NUEVA IZQUIERDA`,
         paso_fit_gil = `POLITICA OBRERA`,
         paso_fit_lopez = UNIDAD,
         paso_olmedo = `OLMEDO GOBERNADOR 2019`,
         paso_saenz = `SAENZ GOBERNADOR`,
         paso_fernandez = `TODOS TENEMOS FUTURO`,
         paso_blancos = `VOTOS EN BLANCO`) %>%
  print()

## La base de datos no posee un id por departamento y municipio, entonces el join entre resultados y datos geo
## deberia ser por nombre del departamento y del municipio, pero como no estan escritos de la misma forma
## una solucion posible es: 
# 1) Crear una base de datos con los nombres de los departamentos escritos como en la base de
## resultados, con los id de departamento de geo, 
# 2) joinear eso a base de resultados, y 
# 3) luego un join con base geo

# Paso 1
base_join_deptos <- tibble(
  departamento=c("CAPITAL", "LA CALDERA", "GENERAL GÜEMES", "METAN","ANTA",
               "RIVADAVIA","ORAN","SAN MARTIN","IRUYA","STA. VICTORIA",
               "CERRILLOS","CHICOANA","LA VIÑA","GUACHIPAS","R. DE LA FRONTERA",
               "LA CANDELARIA","CAFAYATE","SAN CARLOS","MOLINOS","CACHI",
               "R. DE LERMA","LA POMA","LOS ANDES"),
  coddepto = c("001","002","003","004","005",
               "006","007","008","009","010",
               "011","012","013","014","015",
               "016","017","018","019","020",
               "021","022","023")) %>% 
  print()

# Paso 2
paso_joineada <- paso %>%
  select(-departamento, -municipio) %>% 
  left_join(electores_salta, by= c("mesa", "circuito", "establecimiento")) %>% #primero un join con electores
  select(-circuito, -establecimiento, -mesa) %>% 
  select(municipio, departamento, everything()) %>% 
  group_by(departamento) %>%
  summarise_if(is.numeric, sum) %>% 
  left_join(base_join_deptos, by=c("departamento")) %>% #join con base de paso 1
  select(departamento, coddepto, everything()) %>% 
  arrange(coddepto) %>% #hacemos un arrange para verificar
  print()

# Paso 3. Combinar data con geo
paso_geo <- paso_joineada %>%
  left_join(mapa_deptos_salta, by = "coddepto") %>% 
  select(-departamento) %>% 
  select(depto, everything()) %>% 
  print()

#############################
########## MAPAS ############
#############################
## Municipios donde ganó cada candidato (PASO Y GENERALES)
## Diferencias entre frontrunners en % (PASO Y GENERALES)

# Terminado el join pasamos a hacer el (1) mapa de diferencia entre frontrunners por departamento
# (2) Ganador por departamento

## Mapa (1) frontrunners por depto en las paso
frontrunners_paso <- paso_geo %>%
  mutate(porc_leavy = round(paso_fdt_leavy/paso_votos_positivos*100,2),
         porc_saenz = round(paso_saenz/paso_votos_positivos*100,2),
         diferencia = porc_leavy - porc_saenz) %>%
  select(depto, coddepto, porc_leavy, porc_saenz, diferencia, geometry) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = diferencia)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       breaks = c(-40,-20,0,20,40), 
                       labels = c("40","20","0","20","40")) +
  theme_void() +
  bbc_style() +
  theme(axis.text = element_blank(), 
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=13),
        plot.title = element_text(size = 10)) +
  labs(title = "PASO")

## Mapa (2) ganadores por depto en las paso
ganadores_depto_paso <- paso_geo %>% 
  mutate(ganador = ifelse(paso_saenz > paso_fdt_isa & paso_saenz > paso_fdt_leavy & paso_saenz > paso_olmedo, "Saenz",
                          ifelse(paso_fdt_leavy > paso_fdt_isa & paso_fdt_leavy > paso_olmedo, "Leavy",
                                 ifelse(paso_fdt_isa> paso_olmedo, "Isa", "Olmedo")))) %>%
  select(depto, ganador, geometry) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black", aes(fill=ganador)) +
  scale_fill_manual(values = c("#02339A","#B02E16")) +
  theme_void() +
  bbc_style() +
  theme(axis.text = element_blank(), 
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=13),
        plot.title = element_text(size = 10)) +
  ggrepel::geom_label_repel(size = 2.7,
    aes(label = depto, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0) +
  labs(title = "PASO")


###################################################################
### Ahora replicamos el mismo proceso para las elecciones GENERALES 
###################################################################

generales <- read_excel("data/escrutinio-definitivo-generales-2019.xlsx") %>%
  clean_names() %>% #Funcion de Janitor para limpiar nombres de variables
  filter(categoria == "GOBERNADOR") %>% 
  mutate(mesa = as.character(mesa)) %>% 
  drop_na(mesa) %>% 
  select(-ambito, -nº) %>% 
  spread(key = lista, value=votos) %>%
  mutate(grales_votos_positivos = `FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD` +
           `FRENTE DE TODOS` + `OLMEDO GOBERNADOR` + `PARTIDO FRENTE GRANDE` + `SAENZ GOBERNADOR`,
         grales_votos = grales_votos_positivos + `VOTOS EN BLANCO`) %>% 
  rename(circuito = subcircuito,
         grales_fdt_leavy = `FRENTE DE TODOS`,
         grales_fit_lopez = `FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD`,
         grales_olmedo = `OLMEDO GOBERNADOR`,
         grales_saenz = `SAENZ GOBERNADOR`,
         grales_fernandez = `PARTIDO FRENTE GRANDE`,
         grales_blancos = `VOTOS EN BLANCO`) %>% 
  print()

## Hacemos paso 2 y 3 del data transformation de las paso para elecciones generales
generales_geo <- generales %>% 
  select(-departamento, -municipio) %>% 
  left_join(electores_salta, by= c("mesa", "circuito", "establecimiento")) %>% #primero un join con electores
  select(-circuito, -establecimiento, -mesa) %>% 
  select(municipio, departamento, everything()) %>% 
  group_by(departamento) %>%
  summarise_if(is.numeric, sum) %>% 
  left_join(base_join_deptos, by=c("departamento")) %>% #join con base de paso 1
  select(departamento, coddepto, everything()) %>% 
  arrange(coddepto) %>% #hacemos un arrange para verificar
  left_join(mapa_deptos_salta, by = "coddepto") %>% 
  select(-departamento) %>% 
  select(depto, everything()) %>% 
  print()
  
  
## Mapa (3) frontrunners por depto en las generales
frontrunners_generales <- generales_geo %>%
  mutate(porc_leavy = round(grales_fdt_leavy/grales_votos_positivos*100,2),
         porc_saenz = round(grales_saenz/grales_votos_positivos*100,2),
         diferencia = porc_leavy - porc_saenz) %>%
  select(depto, coddepto, porc_leavy, porc_saenz, diferencia, geometry) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = diferencia)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       breaks = c(-55,-40,-20,0,20,40), 
                       labels = c("55","40","20","0","20","40")) +
  theme_void() +
  bbc_style() +
  theme(axis.text = element_blank(), 
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=13),
        plot.title = element_text(size = 10)) +
  labs(title = "GENERALES")

## Mapa (4) ganadores por depto en las paso
ganadores_depto_generales <- generales_geo %>% 
  mutate(ganador = ifelse(grales_saenz > grales_fdt_leavy & grales_saenz > grales_olmedo, "Saenz",
                          ifelse(grales_fdt_leavy > grales_olmedo, "Leavy", "Olmedo"))) %>%
  select(depto, ganador, geometry) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(color = "black", aes(fill=ganador)) +
  scale_fill_manual(values = c("#02339A","#B02E16")) +
  theme_void() +
  bbc_style() +
  theme(axis.text = element_blank(), 
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=13),
        plot.title = element_text(size = 10)) +
  ggrepel::geom_label_repel(size = 2.7,
    aes(label = depto, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0) +
  labs(title = "GENERALES")



#Junto mapas 3 y 4
figura_ganadores <- ggarrange(ganadores_depto_paso,
          ganadores_depto_generales,
          ncol = 2, nrow = 1, common.legend = TRUE,legend = "bottom") %>% 
  annotate_figure(top = text_grob("Ganadores por departamento", color = "black", face = "bold", size = 16))

# exporto 
finalise_plot(plot_name = figura_ganadores,
              source = "Fuente: Tribunal Electoral Salta",
              save_filepath = "plots/mapa_ganadores.png",
              width_pixels = 640,
              height_pixels = 550)

#Junto mapas 1 y 2
figura_frontrunners <- ggarrange(frontrunners_paso,
                                 frontrunners_generales,
                                 ncol=2, nrow = 1) %>% 
  annotate_figure(top = text_grob("Diferencia entre frontruners", color = "black", face = "bold", size = 16),
                  bottom = text_grob("Diferencia en puntos porcentuales", color = "black",
                                     hjust = 1, x = 1, face = "italic", size = 12))
#exporto
finalise_plot(plot_name = figura_frontrunners,
              source = "Fuente: Tribunal Electoral Salta",
              save_filepath = "plots/mapa_frontrunners.png",
              width_pixels = 640,
              height_pixels = 550)




