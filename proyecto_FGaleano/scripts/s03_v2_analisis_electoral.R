## Este script es una continuacion del s03 dada su extension

#### Se realizara un barplot de los resultados en las paso y en las generales
#### Geofacet con resultados de las 2 elecciones

library(bbplot)
library(geofacet) # Grillas como si fueran mapas 
library(ggrepel)
library(ggpubr)
library(janitor)
library(readxl)
library(sf) # Simple features. Para trabajar con data de información geográfica
library(tidyverse)

paso_geofacet <- read_excel("data/escrutinio-definitivo-paso-2019.xlsx") %>%
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
  select(municipio, departamento, everything()) %>% 
  group_by(departamento) %>%
  summarise_if(is.numeric, sum) %>% 
  print()



## CREO GRILLA                  
salta_grid <- data.frame(name = c("S. VICTORIA","IRUYA", "SAN MARTIN", "ORAN", "LA POMA",
                                  "GÜEMES", "RIVADAVIA", "LA CALDERA", "ROS. LERMA", "LOS ANDES",
                                  "CAPITAL","ANTA", "CERRILLOS", "CACHI", "METAN",
                                  "CHICOANA", "MOLINOS", "SAN CARLOS", "ROS. FRONT.", "LA VIÑA",
                                  "GUACHIPAS", "LA CAND.", "CAFAYATE"),
                         code = c("SANTA VICTORIA","IRUYA", "SAN MARTIN", "ORAN", "LA POMA",
                                  "GENERAL GÜEMES", "RIVADAVIA", "LA CALDERA", "ROSARIO DE LERMA", "LOS ANDES",
                                  "CAPITAL","ANTA", "CERRILLOS", "CACHI", "METAN",
                                  "CHICOANA", "MOLINOS", "SAN CARLOS", "ROS. DE LA FRONTERA", "LA VIÑA",
                                  "GUACHIPAS", "LA CANDELARIA", "CAFAYATE"),
                         row = c(1, 2, 2, 3, 3, 3, 2, 3, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7),
                         col = c(5, 5, 6, 6, 2, 5, 7, 4, 3, 2, 5, 6, 4, 3, 5, 4, 2, 3, 6, 4, 5, 5, 4),
                         stringsAsFactors = FALSE)

## COLORES DE CANDIDATOS + RELEVANTES
cols_fza_pol <- c("fdt_isa" =  "lightblue",
                  "fdt_leavy" = "darkblue",
                  "olmedo" = "yellow",
                  "saenz" = "darkred")


## GEOFACET DE LAS PASO
salta_paso_geofacet <- paso_geofacet %>%
  mutate(fdt_leavy = round(paso_fdt_leavy/paso_votos_positivos*100,2),
         fdt_isa = round(paso_fdt_isa/paso_votos_positivos*100,2),
         olmedo = round(paso_olmedo/paso_votos_positivos*100,2),
         saenz = round(paso_saenz/paso_votos_positivos*100,2)) %>% 
  select(departamento, saenz,fdt_leavy,fdt_isa,olmedo) %>% 
  gather("lista", "votos", saenz:olmedo) %>% 
  ggplot() +
  theme_minimal() +
  geom_col(aes(lista, votos, fill = lista)) +
  scale_fill_manual(values = cols_fza_pol) +
  facet_geo(~departamento, grid = salta_grid, label = "name") +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title = "Resultados por departamento - PASO",
       caption= "Fuente: Tribunal Electoral Salta")
salta_paso_geofacet
#exporto 
ggsave("plots/geofacet_paso.png", width = 20, height = 20, units = "cm")




#######################
## ELECCIONES GENERALES
#######################
generales_geofacet <- read_excel("data/escrutinio-definitivo-generales-2019.xlsx") %>% 
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
  select(municipio, departamento, everything()) %>% 
  group_by(departamento) %>%
  summarise_if(is.numeric, sum) %>% 
  print()

salta_generales_geofacet <- generales_geofacet %>%
  mutate(fdt_leavy = round(grales_fdt_leavy/grales_votos_positivos*100,2),
         olmedo = round(grales_olmedo/grales_votos_positivos*100,2),
         saenz = round(grales_saenz/grales_votos_positivos*100,2)) %>% 
  select(departamento, saenz,fdt_leavy,olmedo) %>% 
  gather("lista", "votos", saenz:olmedo) %>% 
  ggplot() +
  theme_minimal() +
  geom_col(aes(lista, votos, fill = lista)) +
  scale_fill_manual(values = cols_fza_pol) +
  facet_geo(~departamento, grid = salta_grid, label = "name") +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  labs(title = "Resultados por departamento - Generales",
       caption= "Fuente: Tribunal Electoral Salta")

salta_generales_geofacet
#exporto 
ggsave("plots/geofacet_generales.png", width = 20, height = 20, units = "cm")






#############################################
#### BARPLOT CON RESULTADOS A MODO DE RESUMEN
#############################################

step1 <- paso_geofacet %>%
  mutate(Eleccion = "01 - PASO") %>% 
  select(departamento, Eleccion, fdt_leavy = paso_fdt_leavy,
         fdt_isa = paso_fdt_isa,
         olmedo = paso_olmedo,
         saenz=paso_saenz,
         fit_villegas = paso_fit_villegas,
         fit_gil = paso_fit_gil,
         fit_lopez = paso_fit_lopez,
         fernandez = paso_fernandez,
         positivos = paso_votos_positivos)

step2 <- generales_geofacet %>% 
  mutate(Eleccion = "02 - GENERALES") %>% 
  select(departamento, Eleccion,
         fdt_leavy = grales_fdt_leavy,
         olmedo = grales_olmedo,
         saenz=grales_saenz,
         fit_lopez = grales_fit_lopez,
         fernandez = grales_fernandez,
         positivos = grales_votos_positivos)

#Creo paleta


colors_barplot <- c("fdt_isa" =  "blue",
                  "fdt_leavy" = "blue",
                  "olmedo" = "yellow",
                  "saenz" = "darkred",
                  "fit_lopez" = "red",
                  "fit_gil" = "red",
                  "fit_villegas" = "red",
                  "fernandez" = "lightblue")



barplot_resultado <- full_join(step1,step2,by= c("departamento", "Eleccion","fdt_leavy",
                            "olmedo","saenz","fit_lopez","fernandez","positivos")) %>%
  group_by(Eleccion) %>% 
  summarise_if(is.numeric, na.rm = TRUE, sum) %>% 
  select(-positivos) %>% 
  adorn_percentages("row") %>% 
  as_tibble() %>% 
  mutate_if(is.numeric,funs(. *100)) %>% 
  gather("Lista", "Porcentaje", fdt_leavy:fernandez) %>% 
  arrange(desc(Eleccion)) %>% 
  ggplot(aes(x=reorder(Lista, Porcentaje), Porcentaje)) +
  geom_col(aes(fill = Lista)) +
  facet_wrap(~Eleccion) +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=round(Porcentaje,0), fontface="bold"), color="black", hjust=0.5,
            position = position_dodge(1), size=7) +
  scale_fill_manual(values = colors_barplot) +
  bbc_style() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        plot.title = element_text(size = 15))+
  labs(title = "Resultado elecciones a gobernador - Salta 2019")


#exporto
finalise_plot(plot_name = barplot_resultado,
              source = "Fuente: Tribunal Electoral Salta",
              save_filepath = "plots/resultados2019.png",
              width_pixels = 640,
              height_pixels = 550)  
