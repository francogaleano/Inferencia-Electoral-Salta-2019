###### Evolucion del electorado salteño 
########################################################
### El objetivo de este script es realizar un area plot, 
### un lineplot, un treemap y un waffle plot.
#########################################################
### Estos plots servirán para describir la composicion del electorado salteño,
### su comportamiento y su evolucion historica


## Libraries & pkgs
library(bbplot)
library(readxl)
library(tidyverse)
library(treemapify)
library(waffle)

#cargamos base de datos
electores <- read_excel("data/electores_salta_historico.xlsx") %>% 
  select(year, participantes, electores) %>% 
  gather(key = "variable", value = "value", -year) %>% 
  print()

# AREA PLOT
plot_electores <- ggplot(electores, aes(x = year, y = value)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0)) +
  scale_color_manual(values = c("#B02E16", "#E7B800")) +
  scale_fill_manual(values = c("#B02E16", "#E7B800")) +
  scale_y_discrete(name = "Cantidad de votantes y electores (en miles)",
                   limits = c(250000,500000,750000,1000000),
                   labels = c("250","500","750","1000")) +
  scale_x_discrete(name ="Año", 
                   limits=c(1946,1949,1951,1958,1962,1973,1983,
                            1987,1991,1995,1999,2003,2007,2011,2015,2019),
                   labels=c("'46","'49","'51","'58","'62","'73","'83",
                            "'87","'91","'95","'99","'03","'07","'11","'15","'19")) +
  geom_vline(aes(xintercept=1983),
             color="blue", linetype="dashed", size=0.5) +
  bbc_style() +
  labs(title = "Electorado y participación en elecciones a Gobernador",
       subtitle = "Provincia de Salta (1946-2019)") +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size = 12))

#EXPORT
finalise_plot(plot_name = plot_electores,
              source = "Fuente: Atlas electoral de Andy Tow (https://www.andytow.com)",
              save_filepath = "plots/S02Electores_Historico_Salta.png",
              width_pixels = 640,
              height_pixels = 550)
 
###################
# QUEREMOS COMPARAR LA PARTICIPACION EN ELECCIONES A GOBERNADOR
## CON LA PART EN ELECCIONES A PRESIDENTE (EN SALTA Y A NIVEL NACIONAL (EN %)

electores_salta <- read_excel("data/electores_salta_historico.xlsx") %>% 
  mutate(participacion_prov_salta = round((participantes/electores*100),2)) %>%
  print()

electores_nac <- read_excel("data/electores_nacionales_historico.xlsx") %>% 
  mutate(participacion_salta = round((participantes_salta/electores_salta*100),2),
         participacion_nac = round((participantes_nac/electores_nac*100),2)) %>%
  print()

electores_comparativo <- full_join(electores_salta, electores_nac, by = "year") %>%
  select(year,
         part_provincial_salta = participacion_prov_salta,
         part_nacional_salta = participacion_salta,
         part_nacional = participacion_nac) %>% 
  arrange(year) %>% 
  gather(key = "variable", value = "value", -year) %>% 
  drop_na() %>% 
  print()



############
# LINE PLOT
############
electores_grafico_comparado <- ggplot(electores_comparativo, aes(x = year, y = value)) + 
  geom_line(aes(color = variable),
            alpha = 1) +
  scale_color_manual(values = c("#B02E16", "#1E9A02", "#02339A")) +
  scale_y_discrete(name = "Participación electoral (en %)") +
  scale_x_discrete(name ="Año", 
                   limits=c(1946,1949,1951,1958,1962,1973,1983,
                            1987,1989, 1991,1995,1999,2003,2007,2011,2015,2019),
                   labels=c("'46","'49","'51","'58","'62","'73","'83",
                            "'87","'89","'91","'95","'99","'03","'07","'11","'15","'19")) +
  geom_vline(aes(xintercept=1983),
             color="blue", linetype="dashed", size=0.5) +
  bbc_style() +
  labs(title = "Participación electoral comparada",
       subtitle = "Elecciones a gobernador salteño, a presidente en Salta y a nivel nacional (1946-2019)") +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        legend.position="bottom",
        legend.text = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size = 12))



## guardo el plot
finalise_plot(plot_name = electores_grafico_comparado,
              source = "Fuente: Atlas electoral de Andy Tow (https://www.andytow.com)",
              save_filepath = "plots/S02Electores_Historico_Comparado.png",
              width_pixels = 640,
              height_pixels = 550)


###############
## TREEMAP POR DEPARTAMENTO Y POR MUNICIPIO
###############
## Base
electores_muni_depto <- read_csv2("data/electores_salta2019.csv") %>% 
  select(departamento, municipio, electores) %>% 
  group_by(departamento, municipio) %>% 
  summarise_if(is.numeric, sum) %>% 
  arrange(electores) %>%
  ungroup() %>% 
  mutate(porc = round((electores_muni_depto$electores / sum(electores_muni_depto$electores)*100),2), #hago con 2 decimales porque hay municipios muy chicos
         municipio_porc = str_c(municipio, as.character(porc),sep = " ", "%")) %>% #var municipio + porc municipio
  print()


#Quiero crear una variable que contenga el % departamental para luego 
# hacer un join con electores_muni_depto y crear una variable str que diga
# por ejemplo: Departamento Capital 43 % 
# es decir concatenar nombre depto + porcentaje de electores para  el treemap.
porcentaje_depto_join <- electores_muni_depto %>%
  group_by(departamento) %>%
  summarise_if(is.numeric, sum) %>% 
  select(departamento, electores_depto = electores, porc_depto = porc) %>% 
  print()

## Treemap
treemap_electores <- electores_muni_depto %>%
  left_join(porcentaje_depto_join, by="departamento") %>%
  ungroup() %>% 
  mutate(departamento_porc = str_c(departamento, as.character(porc_depto), sep= " ", "%")) %>% #creo la variable depto + porc
  ggplot(aes(area = electores,
           fill = municipio_porc,
           label = municipio_porc,
           subgroup = departamento_porc)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_text(colour = "white", place = "top",grow = F) +
  geom_treemap_subgroup_text(place = "bottom", grow= F) +
  bbc_style() + 
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        legend.position="none") +
  labs(title = "Distribución del electorado provincial - Salta 2019")


#EXPORT
finalise_plot(plot_name = treemap_electores,
              source = "Fuente: Tribunal Electoral Salta (http://www.electoralsalta.gob.ar)",
              save_filepath = "plots/S02Treemap_Electores2.png",
              width_pixels = 840,
              height_pixels = 750)

###########################################
## OTRA ALTERNATIVA ES HACER UN WAFFLE PLOT
##########################################
## existe un paquete ggwafle pero encontré muchos errores, por eso uso una version alternativa (no esta en cran)
# llamamos al paquete: library(waffle)

waffle_electores <- porcentaje_depto_join %>%
  select(departamento, electores_depto) %>%
  mutate(electores_depto = round(electores_depto / 1000)) %>% #cada cuadrito de aprox 1000 electores (redondeado)
  print()

waffle_plot_electores <- waffle_electores %>%
  arrange(desc(electores_depto)) %>% 
  pivot_wider(names_from = departamento, values_from = electores_depto) %>% # a diferencia del spread respeta el orden del arrange
  waffle( rows = 20, size=0.25,
  colors = c("#80D39B","#FF0000", "#2E5077", "#FFA630","#ffff00",
               "#611C35","#C64191","#ADD8E6","#1A5E63","#DBD56E",
               "#01bd17","012A36","#100007","#deaddd","#7EF1CF",
               "#E8FE2B","#553D36","#bada55","#DC6900","#E61919",
               "#005A9F","#830000","#8A2BE2")) + 
  bbc_style() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position="bottom",
        legend.text = element_text(size = 8)) +
  labs(title = "Distribución del electorado provincial - Salta 2019",
       subtitle = "1 rectángulo ≈ 1000 electores")

#EXPORT
finalise_plot(plot_name = waffle_plot_electores,
              source = "Fuente: Tribunal Electoral Salta (http://www.electoralsalta.gob.ar)",
              save_filepath = "plots/S02WafflePlotElectores.png",
              width_pixels = 840,
              height_pixels = 750)
