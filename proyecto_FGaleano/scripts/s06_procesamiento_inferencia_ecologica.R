#### PROCESAMIENTO DE RESULTADOS DEL ANALISIS DE INFERENCIA ECOLOGICA
########################################################
### El objetivo de este script es realizar tablas y 
###un diagrama de sankey con los resultados del script s05
#########################################################

library(bbplot)
library(ggalluvial) #paquete para diagramas de sankey
library(ggrepel)
library(janitor)
library(tidyverse)

# Tabla en numeros enteros para el documento
tabla_enteros <- read_csv("data/inferenciaEcoResultados.csv") %>% 
  rename("PASO /GENERALES" = "indice",
         "Leavy"="grales_fdt_leavy",
         "Saenz"="grales_saenz",
         "Olmedo"="grales_olmedo",
         "Lopez" = "grales_fit_lopez",
         "Fernandez" = "grales_fernandez",
         "No Voto / Blanco" = "grales_novoto_blanco") %>% 
  adorn_totals("row") %>%
  mutate(total = rowSums(.[2:7])) %>% 
  print()


## Tabla en porcentajes para el documento  
tabla_pcts <- read_csv("data/inferenciaEcoResultados.csv") %>%
  rename("PASO /GENERALES" = "indice",
         "Leavy"="grales_fdt_leavy",
         "Saenz"="grales_saenz",
         "Olmedo"="grales_olmedo",
         "Lopez" = "grales_fit_lopez",
         "Fernandez" = "grales_fernandez",
         "No Voto / Blanco" = "grales_novoto_blanco") %>% 
  adorn_totals("row") %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting() %>%
  #adorn_ns() %>% 
  print()


######################
### DIAGRAMA DE SANKEY
######################
paso <- tibble(paso = c("Gustavo Saenz", "FDT Sergio Leavy",
                  "FDT Miguel Isa","Alfredo Olmedo",
                  "FIT Villegas", "FIT Gil","FIT Lopez",
                  "Elia Fernandez", "No votó/Blanco Paso"))

base_sankey <- read_csv("data/inferenciaEcoResultados.csv") %>% 
  select(`Gustavo Saenz` = grales_saenz,
         `FDT Sergio Leavy` = grales_fdt_leavy,
         `Alfredo Olmedo` = grales_olmedo,
         `FIT Pablo Lopez` = grales_fit_lopez,
         `Elia Fernandez` = grales_fernandez,
         `No voto/Blanco Generales` = grales_novoto_blanco) %>% 
  bind_cols(paso) %>% 
  select(paso, everything()) %>% 
  gather(key="generales", value = "freq", -paso) %>%
  arrange(desc(paso))%>% 
  print()

plot_sankey <-base_sankey %>% 
  ggplot(aes(axis1 = paso, axis2 = generales,
           y = freq)) +
  scale_x_discrete(limits = c("Primarias", "Generales"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = freq)) +
  geom_stratum() +
  geom_text_repel(stat = "stratum",infer.label = TRUE,size = 3.5) +
  theme_minimal() +
  bbc_style()+
  labs(title = "Transferencia de votos - Elección a gobernador en Salta (2019)") +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

#EXPORT
finalise_plot(plot_name = plot_sankey,
              source = "Fuente: Tribunal Electoral Salta, script de Ernesto Calvo",
              save_filepath = "plots/Sankey.png",
              width_pixels = 640,
              height_pixels = 550)