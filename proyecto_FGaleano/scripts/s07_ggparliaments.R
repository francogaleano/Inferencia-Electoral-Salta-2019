### COMPOSICION DE CAMARAS LEGISLATIVAS DE LA PROV DE SALTA 2019-2021

## Importar paquetes
library(bbplot)
library(ggparliament)
library(tidyverse)


#Link de https://github.com/RobWHickman/ggparliament


#Creamos la base de datos
salta_legislatura <- tibble(year = c(2017, 2017, 2017, 2017, 2017, 2017, 2019,
                                2019, 2019, 2019, 2019, 2019, 2019,
                                2019, 2019, 2019, 2019, 2019, 2019),
                           country = c("Salta", "Salta", "Salta", "Salta", "Salta", "Salta", "Salta",
                                       "Salta", "Salta", "Salta", "Salta", "Salta", "Salta", "Salta",
                                       "Salta", "Salta", "Salta", "Salta", "Salta"),
                           house = c("Senado", "Senado", "Senado", "Senado", "Senado", "Senado",
                                     "Senado", "Senado", "Senado", "Senado",
                                     "Diputados", "Diputados", "Diputados", "Diputados",
                                     "Diputados", "Diputados", "Diputados", "Diputados", "Diputados"),
                           party_long = c("Cambiemos País","Justicialista","Memoria y Movilización",
                                          "Partido de la Victoria","Partido Renovador de Salta", "Unión Cívica Radical",
                                          "Salta Tiene Futuro","Justicialista","Partido de la Victoria","Partido Renovador de Salta",
                                          "17 de Octubre", "Ahora Patria", "Frente para la Victoria", "Justicialista", "Justicialista Sáenz Conducción",
                                          "Partido Obrero", "Partido Renovador de Salta", "Salta tiene Futuro", "Unión Cívica Radical"),
                           party_short = c("CMB", "PJ", "MyM","PV", "PaReS", "UCR", "STF", "PJ", "PV", "PaReS",
                                           "FS", "SST", "FPV", "PJ","PJ-SC","PO","PaReS","STF","UCR"),
                           seats = c(4, 12, 2, 1, 3, 1,12,5,4,2,
                                     2,2,11,4,12,1,2,23,3),
                           government = c(0,1,0,0,1,0,1,0,0,0,
                                          0,0,0,0,1,0,0,1,0),
                           colour = c("#ffff00","#ADD8E6","#CACED0","#72bcd4","#005A9F","#FF0000","#830000","#ADD8E6","#72BCD4","#005A9F",
                                      "#7EF1CF","#E8FE2B","#72BCD4","#ADD8E6","#143CB6","#E61919","#005A9F","#830000","#FF0000"))

### CAMARA DE DIPUTADOS
diputados2019 <- salta_legislatura %>% 
  filter(country == "Salta" &
           year == 2019 &
           house == "Diputados") %>% 
  arrange_at("government", desc) %>%
  print()

diputados2019 <- parliament_data(
  election_data = diputados2019,
  type = "semicircle",
  parl_rows = 4,
  party_seats = diputados2019$seats)

plot_diputados <- ggplot(diputados2019, aes(x, y, colour = party_long)) +
  geom_parliament_seats(size = 8) + 
  geom_highlight_government(government == 1, size = 9) +
  # add bar showing proportion of seats by party in legislature
  geom_parliament_bar(colour = colour, party = party_long) + 
  theme_ggparliament(legend = TRUE) +
  labs(colour = NULL, 
       title = "Cámara de Diputados de la Provincia de Salta 2019-2021",
       subtitle = "Coalición oficialista redondeada en negro") +
  bbc_style() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 6),
        legend.position = "bottom") +
  scale_colour_manual(values = diputados2019$colour,
                      limits = diputados2019$party_long)


# Guardar el plot
finalise_plot(plot_name = plot_diputados,
              source = "Fuente: Cámara de Diputados de la Provincia de Salta",
              save_filepath = "plots/ggparliamentDiputadosSalta.png",
              width_pixels = 640,
              height_pixels = 550)



#### CAMARA DE SENADORES
senado2019 <- salta_senado %>% 
  filter(country == "Salta" &
           year == 2019 &
           house == "Senado") %>% 
  arrange_at("government", desc) %>%
  print()

senado2019 <- parliament_data(
  election_data = senado2019,
  type = "semicircle",
  parl_rows = 2,
  party_seats = senado2019$seats)   

plot_senadores <- ggplot(senado2019, aes(x, y, colour = party_long)) +
  geom_parliament_seats(size = 10) + 
  geom_highlight_government(government == 1, size = 11) +
  # add bar showing proportion of seats by party in legislature
  geom_parliament_bar(colour = colour, party = party_long) + 
  theme_ggparliament(legend = TRUE) +
  labs(colour = NULL, 
       title = "Senado de la Provincia de Salta 2019-2021",
       subtitle = "Coalición oficialista redondeada en negro") +
  bbc_style() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 6),
        legend.position = "bottom") +
  scale_colour_manual(values = senado2019$colour,
                      limits = senado2019$party_long)

## Guardar Plot de Senadores
finalise_plot(plot_name = plot_senadores,
              source = "Fuente: Cámara de Senadores de la Provincia de Salta",
              save_filepath = "plots/ggparliamentSenadoresSalta.png",
              width_pixels = 640,
              height_pixels = 550)
