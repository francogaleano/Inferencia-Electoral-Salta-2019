###### HISTORIA POLITICA DE SALTA 
########################################################
### El objetivo de este script es realizar una linea de tiempo  
### sobre los gobernadores salteños desde 1983
### y barplots con los resultados de cada eleccion a gobernador desde entonces
### luego se exportara un plot de ambos juntos
#########################################################

#libs
library(bbplot)
library(ggpubr)
library(patchwork)
library(readxl)
library(tidyverse)


## LINEA DE TIEMPO DE LOS GOBERNADORES DE SALTA DESDE EL 1983

#creamos base de datos
gobernadores <- tibble(Gobernacion = c("R. Romero (PJ)", "H. Cornejo (PJ)","R. Ulloa (PRS)",
                                   "J. C. Romero (PJ)", "J. M. Urtubey (PJ+PRS)","G. Sáenz (PIS)"),
                      StartDate = c(1983,1987,1991,1995,2007,2019),
                      EndDate = c(1987,1991,1995,2007,2019,2023),
                      Color = c("#008000","#FFA500","#00BFFF","#8B008B","#0000FF","#8B0000"))

## LInea de tiempo
timeline_gobernadores <- gobernadores %>% 
  ggplot() +
  geom_segment(aes(x=StartDate,
                   xend=EndDate,
                   y=0., yend=0.,
                   color=Gobernacion),
               linetype=1,
               size=8) +
  scale_y_continuous(limits=c(0,0.5))+
  scale_x_continuous(limits=c(1981,2026),
                     breaks= c(seq(1983,2023,by=4),
                               gobernadores$StartDate,
                               gobernadores$EndDate[4]))+
  theme_bw() +
  geom_text( aes(x=StartDate-2 + (EndDate- StartDate)/2,
                 y=0.05,
                 label=Gobernacion,
                 angle=40,
                 hjust=0),
             size=5) +
  bbc_style() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=15),
        axis.ticks.y=element_blank(),
        aspect.ratio = .30,
        legend.position="none",
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10)) +
  labs(title = "Gobernadores y elecciones a gobernador en la provincia de Salta (1983-2019*) ",
       subtitle = "*2023 Incluído en línea de tiempo como fecha esperada de finalización legal del mandato en vigencia")


## Resultados de cada eleccion (BARPLOTS)
#datos
elecciones_historico <- read_excel("data/elecciones_salta_historico.xlsx") %>% 
  print()

elecciones_historico <- elecciones_historico %>% 
  ggplot(aes(x=reorder(fuerza_pol, porcentaje),
             y=porcentaje,
             color=fuerza_pol)) +
  geom_bar(stat="identity", aes(fill=fuerza_pol)) +
  coord_flip() +
  geom_text(aes(label=porcentaje), color="black", hjust=1,
            position = position_dodge(0.5), size=4) +
  facet_wrap(year~ ., ncol=5,scale="free") +
  bbc_style() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=10),
        legend.position="none",
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10))


# pegamos los dos graficos
ggarrange(timeline_gobernadores, elecciones_historico,
          ncol = 1, nrow = 2)

dev.off() # Agregas esto para "apagar" el graficador y que no se "rompan" las dimensiones del plot

ggsave("plots/S01plot_introductorio.png", width = 20, height = 20, units = "cm")
## exportado con "Export" de Rstudio porque no logré exportar más que el preview y quedaba muy compactado
