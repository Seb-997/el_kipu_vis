library(sp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(formattable)
library(ggalluvial)
library(extrafont)
library(ggrepel)
library(png)
library(grid)
library(rsvg)

gasto <- read.csv(".//data//gasto_educacion_publico_privada.csv",stringsAsFactors = F, encoding = "UTF-8")
colnames(gasto)[1] <- "año"

img <- readPNG(source = ".//data//book.png")


gasto_lode_sin_total <- gasto %>%
  gather(tipo,gasto,-c(año))%>%
  filter(tipo != "total")

gasto_lode_solo_total <- gasto %>%
  gather(tipo,gasto,-c(año))%>%
  filter(tipo == "total")

ggplot() +
  geom_alluvium(data=gasto_lode_sin_total, aes(x = año, y = gasto/1000000, fill=tipo, stratum = tipo, alluvium=tipo, colour=tipo), width = 1/5,alpha=1)+
  scale_y_continuous(labels = paste0(seq(0,60,10),rep("B",7)),breaks = seq(0,60,10))+
  scale_x_continuous(breaks = seq(1994,2018,4 ))+
  annotate(geom="text",x=2006,y=40,label="Aumentó 687%",family = "Roboto Thin", colour="#3a3535",size=10)+
  annotate(geom="text",x=2017.4,y=32,label="bold('Privado: 29.2B')",family = "Roboto Thin",size=10,colour ="#ffffff",parse=T,angle=90)+
  annotate(geom="text",x=2017.4,y=10,label="bold('Publico: 19.7B')",family = "Roboto Thin",size=10,colour ="#ffffff",parse=T,angle=90)+
  ylab("Gasto S/.")+
  xlab("Año")+
  labs(title="Gasto en educacion - Perú", subtitle = "Publico vs Privado 1994-2018", 
  caption= "Gasto en la educacion peruana por sector. El gasto en el sector /n privado esta definido como el Valor Bruto de la Producción de la actividad de Educación Privada.")+
  scale_fill_manual(values = c("#0A2342","#cc1010"))+
  scale_colour_manual(values = c("#0A2342","#cc1010"))+
theme(
  text=element_text(family = "Roboto Thin", face="bold",colour="#3a3535"),
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  axis.line.x = element_line(colour = "#f4f4f4"),
  axis.line.y = element_line(colour = "#f4f4f4"),
  axis.ticks = element_blank(),
  axis.text = element_text(size=20,colour="#3a3535"),
  axis.title = element_text(margin=c(0,0,2,0),size=20,colour="#3a3535"),
  legend.position = "none",
  title = element_text(size=22),
  panel.background = element_rect(fill="#f4f4f4"),
  plot.background = element_rect(fill="#f4f4f4"),
  plot.caption = element_text(hjust=0,size=10)
  
)

