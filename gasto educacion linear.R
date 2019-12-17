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

grafico <- ggplot() +
  geom_line(data=gasto_lode_sin_total, aes(x = año, y = (gasto/1000000)-0.085,fill=tipo),size=1.85, colour="#000000",alpha=.5)+
  geom_line(data=gasto_lode_sin_total, aes(x = año, y = gasto/1000000, colour=tipo),alpha=1,size=1.5)+
  geom_line(data=gasto_lode_solo_total, aes(x = año, y = gasto/1000000),alpha=1,size=2,colour="black")+
  scale_y_continuous(labels = paste0(seq(0,60,10),rep("B",7)),breaks = seq(0,60,10))+
  scale_x_continuous(breaks = seq(1994,2018,4))+
  annotate(geom="text",x=2017,y=50.8,label="Total S/. 48.9B
          +687%",family = "Roboto", colour="#000000",size=5.2)+
  annotate(geom="text",x=2017,y=31.15,label="Pri: S/. 29.2B 
          +650%",family = "Roboto",size=5.2,colour ="#0A2342")+
  annotate(geom="text",x=2017,y=21.6,label="Púb: S/. 19.7B
           +850%",family = "Roboto",size=5.2,colour ="#cc1010")+
  annotate(geom="text",x=2008.5,y=17.6,label="2006-2007 \n Privado -14.7%",family = "Roboto", colour="#0A2342",size=4.5)+
  annotate(geom="text",x=2012.5,y=3.5,label="2009-2010 \n Público -0.1%",family = "Roboto", colour="#cc1010",size=4.5)+
  geom_curve(aes(x=2008.5,xend=2006.8,y=15.9,yend=12.8),size=1.3,colour="#0A2342",arrow=arrow(length = unit(0.01, "npc")),curvature = -.4,linetype="dashed")+
  geom_curve(aes(x=2010.5,xend=2009.4,y=3,yend=7.2),size=1.3,colour="#cc1010",arrow=arrow(length = unit(0.01, "npc")),curvature = -.4,linetype="dashed")+
  annotate(geom="segment",x=2000.75,xend=2000.75,y=0,yend=59,linetype = "dotted")+
  annotate(geom="segment",x=2001.5,xend=2001.5,y=0,yend=59,linetype = "dotted")+
  annotate(geom="segment",x=2006.5,xend=2006.5,y=0,yend=59,linetype = "dotted")+
  annotate(geom="segment",x=2011.5,xend=2011.5,y=0,yend=59,linetype = "dotted")+
  annotate(geom="segment",x=2015.5,xend=2015.5,y=0,yend=59,linetype = "dotted")+
  annotate(geom="segment",x=2017.22,xend=2017.22,y=0,yend=59,linetype = "dotted")+
  annotate(geom="text",x=1994.95,y=59,label ="AF")+
  annotate(geom="text",x=2001.15,y=59,label ="VP")+
  annotate(geom="text",x=2001.95,y=59,label ="AT")+
  annotate(geom="text",x=2007.00,y=59,label ="AGP")+
  annotate(geom="text",x=2011.95,y=59,label ="OH")+
  annotate(geom="text",x=2015.95,y=59,label ="PPK")+
  annotate(geom="text",x=2017.67,y=59,label ="MV")+
  ylab("Gasto S/.")+
  xlab("Año")+
  labs(title="Gasto en educación - Perú", subtitle = "Público vs Privado 1994-2018", 
       caption= "Gasto en la educación peruana por sector. El gasto en el sector                                           
privado está definido como el Valor Bruto de la Producción de la actividad de Educación Privada.
@elqipu   elqipu.pe                                                                                                                                                             Fuente: INEI"    
)+
  scale_fill_manual(values = c("#0A2342","#cc1010"))+
  scale_colour_manual(values = c("#0A2342","#cc1010"))+
  theme(
    text=element_text(family = "Roboto Thin", face="bold",colour="#3a3535"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(colour = "#f4f4f4",size=.75),
    axis.line.y = element_line(colour = "#f4f4f4",size=.75),
    axis.ticks = element_blank(),
    axis.text = element_text(size=20,colour="#3a3535"),
    axis.title = element_text(margin=c(0,0,2,0),size=20,colour="#3a3535"),
    legend.position = "none",
    title = element_text(size=22),
    panel.background = element_rect(fill="#EBEBEB"),
    plot.background = element_rect(fill="#EBEBEB"),
    plot.caption = element_text(hjust=0,size=13.5)
    
  )


ggsave(filename = "gasto_educacion_pub_pri.png",plot = grafico, height = 11,width = 11,units = "in",dpi=320)
