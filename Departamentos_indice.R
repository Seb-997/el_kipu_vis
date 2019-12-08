library(sp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(formattable)
library(ggrepel)


salario_departamentos <- read.csv("./data/salario-promedio-departamento.csv",stringsAsFactors = F, encoding="UTF-8")
colnames(salario_departamentos) <- c("region","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")


data_plot_linear <- salario_departamentos%>%
  gather('Año','Sueldo', -region)%>%
  group_by(region)%>%
  arrange(Año)%>%
  mutate(pc = Sueldo/Sueldo[Año == 2007],
         indice = 100*pc,
         zona = ifelse(region %in% c("Puno","Arequipa","Cuzco","Apurímac","Ayacucho","Huancavelica"),"Sierra Sur",
                       ifelse(region %in% c("Ica","Moqegua","Tacna"), "Costa Sur",
                              ifelse(region %in% c("Lima","Lima Province","Callao"), "Lima y Callao",
                                     ifelse(region %in% c("Paso","Junín","Huánuco"),"Sierra Centro",
                                            ifelse(region %in% c("Ancash","La Libertad","Lambayeque","Piura","Tumbes"),"Costa Norte",
                                                   ifelse(region %in% c("Cajamarca","San Martin","Amazonas"),"Sierra Norte","Selva")))))),
         islima = ifelse(region == "Lima Province",1,0))




departamentos<-ggplot()+
  geom_line(data = data_plot_linear[data_plot_linear$region != 'Lima Province',], aes(as.numeric(Año),indice,colour=as.factor(islima),alpha=islima,group=region),size=1)+
  geom_line(data = data_plot_linear[data_plot_linear$region == 'Lima Province',], aes(as.numeric(Año),indice,colour=as.factor(islima),alpha=islima,group=region),size=1.2)+
  geom_text_repel(data = data_plot_linear[data_plot_linear$region == 'Lima Province' & data_plot_linear$Año == 2017,], aes(as.numeric(Año),indice,colour=as.factor(islima),label=paste0("Prov. Lima "," ",round(indice,0))),size=5,point.padding = 0.5,nudge_y=-35,nudge_x=-.5,family = "Roboto Thin")+
  geom_text_repel(data = data_plot_linear[data_plot_linear$region == 'Apurímac' & data_plot_linear$Año == 2015,], aes(as.numeric(Año),indice,colour=as.factor(islima),label=paste0("Apurímac"," ",round(indice,0))),size=5,point.padding = 0.5,nudge_y=+10,nudge_x=.25,family = "Roboto Thin")+
  geom_text_repel(data = data_plot_linear[data_plot_linear$region == 'Puno' & data_plot_linear$Año == 2014,], aes(as.numeric(Año),indice,colour=as.factor(islima),label=paste0("Puno"," ",round(indice,0))),size=5,point.padding = 0.5,nudge_y=5,nudge_x=-3,family = "Roboto Thin")+
  geom_text_repel(data = data_plot_linear[data_plot_linear$region == 'Tumbes' & data_plot_linear$Año == 2008,], aes(as.numeric(Año),indice,colour=as.factor(islima),label=paste0("Tumbes"," ",round(indice,0))),size=5,point.padding = 0.5,nudge_y=5,nudge_x=+3,family = "Roboto Thin")+
  scale_x_continuous(limits = c(2007,2017), breaks=seq(2007,2017,2))+
  scale_y_continuous(limits = c(85,230),breaks=seq(0,220,20))+
  scale_alpha_continuous(range=c(0.3,1))+
  scale_colour_manual(values = c("#0A2342","#cc1010"))+
  labs(title= "10 Años de Aumentos en el Perú", subtitle= "Provincia de Lima vs Departamentos", caption ="@elqipu        Fuente: INEI")+
  ylab("Sueldo Promedio (2007=100)")+
  xlab("Año")+
  theme(
    text=element_text(family = "Roboto Thin", face="bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = "#000000"),
    axis.ticks = element_blank(),
    axis.text = element_text(size=13),
    axis.title = element_text(margin=c(0,0,2,0)),
    legend.position = "none",
    panel.background = element_blank()
  )


ggsave("departamentos.png",plot=departamentos,dpi=760,width=15,height=15,units="cm")
