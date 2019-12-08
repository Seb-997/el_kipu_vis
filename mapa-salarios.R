library(sp)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(formattable)

map_departamentos <- readRDS("gadm36_PER_1_sp.rds")
dep_ggplot <- map_data(map_departamentos,namefield = "NAME_1")


salario_departamentos <- read.csv("./data/salario-promedio-departamento.csv",stringsAsFactors = F, encoding="UTF-8")
colnames(salario_departamentos) <- c("region","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")


map_dep_ggplot <- dep_ggplot %>%
  left_join(salario_departamentos,by = "region")%>%
  mutate(porcentage0717 = (`2017`/`2007`)-1,
         average = mean(porcentage0717),
         promedio07 = mean(`2007`),
         promedio17 = mean(`2017`))


top_5_2007 <- salario_departamentos %>%
  select(c(1,2))%>%            
  arrange(-`2007`)%>%
  mutate(`2007` = round(`2007`,0))%>%
  top_n(10)

top_5_2017 <- salario_departamentos %>%
  mutate(`2017` = round(`2017`,0))%>%
  select(c(1,12))%>%
  arrange(-`2017`)%>%
  top_n(10)

mapa_porcentage <- map_dep_ggplot %>%
  ggplot()+
  geom_polygon(aes(x=long, y = lat, group = group, fill=porcentage0717),color="black",alpha=.9) +
  scale_fill_gradient(labels = scales::percent, low = "#CC1010", high = "#0A2342")+
  coord_fixed()+
  labs(title="Incremento (%) del sueldo promedio por departamento",subtitle = "2007-2017",fill = "Cambio porcentual",caption="@elqipu       Fuente: INEI")+
  theme(
    text = element_text(family="Roboto Thin", face = "bold"),
    plot.margin = unit(c(1,1,1,1),"lines"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggsave("departamentos-mapa.png",plot=mapa_porcentage,dpi=760,width=15,height=15,units="cm")
