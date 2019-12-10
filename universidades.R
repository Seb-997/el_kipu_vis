library(sp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(formattable)

carnets <- read.csv(".//data//Carnes_Universitarios.csv",stringsAsFactors = F, encoding = "UTF-8")

universidades <- read.csv(".//data//Universidades.csv",stringsAsFactors = F, encoding = "UTF-8")

numero_estudiantes <- read.csv(".//data//estudiantes_universidad.csv",stringsAsFactors = F, encoding = "UTF-8")

DENEGADAS <- c("UNIVERSIDAD PERUANA SANTO TOMAS DE AQUINO DE CIENCIA E INTEGRACION","UNIVERSIDAD SAN PEDRO","UNIVERSIDAD SEMINARIO BIBLICO ANDINO",
               "UNIVERSIDAD PRIVADA JUAN MEJIA BACA SOCIEDAD ANONIMA CERRADA","UNIVERSIDAD PRIVADA AUTONOMA DEL SUR","UNIVERSIDAD PERUANA AUSTRAL DEL CUSCO",
               "UNIVERSIDAD NACIONAL SAN LUIS GONZAGA DE ICA","UNIVERSIDAD CIENCIAS DE LA SALUD","UNIVERSIDAD PRIVADA SISE","UNIVERSIDAD INCA GARCILASO DE LA VEGA ASOCIACION CIVIL",
               "UNIVERSIDAD PERUANA DEL ORIENTE","UNIVERSIDAD GLOBAL DEL CUSCO SOCIEDAD ANONIMA CERRADA","UNIVERSIDAD PRIVADA JUAN PABLO II","UNIVERSIDAD PRIVADA DE LA SELVA PERUANA",
               "UNIVERSIDAD DE AYACUCHO FEDERICO FROEBEL","GRUPO EDUCATIVO UNIVERSIDAD PRIVADA DE ICA","UNIVERSIDAD PRIVADA DE PUCALLPA","UNIVERSIDAD PARTICULAR DE CHICLAYO")


numero_estudiantes <- numero_estudiantes %>%
                          mutate(nombre = toupper(X.U.FEFF.nombre))%>%
                          select(nombre,estudiantes)


universidades<- universidades %>%
                    mutate(NOMBRE = gsub("Ó","O",NOMBRE),
                           NOMBRE = gsub("Á","A",NOMBRE),
                           NOMBRE = gsub("É","E",NOMBRE),
                           NOMBRE = gsub("Ú","U",NOMBRE),
                           NOMBRE = gsub("Í","I",NOMBRE),
                           NOMBRE = gsub(" S.A.C.","",NOMBRE),
                           NOMBRE = gsub(" S.A.C","",NOMBRE),
                           NOMBRE = gsub(" SAC","",NOMBRE),
                           NOMBRE = gsub(" SAC","",NOMBRE),
                           NOMBRE = gsub(" S.A.","",NOMBRE),
                           ESTADO.LICENCIAMIENTO = ifelse(NOMBRE %in% DENEGADAS,"DENEGADA",ESTADO.LICENCIAMIENTO))

carnets <- carnets %>%
  mutate(NOMBRE_UNIVERSIDAD = gsub(" S.A.C.","",NOMBRE_UNIVERSIDAD),
         NOMBRE_UNIVERSIDAD = gsub(" S.A.C","",NOMBRE_UNIVERSIDAD),
         NOMBRE_UNIVERSIDAD = gsub(" SAC","",NOMBRE_UNIVERSIDAD),
         NOMBRE_UNIVERSIDAD = gsub("UNIV. PERUANA DE INVEST. Y NEGOCIOS","UNIVERSIDAD PERUANA DE INVESTIGACION Y NEGOCIOS",NOMBRE_UNIVERSIDAD))


carnets%>%
  summarise(sum(CANTIDAD_CARNES))

nombres_universidades <- unique(universidades$NOMBRE)


carnets_solo_universidades <- carnets %>%
                                  filter(NOMBRE_UNIVERSIDAD %in% nombres_universidades)


universidades_con_estudiantes <- numero_estudiantes %>%
                              left_join(universidades[,2:6], by=c("nombre" = "NOMBRE"))


estudiantes_por_estatus <- universidades_con_estudiantes%>%
  group_by(ESTADO.LICENCIAMIENTO)%>%
  summarise(n=sum(estudiantes))

ggplot()+
  geom_bar(data=estudiantes_por_estatus,aes(ESTADO.LICENCIAMIENTO,n,fill=ESTADO.LICENCIAMIENTO),stat="identity")

