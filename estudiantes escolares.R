library(sp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(formattable)
library(ggalluvial)

estudiantes_publicos <- read.csv(".//data//estudiantes_escolares_publicos.csv",stringsAsFactors = F, encoding = "UTF-8")
colnames(estudiantes_publicos)[1] <- "Departamentos"

estudiantes_privados <- read.csv(".//data//estudiantes_escolares_privados.csv",stringsAsFactors = F, encoding = "UTF-8")
colnames(estudiantes_privados)[1] <- "Departamentos"

estudiantes_prep_pb <- estudiantes_publicos%>%
                          gather(año,publicos,-Departamentos)

estudiantes_privados_pr <- estudiantes_privados%>%
          gather(año,privados,-Departamentos)


alumnos_total_tipo_dep_a <- estudiantes_prep_pb %>%
                                    left_join(estudiantes_privados_pr,by=c("Departamentos","año"))%>%
                                    gather(tipo,estudiantes,-c(Departamentos,año))%>%
                                    mutate(año = gsub("X","",año),
                                           Departamentos = gsub(" [1-9]/","",Departamentos))


alumnos_total_tipo_dep_a

alumnos_total_tipo_a <- alumnos_total_tipo_dep_a%>%
                          group_by(año,tipo)%>%
                          summarise(n=sum(estudiantes))%>%
                          mutate(pc = n/sum(n))

alumnos_total_dep_a <- alumnos_total_tipo_dep_a%>%
                            group_by(año,Departamentos)%>%
                            summarise(n=sum(estudiantes))%>%
                            mutate(pc = n/sum(n))

ggplot(alumnos_total_tipo_a,
       aes(x = año, y = pc,stratum = tipo, alluvium=tipo)) +
  geom_alluvium(aes(fill=tipo), width = 1/12)

ggplot(alumnos_total_tipo_a,
       aes(x = año, y = n,stratum = tipo, alluvium=tipo)) +
  geom_alluvium(aes(fill=tipo), width = 1/12)

ggplot(alumnos_total_dep_a,
       aes(x = año, y = n,stratum = Departamentos, alluvium=Departamentos)) +
  geom_alluvium(aes(fill=Departamentos), width = 1/12,color="black")
