library(dplyr)
library(tidyr)
library(httr)
library(ggplot2)
library(sp)


route <- GET('http://router.project-osrm.org/route/v1/driving/-71.53693,-16.39893;-77.03052,-12.046?overview=false&annotations=true', 
             accept_json())%>%
             content("parsed")


df <- data.frame(nodes = matrix(unlist(nodes_list), nrow=length(nodes_list), byrow=T),stringsAsFactors=FALSE)

df$order <- 1:nrow(df)
df$nodes <- as.factor(df$nodes)

idx <- c(seq(1,nrow(df),699),nrow(df))

raw_nodes_data <- c()

for(i in 1:(length(idx)-1)){
  print(idx[i])
  nodes_response <- GET(paste0('https://www.overpass-api.de/api/interpreter?data=[out:json];node(id:',
                      paste0(df$nodes[idx[i]:idx[i+1]],c(rep(",",699),""),collapse =""),');out;'), 
                 accept_json())%>%
                 content("parsed")
  
  raw_nodes_data_section <- unlist(nodes_response$elements,recursive = T)  
  
  raw_nodes_data <- c(raw_nodes_data,raw_nodes_data_section)
}

cleaned_nodes_data <- raw_nodes_data[names(raw_nodes_data) %in% c("id","lat","lon")]

nodes_ids <- cleaned_nodes_data[seq(1,length(cleaned_nodes_data),3)]
lat <- cleaned_nodes_data[seq(2,length(cleaned_nodes_data),3)]
lon <- cleaned_nodes_data[seq(3,length(cleaned_nodes_data),3)]

nodes_lat_long <- data.frame(nodes = nodes_ids, lats = as.numeric(as.character(lat)), lon =as.numeric(as.character(lon)) )

df_2 <- df %>%
        mutate(
          destination = lead(nodes)
        )%>%
  left_join(nodes_lat_long,by="nodes")%>%
  left_join(nodes_lat_long,by=c("destination" = "nodes"))


colnames(df_2) <- c("origin","order","destination","start_lat","start_lon","end_lat","end_lon")

ggplot(df_2)+
  geom_point(aes(x=start_lon,y=start_lat))+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

ggplot(df_2)+
  geom_path(aes(x=start_lon,y=start_lat,group=T),size=2,color="red")+
  geom_polygon(data =  dep_ggplot,aes(x=long, y = lat, group = group),color="black",alpha=.9)+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

map_departamentos <- readRDS(".//data//gadm36_PER_0_sp.rds")
dep_ggplot <- map_data(map_departamentos,namefield = "NAME_0")


ggplot()+
  geom_polygon(data= dep_ggplot,aes(x=long, y = lat, group = group),color="black",alpha=1)+
  geom_path(data=df_2,aes(x=as.numeric(start_lon),y=as.numeric(start_lat),group=T),size=1.3,color="red")+
  coord_fixed()
