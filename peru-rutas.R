library(dplyr)
library(tidyr)
library(httr)
library(ggplot2)
library(sp)
library(maps)

cities <- data.frame(cities = c("LIM","AQP","TRU","CHIC","PIU","HUANC","CUZ","CHIM","IQT","TAC","JUL","ICA","CAJ","PUC","SUL","AYA","CHIN","HUANU","HUAC","TAR","PUN","PAI","HUA","TUM","PIS"), 
                     lat=c(-12.046,-16.39893,-8.11182,-6.77071,-5.1955,-12.07105,-13.525,-9.0745,-3.733333,-18.01409,-15.49159,-14.06352,-7.164444,-8.383333,-4.8983,-13.1603,-13.4177,-9.929464,-11.1,-6.483333,-15.843333,-5.091111,-9.533333,-3.566667,-13.709981), 
                     lon=c(-77.03052,-71.53693,-79.02862,-79.84214,-80.6301,-75.20829,-71.972222,-78.5934,-73.25,-70.25135,-70.13222,-75.72863,-78.510556,-74.5402,-80.6895,-74.2255,-76.1328,-76.239714,-77.6,-76.366667,-70.023611,-81.106389, -77.533333, -80.45,-76.203206)
                     ,stringsAsFactors = F)

map_departamentos <- readRDS(".//data//gadm36_PER_0_sp.rds")
dep_ggplot <- map_data(map_departamentos,namefield = "NAME_0")

raw_df <- data.frame(order = numeric(),nodes = character(),ditances=numeric(),durations=numeric(),destination=character(),stringsAsFactors = F)

all_cities_nodes_raw <- c()

for(i in 23:nrow(cities)){
print(cities[i,1])
route <- GET(paste0('http://router.project-osrm.org/route/v1/driving/',cities[1,3],',',cities[1,2],';',cities[i,3],',',cities[i,2],'?overview=false&annotations=true'), 
             accept_json(),timeout(20))%>%
             content("parsed")

nodes_list <- route[["routes"]][[1]][["legs"]][[1]][["annotation"]][["nodes"]]
distances <- unlist(route[["routes"]][[1]][["legs"]][[1]][["annotation"]][["distance"]])
duration <- unlist(route[["routes"]][[1]][["legs"]][[1]][["annotation"]][["duration"]])


temp_df <- data.frame(nodes = matrix(unlist(nodes_list), nrow=length(nodes_list), byrow=T),stringsAsFactors=FALSE)

temp_df$order <- 1:nrow(temp_df)
temp_df$nodes <- as.character(temp_df$nodes)
temp_df$distances <- c(0,distances)
temp_df$durations <- c(0,duration)
temp_df$destination <- rep(cities[i,1],rep(nrow(temp_df)))

idx <- c(seq(1,nrow(temp_df),699),nrow(temp_df))

raw_city_nodes_data <- c()

  for(j in 1:(length(idx)-1)){
    print(c(idx[j],idx[j+1]))
    nodes_response <- GET(paste0('https://www.overpass-api.de/api/interpreter?data=[out:json];node(id:',
                        paste0(temp_df$nodes[idx[j]:idx[j+1]],c(rep(",",699),""),collapse =""),');out;'), 
                   accept_json())%>%
                   content("parsed")
    
    raw_nodes_data_section <- unlist(nodes_response$elements,recursive = T)  
    
    raw_city_nodes_data <- c(raw_city_nodes_data,raw_nodes_data_section)
  }

all_cities_nodes_raw <- c(all_cities_nodes_raw,raw_city_nodes_data)

raw_df <- rbind(raw_df,temp_df)

}


cleaned_nodes_data <- all_cities_nodes_raw[names(all_cities_nodes_raw) %in% c("id","lat","lon")]

nodes_ids <- cleaned_nodes_data[seq(1,length(cleaned_nodes_data),3)]
lat <- cleaned_nodes_data[seq(2,length(cleaned_nodes_data),3)]
lon <- cleaned_nodes_data[seq(3,length(cleaned_nodes_data),3)]

nodes_lat_long <- data.frame(nodes = as.character(nodes_ids), lats = as.numeric(as.character(lat)), lon =as.numeric(as.character(lon)),stringsAsFactors = F)

final_df <- raw_df %>%
  left_join(nodes_lat_long,by="nodes")%>%
  mutate(destination = as.character(destination))%>%
  group_by(destination)%>%
  unique()%>%
  mutate(durations_cumsum = round(((cumsum(durations)/3600)/4),0)*4)


colnames(final_df) <- c("origin","order","distance","durations","destination","lat","lon","durations_cumsum")


ggplot()+
  geom_polygon(data =  dep_ggplot,aes(x=long, y = lat, group = group),color="black",fill=NA)+
  geom_path(data = final_df[final_df$destination == "AQP",], aes(x=final_df[final_df$destination == "AQP",]$lon,y=final_df[final_df$destination == "AQP",]$lat,group=T,color=as.factor(final_df[final_df$destination == "AQP",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "TRU",], aes(x=final_df[final_df$destination == "TRU",]$lon,y=final_df[final_df$destination == "TRU",]$lat,group=T,color=as.factor(final_df[final_df$destination == "TRU",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "PIU",], aes(x=final_df[final_df$destination == "PIU",]$lon,y=final_df[final_df$destination == "PIU",]$lat,group=T,color=as.factor(final_df[final_df$destination == "PIU",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "CHIC",], aes(x=final_df[final_df$destination == "CHIC",]$lon,y=final_df[final_df$destination == "CHIC",]$lat,group=T,color=as.factor(final_df[final_df$destination == "CHIC",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "HUANC",], aes(x=final_df[final_df$destination == "HUANC",]$lon,y=final_df[final_df$destination == "HUANC",]$lat,group=T,color=as.factor(final_df[final_df$destination == "HUANC",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "CUZ",], aes(x=final_df[final_df$destination == "CUZ",]$lon,y=final_df[final_df$destination == "CUZ",]$lat,group=T,color=as.factor(final_df[final_df$destination == "CUZ",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "CHIM",], aes(x=final_df[final_df$destination == "CHIM",]$lon,y=final_df[final_df$destination == "CHIM",]$lat,group=T,color=as.factor(final_df[final_df$destination == "CHIM",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "TAC",], aes(x=final_df[final_df$destination == "TAC",]$lon,y=final_df[final_df$destination == "TAC",]$lat,group=T,color=as.factor(final_df[final_df$destination == "TAC",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "JUL",], aes(x=final_df[final_df$destination == "JUL",]$lon,y=final_df[final_df$destination == "JUL",]$lat,group=T,color=as.factor(final_df[final_df$destination == "JUL",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "ICA",], aes(x=final_df[final_df$destination == "ICA",]$lon,y=final_df[final_df$destination == "ICA",]$lat,group=T,color=as.factor(final_df[final_df$destination == "ICA",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "CAJ",], aes(x=final_df[final_df$destination == "CAJ",]$lon,y=final_df[final_df$destination == "CAJ",]$lat,group=T,color=as.factor(final_df[final_df$destination == "CAJ",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "PUC",], aes(x=final_df[final_df$destination == "PUC",]$lon,y=final_df[final_df$destination == "PUC",]$lat,group=T,color=as.factor(final_df[final_df$destination == "PUC",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "SUL",], aes(x=final_df[final_df$destination == "SUL",]$lon,y=final_df[final_df$destination == "SUL",]$lat,group=T,color=as.factor(final_df[final_df$destination == "SUL",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "AYA",], aes(x=final_df[final_df$destination == "AYA",]$lon,y=final_df[final_df$destination == "AYA",]$lat,group=T,color=as.factor(final_df[final_df$destination == "AYA",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "CHIN",], aes(x=final_df[final_df$destination == "CHIN",]$lon,y=final_df[final_df$destination == "CHIN",]$lat,group=T,color=as.factor(final_df[final_df$destination == "CHIN",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "HUANU",], aes(x=final_df[final_df$destination == "HUANU",]$lon,y=final_df[final_df$destination == "HUANU",]$lat,group=T,color=as.factor(final_df[final_df$destination == "HUANU",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "HUAC",], aes(x=final_df[final_df$destination == "HUAC",]$lon,y=final_df[final_df$destination == "HUAC",]$lat,group=T,color=as.factor(final_df[final_df$destination == "HUAC",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "TAR",], aes(x=final_df[final_df$destination == "TAR",]$lon,y=final_df[final_df$destination == "TAR",]$lat,group=T,color=as.factor(final_df[final_df$destination == "TAR",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "PUN",], aes(x=final_df[final_df$destination == "PUN",]$lon,y=final_df[final_df$destination == "PUN",]$lat,group=T,color=as.factor(final_df[final_df$destination == "PUN",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "PAI",], aes(x=final_df[final_df$destination == "PAI",]$lon,y=final_df[final_df$destination == "PAI",]$lat,group=T,color=as.factor(final_df[final_df$destination == "PAI",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "HUA",], aes(x=final_df[final_df$destination == "HUA",]$lon,y=final_df[final_df$destination == "HUA",]$lat,group=T,color=as.factor(final_df[final_df$destination == "HUA",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "TUM",], aes(x=final_df[final_df$destination == "TUM",]$lon,y=final_df[final_df$destination == "TUM",]$lat,group=T,color=as.factor(final_df[final_df$destination == "TUM",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_path(data = final_df[final_df$destination == "PIS",], aes(x=final_df[final_df$destination == "PIS",]$lon,y=final_df[final_df$destination == "PIS",]$lat,group=T,color=as.factor(final_df[final_df$destination == "PIS",]$durations_cumsum)),lineend="round",linejoin="round",size=2)+
  geom_text(data=cities,aes(x=lon,y=lat+0.25, label = cities))+
  geom_point(data=cities,aes(x=lon,y=lat))+
  #scale_color_manual(labels = c("0-4 hours","4-8 hours","8-12 hours","12-16 hours","16-20 hours"),)+
  labs(colour = "driving hours (car)")+
  coord_fixed()+
  theme(
        panel.background=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position =  "bottom")



