library(dplyr)
library(tidyr)
library(httr)
library(ggplot2)
library(sp)
library(maps)
library(ggrepel)
library(extrafont)

cities <- data.frame(cities = c("LIM","AQP","TRU","CHIC","PIU","HUANC","CUZ","CHIM","IQT","TAC","JUL","ICA","CAJ","PUC","SUL","AYA","CHIN","HUANU","HUAC","TAR","PUN","PAI","HUA","TUM","PIS"), 
                     lat=c(-12.046,-16.39893,-8.11182,-6.77071,-5.1955,-12.07105,-13.525,-9.0745,-3.733333,-18.01409,-15.49159,-14.06352,-7.164444,-8.383333,-4.8983,-13.1603,-13.4177,-9.929464,-11.1,-6.483333,-15.843333,-5.091111,-9.533333,-3.566667,-13.709981), 
                     lon=c(-77.03052,-71.53693,-79.02862,-79.84214,-80.6301,-75.20829,-71.972222,-78.5934,-73.25,-70.25135,-70.13222,-75.72863,-78.510556,-74.5402,-80.6895,-74.2255,-76.1328,-76.239714,-77.6,-76.366667,-70.023611,-81.106389, -77.533333, -80.45,-76.203206)
                     ,stringsAsFactors = F)

cities_to_the_left <- c(1,3,4,8,17,19,22,24)
cities_to_the_right <- c(2,5,12,18,21,6)
cities_left_and_down <- c(25)

map_departamentos <- readRDS(".//data//gadm36_PER_0_sp.rds")
dep_ggplot <- map_data(map_departamentos,namefield = "NAME_0")


raw_data <- list()

for(i in 2:nrow(cities)){
  print(cities[i,1])
  route <- GET(paste0("https://graphhopper.com/api/1/route?point=",cities[1,2],',',cities[1,3],"&point=",cities[i,2],',',cities[i,3],"&points_encoded=false&detail=street_name&vehicle=car&locale=en&calc_points=true&key=API_KEY"), 
                     accept_json())%>%
    content("parsed")
  
  raw_data[[cities[i,1]]] <- route
  
}

final_df <- data.frame(destination = character(),order = numeric(),lon = numeric(),lat=numeric(), duration = numeric(), stringsAsFactors = F)

cities_to_route <- c(1:7,9:23)

for(j in cities_to_route){
  
  raw_lat_lon <- unlist(raw_data[[j]][["paths"]][[1]][["points"]][["coordinates"]])
  order <- 1:(length(raw_lat_lon)/2)
  destination <- rep(names(raw_data)[j],(length(raw_lat_lon)/2))
  
  print(names(raw_data)[j])
  df <-data.frame(destination = destination, order = order, lon = raw_lat_lon[seq(1,length(raw_lat_lon),2)], lat= raw_lat_lon[seq(2,length(raw_lat_lon),2)], stringsAsFactors = F)
  
  
  instructions_raw <- unlist(raw_data[[j]][["paths"]][[1]][["instructions"]])
  
  intervals <- as.numeric(as.character(instructions_raw[names(instructions_raw) %in% c("interval1","interval2")]))+1
  distances <- as.numeric(as.character(instructions_raw[names(instructions_raw) %in% c("distance")]))
  duration <- as.numeric(as.character(instructions_raw[names(instructions_raw) %in% c("time")]))
  
  ## need to undertstamd what on earth the itnervals are and how we can get them to be the coordinates?
  # look at points_encoded = ture in the API call.
  
  instructions_df <- data.frame(interval1 = intervals[seq(1,length(intervals),2)], interval2 = intervals[seq(2,length(intervals),2)],
                                distances = distances, duration = duration)%>%
                    mutate(no_intervals = (interval2-interval1),
                           avg_dist_per_int = distances/no_intervals,
                           avg_time_per_int = duration/no_intervals)
  
  durations_vector <- numeric()
  
  for(k in 1:nrow(instructions_df)){
    durations_vector <- c(durations_vector,rep(instructions_df[k,7],instructions_df[k,5]))
    
  }
  
  durations_df <- data.frame(intervals = 1:(length(durations_vector)), duration = durations_vector, destination = rep(names(raw_data)[j],length(durations_vector)),stringsAsFactors = F)


  df_with_durations <- df %>%
                        left_join(durations_df, by = c("order" = "intervals","destination" = "destination"))%>%
                        mutate(duration = replace_na(duration,0))

final_df <- rbind(final_df,df_with_durations)  
}

final_df <- final_df %>%
  group_by(destination)%>%
  mutate(
         cumsum_duration_raw_hours = cumsum(duration)/3600000,
         cumsum_duration_hours = as.integer(cumsum_duration_raw_hours),
         cumsum_duration_2_hour_int = (as.integer(cumsum_duration_raw_hours/2))*2
  )



ggplot()+
  geom_polygon(data =  dep_ggplot,aes(x=long, y = lat, group = group),color="white",fill=NA)+
  geom_path(data = final_df, aes(x=lon,y=lat,group=destination,colour=as.factor(cumsum_duration_2_hour_int)),lineend="round",linejoin="round",size=2)+
  geom_text(data=cities[-c(cities_to_the_left,cities_to_the_right,cities_left_and_down),],aes(x=lon,y=lat+0.35, label = cities),colour="white",family="Roboto",size=4.5)+
  geom_text(data=cities[cities_to_the_left,],aes(x=lon-.7,y=lat, label = cities),colour="white",family="Roboto",size=4.5)+
  geom_text(data=cities[cities_to_the_right,],aes(x=lon+.8,y=lat, label = cities),colour="white",family="Roboto",size=4.5)+
  geom_text(data=cities[cities_left_and_down,],aes(x=lon-.7,y=lat-.2, label = cities),colour="white",family="Roboto",size=4.5)+
  geom_point(data=cities,aes(x=lon,y=lat),colour="white",size=2)+
  annotate("text",x=-73.4,y=-4,label="No road connection",colour="white",family="Roboto")+
  scale_color_manual(labels = c(paste0(seq(0,16,2),'-',seq(2,18,2),"hours")),
                     values = c('green4','green2','green1','greenyellow','yellow','yellow3','orange','tomato','firebrick'))+
  labs(colour = "Car trip duration", title = "Driving travel time to major cities in Peru from Lima",
       subtitle = "Using Grasshopper Routing Service",caption = "@elqipu        www.elqipu.pe")+
  coord_fixed()+
  theme(
    text = element_text(colour="white", family = "Roboto Thin", face ="bold"),
    plot.background =element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill="black"),
    legend.box.background = element_rect(fill="black"),
    axis.title = element_blank(),
    legend.key = element_rect(fill="black",colour="black"),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size=18),
    plot.caption = element_text(size=16),
    legend.text = element_text(size=13),
    legend.title = element_text(size=13)
    
  )

ggsave("rutas_peru.png",plot = last_plot(), height=10,width=10, dpi = 320,unit="in")
