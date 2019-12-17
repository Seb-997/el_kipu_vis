library(dplyr)
library(tidyr)
library(httr)
library(ggplot2)


nodes <- httr::GET('https://www.overpass-api.de/api/interpreter?data=[out:json];node(id:497082163);out;', 
               accept_json())%>%
               content("parsed")




