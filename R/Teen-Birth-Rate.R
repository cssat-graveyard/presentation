# Code for map of totals

RaceBirth<-read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=3&loc=1",na.strings = "N.A.")

Total<-subset(RaceBirth, Race == "Total")
Total<-subset(Total, DataFormat == "Rate per 1,000")
Total<-subset(Total, TimeFrame == "2012") 

loadfonts(quiet = T)

mapstates<-map_data("state")

Total$Location <- tolower(as.character(Total$Location))


Total.Map <-merge(mapstates,Total,by.x='region',by.y='Location')
Total.Map <- Total.Map[order(Total.Map$order),]
Total.Map$Data <- as.numeric(as.character(Total.Map$Data))

map.legend <- ggplot(Total.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
map.legend


ggsave("teen-birth.pdf", map.legend, height = 8, width = 10)


# Code for maps by race

RaceBirth<-read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=3&loc=1",na.strings = "N.A.")

SubT<-subset(RaceBirth, DataFormat == "Rate per 1,000")
SubT<-subset(SubT, TimeFrame == "2012")
SubT<-subset(SubT, Race != "Total")
SubT<-subset(SubT, Race != "Asian and Pacific Islander")


library(maptools)
library(ggplot2)
library(ggmap)
library(maps)
library(extrafont)

loadfonts(quiet = T)

mapstates<-map_data("state")

SubT$Location <- tolower(as.character(SubT$Location))


Sub.Map <-merge(mapstates,SubT,by.x='region',by.y='Location')
Sub.Map <- Sub.Map[order(Sub.Map$order),]
Sub.Map$Data <- as.numeric(as.character(Sub.Map$Data))

map.by.race<- ggplot(Sub.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data), color = "gray60", size = 0.1) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  facet_wrap(~Race) +
  #scale_fill_continuous(expand = c(0, 0)) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
#map.by.race

ggsave("teen-birth-by-race.svg", map.by.race,  height = 8, width = 10)

ggsave("teen-birth-by-race.pdf", map.by.race,  height = 8, width = 10)