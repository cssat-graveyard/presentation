RaceBirth<-read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=3&loc=1",na.strings = "N.A.")
head(RaceBirth)
View(RaceBirth)

View(RaceBirth)


Total<-subset(RaceBirth, Race == "Total")
Total<-subset(Total, DataFormat == "Rate per 1,000")
Total<-subset(Total, TimeFrame == "2012") 
View(Total)

head(Total,20)
Total

library(maptools)
library(ggplot2)
library(ggmap)
library(maps)

mapstates<-map_data("state")

Total$Location <- tolower(as.character(Total$Location))


Total.Map <-merge(mapstates,Total,by.x='region',by.y='Location')
Total.Map <- Total.Map[order(Total.Map$order),]
Total.Map$Data <- as.numeric(as.character(Total.Map$Data))

map.legend <- ggplot(Total.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) #+
  #theme_nothing() #+
#  guides(fill = guide_colorbar())

require(extrafont)
loadfonts()

leg <- map.legend + #scale_fill_brewer(palette="PuRd") +
  coord_map(project="globular") +
  theme_bw(base_family = "Frutiger LT Std 45 Light")

map <- ggplot(Total.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) +
  theme_nothing() #+
#  guides(fill = guide_colorbar())

map  + #scale_fill_brewer(palette="PuRd") +
  coord_map(project="globular") 

ggsave("map.svg", height = 8, width = 10)

require(grid)
require(gtable)
leg <- gtable_filter(ggplot_gtable(ggplot_build(leg)), "guide-box")
svg("map-legend.svg", width = 1, height = 4)
grid.newpage()
grid.draw(leg)
dev.off()
















