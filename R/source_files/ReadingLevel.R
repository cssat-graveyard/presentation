rm(list=ls()) 

library(maptools)
library(ggplot2)
library(ggmap)
library(maps)
library(extrafont)
library(scales)

# Grabbing data from kidscount

reading<-read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=5116&loc=1",na.strings = "N.A.")

# subsetting data by achievement level and year

readsub<-subset(reading,Achievement.Level == "At or above proficient")
readsub<-subset(readsub,TimeFrame == "2013")


mapstates<-map_data("state")

# Lower case to match names in mapstates

readsub$Location <- tolower(as.character(readsub$Location))

# merging data

readsub.merge <-merge(mapstates,readsub,by.x='region',by.y='Location')
readsub.merge <- readsub.merge[order(readsub.merge$order),]
readsub.merge$Data <- as.numeric(as.character(readsub.merge$Data))

# Code for map

read.map <-ggplot(readsub.merge, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = Data) )+
  coord_map(projection = "globular") +
  labs(x= " ", y=" " ) +
  scale_fill_continuous(guide = F) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
read.map

ggsave("reading-map.pdf", read.map, height = 8, width = 10)

# code for legend 

read.legend <-ggplot(readsub.merge, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = Data) )+
  coord_map(projection = "globular") +
  labs(x= " ", y=" " ) +
  scale_fill_continuous(name = "Percentage\nat or above\nproficient",
                        labels = percent_format()) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
read.legend

leg <- read.legend + 
  coord_map(project="globular") +
  theme_bw(base_family = "Frutiger LT Std 45 Light")

require(grid)
require(gtable)
leg <- gtable_filter(ggplot_gtable(ggplot_build(leg)), "guide-box")
grid.newpage()
grid.draw(leg)
dev.off()























