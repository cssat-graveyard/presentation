# Code for map of totals

library(maptools)
library(ggplot2)
library(ggmap)
library(maps)
library(extrafont)

RaceBirth<-read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=3&loc=1",na.strings = "N.A.")

unique(RaceBirth$TimeFrame)
subset(RaceBirth, Location == "Alabama")
head(RaceBirth)

# Code to subset data for 1999

Total1999<-subset(RaceBirth, Race == "Total")
Total1999<-subset(Total1999, DataFormat == "Rate per 1,000")
Total1999<-subset(Total1999, TimeFrame == "1999") 

loadfonts(quiet = T)

mapstates<-map_data("state")

Total1999$Location <- tolower(as.character(Total1999$Location))


Total.Map1999 <-merge(mapstates,Total1999,by.x='region',by.y='Location')
Total.Map1999 <- Total.Map1999[order(Total.Map1999$order),]
Total.Map1999$Data <- as.numeric(as.character(Total.Map1999$Data))

# Code for aggregate birth rates 1999 map

# range for data

total.range<-c(min(na.omit(Total.Map$Data)),max(na.omit(Total.Map2007$Data)))


birth_map99 <- ggplot(Total.Map1999, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  scale_fill_continuous(limits = total.range, guide = F) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = 'left')
#legend.position=c(0.90, 0.3),legend.title=element_text(size=10))
birth_map99 

ggsave("teen-birth-1999.pdf", birth_map99, height = 8, width = 10)

# Code to subset data for 2007

Total2007<-subset(RaceBirth, Race == "Total")
Total2007<-subset(Total2007, DataFormat == "Rate per 1,000")
Total2007<-subset(Total2007, TimeFrame == "2007") 

mapstates<-map_data("state")

Total2007$Location <- tolower(as.character(Total2007$Location))

Total.Map2007 <-merge(mapstates,Total2007,by.x='region',by.y='Location')
Total.Map2007 <- Total.Map2007[order(Total.Map2007$order),]
Total.Map2007$Data <- as.numeric(as.character(Total.Map2007$Data))

# Code for aggregate birth rates 2007 map

birth_map07 <- ggplot(Total.Map2007, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  scale_fill_continuous(limits = total.range, guide = F) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = 'left')
#legend.position=c(0.90, 0.3),legend.title=element_text(size=10))
birth_map07 

ggsave("teen-birth-2007.pdf", birth_map07, height = 8, width = 10)

# Code for seperating 2012 Data

Total<-subset(RaceBirth, Race == "Total")
Total<-subset(Total, DataFormat == "Rate per 1,000")
Total<-subset(Total, TimeFrame == "2012") 

loadfonts(quiet = T)

mapstates<-map_data("state")

Total$Location <- tolower(as.character(Total$Location))

Total.Map <-merge(mapstates,Total,by.x='region',by.y='Location')
Total.Map <- Total.Map[order(Total.Map$order),]
Total.Map$Data <- as.numeric(as.character(Total.Map$Data))

# Code for aggregate birth rates 2012 map

birth_map12 <- ggplot(Total.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  scale_fill_continuous(limits = total.range, guide = F) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = 'left')
#legend.position=c(0.90, 0.3),legend.title=element_text(size=10))
birth_map12 

ggsave("teen-birth-2012.pdf", birth_map12, height = 8, width = 10)

# legend


birth_leg <- ggplot(Total.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  scale_fill_continuous(limits = total.range) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = 'left')
#legend.position=c(0.90, 0.3),legend.title=element_text(size=10))
birth_leg

leg <- birth_leg + #scale_fill_brewer(palette="PuRd") +
  coord_map(project="globular") +
  theme_bw(base_family = "Frutiger LT Std 45 Light")

require(grid)
require(gtable)
leg <- gtable_filter(ggplot_gtable(ggplot_build(leg)), "guide-box")
# ggsave("teen-birth-race-legend.pdf")#, width = 1, height = 4)
grid.newpage()
grid.draw(leg)
dev.off()

# Code for data for birth rate by race

RaceBirth<-read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=3&loc=1",na.strings = "N.A.")

SubT<-subset(RaceBirth, DataFormat == "Rate per 1,000")
SubT<-subset(SubT, TimeFrame == "2012")
SubT<-subset(SubT, Race != "Total")
SubT<-subset(SubT, Race != "Asian and Pacific Islander")

mapstates<-map_data("state")

SubT$Location <- tolower(as.character(SubT$Location))

Sub.Map <-merge(mapstates,SubT,by.x='region',by.y='Location')
Sub.Map <- Sub.Map[order(Sub.Map$order),]
Sub.Map$Data <- as.numeric(as.character(Sub.Map$Data))

# Code for maps on one page

map.by.race <- ggplot(Sub.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data), color = "gray60", size = 0.1) +
  scale_fill_continuous(name = "Rate\n(per 1,000)") +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  facet_wrap(~Race) +
  #scale_fill_continuous(expand = c(0, 0)) +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
map.by.race

# Loop to split the maps

fill.range <- range(na.omit(Sub.Map$Data))
my_colors <- c("gray60", "#1A334B", "#1A334B", "#1A334B")
names(my_colors) <- unique(Sub.Map$Race)

for(i.Race in unique(Sub.Map$Race)){
  map.by.race <- ggplot(subset(Sub.Map, Race == i.Race), aes(long,lat,group=group)) + 
    geom_polygon(aes(fill=Data) , color = my_colors[i.Race], size = 0.00001) +
    coord_map(projection = "globular") +
    labs(x = "", y = "") +
    facet_wrap(~Race) +
    theme_minimal(base_family = "Frutiger LT Std 45 Light") +
    theme(panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())+
    scale_fill_continuous(limits = fill.range)
  ggsave(paste0("teen-birth", i.Race, ".pdf"), map.by.race, width = 7, height = 5)
}

?scale_fill_continuous

names(Sub.Map)

# ggsave("teen-birth-by-race.svg", map.by.race,  height = 8, width = 10)

# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.14/bin/gswin64c.exe")
# ggsave("teen-birth-by-race.pdf", map.by.race,  height = 8, width = 10)
# embed_fonts("teen-birth-by-race.pdf", outfile="teen-birth-by-race-embed.pdf")

# Code for Legend for Teen Birth Rates by Race

map.by.race.leg <- ggplot(Sub.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data), color = "gray60", size = 0.1) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  facet_wrap(~Race) +
  scale_fill_continuous(limits = fill.range, name = "Rate\n(per 1,000)")+
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
map.by.race.leg

leg <- map.by.race.leg + 
  coord_map(project="globular") +
  theme_bw(base_family = "Frutiger LT Std 45 Light")

require(grid)
require(gtable)
leg <- gtable_filter(ggplot_gtable(ggplot_build(leg)), "guide-box")
# ggsave("teen-birth-race-legend.pdf",leg, width = 1, height = 4)
grid.newpage()
grid.draw(leg)
dev.off()


