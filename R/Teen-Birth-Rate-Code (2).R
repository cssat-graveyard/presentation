# Code for map of totals

library(maptools)
library(ggplot2)
library(ggmap)
library(maps)
library(extrafont)

RaceBirth<-read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=3&loc=1",na.strings = "N.A.")

# Code to subset data 

Total<-subset(RaceBirth, Race == "Total")
Total<-subset(Total, DataFormat == "Rate per 1,000")
Total<-subset(Total, TimeFrame == "2012") 

loadfonts(quiet = T)

mapstates<-map_data("state")

Total$Location <- tolower(as.character(Total$Location))


Total.Map <-merge(mapstates,Total,by.x='region',by.y='Location')
Total.Map <- Total.Map[order(Total.Map$order),]
Total.Map$Data <- as.numeric(as.character(Total.Map$Data))

# Code for aggregate birth rates

map.legend <- ggplot(Total.Map, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill=Data)) +
  coord_map(projection = "globular") +
  labs(x = "", y = "") +
  scale_fill_continuous(name = "Rate\n(per 1,000)") +
  theme_minimal(base_family = "Frutiger LT Std 45 Light") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = 'left')
        #legend.position=c(0.90, 0.3),legend.title=element_text(size=10))
map.legend 

?theme()

ggsave("teen-birth.pdf", map.legend, height = 8, width = 10)

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
    scale_fill_continuous(limits = fill.range, guide = F)
  ggsave(paste0("teen-birth", i.Race, ".pdf"), map.by.race, width = 7, height = 5)
}

?scale_fill_continuous

names(Sub.Map)

# ggsave("teen-birth-by-race.svg", map.by.race,  height = 8, width = 10)

# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.14/bin/gswin64c.exe")
# ggsave("teen-birth-by-race.pdf", map.by.race,  height = 8, width = 10)
# embed_fonts("teen-birth-by-race.pdf", outfile="teen-birth-by-race-embed.pdf")

# Code for Legend for Teen Birth Rates by Race

leg <- map.by.race + #scale_fill_brewer(palette="PuRd") +
  coord_map(project="globular") +
  theme_bw(base_family = "Frutiger LT Std 45 Light")

require(grid)
require(gtable)
leg <- gtable_filter(ggplot_gtable(ggplot_build(leg)), "guide-box")
ggsave("teen-birth-race-legend.pdf")#, width = 1, height = 4)
grid.newpage()
grid.draw(leg)
dev.off()


