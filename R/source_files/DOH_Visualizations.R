setwd("C:/Users/oieeri26.NEBULA2/Documents/working directory/DOH Presentation Data")

# Importing Data

library(xlsx)
library(pocr)
library(plyr)
library(ggplot2)
library(stringr)
library(scales)
library(extrafont)
library(dplyr)

sel.font <- "PT Sans"

# reading in the workbook

wb <- loadWorkbook("health_data_Steering_Present.xlsx")
sheets <- getSheets(wb)
top <- list()
top$startRow <- 3
top$endRow <- 14
bottom <- list()
bottom$startRow <- 15
bottom$endRow <- 26


# readColumns(sheet = sheets[[1]], startColumn = 1, endColumn = 9,
           # startRow = 3, endRow = 14, header = T, stringsAsFactors = FALSE)


top.data <- list()
for (i in seq_along(sheets)) {
  top.i <- readColumns(sheet = sheets[[i]], startColumn = 1, endColumn = 5,
                       startRow = top$startRow, endRow = top$endRow,
                       header = T, stringsAsFactors = FALSE)
  top.data[[i]] <- top.i
  names(top.data)[i] <- names(sheets)[i]
}

lapply(top.data, head)
#top.data[[1]]


top.data$`PRAMS-Stressors`

bottom.data <- list()
for (i in seq_along(sheets)) {
  bottom.i <- readColumns(sheet = sheets[[i]], startColumn = 1, endColumn = 5,
                       startRow = bottom$startRow, endRow = bottom$endRow,
                       header = T, stringsAsFactors = FALSE)
  bottom.data[[i]] <- bottom.i
  names(bottom.data)[i] <- names(sheets)[i]
}

lapply(bottom.data, head)

# hacking the data up

# slide 1

top.data
top.data$`OSPI-4th & graduation`
top.data$`OSPI-4th & graduation`[3,]

# reading by ses

read.ses <- filter(top.data$`OSPI-4th & graduation`, type == "ses")
names(read.ses) <- c("type", "group", "n", "percent", "measure")
read.ses$percent <- read.ses$percent/100

read.ses$mycolor <- factor(ifelse(read.ses$group == "All children",1,0))

read.ses.plot <- ggplot(read.ses, aes(x = group, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "Percent", title = "Met 4th grade reading standard, 2012-13.")+
  geom_text(aes(label = percent(percent)), y = 0.02, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.border = element_rect(size = 2, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        panel.grid.minor = element_blank())
read.ses.plot


ggsave("read.ses.plot.pdf", read.ses.plot , width = 7, height = 7)
embed_fonts("read.ses.plot.pdf")

# reading by race

read.race <- filter(top.data$`OSPI-4th & graduation`, type == "race")
names(read.race) <- c("type", "group", "n", "percent", "measure")
read.race$percent <- read.race$percent/100

read.race.plot <- ggplot(read.race, aes(x = group, y = percent))+
  geom_bar(stat = "identity" , fill = "#87A318")+
  labs(x = "", y = "Percent", title = "Met 4th grade reading standard, 2012-13.")+
  geom_text(aes(label = percent(percent)), y = 0.02, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  scale_x_discrete(labels=c("American\nIndian", "Asian", "Black", "Hispanic", "Pacific\nIslander", "Two or\nmore races", "White"))+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#E8F4FF"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.border = element_rect(size = 2, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        panel.grid.minor = element_blank())

read.race.plot
  
ggsave("read.race.plot.pdf", read.race.plot , width = 7, height = 7)
embed_fonts("read.race.plot.pdf")

rgb(red = 55, green = 81, blue = 106, max = 255) # "#37516A"
rgb(red = 232, green = 244, blue = 255, max = 255) # "#E8F4FF"
rgb(red = 135, green = 163, blue = 24, max = 255) # "#87A318"

# high school grad by ses

dropout.ses <- filter(bottom.data$`OSPI-4th & graduation`, X. == "ses")
names(dropout.ses) <- c("type", "group", "n", "percent", "measure")
dropout.ses$percent <- dropout.ses$percent/100

dropout.ses$mycolor <- factor(ifelse(dropout.ses$group == "All children",1,0))

dropout.ses.plot <- ggplot(dropout.ses, aes(x = group, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "Percent", title = "On-Time Graduation, 2012-2013")+
  geom_text(aes(label = percent(percent)), y = 0.02, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

dropout.ses.plot

ggsave("dropout.ses.plot.pdf", dropout.ses.plot, width = 7, height = 7)
embed_fonts("dropout.ses.plot.pdf")


# high school grad by race

dropout.race <- filter(bottom.data$`OSPI-4th & graduation`, X. == "race")
names(dropout.race) <- c("type", "group", "n", "percent", "measure")
dropout.race$percent <- dropout.race$percent/100

dropout.race.plot <- ggplot(dropout.race, aes(x = group, y = percent))+
  geom_bar(stat = "identity", fill = "#87A318")+
  labs(x = "Race/Ethnicity", y = "Percent", title = "On-Time Graduation, 2012-2013")+
  geom_text(aes(label = percent(percent)), y = 0.02, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_x_discrete(labels=c("American\nIndian", "Asian", "Black", "Hispanic", "Pacific\nIslander", "Two or\nmore races", "White"))+                 
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

dropout.race.plot

ggsave("dropout.race.plot.pdf", dropout.race.plot, width = 7, height = 7)
embed_fonts("dropout.race.plot.pdf")


# slide 2

# teen birth by race

teen.birth.race <- top.data$`teen birth`[-c(8:11),]
names(teen.birth.race) <- c("raceeth","rate","n","population","measure")
teen.birth.race

teen.birth.race.plot <- ggplot(teen.birth.race, aes(x = raceeth, y = rate))+
  geom_bar(stat = "identity", fill = "#87A318")+
  labs(x = "Race/Ethnicity", y = "Births (per 1000 females)", title = "Teen Births (Ages 15-19)\nWashington, 2012")+
  geom_text(aes(label = rate), y = 1, color = "white", family = sel.font)+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_x_discrete(labels=c("American Indian/\nAlaska Native", "Asian", "Black", "Hispanic", "Multi-\nRacial", "Pacific\nIslander", "White"))+                 
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

teen.birth.race.plot

ggsave("teen.birth.race.plot.pdf", teen.birth.race.plot, width = 7, height = 7)
embed_fonts("teen.birth.race.plot.pdf")

# teen births by age

teen.birth.age <- bottom.data$`teen birth`

teen.birth.age.plot <- ggplot(teen.birth.age, aes(x = age, y = births.per.1.000))+
  geom_bar(stat = "identity", fill = "#87A318")+
  labs(x = "Age", y = "Births (per 1000 females)", title = "Teen Births\nWashington, 2012")+
  geom_text(aes(label = births.per.1.000), y = 1, color = "white", family = sel.font)+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#E8F4FF"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

teen.birth.age.plot

ggsave("teen.birth.age.plot.pdf", teen.birth.age.plot, width = 7, height = 7)
embed_fonts("teen.birth.age.plot.pdf")

# slide 3

# parenting and stress by race

race.parenting <- top.data$NSCH[-c(6:11),]
race.parenting <- cbind(race.parenting, str_trim(str_split_fixed(race.parenting$X95..CI, ",", n = 2)))
race.parenting <- race.parenting[!names(race.parenting) %in% c("X95..CI","X..1")]
names(race.parenting) <- c("raceeth", "percent", "measure", "LoCI", "UpCI")
race.parenting$percent <- race.parenting$percent/100
race.parenting$LoCI <- as.numeric(as.character(race.parenting$LoCI))/100
race.parenting$UpCI <- as.numeric(as.character(race.parenting$UpCI))/100
race.parenting

race.parenting$mycolor <- factor(ifelse(race.parenting$raceeth == "Total",1,0))

race.parenting.plot <- ggplot(race.parenting, aes(x = raceeth, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = LoCI, ymax = UpCI), position = dodge, width = .15)+
  labs(x = "Race/Ethnicity", y = "Percent", title = "Percent of children whose parent reports not having day-to-day\n emotional help with parenthood/raising children.")+
  geom_text(aes(label = percent(percent)), y = .01, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  scale_x_discrete(labels=c("Black", "Hispanic", "Hispanic", "Multi-racial", "Total", "Whites"))+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

race.parenting.plot

ggsave("race.parenting.plot.pdf", race.parenting.plot, width = 7, height = 7)
embed_fonts("race.parenting.plot.pdf")

# parenting and stress by ses

ses.parenting <- bottom.data$NSCH[-c(6:8),]
ses.parenting <- cbind(ses.parenting,str_trim(str_split_fixed(ses.parenting$X95..CI, ",", n = 2)))
ses.parenting <- ses.parenting[!names(ses.parenting) %in% c("X95..CI", "X..1", "MEASURE")]
names(ses.parenting) <- c("pov.level", "percent", "LoCI", "UpCI")
ses.parenting$percent <- ses.parenting$percent/100
ses.parenting$LoCI <- as.numeric(as.character(ses.parenting$LoCI))/100
ses.parenting$UpCI <- as.numeric(as.character(ses.parenting$UpCI))/100
ses.parenting

ses.parenting$mycolor <- factor(ifelse(ses.parenting$pov.level == "Total",1,0))

ses.parenting.plot <- ggplot(ses.parenting, aes(x = pov.level, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = LoCI, ymax = UpCI), position = dodge, width = .15)+
  labs(x = "Poverty Level", y = "Percent", title = "Percent of children whose parent reports not having day-to-day\n emotional help with parenthood/raising children.")+
  geom_text(aes(label = percent(percent)), y = .01, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

ses.parenting.plot

ggsave("ses.parenting.plot.pdf", ses.parenting.plot, width = 7, height = 7)
embed_fonts("ses.parenting.plot.pdf")

# slide 4

# breast feeding by age

b.feed.ages <- top.data$`PRAMS -Breastfeeding`[-c(7:11),]
names(b.feed.ages) <- c("age", "percent", "LoCI", "UpCI", "measure")
b.feed.ages$percent <- b.feed.ages$percent/100
b.feed.ages$LoCI <- b.feed.ages$LoCI/100
b.feed.ages$UpCI <- b.feed.ages$UpCI/100
b.feed.ages

b.feed.ages$mycolor <- factor(ifelse(b.feed.ages$age == "All Ages Combined", 1, 0))

b.feed.ages.plot <- ggplot(b.feed.ages, aes(x = age, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = LoCI, ymax = UpCI), position = dodge, width = .15)+
  labs(x = "Ages", y = "Percent", title = "Breastfed 8 or more Weeks")+
  geom_text(aes(label = percent(percent)), y = .02, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  scale_x_discrete(labels=c("<20", "20-24", "25-29", "30-34", "35+", "All Ages\nCombined"))+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

b.feed.ages.plot

ggsave("b.feed.ages.plot.pdf", b.feed.ages.plot, width = 7, height = 7)
embed_fonts("b.feed.ages.plot.pdf")

# breast feeding by race

b.feed.race <- bottom.data$`PRAMS -Breastfeeding`[-c(8:9),]
b.feed.race <- b.feed.race[!names(b.feed.race) %in% c("measure")]
names(b.feed.race) <- c("raceeth", "percent", "LoCI", "UpCI")
b.feed.race$percent <- b.feed.race$percent/100
b.feed.race$LoCI <- b.feed.race$LoCI/100
b.feed.race$UpCI <- b.feed.race$UpCI/100
b.feed.race


b.feed.race$mycolor <- factor(ifelse(b.feed.race$racee == "All Races Combined*", 1, 0))

b.feed.race.plot <- ggplot(b.feed.race, aes(x = raceeth, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = LoCI, ymax = UpCI), position = dodge, width = .15)+
  labs(x = "Race/Ethnicity", y = "Percent", title = "Breastfed 8 or more Weeks")+
  geom_text(aes(label = percent(percent)), y = .02, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  scale_x_discrete(labels=c("All Races\nCombined", "Hispanic", "American\nIndian/\nAlaska Native", 
    "Asian", "Black", "Pacific\nIslander", "White"))+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

b.feed.race.plot

ggsave("b.feed.race.plot.pdf", b.feed.race.plot, width = 7, height = 7)
embed_fonts("b.feed.race.plot.pdf")

# slide 5

# stressors reporting during pregnancy by age

preg.stress.age <- top.data$`PRAMS-Stressors`[-c(7:11),]
names(preg.stress.age) <- c("age","percent","LoCI", "UpCI", "measure")
preg.stress.age$percent <- preg.stress.age$percent/100
preg.stress.age$LoCI <- preg.stress.age$LoCI/100
preg.stress.age$UpCI <- preg.stress.age$UpCI/100
preg.stress.age

preg.stress.age$mycolor <- factor(ifelse(preg.stress.age$age == "All Ages Combined", 1, 0))

preg.stress.age.plot <- ggplot(preg.stress.age, aes(x = age, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = LoCI, ymax = UpCI), position = dodge, width = .15)+
  labs(x = "Age", y = "Percent", title = "6+ Stressors Reported During Pregnancy")+
  geom_text(aes(label = percent(percent)), y = .005, color = "white", family = sel.font)+
  scale_y_continuous(labels = percent_format())+
  scale_x_discrete(labels=c("<20", "20-24", "25-29", "30-34", "35+", "All Ages\nCombined"))+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

preg.stress.age.plot

ggsave("preg.stress.age.plot.pdf", preg.stress.age.plot, width = 7, height = 7)
embed_fonts("preg.stress.age.plot.pdf")

# stressors reporting during pregnancy by race

preg.stress.race <- bottom.data$`PRAMS-Stressors`[-c(1,3,6,9:11),]
names(preg.stress.race) <- c("raceeth", "percent", "LoCI", "UpCI")
preg.stress.race$percent <- as.numeric(preg.stress.race$percent)/100
preg.stress.race$LoCI <- as.numeric(preg.stress.race$LoCI)/100
preg.stress.race$UpCI <- as.numeric(preg.stress.race$UpCI)/100
preg.stress.race

preg.stress.race$mycolor <- factor(ifelse(preg.stress.race$raceeth == "All Races Combined*", 1, 0))

preg.stress.race.plot <- ggplot(preg.stress.race, aes(x = raceeth, y = percent, fill = mycolor))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = LoCI, ymax = UpCI), size = .8, width = .15)+
  labs(x = "Race/Ethnicity", y = "Percent", title = "6+ Stressors Reported During Pregnancy")+
  geom_text(aes(label = percent(percent)), y = .005, color = "white", family = sel.font, size = 5)+
  scale_y_continuous(labels = percent_format())+
  scale_x_discrete(labels=c("All Races Combined", "Hispanic", "American Indian/\nAlaska Native", "Black", "White"))+
  theme_bw(base_family = sel.font, base_size = 12)+
  scale_fill_manual(values = c("#87A318", "#37516A"), guide = F)+
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.border = element_rect(size = 2, color = "#37516A"),
        axis.ticks = element_line(size = 1, color = "#37516A"),
        panel.grid.major = element_blank(),
        plot.title = element_text(angle = 0, vjust = 1.5, size = 16),
        axis.title.x = element_text(vjust = .3, size = 14),
        axis.title.y = element_text(vjust = .3, size = 14),
        panel.grid.minor = element_blank())

preg.stress.race.plot

ggsave("preg.stress.race.plot.pdf", preg.stress.race.plot, width = 7, height = 7)
embed_fonts("preg.stress.race.plot.pdf")

?scale_fill_manual

?unit

?geom_errorbar



