setwd("C:/Users/oieeri26.NEBULA2/Documents/working directory/HYS_Suicidal_Ideation_Grade_10")

hys.suicide<-read.csv("Essentials_for_Childhood-Healthy_Youth_Survey.csv", skip = 5)
hys.suicide
names(hys.suicide)

hys.suicide <- hys.suicide[!names(hys.suicide) %in% c("X.1","X.2","X.3","X.4","X.5","X.6")]

names(hys.suicide)<-c("race","percent","CI","percent.a","CI.a")

# creating seperate data frame for "During the past 12 months, did you ever seriously consider attempting suicide(yes,no)?

hys.ideation<-hys.suicide[names(hys.suicide) %in% c("race","percent","CI")]
hys.ideation

hys.ideation$percent <- hys.ideation$percent/100
hys.ideation$CI <- hys.ideation$CI/100

hys.ideation

# creating a seperate data frame for "When you feel sad or hopeless, are there adults that you can turn to for help 
# (I never feel sad or hopeless, yes, no, not sure)?

hys.hopeless <- hys.suicide[names(hys.suicide) %in% c("race","percent.a","CI.a")]
names(hys.hopeless)<-c("race","percent","CI")

hys.hopeless$percent <- hys.hopeless$percent/100
hys.hopeless$CI <- hys.hopeless$CI/100

hys.hopeless

# loading required packages

library(ggplot2)
library(reshape2)
library(plyr)
library(extrafont)

sel.font <- "PT Sans"

# for ideation

hys.ideation$mycolor <- factor(ifelse(hys.ideation$race == "State Average", 1, 0))
dodge <- position_dodge(width=0.9)

limits <- aes(ymax = hys.ideation$percent + hys.ideation$CI, ymin = hys.ideation$percent - hys.ideation$CI)

hys.ideation$race3 <- reorder(hys.ideation$race, -hys.ideation$percent + as.numeric(as.character(hys.ideation$mycolor)))


ideation.plot <- ggplot(hys.ideation, aes(x = race3, y = percent, fill = hys.ideation$mycolor ))+
  geom_bar(stat = "identity")+
  geom_errorbar(limits, position = dodge, width = .15)+
  labs(x = "", y = "")+
  geom_text(aes(label = percent(percent)), y = 0.01, color = "white", family = sel.font)+ 
  scale_y_continuous(labels = percent_format())+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c(poc_colors[1], poc_colors[2]), guide = F)+
  theme(axis.text.x = element_text(angle = -25, hjust = 0),
        plot.margin = unit(c(1, 3, 1, 1) * 5, "mm"),
        panel.border = element_rect(size = 1, colour = poc_colors[1]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1, colour = poc_colors[1]))

ideation.plot

ggsave("ideation.plot.pdf", ideation.plot, width = 7, height = 7)
embed_fonts("ideation.plot.pdf")



?geom_errorbar

# for hopeless

hys.hopeless$mycolor <- factor(ifelse(hys.hopeless$race == "State Average", 1, 0))


limits <- aes(ymax = hys.hopeless$percent + hys.hopeless$CI, ymin = hys.hopeless$percent - hys.hopeless$CI)
dodge <- position_dodge(width=0.9)

# # hys.hopeless$order <- paste(hys.hopeless$percent, hys.hopeless$mycolor)
# levels(hys.hopeless$percent) <- c(levels(hys.hopeless$percent)[paste(hys.hopeless$percent, hys.hopeless$mycolor)])
# 
# levels(hys.hopeless$percent)[paste(hys.hopeless$percent, hys.hopeless$mycolor)]

levels(hys.hopeless$race)

hys.hopeless$race3 <- reorder(hys.hopeless$race, -hys.hopeless$percent + as.numeric(as.character(hys.hopeless$mycolor)))
levels(hys.hopeless$race3)


hopeless.plot <- ggplot(hys.hopeless, aes(x = race3, y = percent, fill = hys.ideation$mycolor))+
  geom_bar(stat = "identity")+
  geom_errorbar(limits, position = dodge, width = .15)+
  labs(x = "", y = "")+
  geom_text(aes(label = percent(percent)), y = 0.01, color = "white", family = sel.font)+ 
  scale_y_continuous(labels = percent_format())+
  theme_bw(base_family = sel.font, base_size = 14)+
  scale_fill_manual(values = c(poc_colors[1], poc_colors[2]), guide = F)+
  theme(axis.text.x = element_text(angle = -25, hjust = 0),
        plot.margin = unit(c(1, 3, 1, 1) * 5, "mm"),
        panel.border = element_rect(size = 1, colour = poc_colors[1]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1, colour = poc_colors[1]))

hopeless.plot

ggsave("hopeless.plot.pdf", hopeless.plot, width = 7, height = 7)
embed_fonts("hopeless.plot.pdf")

?geom_bar



levels(hys.hopeless$race) <- c(
  
  c(levels(hys.hopeless$percent)[levels(hys.hopeless$percent) != "0.121"],"0.121")

  
  ?levels

hys.hopeless$order <- paste(hys.hopeless$percent, hys.hopeless$mycolor)


require(dplyr)
mtsumm <- mtcars %>% group_by(cyl) %>% summarize(mmpg = mean(mpg))

ggplot(mtsumm, aes(x = cyl, y = mmpg)) + geom_bar(stat = "identity")

levels(mtsumm$cyl)

# Step 1, initial reorder
mtsumm$cyl <- reorder(mtsumm$cyl, mtsumm$mmpg)


ggplot(mtsumm, aes(x = cyl, y = mmpg)) + geom_bar(stat = "identity")

# Step 2, extract "total" and stick it at end
levels(mtsumm$cyl) <- c(levels(mtsumm$cyl)[levels(mtsumm$cyl) != "6"], "6")

                                                             
                                                             paste(hys.hopeless$racea, hys.hopeless$mycolor)])





poverty <- read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=4748&loc=49", na.strings = "N.A.",
                    stringsAsFactors = FALSE)

head(poverty)

pov <- filter(poverty, Location == "Washington" & DataFormat == "Percent" & TimeFrame == "2008-2010")
pov$Data <- as.numeric(str_replace_all(pov$Data, "\\*|%", ""))
pov$Data <- ifelse(pov$Data > 1, pov$Data / 100, pov$Data)




