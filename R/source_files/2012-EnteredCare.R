###Clear Memory
rm(list=ls()) 

###Load required libraries
library(RCurl)
library(stringr) 
library(XML)
library(ggplot2)
library(reshape2)
require(plyr)
library(RODBC)

###Create Connections to Database
#cn <- odbcConnect("POC")

###Define a span of states
state_min <- 1
state_max <- 52 #PR and Washington DC are also included

###Create an empty data frame to write to
state_entries_by_age = data.frame()

###Loop through all states
for(i in state_min:state_max) {

  ###Get office web page
  page_cn <- noquote(paste('getURI("http://cwoutcomes.acf.hhs.gov/data/tables/foster_age_enteredcares?states[]='
                           ,i
                           ,'&state=&region=")'
                           ,sep = ""))
  
  page <- eval(parse(text=page_cn))
  
  table <- readHTMLTable(page, stringsAsFactors = F)[[1]]
  
  #exclude_2008 <- sum(as.numeric(ifelse(table[22,2] %in% c("<.1"), .05/100, as.numeric(table[22,2])/100))
                  #,as.numeric(ifelse(table[21,2] %in% c("<.1"), .05/100, as.numeric(table[21,2])/100))
                  #,as.numeric(ifelse(table[20,2] %in% c("<.1"), .05/100, as.numeric(table[20,2])/100))
                  #,as.numeric(ifelse(table[19,2] %in% c("<.1"), .05/100, as.numeric(table[19,2])/100)))*
                  #as.numeric(gsub(",", "", table[23,2], fixed = TRUE))
  exclude_2009 <- sum(as.numeric(ifelse(table[22,3] %in% c("<.1"), .05/100, as.numeric(table[22,3])/100))
                      ,as.numeric(ifelse(table[21,3] %in% c("<.1"), .05/100, as.numeric(table[21,3])/100))
                      ,as.numeric(ifelse(table[20,3] %in% c("<.1"), .05/100, as.numeric(table[20,3])/100))
                      ,as.numeric(ifelse(table[19,3] %in% c("<.1"), .05/100, as.numeric(table[19,3])/100)))*
                      as.numeric(gsub(",", "", table[23,2], fixed = TRUE))
  exclude_2010 <- sum(as.numeric(ifelse(table[22,4] %in% c("<.1"), .05/100, as.numeric(table[22,4])/100))
                      ,as.numeric(ifelse(table[21,4] %in% c("<.1"), .05/100, as.numeric(table[21,4])/100))
                      ,as.numeric(ifelse(table[20,4] %in% c("<.1"), .05/100, as.numeric(table[20,4])/100))
                      ,as.numeric(ifelse(table[19,4] %in% c("<.1"), .05/100, as.numeric(table[19,4])/100)))*
                      as.numeric(gsub(",", "", table[23,3], fixed = TRUE))
  exclude_2011 <- sum(as.numeric(ifelse(table[22,5] %in% c("<.1"), .05/100, as.numeric(table[22,5])/100))
                      ,as.numeric(ifelse(table[21,5] %in% c("<.1"), .05/100, as.numeric(table[21,5])/100))
                      ,as.numeric(ifelse(table[20,5] %in% c("<.1"), .05/100, as.numeric(table[20,5])/100))
                      ,as.numeric(ifelse(table[19,5] %in% c("<.1"), .05/100, as.numeric(table[19,5])/100)))*
                      as.numeric(gsub(",", "", table[23,4], fixed = TRUE))
  exclude_2012 <- sum(as.numeric(ifelse(table[22,5] %in% c("<.1"), .05/100, as.numeric(table[22,5])/100))
                      ,as.numeric(ifelse(table[21,5] %in% c("<.1"), .05/100, as.numeric(table[21,5])/100))
                      ,as.numeric(ifelse(table[20,5] %in% c("<.1"), .05/100, as.numeric(table[20,5])/100))
                      ,as.numeric(ifelse(table[19,5] %in% c("<.1"), .05/100, as.numeric(table[19,5])/100)))*
                      as.numeric(gsub(",", "", table[23,5], fixed = TRUE))
  
  
  #table$total_2008 <- as.numeric(gsub(",", "", table[23,2], fixed = TRUE))-exclude_2008
  table$total_2009 <- as.numeric(gsub(",", "", table[23,2], fixed = TRUE))-exclude_2009
  table$total_2010 <- as.numeric(gsub(",", "", table[23,3], fixed = TRUE))-exclude_2010
  table$total_2011 <- as.numeric(gsub(",", "", table[23,4], fixed = TRUE))-exclude_2011
  table$total_2012 <- as.numeric(gsub(",", "", table[23,5], fixed = TRUE))-exclude_2012
    
  
  state_entries_by_age <- rbind(state_entries_by_age, cbind(i,table))
}

state_entries_by_age <- subset(state_entries_by_age, !(Var.2 %in% c("Missing Data", "Number", "18 Years", "19 Years", "20+ Years")))


head(state_entries_by_age)

# names(state_entries_by_age) <- c("StateID", "Age", "Year2008", "Year2009", "Year2010", "Year2011", "total_2008", 
                                  "total_2010", "total_2011", "total_2012")

names(state_entries_by_age) <- c("StateID", "Age", "Year2009", "Year2010", "Year2011", "Year2012", "total_2009", 
                                 "total_2010", "total_2011", "total_2012")

state_entries_by_age[,3] <- as.numeric(state_entries_by_age[,3])
state_entries_by_age[,4] <- as.numeric(state_entries_by_age[,4])
state_entries_by_age[,5] <- as.numeric(state_entries_by_age[,5])
state_entries_by_age[,6] <- as.numeric(state_entries_by_age[,6])


#state_entries_by_age$age_count_2008 <- state_entries_by_age[,3]/100*state_entries_by_age[,7]
state_entries_by_age$age_count_2009 <- state_entries_by_age[,3]/100*state_entries_by_age[,7]
state_entries_by_age$age_count_2010 <- state_entries_by_age[,4]/100*state_entries_by_age[,8]
state_entries_by_age$age_count_2011 <- state_entries_by_age[,5]/100*state_entries_by_age[,9]
state_entries_by_age$age_count_2012 <- state_entries_by_age[,6]/100*state_entries_by_age[,10]

#elect2012 <- c("R", "R", "R", "R", "D", "D", "D", "D", "D", "D", "R", "D", "R", "D", "R", "D", "R", "R", "R", "D", "D", "D", "D", 
#"D", "R", "R", "R", "R", "D", "D", "D", "D", "D", "R", "R", "D", "R", "D", "D", "R", "D", "R", "R", "R", "R", "R", 
#"D", "D", "D", "R", "D", "R")

#child_pop2008 <-  as.numeric(c(1136093,182591,1628092,706506,9321509,1203697,827029,205946,102275,4017192,2476961,300320,421648,3152610,1610215,722614,713589
#,1020960,1109963,281714,1359000,1429298,2418742,1282500,761312,1429413,223639,452890,662595,295896,2077170,512286,4377432,2252837
#,146950,2769618,911099,865892,2822785,945705,230124,1073977,198958,1495165,6678677,836299,132466,1840282,1559513,390130,1344680,131453))
#child_pop2009 <- as.numeric(c(1135039,185688,1626539,708401,9293604,1217397,821157,206106,102236,3997562,2485696,302239,426258,3138534,1609815,725565,721426
#,1022060,1116056,277731,1353767,1422845,2372505,1282317,760555,1427029,223381,455514,665903,290761,2069591,516064,4346160,2273282
#,147903,2748083,922668,865573,2804867,920794,226045,1079447,199783,1494733,6794148,856621,130357,1845996,1573981,389001,1341267,134911))
#child_pop2010 <- as.numeric(c(1130523,188169,1630756,711084,9297344,1225617,815431,205616,101095,3996070,2490955,304573,428721,3123630,1605298,726778,726079
#,1023118,1116293,273457,1352083,1417262,2333718,1282527,754068,1423109,222979,458894,664456,286377,2062462,518998,4317426,2279498
#,149771,2723536,929314,865169,2789150,897920,223542,1079569,202892,1494958,6879384,871474,128735,1853407,1580986,386435,1336617,135155))
#child_pop2011 <- as.numeric(c(1127143,188441,1625114,710474,9271919,1230088,803314,204668,105334,3994431,2489858,304604,428116,3098125,1597603,724370,723922
#,1020955,1118196,269218,1346635,1405015,2295812,1277526,750239,1412121,222354,460065,663775,279984,2042810,519419,4286008,2287593
#,151156,2693092,936159,863767,2761159,876494,219536,1080555,203156,1492136,6960738,880309,126018,1853546,1581757,384794,1326208,134937))




# states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia",
  "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
  "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
  "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
  "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

## Changed order of states, Puerto Rico moved to end of the list to reflect correct state ID number

states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia",
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
            "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
            "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
            "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
            "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming","Puerto Rico")

states <- as.data.frame(states)
states$StateID <- seq(1,52)
# states$elect2012 <- elect2012
#states$child_pop2008 <- child_pop2008
#states$child_pop2009 <- child_pop2009
#states$child_pop2010 <- child_pop2010
#states$child_pop2011 <- child_pop2011
#states$child_pop2012 <- child_pop2012

state_entries_by_age_labels <- join(x = states, y = state_entries_by_age, by = "StateID", type = "left")
int_age <- rep(seq(0,17), 52)

## setwd("S:/Data Portal/joe_partially_sorted")
pop_by_age <- melt(read.csv("pop_totals_by_age.csv"), id="GEO.display.label")
pop_by_age$age <- str_extract(pop_by_age$variable, pattern= "[0-9]*$")
names(pop_by_age)[1] <- "states"

state_entries_by_age_labels$age <- str_extract(state_entries_by_age_labels$Age, pattern = "[0-9]*")
state_entries_by_age_labels$age[state_entries_by_age_labels$age == ""] <- "0"
state_entries_by_age_labels$age <- as.integer(state_entries_by_age_labels$age)

##state_entries_by_age_labels$child_pop_byage2010 <- pop_by_age$value
setwd("C:/Users/oieeri26.NEBULA2/Downloads")
seba <- join(state_entries_by_age_labels, pop_by_age)

# seba$plc_rate_by_age2008 <- 1000*(seba$age_count_2008/seba$value)
seba$plc_rate_by_age2009 <- 1000*(seba$age_count_2009/seba$value)
seba$plc_rate_by_age2010 <- 1000*(seba$age_count_2010/seba$value)
seba$plc_rate_by_age2011 <- 1000*(seba$age_count_2011/seba$value)
seba$plc_rate_by_age2012 <- 1000*(seba$age_count_2012/seba$value)

library(maps)

states_map <- map_data("state")


# seba$states <- tolower(seba$states) # Removed to keep names upper case
agemaps <- list()
for (age in 1:length(unique(seba$variable))) {
  agemaps[[age]] <-   ggplot(seba[seba$variable == unique(seba$variable)[age], ],
                             aes(map_id = states)) + geom_map(aes(fill = plc_rate_by_age2010), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) + coord_map(projection = "azequalarea") + labs(title = unique(seba$variable)[age])
}


#require(grid)
#do.call(grid.arrange, args=agemaps)

ggplot(seba, aes(y=plc_rate_by_age2010, x=factor(age))) + geom_smooth(aes(group=states), se = FALSE)

subset(seba,state="alabama")

head(seba)


EnCare<-seba[names(seba)%in%c("states","StateID","Age","Year2012")]

head(EnCare)
EnCare

castEnCare<-dcast(EnCare, states+StateID~Age, value.var = "Year2012")

rowSums(castEnCare[3:20])


# re ordering columns
castEnCare<-castEnCare[, c(1, 2, 20, 3, 12, 13, 14, 15, 16, 17, 18, 19, 4, 5, 6, 7, 8, 9, 10, 11)]

write.csv(castEnCare,"EnteredCare.csv")



?write.csv























