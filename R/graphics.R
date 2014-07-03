library(pocr)
require(rCharts)
require(ggplot2)
require(dplyr)
require(RODBC)
require(lubridate)
require(extrafont)

setwd("C:/Users/gregorp.NEBULA2/Desktop/GitHub/presentation/R")
con_test <- odbcConnect("test_annie")
loadfonts()

sel.date.type <- 2
min.date <- as.Date(ymd('2006-01-01'))
sel.qry.type <- 0
sel.font <- "Frutiger LT Std 45 Light"

bo.ramp <- colorRampPalette(colors = portal_colors[c(4, 8)],
                            space = "Lab", bias = 1,
                            interpolate = "linear")
# show_col(bo.ramp(7))

col7 <- c("#7A4237",
          "#895267",
          "#727192",
          "#44919C",
          "#51A981",
          "#9DB65B",
          "#F2B45B")

format_sp <- function(x, date = T) {
    names(x) <- tolower(names(x))
    names(x) <- str_replace_all(names(x), " ", ".")
    names(x) <- str_replace_all(names(x), "/", ".")
    names(x) <- str_replace_all(names(x), "_", ".")
    names(x) <- str_replace_all(names(x), "-", ".")
    if (date) {
        to.change <- which(sapply(x, is.POSIXct))
        for (i in to.change) {
            x[, i] <- as.Date(x[, i])
        }
    }
    return(x)
}

## Entries data ----

entries <- list()
entries$by.age <- sqlQuery(con_test,
                           "
call test_annie.sp_ooh_flow_entries_rates('2000-01-01,2014-01-01',         
    '1,2,3,4' ,--  @age_grouping_cd =
        '0', -- race_cd =
        '0',--  @gender_cd =
        '0',--  @init_cd_plcm_setg = 
        '0',--  @long_cd_plcm_setg =
        '0',--  county_cd
        '0',--  @bin_los_cd 
        '0',--  @bin_placemet_cd =
        '0',--  @bin_ihs_svc_cd =
        '0',--  @cd_reporter_type = 
        '0',--  @filter_access_type =
        '0',--  @filter_allegation =
        '0',--  @filter_findig = 
        '0',--  @filter_service_category =
        '0',--  @filter_service_budget   
        '0'); -- bin_dep_cd
                           ") %>%
    format_sp() %>%
    filter(date.type == sel.date.type,
           qry.type.poc1 == sel.qry.type,
           cohort.period > min.date) %>%
    select(cohort.period, age.grouping.cd, age.grouping, rate.of.entries)

entries$by.age$age.grouping <- reorder(entries$by.age$age.grouping, X = entries$by.age$age.grouping.cd)

ent1 <- ggplot(entries$by.age, aes(x = cohort.period, y = rate.of.entries, color = age.grouping)) +
    geom_line(size = 1) +
    scale_color_manual(values = portal_colors[c(1, 4, 5, 8)]) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 8)) + 
    #scale_y_continuous(trans = "log") +
    labs(x = "Entry Quarter",
         y  = "Rate of Entries (per 1,000 population)",
         title = "WA Entries into Out-of-Home Care by Age",
         color = "Age Group") +
    theme_bw(base_family = sel.font)

ggsave("entry-rate-by-age.svg", ent1, width = 8, height = 6)

## Outcomes by Age Group (recreating Newby) ----

outcomes.raw <- format_sp(sqlQuery(con_test, "call test_annie.sp_ooh_outcomes(
    '1,2,3,4,5,6,7,8','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0'); "),
                          date = FALSE)

outcomes <- outcomes.raw %>%
    filter(cohort.period == 2009 &
               qry.type.poc1.first.unique == 0 &
               discharge %in% c("Reunification", "Adoption")) %>%
    mutate(months = months.since.entering.out.of.home.care) %>%
    select(months, age.grouping, age.grouping.cd, discharge, percent) %>%
    mutate(age.grouping = factor(str_trim(as.character(age.grouping)))) %>%
    mutate(age.grouping = reorder(age.grouping, X = age.grouping.cd)) %>%
    filter(!(age.grouping.cd == 7 & months > 30)) %>%
    mutate(discharge = factor(discharge, levels = c("Reunification", "Adoption")))

outcome.plot <- ggplot(outcomes, aes(x = months, y = percent/100,
                                     group = age.grouping, color = age.grouping)) +
    geom_line(size = 1) +
    facet_wrap(~ discharge) +
    labs(x = "Months since Entering Care",
         y = "Percent Achieving Outcome",
         color= "Age at Entry",
         title = "Outcomes by Age for WA Children\nEntering Care in 2009") +
    scale_x_continuous(breaks = c(3, 12, 24, 36, 48)) +
    scale_y_continuous(labels = percent_format()) +
    scale_color_manual(values = col7) +
    theme_bw(base_family = sel.font) +
    theme(strip.background = element_rect(fill = alpha(portal_colors[3], 0.5)),
          legend.position = "bottom") +
    guides(col = guide_legend(nrow = 3))


ggsave(filename = "outcomes-by-age.svg", outcome.plot, height = 5, width = 8)

## National Census Data

us <- read.csv("census.data.csv", stringsAsFactors = FALSE)
us2010 <- us %>% filter(Year == 2010) %>% select(-Year) %>% melt(id.vars = "State")

# ggplot(us, aes(x = Year, y = age0to17, group = State)) +
#     geom_line(alpha = 0.4) +
#     facet_wrap(~ State)

## Maltreatment from KidsCount (founded allegations) ----

# We can read data straight from Kids Count!
# This is essentially founded allegations:

mal.raw <- read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=6225&loc=1",
                    colClasses = c("character", "factor", "numeric", "factor", "numeric"),
                    na.strings = "N.A.")


mal <- mal.raw %>% filter(DataFormat == "Number" & !is.na(Data),
                   TimeFrame > 2002)

conversion <- c("age0to4", "age5to13", "age14to17", "age5to13", "age0to17")
names(conversion) <- levels(mal$Age.group)
mal$age <- conversion[as.character(mal$Age.group)]

mal.comb <- mal %>% group_by(Location, TimeFrame, DataFormat, age) %>% select(-Age.group) %>%
    summarize(Data = sum(Data)) %>%
    left_join(us2010 %>% mutate(Location = State, age = variable, total_pop = value) %>%
                  select(Location, age, total_pop)) %>%
    mutate(rate = Data / total_pop * 1000)


highlight.state <- c("Washington", "Oregon", "Texas", "Florida", "New York")

mal.for.plot <- na.omit(mal.comb) %>%
    mutate(state = factor(ifelse(Location %in% highlight.state, Location, "Other"),
                          levels = c(highlight.state, "Other"))) %>%
    arrange(state)

mal.plot <- ggplot(filter(mal.for.plot),
                   aes(x = TimeFrame, y = rate, group = Location, color = state)) +
    geom_line() +
    scale_color_manual(values = c(portal_colors[seq_along(highlight.state)],
                                  alpha("black", 0.5))) +
    scale_size_manual(values = c(rep(1.2, length(highlight.state)), 0.8)) +
    facet_wrap(~ age) +
    labs(x = "Year", y = "Count of Founded Allegations",
         title = "Founded Allegations for Each State",
         color = "") +
    theme_bw(base_family = sel.font)
mal.plot
ggsave(filename = "founded-allegations-us.svg", plot = mal.plot, width = 8, height = 6)

# Getting ready for D3 lines

region <- data.frame(State = unique(mal.for.plot$Location))
region$RegionCode <- NA
region$RegionName <- NA

region$RegionName <- ifelse(region$State %in%
                                c("Connecticut",
                                  "Maine",
                                  "Massachusetts",
                                  "New Hampshire",
                                  "Rhode Island",
                                  "Vermont"), "New England",
                            ifelse(region$State %in% c(
                                "Delaware",
                                "District of Columbia",
                                "Maryland",
                                "New Jersey",
                                "New York",
                                "Pennsylvania"), "Mideast",
                                ifelse(region$State %in% c(
                                    "Illinois",
                                    "Indiana",
                                    "Michigan",
                                    "Ohio",
                                    "Wisconsin"), "Great Lakes",
                                    ifelse(region$State %in% c(
                                        "Iowa",
                                        "Kansas",
                                        "Minnesota",
                                        "Missouri",
                                        "Nebraska",
                                        "North Dakota",
                                        "South Dakota"), "Plains",
                                        ifelse(region$State %in% c(
                                            "Alabama",
                                            "Arkansas",
                                            "Florida",
                                            "Georgia",
                                            "Kentucky",
                                            "Louisiana",
                                            "Mississippi",
                                            "North Carolina",
                                            "South Carolina",
                                            "Tennessee",
                                            "Virginia",
                                            "West Virginia"), "Southeast",
                                            ifelse(region$State %in% c(
                                                "Arizona",
                                                "New Mexico",
                                                "Oklahoma",
                                                "Texas"), "Southwest",
                                                ifelse(region$State %in% c(
                                                    "Colorado",
                                                    "Idaho",
                                                    "Montana",
                                                    "Utah",
                                                    "Wyoming"), "Rocky Mountain", 
                                                    "Far West")))))))

region.codes <- c("Far West" = "FW", "Rocky Mountain" = "MT", "Southwest" = "SW", "Southeast" = "SE",
                  "Plains" = "GP", "Great Lakes"= "GL", "Mideast" = "ME", "New England" = "NE")
region$RegionCode <- region.codes[region$RegionName]
write.csv(region, "state-regions.csv", row.names = FALSE)

mal.for.d3 <- mal.for.plot %>%
    filter(age == "age0to17") %>%
    select(Location, TimeFrame, rate) %>%
    dcast(Location ~ TimeFrame, value.var = "rate") %>%
    mutate(StateName = Location, State = Location) %>%
    select(-Location)

nc <- ncol(mal.for.d3)
mal.for.d3 <- mal.for.d3[, c(nc-1, nc, 1:(nc-2))]

write.csv(mal.for.d3, "maltreatment-for-d3.csv", na = "")

