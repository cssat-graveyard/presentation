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

sel.date.type <- 1
min.date <- as.Date(ymd('2009-03-01'))
sel.qry.type <- 0

format_sp <- function(x, date = T) {
    names(x) <- tolower(names(x))
    names(x) <- str_replace_all(names(x), " ", ".")
    if (date) {
        to.change <- which(sapply(x, is.POSIXct))
        for (i in to.change) {
            x[, i] <- as.Date(x[, i])
        }
    }
    return(x)
}

## Getting data ----

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
    filter(date_type == sel.date.type,
           qry_type_poc1 == sel.qry.type,
           cohort.period > min.date) %>%
    select(cohort.period, age_grouping_cd, age.grouping, rate.of.entries)

entries$by.age$age.grouping <- reorder(entries$by.age$age.grouping, X = entries$by.age$age_grouping_cd)

ent1 <- ggplot(entries$by.age, aes(x = cohort.period, y = rate.of.entries, color = age.grouping)) +
    geom_line(size = 1) +
    scale_color_manual(values = portal_colors[c(1, 4, 5, 8)]) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2.4)) + 
    labs(x = "Entry Quarter",
         y  = "Rate of Entries (per 1,000 population)",
         title = "WA Entries into Out-of-Home Care by Age",
         color = "Age Group") +
    theme_bw(base_family = "")

ggsave("entry-rate-by-age.svg", ent1, width = 8, height = 6)



# We can read data straight from Kids Count!

mal <- read.csv("http://datacenter.kidscount.org/rawdata.axd?ind=6225&loc=1",
                colClasses = c("factor", "factor", "factor", "factor", "numeric"),
                na.strings = "N.A.")

ggplot(filter(mal, DataFormat == "Percent"), aes(x = TimeFrame, y = Data, group = Location)) +
    geom_line() +
    facet_wrap(~ Age.group)

