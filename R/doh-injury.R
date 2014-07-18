require(pocr)
require(dplyr)
require(extrafont)

setwd("C:/Users/gregorp.NEBULA2/Desktop/GitHub/presentation/R")

source("S:/Data Portal/data/doh/doh-r-import.R")
sel.font <- "PT Sans"

doh.rates$age <- factor(doh.rates$age, levels = levels(doh.rates$age),
                        labels = c("0", "1-4", "5-9", "10-14", "15-17"))
state.rates$age <- factor(state.rates$age, levels = levels(state.rates$age),
                          labels = c("0", "1-4", "5-9", "10-14", "15-17"))
state.avg <- state.rates %>% filter(type == "All Nonfatal Injury Hospitalizations") %>%
    mutate(state_rate = rate) %>%
    select(-county, -rate, -injury)

doh.for.plot <- doh.rates %>%
    filter(type == "All Nonfatal Injury Hospitalizations") %>%
    select(-injury) %>%
    left_join(state.avg) %>%
    mutate(state_label = factor("Washington\nAverage"))

doh.for.plot$state_label <- factor("Washington\nAverage")
doh.for.plot$county <- reorder(x = doh.for.plot$county, X = doh.for.plot$rate, FUN = function(x) -sum(x, na.rm = T))

doh.rates.plot <-  
    ggplot(doh.for.plot, aes(x = age, y = rate, fill = age)) +
    geom_bar(stat = "identity", color = "gray40") +
    geom_point(aes(y = state_rate, group = county, shape = state_label), color = "black") +
    scale_fill_brewer(palette = 2) +
    scale_shape_manual(values = 4, name = "") +
    facet_wrap(~ county, ncol = 8) +
    scale_x_discrete(labels = "", breaks = NULL) +
    labs(x = "Age",
         fill = "Age",
         y = "Rate of All Nonfatal Injury Hospitalizations\n(per 100,000 population)\n") +
    guides(fill = guide_legend(override.aes = list(colour = NA))) +
    theme_bw(base_family = sel.font) +
    theme(panel.border = element_rect(size = 0.8, colour = poc_colors[1]),
          axis.ticks = element_line(size = 1, colour = poc_colors[1]),
          strip.background = element_rect(fill = poc_colors[1]),
          strip.text.x = element_text(colour = "white"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray80"))

ggsave("S:/Data Portal/data/doh/nonfatal-injuries-by-county.pdf", doh.rates.plot, width = 14, height = 8)
