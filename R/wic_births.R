
setwd("C:/Users/mienkoja/Dropbox/repos/presentation/R")
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.05/bin/gswin64c.exe")
sel.font <- "PT Sans"

require(xlsx)
require(tm)
require(ggplot2)
require(lubridate)
require(extrafont)
require(pocr)

# Get percentage of wic births

# downloading PDF file using script at https://github.com/gimoya/theBioBucket-Archives/blob/master/R/txtmining_pdf.R

url <- "http://www.doh.wa.gov/portals/1/Documents/Pubs/960-221_2010WICDataByCounty.pdf"
dest <- "C:\\Users\\mienkoja\\Dropbox\\repos\\presentation\\R\\960-221_2010WICDataByCounty.pdf"
download.file(url, dest, mode = "wb")

# set path to pdftotxt.exe and convert pdf to text (get pdftotxt.exe from http://www.foolabs.com/xpdf/download.html))

exe <- "C:\\Program Files\\xpdfbin-win-3.04\\bin64\\pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)
shell.exec(filetxt); shell.exec(filetxt)    # strangely the first try always throws an error..

txt <- readLines(filetxt) # don't mind warning...
txt <- readLines("960-221_2010WICDataByCounty.txt")

wic_births_2010c <- c(strsplit(txt[48], split=" ")[[1]], strsplit(txt[185], split=" ")[[1]])
wic_births_2010n <- as.numeric(sub("%", "", wic_births_2010c))/100

# Get percentage of teen births
# importing xls downloaded in repo but available from http://www.doh.wa.gov/portals/1/Documents/5400/BirthA92012.xls

birth_by_mage <- read.xlsx("BirthA92012.xls"
                           ,sheetName = "2010"
                           ,startRow = 5
                           ,endRow = 43
                           ,header = FALSE
                           ,colIndex = 1:6
                           )

teen_births_2010 <- rowSums(x = birth_by_mage[3:6])
totl_births_2010 <- birth_by_mage[2]
teen_bth_rt_2010 <- teen_births_2010/totl_births_2010[,1]


# Get percentage of low ed mother births
# importing xls downloaded in repo but available from http://www.doh.wa.gov/portals/1/Documents/5400/BirthA142012.xls



birth_by_medu <- read.xlsx("BirthA142012.xls"
                           ,sheetName = "2010"
                           ,startRow = 5
                           ,endRow = 43
                           ,header = FALSE
                           ,colIndex = 1:4
)

lwed_births_2010 <- rowSums(x = birth_by_medu[3:4])
lwed_bth_rt_2010 <- lwed_births_2010/totl_births_2010[,1]

# plot stuff

plot_dat <- data.frame(wic = wic_births_2010n,
                       lwed = lwed_bth_rt_2010,
                       teen = teen_bth_rt_2010,
                       county = birth_by_medu$X1)

ggplot(plot_dat, aes(x=wic, y=lwed)) +
        geom_smooth(size = 2, colour=portal_colors[1], fill=NA) +
        geom_point(size = 4, colour=portal_colors[4]) +
        scale_y_continuous(labels = percent) +
        scale_x_continuous(labels = percent) +
        labs(x = "Percentage of WIC Births",
             y = "Percentage of Mothers with Less than High School Education") +
        theme_bw(base_family = "Frutiger LT Std 45 Light",
                 base_size = 18)
ggsave(file="lwedBYwic.pdf")
embed_fonts("lwedBYwic.pdf")

ann <- subset(plot_dat, county %in% c("Adams", "King", "Pierce", "Spokane", "Yakima"))
ann$xadj <- c(-3, 2.5, -2.7, 3.7, -3.1) / 100

ggplot(plot_dat, aes(x=wic, y=teen)) +
    geom_smooth(size = 2, colour=portal_colors[1], fill=NA, span = .9, method = "loess") +
    geom_point(size = 4, colour=portal_colors[4]) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = percent) +
    labs(x = "Percentage of WIC Births",
         y = "Percentage of Teen Mothers") +
    geom_text(aes(label = county, x = wic + xadj), data = ann, family = sel.font) +
    coord_fixed() +
    theme_bw() +
    theme(panel.border = element_rect(size = 1, colour = poc_colors[1]),
          axis.ticks = element_line(size = 1, colour = poc_colors[1]),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray80"),
          text = element_text(family = sel.font))

ggsave(file="teenBYwic.pdf", height = 7, width = 7)
embed_fonts("teenBYwic.pdf")



