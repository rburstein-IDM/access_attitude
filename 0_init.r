# load libraries
pacman::p_load(data.table, dplyr, tidyr, purrr, haven,
               stringi, stringr, stringdist, fuzzyjoin,
               jtools,
               lubridate,
               ggplot2, ggrepel, ggspatial, cowplot,sysfonts,showtext,
               lme4,
               sjPlot,
               sf, sp, leaflet, raster, terra,
               rio, jsonlite,
               poLCA,
               forcats,
               NCmisc,
               table1, flextable)

## Source utility functions
# ub <- new.env()
# files <- list.files("0_utility", full.names=T)
# for (file in files) sys.source(file, envir=ub)

## Set data path
db.paths <- fread("paths.csv", sep = ",")

# RB specifc paths
if(Sys.info()[['user']]=='royb') db.paths <- fread("pathsRB.csv", sep = ",")
paths <- new.env()
for (.name in db.paths$name) {
  paths[[.name]] <- db.paths[name==.name]$path
  if (.name!="root") paths[[.name]] <- paths[[.name]] %>% gsub("root", paths$root, .)
}

# db <- new.env()

## Unmask select
select <- dplyr::select


## Set up for some plotting stuff
font_add("Garamond", "GARA.TTF")
font_families()
showtext_auto()
theme_set(theme_classic() + 
            theme(text = element_text(size=25,family = "Garamond")))
clz    <- c('#5F4690','#1D6996','#38A6A5','#0F8554',
            '#73AF48','#EDAD08',
            '#E17C05','#CC503E','#94346E','#6F4070',
            '#994E95','#666666')
clz2   <- c('#11052C','#3D087B','#F43B86','#FFE459',
            '#9CC094','#1DB9C3')
clz3   <- c("#FF0075","#81B214","#334756")
blues  <- c('#190482','#7752FE','#8E8FFA','#C2D9FF')
blues2 <- c('#363062','#435585','#818FB4','#F5E8C7')
statclz<- c('#FF6363','#F8B400','#125B50')


