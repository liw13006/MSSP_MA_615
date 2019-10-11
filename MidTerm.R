pacman::p_load(tidyverse,rattle,psych,GPArotation,magrittr)
ClimateRaw1 = readxl::read_xls("Climate_Change.xls",sheet = 1,na = "..")
ClimateRaw2 = readxl::read_xls("Climate_Change.xls",sheet = 2,na = "..")
ClimateRaw3 = readxl::read_xls("Climate_Change.xls",sheet = 3,na = "..")

Regioncode = dplyr::filter(ClimateRaw2,Region == "Aggregates")[-grep("income",ClimateRaw2$`Country name`),] 
Regioncode = Regioncode %>% select(`Country code`) %>% pull()

unique(select(RegionClimate,`Series code`))

RegionClimate = dplyr::filter(ClimateRaw1,`Country code` %in% Regioncode)


