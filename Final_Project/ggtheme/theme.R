library(tidyverse)
library(RPMG)
library(RColorBrewer)
library(viridis)
library(scales)
## To set default theme for all ggplot, you just need to run the code in 1, 2, 3 stpes before your code.  
## There are 26 colours in total. If you need more colours, please tell me.  

##------------
##1. Build Fidelity colour:   
fidelity.col <- c("#288dc1", "#799900", "#e89719", "#d0746f", "#56004e","#94c6e0", "#817777", "#5d87a1", "#429080", "#c2a204", "#b45340", "#4a7628", "#4c5f6c", "#b4cc95", "#dc8633", "#477718", "#e6b17f", "#7a9a01", "#755372", "#477227", "#c19943", "#d89d8a", "#c8cfd3", "#977f96", "#ffffff", "#bccc80")


##--------------
##2. Set default theme:
theme_set(theme_minimal())

##--------------
##3. Set default colour:
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = fidelity.col)
}

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = fidelity.col)
}

scale_colour_continuous<- function(...) {
  scale_color_gradient2(low = "#288dc1", high = "#4a7628", mid = "#e89719")
}

scale_fill_continuous<- function(...) {
  scale_fill_gradient2(low = "#288dc1", high = "#4a7628", mid = "#e89719")
}

