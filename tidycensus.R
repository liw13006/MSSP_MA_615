pacman::p_load(tmap,tmaptools,ggvis)
install.packages("tidycensus","tmap")
library(tidycensus)
census_api_key("8c7840d98c245c52581b73480949cd3861a0594b",install = TRUE)
dat12 <- get_acs("county", table = "B27001", year = 2012, 
                 output = "tidy", state = NULL, geometry = FALSE) %>%
  rename(`2012` = estimate) %>%
  select(-NAME, -moe)

mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
