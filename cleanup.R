#week2 homework (better late than never eh?)

library(tidyverse)
library(janitor)

meteorite <- read_csv("meteorite_landings.csv")


#fix the column headings to align with best practice
meteorite_clean <- clean_names(meteorite)

#remove the parenthesis from the geo_location column
meteorite_clean <- meteorite_clean %>%
  mutate(geo_location = str_remove_all(geo_location, "\\(")) %>%
    mutate(geo_location = str_remove_all(geo_location, "\\)"))

#split the geo_location into lat & long and remove unnecessary comma
meteorite_long_lat <- meteorite_clean %>% 
mutate(latitude = str_extract(geo_location, "[0-9]+.[0-9]+,")) %>% 
mutate(latitude = str_remove_all(latitude, ",")) %>% 
mutate(geo_location = str_remove_all(geo_location, "[0-9]+.[0-9]+, ")) %>% 
rename(longtitude = geo_location) %>% 
rename(mass = mass_g) %>% 
  #convert variables to numeric type
mutate(longtitude = as.numeric(longtitude)) %>% 
mutate(latitude = as.numeric(latitude)) %>% 
  #replace NAs with 0
mutate(longtitude = coalesce(longtitude, 0)) %>% 
mutate(latitude = coalesce(latitude, 0))

#include only meteors with weight > 1000g & sort by discovery date
meteorite_final <- meteorite_long_lat %>% 
  filter(mass >= 1000) %>% 
    arrange(year)


#write the cleaned data out to a separate file
write_csv(meteorite_final, "clean_data/meteorite_final.csv")



