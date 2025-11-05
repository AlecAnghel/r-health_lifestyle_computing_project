library(janitor)
sleep <- clean_names(sleep)
lifestyle <- clean_names(lifestyle)

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

ggplot(sleep, aes(x = physical_activity_level, y = quality_of_sleep))+
  geom_point()+
  geom_smooth()
