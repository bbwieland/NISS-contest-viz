library(tidyverse)
library(maps)
library(RColorBrewer)

data = read.csv("data/NCES.csv") %>%
  mutate(State = gsub("\\ ","",State))
data.50states = data %>% filter(State != "UnitedStates" & State != "DistrictofColumbia")
data.hs = data.50states %>% filter(Degree == "High School") %>% mutate(State = tolower(State))
data.bach = data.50states %>% filter(Degree == "Bachelors") %>% mutate(State = tolower(State))

usMap = map_data("state") 

usMapHS = usMap %>% left_join(data.hs, by = c("region" = "State"))
usMapBach = usMap %>% left_join(data.bach, by = c("region" = "State"))

# draw a blank U.S. map 

ggplot() +
  geom_polygon(data = usMap,aes(x = long, y = lat, group = group),
               color = "black",fill = "lightblue") +
  theme_void()

# fill the map using

ggplot() +
  geom_polygon(data = usMapBach,aes(x = long, y = lat, group = group,fill = Black),
               color = "black") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1,na.value = "lightgrey") +
  theme_void()

