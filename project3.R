#install.packages("tidyverse") #run once to install
library(tidyverse)

#get raw data from CSV
setwd("C:/Users/Denton/Documents/.School/cs458/project3")
dataAll <- read.csv("v1.csv")

# #construct our dataset
dataset <- dataAll %>%
  filter(Country=="US") %>% 
  select(Latitude, Longitude, Facility.Type, CNG.Vehicle.Class) %>%
  rename(lat=Latitude, long=Longitude, type=Facility.Type, class=CNG.Vehicle.Class) %>% 
  filter(long > -125) %>% 
  filter(lat > 22)

#get all of the rows with any missing information
missing <- !complete.cases(dataset)
missing.df <- dataset[missing,]

#factorize categorical variables
dataset$type <- as.factor(dataset$type)
dataset$class <- as.factor(dataset$class)

MIN_LAT = min(dataset$lat)
MAX_LAT = max(dataset$lat)
MIN_LONG = min(dataset$long)
MAX_LONG = max(dataset$long)
MARGIN = 2
bg_map <- map_data("state")

bg_map %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), fill="lightgray", color="darkblue") +
  stat_density2d(data=dataset, aes(x = long, y = lat, fill = after_stat(level)), alpha=0.5, geom = "polygon", bins = 25) +
  scale_fill_gradient(low = "blue", high = "red") +
  #scale_color_brewer("PuBuGn") +
  scale_x_continuous(limits=c(MIN_LONG-MARGIN, MAX_LONG))+
  scale_y_continuous(limits=c(MIN_LAT-MARGIN, MAX_LAT+MARGIN))+
  theme_minimal() +
  labs(title = "Alternative Fueling Stations in the United States",
       x = "Longitude",
       y = "Latitude",
       fill = "Density")

# bg_map %>% 
#   ggplot() +
#   geom_polygon(aes(x=long, y=lat, group=group), fill="lightblue", color="darkblue") +
#   geom_point(data=dataset, aes(x = long, y = lat), alpha=0.5) +
#   theme_minimal() +
#   labs(title = "Heatmap of Latitude and Longitude Data",
#        x = "Longitude",
#        y = "Latitude")