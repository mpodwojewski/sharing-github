# Wczytano wymagane pakiety.

library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(tidyr)

# Ustawiono katalog roboczy.

getwd()
setwd("C:/Users/Maciuœ/Desktop/R")

# Pobrano mapê Europy.

EU <- get_map(location = 'Europe', zoom = 4, maptype = "toner-background", source = "stamen")

ggmap(EU)

# Wczytano dane.

cities <- read.csv("Airbnb_map.csv")

cities <- select(cities, c("X", "X2014","X2015","X2016","X2017","X2018","X2019"))

names(cities) <- c("city", "2014", "2015", "2016", "2017", "2018", "2019")

# Pobrano d³ugoœæ i szerokoœæ geograficzn¹ miast.

cities <- cbind(geocode(as.character(cities$city)), cities)

# Zapisano dane do pliku csv.

write.csv(cities, file = "cities_geocode")

# Zebrano dane w d³ug¹ ramkê danych o nastêpujacych kolumnach: "lon", "lat", "city", "year" oraz "rate".

cities_long <- data.table(gather(cities, key = "year", value = 'rate', "2014":"2019"))

# Usuniêto wiersze zawierajace zerow¹ wartoœæ w kolumnie rate.

cities_long <- cities_long[cities_long$rate != 0,]

str(cities_long)

# Stworzono wykresy.

ggmap(EU) +
  geom_point(data=cities_long, aes(x=lon, y=lat, size=rate, alpha = 0.9), color="orange") +
  guides(alpha = FALSE, size = FALSE) + 
  facet_wrap(~year) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  
