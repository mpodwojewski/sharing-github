# Wczytano wymagane pakiety.

library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(dtwclust)
library(BBmisc)

# Ustawiono katalog roboczy.

getwd()
setwd("C:/Users/Maciuœ/Desktop/R")

# Wczytano dane.

cities <- read.csv("dtw.csv")

# Nadano kolumnom w³aœciwe nazwy.

names(cities) <- c("city", "2014":"2019")

# Zebrano dane w d³ugi¹ ramkê danych o nastêpuj¹cych zmiennych: 'city', 'year' oraz 'rate'.

cities_long <- data.table(gather(cities, key = "year", value = 'rate', "2014":"2019"))

# Nadano danym w kolumnie 'year' klasê data.

cities_long$year <- as.Date(ISOdate(cities_long$year, 1, 1))

# Nadano danym w kolumnie 'city' klasê faktor.

cities_long$city <- as.factor(cities_long$city)

str(cities_long)

# DTW: dokonano standaryzacji danych

cities.norm <- BBmisc::normalize(cities, method = "standardize")

# DTW: stworzono wektor nazw

cities.names <- as.character(cities.norm[,1])

# DTW: stworzono macierz wartoœci

cities.norm <- cities.norm[,-1]

# DTW: okreœlenie liczby klastrów przy pomocy Silhouette index.

clust.pam <- tsclust(cities.norm, type = "partitional", k = 2L:5L, distance = "dtw", centroid = "mean")

sapply(clust.pam, cvi, type = "Sil")

# DTW: okreœlenie liczby klastrów prz pomocy Elbow method

y <- sapply(clust.pam, function(cl) { sum(cl@cldist ^ 2) })

x <- (2:5)

plot(x = x, y = y, type = "l")

# DTW: Zdecycdowano o podziale danych na 3 grupy

# DTW: dokonano klastrowania z wykorzystanie DTW oraz analizy skupieñ z wykorzysaniem wartoœci œredniej.

clust.pam <- tsclust(cities.norm, type = "partitional", k = 3L, distance = "dtw", centroid = "mean")

# Zwizualizowano efekty klastrowania miast.

plot(clust.pam, type = "sc")

# Stworzono ramkê danych zawierajac¹ wyniki klastrowaniam oraz po³aczono j¹ z d³ug¹ ramk¹ danych zawierajac¹ wartoœci wskaŸnika w kolejnych latach

clast <- as.data.frame(cbind(cities.names, cluster = clust.pam@cluster))

names(clast) <- c('city', 'clast')

cities_long <- merge(cities_long, clast, by = 'city')

# Dokonano wizualizaji danych.

ggplot(cities_long, aes(x = year, y = rate, linetype = clast)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)

(cities_selected_1 <- cities_long %>%
    filter(clast == 1) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

(cities_selected_2 <- cities_long %>%
    filter(clast == 2) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

(cities_selected_3 <- cities_long %>%
    filter(clast == 3) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

cities_long %>%
  filter(clast == 1) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)

cities_long %>%
  filter(clast == 2) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)

cities_long %>%
  filter(clast == 3) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)
