# Wczytano wymagane pakiety.

library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(dtwclust)
library(BBmisc)

# Ustawiono katalog roboczy.

getwd()
setwd("C:/Users/Maciu�/Desktop/R")

# Wczytano dane.

cities <- read.csv("dtw.csv")

# Nadano kolumnom w�a�ciwe nazwy.

names(cities) <- c("city", "2014":"2019")

# Zebrano dane w d�ugi� ramk� danych o nast�puj�cych zmiennych: 'city', 'year' oraz 'rate'.

cities_long <- data.table(gather(cities, key = "year", value = 'rate', "2014":"2019"))

# Nadano danym w kolumnie 'year' klas� data.

cities_long$year <- as.Date(ISOdate(cities_long$year, 1, 1))

# Nadano danym w kolumnie 'city' klas� faktor.

cities_long$city <- as.factor(cities_long$city)

str(cities_long)

# DTW: dokonano standaryzacji danych

cities.norm <- BBmisc::normalize(cities, method = "standardize")

# DTW: stworzono wektor nazw

cities.names <- as.character(cities.norm[,1])

# DTW: stworzono macierz warto�ci

cities.norm <- cities.norm[,-1]

# DTW: okre�lenie liczby klastr�w przy pomocy Silhouette index.

clust.pam <- tsclust(cities.norm, type = "partitional", k = 2L:5L, distance = "dtw", centroid = "mean")

sapply(clust.pam, cvi, type = "Sil")

# DTW: okre�lenie liczby klastr�w prz pomocy Elbow method

y <- sapply(clust.pam, function(cl) { sum(cl@cldist ^ 2) })

x <- (2:5)

plot(x = x, y = y, type = "l")

# DTW: Zdecycdowano o podziale danych na 3 grupy

# DTW: dokonano klastrowania z wykorzystanie DTW oraz analizy skupie� z wykorzysaniem warto�ci �redniej.

clust.pam <- tsclust(cities.norm, type = "partitional", k = 3L, distance = "dtw", centroid = "mean")

# Zwizualizowano efekty klastrowania miast.

plot(clust.pam, type = "sc")

# Stworzono ramk� danych zawierajac� wyniki klastrowaniam oraz po�aczono j� z d�ug� ramk� danych zawierajac� warto�ci wska�nika w kolejnych latach

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