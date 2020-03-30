# Wczytano wymagane pakiety.

library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(cluster)
library(cluster.datasets)
library(ggvis)
library(numDeriv)
library(pspline)

# Ustawiono katalog roboczy.

getwd()
setwd("C:/Users/Maciuœ/Desktop/R")

# Wczytano dane.

cities <- read.csv("city_clastering_tRamax.csv")

# Nadano kolumnom w³aœciwe nazwy.

names(cities) <- c("city", "2014":"2019")
 
# Zebrano dane w d³ugi¹ ramkê danych o nastêpuj¹cych zmiennych: 'city', 'year' oraz 'rate'.

cities_long <- data.table(gather(cities, key = "year", value = 'rate', "2014":"2019"))

# Nadano danym w kolumnie 'year' klasê data.

cities_long$year <- as.Date(ISOdate(cities_long$year, 1, 1))

# Nadano danym w kolumnie 'city' klasê faktor.

cities_long$city <- as.factor(cities_long$city)

str(cities_long)

# Przygotowano pust¹ ramkê danych w celu wprowadzenia do niej wartoœci pochodnych funkcji.

colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
col.names = c("2014":"2019")

df <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)

# Stworzono funkcjê obliczaj¹c¹ pochodn¹ funkcji zmiany wskaŸnika w kolejnych latach.

derivative <- function(x, y) {
  f1 <- predict(smooth.Pspline(x, y), x, nderiv=1)
  return(f1) }

# Dokonano obliczenia pochodnych dla wszystkich miast.

#1
f1<- derivative(x = cities_long[city == "London",]$year, y = cities_long[city == "London",]$rate)
df <- rbind(df,t(f1))
#2
f1<- derivative(x = cities_long[city == "Paris",]$year, y = cities_long[city == "Paris",]$rate)
df <- rbind(df,t(f1))
#3
f1<- derivative(x = cities_long[city == "Rome",]$year, y = cities_long[city == "Rome",]$rate)
df <- rbind(df,t(f1))
#4
f1<- derivative(x = cities_long[city == "Barcelona",]$year, y = cities_long[city == "Barcelona",]$rate)
df <- rbind(df,t(f1))
#5
f1<- derivative(x = cities_long[city == "Madrid",]$year, y = cities_long[city == "Madrid",]$rate)
df <- rbind(df,t(f1))
#6
f1<- derivative(x = cities_long[city == "Lisbon",]$year, y = cities_long[city == "Lisbon",]$rate)
df <- rbind(df,t(f1))
#7
f1<- derivative(x = cities_long[city == "Berlin",]$year, y = cities_long[city == "Berlin",]$rate)
df <- rbind(df,t(f1))
#8
f1<- derivative(x = cities_long[city == "Milan",]$year, y = cities_long[city == "Milan",]$rate)
df <- rbind(df,t(f1))
#9
f1<- derivative(x = cities_long[city == "Copenhagen",]$year, y = cities_long[city == "Copenhagen",]$rate)
df <- rbind(df,t(f1))
#10
f1<- derivative(x = cities_long[city == "Amsterdam",]$year, y = cities_long[city == "Amsterdam",]$rate)
df <- rbind(df,t(f1))
#11
f1<- derivative(x = cities_long[city == "Prague",]$year, y = cities_long[city == "Prague",]$rate)
df <- rbind(df,t(f1))
#12
f1<- derivative(x = cities_long[city == "Edinburgh",]$year, y = cities_long[city == "Edinburgh",]$rate)
df <- rbind(df,t(f1))
#13
f1<- derivative(x = cities_long[city == "Budapest",]$year, y = cities_long[city == "Budapest",]$rate)
df <- rbind(df,t(f1))
#14
f1<- derivative(x = cities_long[city == "Florence",]$year, y = cities_long[city == "Florence",]$rate)
df <- rbind(df,t(f1))
#15
f1<- derivative(x = cities_long[city == "Nice",]$year, y = cities_long[city == "Nice",]$rate)
df <- rbind(df,t(f1))
#16
f1<- derivative(x = cities_long[city == "Athens",]$year, y = cities_long[city == "Athens",]$rate)
df <- rbind(df,t(f1))
#17
f1<- derivative(x = cities_long[city == "Vienna",]$year, y = cities_long[city == "Vienna",]$rate)
df <- rbind(df,t(f1))
#18
f1<- derivative(x = cities_long[city == "Split",]$year, y = cities_long[city == "Split",]$rate)
df <- rbind(df,t(f1))
#19
f1<- derivative(x = cities_long[city == "Venice",]$year, y = cities_long[city == "Venice",]$rate)
df <- rbind(df,t(f1))
#20
f1<- derivative(x = cities_long[city == "Marseille",]$year, y = cities_long[city == "Marseille",]$rate)
df <- rbind(df,t(f1))
#21
f1<- derivative(x = cities_long[city == "Napoli",]$year, y = cities_long[city == "Napoli",]$rate)
df <- rbind(df,t(f1))
#22
f1<- derivative(x = cities_long[city == "Kiev",]$year, y = cities_long[city == "Kiev",]$rate)
df <- rbind(df,t(f1))
#23
f1<- derivative(x = cities_long[city == "Munich",]$year, y = cities_long[city == "Munich",]$rate)
df <- rbind(df,t(f1))
#24
f1<- derivative(x = cities_long[city == "Porto",]$year, y = cities_long[city == "Porto",]$rate)
df <- rbind(df,t(f1))
#25
f1<- derivative(x = cities_long[city == "Warsaw",]$year, y = cities_long[city == "Warsaw",]$rate)
df <- rbind(df,t(f1))
#26
f1<- derivative(x = cities_long[city == "Valencia",]$year, y = cities_long[city == "Valencia",]$rate)
df <- rbind(df,t(f1))
#27
f1<- derivative(x = cities_long[city == "Brussels",]$year, y = cities_long[city == "Brussels",]$rate)
df <- rbind(df,t(f1))
#28
f1<- derivative(x = cities_long[city == "Lyon",]$year, y = cities_long[city == "Lyon",]$rate)
df <- rbind(df,t(f1))
#29
f1<- derivative(x = cities_long[city == "KrakÃ³w",]$year, y = cities_long[city == "KrakÃ³w",]$rate)
df <- rbind(df,t(f1))
#30
f1<- derivative(x = cities_long[city == "Dublin",]$year, y = cities_long[city == "Dublin",]$rate)
df <- rbind(df,t(f1))
#31
f1<- derivative(x = cities_long[city == "MÃ¡laga",]$year, y = cities_long[city == "MÃ¡laga",]$rate)
df <- rbind(df,t(f1))
#32
f1<- derivative(x = cities_long[city == "Seville",]$year, y = cities_long[city == "Seville",]$rate)
df <- rbind(df,t(f1))
#33
f1<- derivative(x = cities_long[city == "Marbella",]$year, y = cities_long[city == "Marbella",]$rate)
df <- rbind(df,t(f1))
#34
f1<- derivative(x = cities_long[city == "Palermo",]$year, y = cities_long[city == "Palermo",]$rate)
df <- rbind(df,t(f1))
#35
f1<- derivative(x = cities_long[city == "Cologne",]$year, y = cities_long[city == "Cologne",]$rate)
df <- rbind(df,t(f1))
#36
f1<- derivative(x = cities_long[city == "Odessa",]$year, y = cities_long[city == "Odessa",]$rate)
df <- rbind(df,t(f1))
#37
f1<- derivative(x = cities_long[city == "Bordeaux",]$year, y = cities_long[city == "Bordeaux",]$rate)
df <- rbind(df,t(f1))
#38
f1<- derivative(x = cities_long[city == "Montpellier",]$year, y = cities_long[city == "Montpellier",]$rate)
df <- rbind(df,t(f1))
#39
f1<- derivative(x = cities_long[city == "Oslo",]$year, y = cities_long[city == "Oslo",]$rate)
df <- rbind(df,t(f1))
#40
f1<- derivative(x = cities_long[city == "Belgrade",]$year, y = cities_long[city == "Belgrade",]$rate)
df <- rbind(df,t(f1))
#41
f1<- derivative(x = cities_long[city == "Toulouse",]$year, y = cities_long[city == "Toulouse",]$rate)
df <- rbind(df,t(f1))
#42
f1<- derivative(x = cities_long[city == "GdaÅ„sk",]$year, y = cities_long[city == "GdaÅ„sk",]$rate)
df <- rbind(df,t(f1))
#43
f1<- derivative(x = cities_long[city == "Bucharest",]$year, y = cities_long[city == "Bucharest",]$rate)
df <- rbind(df,t(f1))
#44
f1<- derivative(x = cities_long[city == "Manchester",]$year, y = cities_long[city == "Manchester",]$rate)
df <- rbind(df,t(f1))
#45
f1<- derivative(x = cities_long[city == "Bologna",]$year, y = cities_long[city == "Bologna",]$rate)
df <- rbind(df,t(f1))
#46
f1<- derivative(x = cities_long[city == "Turin",]$year, y = cities_long[city == "Turin",]$rate)
df <- rbind(df,t(f1))
#47
f1<- derivative(x = cities_long[city == "Siracusa",]$year, y = cities_long[city == "Siracusa",]$rate)
df <- rbind(df,t(f1))
#48
f1<- derivative(x = cities_long[city == "Hamburg",]$year, y = cities_long[city == "Hamburg",]$rate)
df <- rbind(df,t(f1))
#49
f1<- derivative(x = cities_long[city == "Alicante",]$year, y = cities_long[city == "Alicante",]$rate)
df <- rbind(df,t(f1))
#50
f1<- derivative(x = cities_long[city == "Hannover",]$year, y = cities_long[city == "Hannover",]$rate)
df <- rbind(df,t(f1))
#51
f1<- derivative(x = cities_long[city == "Brighton and Hove",]$year, y = cities_long[city == "Brighton and Hove",]$rate)
df <- rbind(df,t(f1))
#52
f1<- derivative(x = cities_long[city == "Zurich",]$year, y = cities_long[city == "Zurich",]$rate)
df <- rbind(df,t(f1))
#53
f1<- derivative(x = cities_long[city == "Avignon",]$year, y = cities_long[city == "Avignon",]$rate)
df <- rbind(df,t(f1))
#54
f1<- derivative(x = cities_long[city == "Zagreb",]$year, y = cities_long[city == "Zagreb",]$rate)
df <- rbind(df,t(f1))
#55
f1<- derivative(x = cities_long[city == "Catania",]$year, y = cities_long[city == "Catania",]$rate)
df <- rbind(df,t(f1))
#56
f1<- derivative(x = cities_long[city == "Aix-en-Provence",]$year, y = cities_long[city == "Aix-en-Provence",]$rate)
df <- rbind(df,t(f1))
#57
f1<- derivative(x = cities_long[city == "LiÃ¨ge",]$year, y = cities_long[city == "LiÃ¨ge",]$rate)
df <- rbind(df,t(f1))
#58
f1<- derivative(x = cities_long[city == "Stockholm",]$year, y = cities_long[city == "Stockholm",]$rate)
df <- rbind(df,t(f1))
#59
f1<- derivative(x = cities_long[city == "Reykjavik",]$year, y = cities_long[city == "Reykjavik",]$rate)
df <- rbind(df,t(f1))
#60
f1<- derivative(x = cities_long[city == "Glasgow",]$year, y = cities_long[city == "Glasgow",]$rate)
df <- rbind(df,t(f1))
#61
f1<- derivative(x = cities_long[city == "Verona",]$year, y = cities_long[city == "Verona",]$rate)
df <- rbind(df,t(f1))
#62
f1<- derivative(x = cities_long[city == "Helsinki",]$year, y = cities_long[city == "Helsinki",]$rate)
df <- rbind(df,t(f1))
#63
f1<- derivative(x = cities_long[city == "Granada",]$year, y = cities_long[city == "Granada",]$rate)
df <- rbind(df,t(f1))
#64
f1<- derivative(x = cities_long[city == "Strasbourg",]$year, y = cities_long[city == "Strasbourg",]$rate)
df <- rbind(df,t(f1))
#65
f1<- derivative(x = cities_long[city == "Lviv",]$year, y = cities_long[city == "Lviv",]$rate)
df <- rbind(df,t(f1))
#66
f1<- derivative(x = cities_long[city == "Sofia",]$year, y = cities_long[city == "Sofia",]$rate)
df <- rbind(df,t(f1))
#67
f1<- derivative(x = cities_long[city == "DÃ¼sseldorf",]$year, y = cities_long[city == "DÃ¼sseldorf",]$rate)
df <- rbind(df,t(f1))
#68
f1<- derivative(x = cities_long[city == "Cork",]$year, y = cities_long[city == "Cork",]$rate)
df <- rbind(df,t(f1))
#69
f1<- derivative(x = cities_long[city == "Braga",]$year, y = cities_long[city == "Braga",]$rate)
df <- rbind(df,t(f1))
#70
f1<- derivative(x = cities_long[city == "Constanta",]$year, y = cities_long[city == "Constanta",]$rate)
df <- rbind(df,t(f1))
#71
f1<- derivative(x = cities_long[city == "Las Palmas de Gran Canaria",]$year, y = cities_long[city == "Las Palmas de Gran Canaria",]$rate)
df <- rbind(df,t(f1))
#72
f1<- derivative(x = cities_long[city == "Tallinn",]$year, y = cities_long[city == "Tallinn",]$rate)
df <- rbind(df,t(f1))
#73
f1<- derivative(x = cities_long[city == "Nantes",]$year, y = cities_long[city == "Nantes",]$rate)
df <- rbind(df,t(f1))
#74
f1<- derivative(x = cities_long[city == "RÄ«ga",]$year, y = cities_long[city == "RÄ«ga",]$rate)
df <- rbind(df,t(f1))
#75
f1<- derivative(x = cities_long[city == "Lille",]$year, y = cities_long[city == "Lille",]$rate)
df <- rbind(df,t(f1))
#76
f1<- derivative(x = cities_long[city == "Geneva",]$year, y = cities_long[city == "Geneva",]$rate)
df <- rbind(df,t(f1))
#77
f1<- derivative(x = cities_long[city == "Minsk",]$year, y = cities_long[city == "Minsk",]$rate)
df <- rbind(df,t(f1))
#78
f1<- derivative(x = cities_long[city == "Sarajevo",]$year, y = cities_long[city == "Sarajevo",]$rate)
df <- rbind(df,t(f1))
#79
f1<- derivative(x = cities_long[city == "Vilnius",]$year, y = cities_long[city == "Vilnius",]$rate)
df <- rbind(df,t(f1))
#80
f1<- derivative(x = cities_long[city == "Liverpool",]$year, y = cities_long[city == "Liverpool",]$rate)
df <- rbind(df,t(f1))
#81
f1<- derivative(x = cities_long[city == "Cardiff",]$year, y = cities_long[city == "Cardiff",]$rate)
df <- rbind(df,t(f1))
#82
f1<- derivative(x = cities_long[city == "Leipzig",]$year, y = cities_long[city == "Leipzig",]$rate)
df <- rbind(df,t(f1))
#83
f1<- derivative(x = cities_long[city == "Palma",]$year, y = cities_long[city == "Palma",]$rate)
df <- rbind(df,t(f1))
#84
f1<- derivative(x = cities_long[city == "Utrecht",]$year, y = cities_long[city == "Utrecht",]$rate)
df <- rbind(df,t(f1))
#85
f1<- derivative(x = cities_long[city == "Gothenburg",]$year, y = cities_long[city == "Gothenburg",]$rate)
df <- rbind(df,t(f1))
#86
f1<- derivative(x = cities_long[city == "Birmingham",]$year, y = cities_long[city == "Birmingham",]$rate)
df <- rbind(df,t(f1))
#87
f1<- derivative(x = cities_long[city == "WrocÅ‚aw",]$year, y = cities_long[city == "WrocÅ‚aw",]$rate)
df <- rbind(df,t(f1))
#88
f1<- derivative(x = cities_long[city == "Thessaloniki",]$year, y = cities_long[city == "Thessaloniki",]$rate)
df <- rbind(df,t(f1))
#89
f1<- derivative(x = cities_long[city == "Bristol",]$year, y = cities_long[city == "Bristol",]$rate)
df <- rbind(df,t(f1))
#90
f1<- derivative(x = cities_long[city == "Genoa",]$year, y = cities_long[city == "Genoa",]$rate)
df <- rbind(df,t(f1))
#91
f1<- derivative(x = cities_long[city == "Ljubljana",]$year, y = cities_long[city == "Ljubljana",]$rate)
df <- rbind(df,t(f1))
#92
f1<- derivative(x = cities_long[city == "Annecy",]$year, y = cities_long[city == "Annecy",]$rate)
df <- rbind(df,t(f1))
#93
f1<- derivative(x = cities_long[city == "Frankfurt",]$year, y = cities_long[city == "Frankfurt",]$rate)
df <- rbind(df,t(f1))
#94
f1<- derivative(x = cities_long[city == "Cagliari",]$year, y = cities_long[city == "Cagliari",]$rate)
df <- rbind(df,t(f1))
#95
f1<- derivative(x = cities_long[city == "Varna",]$year, y = cities_long[city == "Varna",]$rate)
df <- rbind(df,t(f1))
#96
f1<- derivative(x = cities_long[city == "Antwerp",]$year, y = cities_long[city == "Antwerp",]$rate)
df <- rbind(df,t(f1))
#97
f1<- derivative(x = cities_long[city == "Bruges",]$year, y = cities_long[city == "Bruges",]$rate)
df <- rbind(df,t(f1))
#98
f1<- derivative(x = cities_long[city == "Cluj-Napoca",]$year, y = cities_long[city == "Cluj-Napoca",]$rate)
df <- rbind(df,t(f1))
#99
f1<- derivative(x = cities_long[city == "Aarhus",]$year, y = cities_long[city == "Aarhus",]$rate)
df <- rbind(df,t(f1))
#100
f1<- derivative(x = cities_long[city == "Coimbra",]$year, y = cities_long[city == "Coimbra",]$rate)
df <- rbind(df,t(f1))
#101
f1<- derivative(x = cities_long[city == "The Hague",]$year, y = cities_long[city == "The Hague",]$rate)
df <- rbind(df,t(f1))
#102
f1<- derivative(x = cities_long[city == "Donostia/San SebastiÃ¡n",]$year, y = cities_long[city == "Donostia/San SebastiÃ¡n",]$rate)
df <- rbind(df,t(f1))
#103
f1<- derivative(x = cities_long[city == "Namur",]$year, y = cities_long[city == "Namur",]$rate)
df <- rbind(df,t(f1))
#104
f1<- derivative(x = cities_long[city == "Rouen",]$year, y = cities_long[city == "Rouen",]$rate)
df <- rbind(df,t(f1))
#105
f1<- derivative(x = cities_long[city == "Bratislava",]$year, y = cities_long[city == "Bratislava",]$rate)
df <- rbind(df,t(f1))
#106
f1<- derivative(x = cities_long[city == "CÃ³rdoba",]$year, y = cities_long[city == "CÃ³rdoba",]$rate)
df <- rbind(df,t(f1))
#107
f1<- derivative(x = cities_long[city == "Oxford",]$year, y = cities_long[city == "Oxford",]$rate)
df <- rbind(df,t(f1))
#108
f1<- derivative(x = cities_long[city == "Belfast",]$year, y = cities_long[city == "Belfast",]$rate)
df <- rbind(df,t(f1))
#109
f1<- derivative(x = cities_long[city == "Rennes",]$year, y = cities_long[city == "Rennes",]$rate)
df <- rbind(df,t(f1))
#110
f1<- derivative(x = cities_long[city == "Brasov",]$year, y = cities_long[city == "Brasov",]$rate)
df <- rbind(df,t(f1))
#111
f1<- derivative(x = cities_long[city == "NÃ¼rnberg",]$year, y = cities_long[city == "NÃ¼rnberg",]$rate)
df <- rbind(df,t(f1))
#112
f1<- derivative(x = cities_long[city == "York",]$year, y = cities_long[city == "York",]$rate)
df <- rbind(df,t(f1))
#113
f1<- derivative(x = cities_long[city == "Rotterdam",]$year, y = cities_long[city == "Rotterdam",]$rate)
df <- rbind(df,t(f1))
#114
f1<- derivative(x = cities_long[city == "Toulon",]$year, y = cities_long[city == "Toulon",]$rate)
df <- rbind(df,t(f1))
#115
f1<- derivative(x = cities_long[city == "Dresden",]$year, y = cities_long[city == "Dresden",]$rate)
df <- rbind(df,t(f1))
#116
f1<- derivative(x = cities_long[city == "Skopje",]$year, y = cities_long[city == "Skopje",]$rate)
df <- rbind(df,t(f1))
#117
f1<- derivative(x = cities_long[city == "Novi Sad",]$year, y = cities_long[city == "Novi Sad",]$rate)
df <- rbind(df,t(f1))
#118
f1<- derivative(x = cities_long[city == "Bari",]$year, y = cities_long[city == "Bari",]$rate)
df <- rbind(df,t(f1))
#119
f1<- derivative(x = cities_long[city == "NÃ®mes",]$year, y = cities_long[city == "NÃ®mes",]$rate)
df <- rbind(df,t(f1))
#120
f1<- derivative(x = cities_long[city == "Cartagena",]$year, y = cities_long[city == "Cartagena",]$rate)
df <- rbind(df,t(f1))
#121
f1<- derivative(x = cities_long[city == "Bergen",]$year, y = cities_long[city == "Bergen",]$rate)
df <- rbind(df,t(f1))
#122
f1<- derivative(x = cities_long[city == "Cambridge",]$year, y = cities_long[city == "Cambridge",]$rate)
df <- rbind(df,t(f1))
#123
f1<- derivative(x = cities_long[city == "CÃ¡diz",]$year, y = cities_long[city == "CÃ¡diz",]$rate)
df <- rbind(df,t(f1))
#124
f1<- derivative(x = cities_long[city == "Rijeka",]$year, y = cities_long[city == "Rijeka",]$rate)
df <- rbind(df,t(f1))
#125
f1<- derivative(x = cities_long[city == "LÃ¼beck",]$year, y = cities_long[city == "LÃ¼beck",]$rate)
df <- rbind(df,t(f1))
#126
f1<- derivative(x = cities_long[city == "Zwolle",]$year, y = cities_long[city == "Zwolle",]$rate)
df <- rbind(df,t(f1))
#127
f1<- derivative(x = cities_long[city == "Aalborg",]$year, y = cities_long[city == "Aalborg",]$rate)
df <- rbind(df,t(f1))
#128
f1<- derivative(x = cities_long[city == "Poznan",]$year, y = cities_long[city == "Poznan",]$rate)
df <- rbind(df,t(f1))
#129
f1<- derivative(x = cities_long[city == "Perugia",]$year, y = cities_long[city == "Perugia",]$rate)
df <- rbind(df,t(f1))
#130
f1<- derivative(x = cities_long[city == "Rimini",]$year, y = cities_long[city == "Rimini",]$rate)
df <- rbind(df,t(f1))
#131
f1<- derivative(x = cities_long[city == "Basel",]$year, y = cities_long[city == "Basel",]$rate)
df <- rbind(df,t(f1))
#132
f1<- derivative(x = cities_long[city == "Leeds",]$year, y = cities_long[city == "Leeds",]$rate)
df <- rbind(df,t(f1))
#133
f1<- derivative(x = cities_long[city == "Vila Nova de Gaia",]$year, y = cities_long[city == "Vila Nova de Gaia",]$rate)
df <- rbind(df,t(f1))
#134
f1<- derivative(x = cities_long[city == "Gdynia",]$year, y = cities_long[city == "Gdynia",]$rate)
df <- rbind(df,t(f1))
#135
f1<- derivative(x = cities_long[city == "Stuttgart",]$year, y = cities_long[city == "Stuttgart",]$rate)
df <- rbind(df,t(f1))
#136
f1<- derivative(x = cities_long[city == "Le Mans",]$year, y = cities_long[city == "Le Mans",]$rate)
df <- rbind(df,t(f1))
#137
f1<- derivative(x = cities_long[city == "Bayonne",]$year, y = cities_long[city == "Bayonne",]$rate)
df <- rbind(df,t(f1))
#138
f1<- derivative(x = cities_long[city == "Swansea",]$year, y = cities_long[city == "Swansea",]$rate)
df <- rbind(df,t(f1))
#139
f1<- derivative(x = cities_long[city == "Tirana",]$year, y = cities_long[city == "Tirana",]$rate)
df <- rbind(df,t(f1))
#140
f1<- derivative(x = cities_long[city == "Salerno",]$year, y = cities_long[city == "Salerno",]$rate)
df <- rbind(df,t(f1))
#141
f1<- derivative(x = cities_long[city == "Dijon",]$year, y = cities_long[city == "Dijon",]$rate)
df <- rbind(df,t(f1))
#142
f1<- derivative(x = cities_long[city == "Grenoble",]$year, y = cities_long[city == "Grenoble",]$rate)
df <- rbind(df,t(f1))
#143
f1<- derivative(x = cities_long[city == "Trieste",]$year, y = cities_long[city == "Trieste",]$rate)
df <- rbind(df,t(f1))
#144
f1<- derivative(x = cities_long[city == "Perpignan",]$year, y = cities_long[city == "Perpignan",]$rate)
df <- rbind(df,t(f1))
#145
f1<- derivative(x = cities_long[city == "Groningen",]$year, y = cities_long[city == "Groningen",]$rate)
df <- rbind(df,t(f1))
#146
f1<- derivative(x = cities_long[city == "Bilbao",]$year, y = cities_long[city == "Bilbao",]$rate)
df <- rbind(df,t(f1))
#147
f1<- derivative(x = cities_long[city == "Tours",]$year, y = cities_long[city == "Tours",]$rate)
df <- rbind(df,t(f1))
#148
f1<- derivative(x = cities_long[city == "Luzern",]$year, y = cities_long[city == "Luzern",]$rate)
df <- rbind(df,t(f1))
#149
f1<- derivative(x = cities_long[city == "Santander",]$year, y = cities_long[city == "Santander",]$rate)
df <- rbind(df,t(f1))
#150
f1<- derivative(x = cities_long[city == "Sibiu",]$year, y = cities_long[city == "Sibiu",]$rate)
df <- rbind(df,t(f1))
#151
f1<- derivative(x = cities_long[city == "Ghent",]$year, y = cities_long[city == "Ghent",]$rate)
df <- rbind(df,t(f1))
#152
f1<- derivative(x = cities_long[city == "Mostar",]$year, y = cities_long[city == "Mostar",]$rate)
df <- rbind(df,t(f1))
#153
f1<- derivative(x = cities_long[city == "Vannes",]$year, y = cities_long[city == "Vannes",]$rate)
df <- rbind(df,t(f1))
#154
f1<- derivative(x = cities_long[city == "Plovdiv",]$year, y = cities_long[city == "Plovdiv",]$rate)
df <- rbind(df,t(f1))
#155
f1<- derivative(x = cities_long[city == "Freiburg im Breisgau",]$year, y = cities_long[city == "Freiburg im Breisgau",]$rate)
df <- rbind(df,t(f1))
#156
f1<- derivative(x = cities_long[city == "Haarlem",]$year, y = cities_long[city == "Haarlem",]$rate)
df <- rbind(df,t(f1))
#157
f1<- derivative(x = cities_long[city == "Santa Cruz de Tenerife",]$year, y = cities_long[city == "Santa Cruz de Tenerife",]$rate)
df <- rbind(df,t(f1))
#158
f1<- derivative(x = cities_long[city == "MalmÃ¶",]$year, y = cities_long[city == "MalmÃ¶",]$rate)
df <- rbind(df,t(f1))
#159
f1<- derivative(x = cities_long[city == "L'Hospitalet",]$year, y = cities_long[city == "L'Hospitalet",]$rate)
df <- rbind(df,t(f1))
#160
f1<- derivative(x = cities_long[city == "Reims",]$year, y = cities_long[city == "Reims",]$rate)
df <- rbind(df,t(f1))
#161
f1<- derivative(x = cities_long[city == "Salamanca",]$year, y = cities_long[city == "Salamanca",]$rate)
df <- rbind(df,t(f1))
#162
f1<- derivative(x = cities_long[city == "Aachen",]$year, y = cities_long[city == "Aachen",]$rate)
df <- rbind(df,t(f1))
#163
f1<- derivative(x = cities_long[city == "Salzburg",]$year, y = cities_long[city == "Salzburg",]$rate)
df <- rbind(df,t(f1))
#164
f1<- derivative(x = cities_long[city == "ChiÈ™inÄƒu",]$year, y = cities_long[city == "ChiÈ™inÄƒu",]$rate)
df <- rbind(df,t(f1))
#165
f1<- derivative(x = cities_long[city == "Sheffield",]$year, y = cities_long[city == "Sheffield",]$rate)
df <- rbind(df,t(f1))
#166
f1<- derivative(x = cities_long[city == "Rostock",]$year, y = cities_long[city == "Rostock",]$rate)
df <- rbind(df,t(f1))
#167
f1<- derivative(x = cities_long[city == "Lausanne",]$year, y = cities_long[city == "Lausanne",]$rate)
df <- rbind(df,t(f1))
#168
f1<- derivative(x = cities_long[city == "Caen",]$year, y = cities_long[city == "Caen",]$rate)
df <- rbind(df,t(f1))
#169
f1<- derivative(x = cities_long[city == "Padova",]$year, y = cities_long[city == "Padova",]$rate)
df <- rbind(df,t(f1))
#170
f1<- derivative(x = cities_long[city == "Katowice",]$year, y = cities_long[city == "Katowice",]$rate)
df <- rbind(df,t(f1))
#171
f1<- derivative(x = cities_long[city == "Ravenna",]$year, y = cities_long[city == "Ravenna",]$rate)
df <- rbind(df,t(f1))
#172
f1<- derivative(x = cities_long[city == "Graz",]$year, y = cities_long[city == "Graz",]$rate)
df <- rbind(df,t(f1))
#173
f1<- derivative(x = cities_long[city == "Eindhoven",]$year, y = cities_long[city == "Eindhoven",]$rate)
df <- rbind(df,t(f1))
#174
f1<- derivative(x = cities_long[city == "Kharkiv",]$year, y = cities_long[city == "Kharkiv",]$rate)
df <- rbind(df,t(f1))
#175
f1<- derivative(x = cities_long[city == "Trondheim",]$year, y = cities_long[city == "Trondheim",]$rate)
df <- rbind(df,t(f1))
#176
f1<- derivative(x = cities_long[city == "Bergamo",]$year, y = cities_long[city == "Bergamo",]$rate)
df <- rbind(df,t(f1))
#177
f1<- derivative(x = cities_long[city == "Newcastle upon Tyne",]$year, y = cities_long[city == "Newcastle upon Tyne",]$rate)
df <- rbind(df,t(f1))
#178
f1<- derivative(x = cities_long[city == "GijÃ³n/XixÃ³n",]$year, y = cities_long[city == "GijÃ³n/XixÃ³n",]$rate)
df <- rbind(df,t(f1))
#179
f1<- derivative(x = cities_long[city == "Bremen",]$year, y = cities_long[city == "Bremen",]$rate)
df <- rbind(df,t(f1))
#180
f1<- derivative(x = cities_long[city == "Murcia",]$year, y = cities_long[city == "Murcia",]$rate)
df <- rbind(df,t(f1))
#181
f1<- derivative(x = cities_long[city == "Pamplona/IruÃ±a",]$year, y = cities_long[city == "Pamplona/IruÃ±a",]$rate)
df <- rbind(df,t(f1))
#182
f1<- derivative(x = cities_long[city == "Cheltenham",]$year, y = cities_long[city == "Cheltenham",]$rate)
df <- rbind(df,t(f1))
#183
f1<- derivative(x = cities_long[city == "Stavanger",]$year, y = cities_long[city == "Stavanger",]$rate)
df <- rbind(df,t(f1))
#184
f1<- derivative(x = cities_long[city == "KoÅ¡ice",]$year, y = cities_long[city == "KoÅ¡ice",]$rate)
df <- rbind(df,t(f1))
#185
f1<- derivative(x = cities_long[city == "Bonn",]$year, y = cities_long[city == "Bonn",]$rate)
df <- rbind(df,t(f1))
#186
f1<- derivative(x = cities_long[city == "Saint-Ã‰tienne	",]$year, y = cities_long[city == "Saint-Ã‰tienne	",]$rate)
df <- rbind(df,t(f1))
#187
f1<- derivative(x = cities_long[city == "Kassel",]$year, y = cities_long[city == "Kassel",]$rate)
df <- rbind(df,t(f1))

# Do ramki pochodnych danych dodano kolumnê 'city'.

derivatives <- cbind(cities$city, df)

# Nadano kolumnom w³aœciwe nazwy.

names(derivatives) <- c("city", "2014":"2019")

# Obliczono macierz odleg³oœci miêdzy zeskalowanymi obserwacjami.

dist_derivatives <- dist(scale(derivatives[,-1]))

# Bez skalowania: dist_derivatives <- dist(derivatives[,-1])

# Stworzono dendrogram, z wykorzystaniem analizy wariancji .

hc_ward <- hclust(dist_derivatives, method = "ward.D2")

plot(hc_ward, main = 'Ward Linkage')

# Zdecydowano siê na dokonanie odciêcia na poziomie 12

cluster_hc <- cutree(hc_ward, h = 12)

# Dodano klaster do pierwotnej ramki danych.

cities$cluster_ward <- cluster_hc

cities_long <- data.table(gather(cities, key = "year", value = 'rate', "2014":"2019"))

cities_long$year <- as.Date(ISOdate(cities_long$year, 1, 1))

cities_long$cluster_ward <- as.factor(cities_long$cluster_ward)

cities_long$city <- as.factor(cities_long$city)

str(cities_long)

# Dokonano wizulizaji wyników

ggplot(cities_long, aes(x = year, y = rate, linetype = cluster_ward)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)


(cities_selected_1 <- cities_long %>%
    filter(cluster_ward == 1) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

(cities_selected_2 <- cities_long %>%
    filter(cluster_ward == 2) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

(cities_selected_3 <- cities_long %>%
    filter(cluster_ward == 3) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

(cities_selected_4 <- cities_long %>%
    filter(cluster_ward == 4) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

(cities_selected_5 <- cities_long %>%
    filter(cluster_ward == 5) %>%
    filter(year == "2019-01-01")) %>%
  arrange(desc(rate))

cities_long %>%
  filter(cluster_ward == 1) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)

cities_long %>%
  filter(cluster_ward == 2) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)

cities_long %>%
  filter(cluster_ward == 3) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)

cities_long %>%
  filter(cluster_ward == 4) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)

cities_long %>%
  filter(cluster_ward == 5) %>%
  ggplot(aes(x = year, y = rate)) +
  geom_line(aes(color = city)) +
  guides(color = FALSE)





