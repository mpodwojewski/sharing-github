library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)
library(tidyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(forecast)
library(xts)
library(zoo)

###

# CZĘSĆ PIERWSZA - PROGNOZA LICZBY NOWYCH LICENCJI WYKONYWANIA ZAWODU PIELĘGNIARKI I POŁOŻNEJ

licences <- read.csv2('licences_given.csv')

licences$date <- as.Date(licences$date)

licences <- licences[,1:2]

str(licences)

licences <- licences[complete.cases(licences),]


# Stworzenie szeregu czasowego

time <- as.POSIXct(licences$date, format = '%Y-%m-%d')

licences_xts <- xts(licences$count, order.by = time)


# Interpolacja danych

dates <- data.frame(format(seq(as.POSIXct("2005-01-01"), by="month", length.out=14*12), "%Y-%m-%d"))

colnames(dates) <- 'date'

dates$date <- as.Date(dates$date)

interpolated <- merge(x = dates, y = licences, by = "date", all = TRUE)

interpolated$date <- as.integer(interpolated$date)

interpolated <- data.frame(na.spline(interpolated, na.rm = FALSE))

interpolated$date <- as.Date(interpolated$date)

time <- as.POSIXct(interpolated$date, format = '%Y-%m-%d')

interpolated_xts <- xts(interpolated$count, order.by = time)


# Stworzenie modelu arima

licences_fit = auto.arima(interpolated_xts)


# Prognoza wartości na następne 10 lat (w ujęciu miesięcznym)

licences_forecast = forecast(licences_fit, h = 144)


# Wizualiacja wyników

plot(licences_forecast)


# Zebranie wyników rocznych (01-01)

forecast <- licences_forecast$mean[c(1,13,25,37,49,61,73,85,97,109, 121, 133)]


###

# CZĘSĆ DRUGA - PROGNOZA LICZBY PIELĘGNIAREK I POŁOŻNYCH W SKALI KRAJU

# Określenie struktury wiekowej pielęgniarek i położnych w wieku produkcyjnym na podstawie danych 

nurses <- read.csv2("data2018.csv")
names(nurses)

names(nurses) <- c("profession", "88":"21")

nurses <- gather(nurses, 'age', 'number',  -profession)

nurses$age <- as.numeric(nurses$age)

summary(nurses)

nurses_1 <- nurses# %>%
#  filter(age <= 60)


# Zagregowano dane

nurses_1 <- aggregate(nurses_1$number, by = list(nurses_1$age), FUN = sum)

colnames(nurses_1) <- c('age', 'number')

head(nurses_1)

ggplot(nurses_1, aes(x = age, y = number)) +
  geom_bar(stat = "identity")


# Wczytano tablice trwania życia

life_table <- read.csv2("life_table.csv")

life_table$death_prop_100k <- as.numeric(life_table$death_prop_100k)

life_table$death_prop <- life_table$death_prop_100k/100000

life_table$death_prop_100k <- NULL


# Przypisano prawdopodobieństwo zgonu w danym roku dla danego wieku.

nurses_1 <- merge(x = life_table, y = nurses_1, by = 'age')

nurses_1

nurses_1[nurses_1$age == 60, 'number']

# Funkcja pozwalająca przewidzieć liczbę pielgniarek i pożnych w n-tym roku.

mortality <- function(df, new, n) {
  x = df
  a = 1
  while (a <= n) {
    x$number <- floor(x$number * (1-x$death_prop))
    x[x$age == 60, 'number'] <- floor(x[x$age == 60, 'number'] * (1-0.1661397))
    x[x$age == 61, 'number'] <- floor(x[x$age == 61, 'number'] * (1-0.1661397))
    x[x$age == 62, 'number'] <- floor(x[x$age == 62, 'number'] * (1-0.1661397))
    x[x$age == 63, 'number'] <- floor(x[x$age == 63, 'number'] * (1-0.1661397))
    x[x$age == 64, 'number'] <- floor(x[x$age == 64, 'number'] * (1-0.1661397))
    x[x$age == 65, 'number'] <- floor(x[x$age == 65, 'number'] * (1-0.1661397))
    x[x$age == 66, 'number'] <- floor(x[x$age == 66, 'number'] * (1-0.1661397))
    x[x$age == 67, 'number'] <- floor(x[x$age == 67, 'number'] * (1-0.1661397))
    x[x$age == 68, 'number'] <- floor(x[x$age == 68, 'number'] * (1-0.1661397))
    x[x$age == 69, 'number'] <- floor(x[x$age == 69, 'number'] * (1-0.1661397))
    x[x$age == 70, 'number'] <- floor(x[x$age == 70, 'number'] * (1-0.1661397))
    x[x$age == 71, 'number'] <- floor(x[x$age == 71, 'number'] * (1-0.1661397))
    x[x$age == 72, 'number'] <- floor(x[x$age == 72, 'number'] * (1-0.1661397))
    x[x$age == 73, 'number'] <- floor(x[x$age == 73, 'number'] * (1-0.1661397))
    x[x$age == 74, 'number'] <- floor(x[x$age == 74, 'number'] * (1-0.1661397))
    x[x$age == 75, 'number'] <- floor(x[x$age == 75, 'number'] * (1-0.1661397))
    x[x$age == 76, 'number'] <- floor(x[x$age == 76, 'number'] * (1-0.1661397))
    x[x$age == 77, 'number'] <- floor(x[x$age == 77, 'number'] * (1-0.1661397))
    x[x$age == 78, 'number'] <- floor(x[x$age == 78, 'number'] * (1-0.1661397))
    x[x$age == 79, 'number'] <- floor(x[x$age == 79, 'number'] * (1-0.1661397))
    x[x$age == 80, 'number'] <- floor(x[x$age == 80, 'number'] * (1-0.1661397))
    x[x$age == 81, 'number'] <- floor(x[x$age == 81, 'number'] * (1-0.1661397))
    x[x$age == 82, 'number'] <- floor(x[x$age == 82, 'number'] * (1-0.1661397))
    x[x$age == 83, 'number'] <- floor(x[x$age == 83, 'number'] * (1-0.1661397))
    x[x$age == 84, 'number'] <- floor(x[x$age == 84, 'number'] * (1-0.1661397))
    x[x$age == 85, 'number'] <- floor(x[x$age == 85, 'number'] * (1-0.1661397))
    x[x$age == 86, 'number'] <- floor(x[x$age == 86, 'number'] * (1-0.1661397))
    x[x$age == 87, 'number'] <- floor(x[x$age == 87, 'number'] * (1-0.1661397))
    x[x$age == 88, 'number'] <- floor(x[x$age == 88, 'number'] * (1-0.1661397))
    x$number <- lag(x$number, n = 1L)
    x[1,3] <- round(new[a] * rate[1])
    x[2,3] <- x[2,3] + round(new[a] * rate[2])
    x[3,3] <- x[3,3] + round(new[a] * rate[3])
    x[4,3] <- x[4,3] + round(new[a] * rate[4])
    x[5,3] <- x[5,3] + round(new[a] * rate[5])
    x[6,3] <- x[6,3] + round(new[a] * rate[6])
    x[7,3] <- x[7,3] + round(new[a] * rate[7])
    x[8,3] <- x[8,3] + round(new[a] * rate[8])
    x[9,3] <- x[9,3] + round(new[a] * rate[9])
    x[10,3] <- x[10,3] + round(new[a] * rate[10])
    print(x$number)
    a = a +1
  }
  return(x$number)
}

# Zdecydowano o podziale liczby nowych licencji na 10 grup w wieku 21-30 lat zgodnie z rozkładem normalnym.

rate <- c(0.004, 0.019, 0.068, 0.161, 0.248, 0.249, 0.161, 0.068, 0.019, 0.004)

# Przyjęto założenie, że połowa nowych osób, które uzyskały prawo wykonywania zawodu pielęgniareki i położnej podejmie w nim pracę.

forecast <- forecast * 0.5

# Dokonano prognozy liczby pielęgniarek i położnych w skali kraju

for (i in 1:length(forecast)) {
  a <- mortality(nurses_1, forecast, i)
  nurses_1 <- cbind(nurses_1, a)
  names(nurses_1)[ncol(nurses_1)] <- i
}

total_number <- c( 
  sum(nurses_1$'1'),
  sum(nurses_1$'2'),
  sum(nurses_1$'3'),
  sum(nurses_1$'4'),
  sum(nurses_1$'5'),
  sum(nurses_1$'6'),
  sum(nurses_1$'7'),
  sum(nurses_1$'8'),
  sum(nurses_1$'9'),
  sum(nurses_1$'10'),
  sum(nurses_1$'11'),
  sum(nurses_1$'12')
)

#Dane zwizualizowano

years <- 2019:2030
total_number
type <- 'historic'

z <- data.frame(years, total_number, type)

h <- data.frame(c(2015, 2016, 2017, 2018, 2019), 
                c(245625, 248111, 252426, 255805, 257833), c('forecast', 'forecast', 'forecast', 'forecast','forecast'))

names(h) <- c('years', 'total_number', 'type')

to_plot <- rbind(z, h)

ggplot(to_plot, aes(years, total_number, color = type)) +
  geom_line()