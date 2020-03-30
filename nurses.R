library(ggplot2)
library(gridExtra)
library(dplyr)

# Celem analizy jest okreœlenie liczby zatrudnonych pielêgniarek i po³o¿nych w 2027 roku.
# Wykorzystane dane o strukturze wiekowej pielêgniarek i po³oznych z 2017 roku pochodz¹ z raportu nipip: http://arch.nipip.pl/attachments/article/3368/Raport_2015_NIPiP.pdf

# Zaobserwowano, ¿e w roku 2017 a¿ 35627 spoœród ogó³u zatrudnionych pielêgniarek i po³o¿nych znajduje siê w wieku emerytalnym. Na potrzeby analiy zalo¿ono, ¿e podobna tendecja utrzyma siê w przysz³oœci.

# W celu okreœlenia tempa przejœæ na emerytujê wykonano eksploracyjna analizê danych obejmuj¹c¹ liczebnoœæ nastepuj¹cych grup wiekowych: "56-60", "61-65", "66-70" oraz "powy¿ej 70".
# Na potrzeby analizy za³o¿ono brak pielêgniarek i po³o¿nych w ostatniej z wymienonych grup oraz zbli¿on¹ liczebnosæ ka¿dej kohorty przed osi¹gnieciem wieku emerytalnego.

age <- c(0, 1, 2, 3)
count <- c(42498, 25463, 10164, 0)
sum(count)

(df <- data.frame(age, count))

df %>%
  ggplot(aes(x = age, y = count)) +
  geom_col()+
  geom_line(aes(x = age, y = count))

# Na podstawie wykresu okreœlono, ¿e tempo odp³ywu zatrudnionych wynosi 33,3% wartoœci bazowej w ci¹gu 5 lat (czyli przy przejsciu do wy¿szej grupy wiekowej).
# Powy¿ej wieku emerytalnego ka¿da kohorta o zmieniejsza swoj¹ liczebnoœæ o 1/3 wartoœci sprzed osi¹gniecia wieku emerytlnego po up³ywie 5 latach, a¿ do osi¹gniêcia wartoœci 0 w grupie "powy¿ej 70".

# Stan na 12.2017

# Wczytano kompletne dane oraz utworzono z nich ramkê danych.

age <- as.ordered(c("26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70"))

count <- c(8546, 9352, 10723, 39304, 46598, 44306, 42498, 25463, 10164)

df <- data.frame(age, count)

# Okreœlono liczbê pielêgniarek i po³onych w 2017 roku.

sum(df$count)

# Przedstawiono strukturê wiekow¹ w formie histogramu.

(plot2017 <- ggplot(df, aes(x = age, y = count)) +
    geom_col(fill= 'skyblue') +
    xlab("age") +
    ggtitle(paste("Number of nurses and midwives by age in 2017-12-31. Total:", sum(df$count), sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_light() +
    scale_y_continuous(name = "number (in thousands)", breaks = c(0, 10000, 20000, 30000, 40000), labels = c(0, 10, 20, 30, 40)))

# Stan na 12.2022

# Przesuniêto pielêgniareki i po³o¿ne do wy¿szej grupy wiekowej. Za³o¿ono identyczny przyrost liczby nowych pielêgniarek (liczebnoœæ grupy "26-30) jak w roku 2017.

df$x2022 = lag(df$count)
df[1, 'x2022'] <- df[2, 'x2022']
df[8, 'x2022'] <- round(2/3 * df[8, 'x2022'])
df[9, 'x2022'] <- round(1/2 * df[9, 'x2022'])

# Okreœlono liczbê pielêgniarek i po³onych w 2022 roku.

sum(df$x2022)

# Przedstawiono strukturê wiekow¹ w formie histogramu.

(plot2022 <- ggplot(df, aes(x = age, y = x2022)) +
    geom_col(fill= 'skyblue') +
    xlab("age") +
    ggtitle(paste("Number of nurses and midwives by age in 2022-12-31. Total:", sum(df$x2022), sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_light() +
    scale_y_continuous(name = "number (in thousands)", breaks = c(0, 10000, 20000, 30000, 40000), labels = c(0, 10, 20, 30, 40)))

# Stan na 12.2027

# Przesuniêto pielêgniareki i po³o¿ne do wy¿szej grupy wiekowej. Za³o¿ono identyczny przyrost liczby nowych pielêgniarek (liczebnoœæ grupy "26-30) jak w roku 2017.

df$x2027 = lag(df$x2022)
df[1, 'x2027'] <- df[2, 'x2027']
df[8, 'x2027'] <- round(2/3 * df[8, 'x2027'])
df[9, 'x2027'] <- round(1/2 * df[9, 'x2027'])

# Okreœlono liczbê pielêgniarek i po³onych w 2027 roku.

sum(df$x2027)

# Przedstawiono strukturê wiekow¹ w formie histogramu.

(plot2027 <- ggplot(df, aes(x = age, y = x2027)) +
    geom_col(fill= 'skyblue') +
    xlab("age") +
    ggtitle(paste("Number of nurses and midwives by age in 2027-12-31. Total:", sum(df$x2027), sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_light() +
    scale_y_continuous(name = "number (in thousands)", breaks = c(0, 10000, 20000, 30000, 40000), labels = c(0, 10, 20, 30, 40)))

# Porównano powsta³e wykresy.

grid.arrange(plot2017, plot2022, plot2027, ncol=1)

# Okreœlono o jaki procent liczba pielêgniarek i po³onych zmiejszy siê w ci¹gu pierwych 5 lat.
(sum(df$count) - sum(df$x2022))/sum(df$count)

# Okreœlono o jaki procent liczba pielêgniarek i po³onych zmiejszy siê w ci¹gu kolejnych 5 lat.
(sum(df$x2022) - sum(df$x2027))/sum(df$x2022)

# Okreœlono o ile zmniejszy siê liczba pielêgniarek i po³onych w ciagu ca³ego okresu objêtego prognoz¹.
sum(df$count) - sum(df$x2027)

# Wartoœæ ta stanowi 26% ogó³u zatrudnonych pielêgniarek i po³o¿nych.
(sum(df$count) - sum(df$x2027))/sum(df$count)

# Powy¿szy wariant nale¿y uznaæ za pozytywny, gdy¿ zak³ada on pracê zawodow¹ osób po osi¹gnieciu wieku emerytalnego oraz wkraczanie nowych kohort o równej liczebnoœci tej z 2017 r pomimo zauwa¿anej tendencji spadkowej.
