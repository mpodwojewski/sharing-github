library(readxl)
library(caret)
library(caTools)

# Celem analizy jest budowa modelu klasyfikacyjnego GLM zdolnego okreœliæ czy klient dokona zg³oszenia zdarzenia. 
# Wykorzystane dane AutoClaim: SAS Enterprise Miner database
  
getwd()
setwd("C:/Users/Maciuœ/Documents/ID")

# Sprawdzono jakie arkusze zawiera plik "dane.xls" 

excel_sheets("claims_data.xls")

# Wczytano kompletne dane z akrusza claims..

claims <- read_excel("claims_data.xls", sheet = "claims")

# Dokonano sprz¹tania danych.

#Eksploracja danych (zrozumienie danych, obejrzenie danych, wizualizacja danych)

# Sprawdzono klase obiektu "claims'. Jest to ramka danych.
class(claims)

# Sprawdzono wymiary tabeli. Zawiera ona 10296 wierszy (obserwacji) oraz 34 kolumny (zmienne)
dim(claims)

# Sprawdzono tytuÅ‚y kolumn (nazwy zmiennych)
names(claims)

# Podsumowanie informacji o ramce danych.
str(claims)

# 1. ID - numer identyfikacyjny. "intiger"

# Zmienna wykluczona z dalszej analizy.

#2. KIDSDRIV - ”integer”, liczba dzieci prowadzacych samochód

# Przeanalizowano dane w kolumnie KIDSDRIV.

hist(claims$KIDSDRIV)
summary(claims$KIDSDRIV)

claims$KIDSDRIV <- scale(claims$KIDSDRIV, center = TRUE, scale = TRUE)

#3. PLCYDATE - data zawarcie polisy. Zapisana jako "character" zamiast "date", ponadto miesiace maja francuskie nazwy.Stoworzono funkcje fr_months. 2-cyfrowy format lat powodowal problemy (56 zamienialo sie na 2056, a nie 1956, st¹d ostania linia kodu rozwi¹zuje ten problem)

fr_months <- function(data) {
  data <- sub(pattern = "janvier", replace = "-01-", x = data)
  data <- sub(pattern = "février", replace = "-02-", x = data)
  data <- sub(pattern = "mars", replace = "-03-", x = data)
  data <- sub(pattern = "avril", replace = "-04-", x = data)
  data <- sub(pattern = "mai", replace = "-05-", x = data)
  data <- sub(pattern = "juin", replace = "-06-", x = data)
  data <- sub(pattern = "juillet", replace = "-07-", x = data)
  data <- sub(pattern = "août", replace = "-08-", x = data)
  data <- sub(pattern = "septembre", replace = "-09-", x = data)
  data <- sub(pattern = "octobre", replace = "-10-", x = data)
  data <- sub(pattern = "novembre", replace = "-11-", x = data)
  data <- sub(pattern = "décembre", replace = "-12-", x = data)
  data <- as.Date(format(as.Date(data,"%d-%m-%y"), "19%y-%m-%d"), "%Y-%m-%d")
}

PLCYDATE <- claims$PLCYDATE
PLCYDATE <-fr_months(PLCYDATE)
claims$PLCYDATE <- PLCYDATE

class(claims$PLCYDATE)

# Przeanalizowano dane w kolumnie PLCYDATE.

summary(claims$PLCYDATE)

hist(claims$PLCYDATE, 17)

#4. TRAVTIME - czas dojazdu do pracy. ”integer"

# Przeanalizowano dane w kolumnie PLCYDATE.

hist(claims$TRAVTIME, 10)

summary(claims$TRAVTIME)

claims$TRAVTIME <- scale(claims$TRAVTIME, center = TRUE, scale = TRUE)

#5. CAR_USE - wykorzystanie samochodu. Zapisane jako "character" zamiast "factor" o dwóch poziomach  ”Commercial”, ”Private”. 

claims$CAR_USE <-as.factor(claims$CAR_USE)

summary(claims$CAR_USE)

#6. POLICYNO - numer polisy. "intiger" 

# Sprawdzono czy numery polisy zale¿a od daty jej podpisania.
plot(claims$POLICYNO, claims$PLCYDATE)

# Numery w pelni zrandomiozwane.

# Zmienna wykluczona z dalszej analizy.

#7. BLUEBOOK - wartosc samochodu na podstawie wyceny z Kelley Blue Book. Zapisane jako "character" zamiast "intiger".

# Zastosowano funkcje sub w celu usuniecia znaku "$" z wartosci (zapis "\\" poprzedajacy symbol "$" informuje program, ¿e stanowi on zwykly znak, a nie wyra¿enie regularne)

claims$BLUEBOOK <- as.numeric(sub(pattern = "\\$", replace = "", x = claims$BLUEBOOK))

class(claims$BLUEBOOK)

sum(is.na(claims$BLUEBOOK))

claims$BLUEBOOK <- scale(claims$BLUEBOOK, center = TRUE, scale = TRUE)

summary(claims$BLUEBOOK)

# Przeanalizowano dane w kolumnie BLUEBOOK.

hist(claims$BLUEBOOK, 10)

#8. INITDATE - data wejscia w ¿ycie polisy. Zapisana jako "character" zamiast "date", ponadto miesiace maja francuskie nazwy. Zastsowano autorsk¹ funkcjê. 

INITDATE <- claims$INITDATE

INITDATE <- fr_months(INITDATE)

claims$INITDATE <- INITDATE

#9. RETAINED - Liczba lat jako klient. ”integer”.

# Przeanalizowano dane w kolumnie RETAINED.

hist(claims$RETAINED, 10)

sum(is.na(claims$RETAINED))

claims$RETAINED <- scale(claims$RETAINED, center = TRUE, scale = TRUE)

#10. NPOLICY - Liczba polis wykupinych przez klienta. ”integer”

# Przeanalizowano dane w kolumnie NPOLICY.

hist(claims$NPOLICY)
sum(is.na(claims$NPOLICY))

#11. CAR_TYPE - typ samochodu. Zapisane jako "character" zamiast "factor" o szesciu poziomach  ”Panel Truck”, ”Pickup”, ”Sedan”, ”Sports Car”, ”SUV”, ”Van”.

claims$CAR_TYPE <- as.factor(claims$CAR_TYPE)

class(claims$NPOLICY)
sum(is.na(claims$NPOLICY))
claims$NPOLICY <- as.integer(claims$NPOLICY)
claims$NPOLICY <- scale(claims$NPOLICY, center = TRUE, scale = TRUE)

# Przeanalizowano dane w kolumnie CAR_TYPE.

plot(claims$CAR_TYPE)

#12. RED_CAR - zmienna okreslajaca czy samochód jest czerwony. Zapisana jako "character": "yes", "no". Zamieniona na zmienna boolowska (logiczna). 

claims$RED_CAR <- ifelse(claims$RED_CAR == "yes", TRUE, FALSE)

sum(is.na(claims$RED_CAR))

# Przeanalizowano dane w kolumnie RED_CAR.

sum(claims$RED_CAR)/length(claims$RED_CAR)

#13. OLDCLAIM - calkowita wartosc zgloszen w poprzednich latach. Zapisane jako "character" z symbolem "$" zamiast "intiger". 

claims$OLDCLAIM <- as.numeric(sub(pattern = "\\$", replace = "", x = claims$OLDCLAIM))

class(claims$OLDCLAIM)

hist(claims$OLDCLAIM)

sum(is.na(claims$OLDCLAIM))

claims$OLDCLAIM <- scale(claims$OLDCLAIM, center = TRUE, scale = TRUE)

#14. CLM_FREQ - liczba zgloszen w ciagu ostatnch 5 lat. Zapisane jako "character" zamiast "intiger".

claims$CLM_FREQ <- as.numeric(claims$CLM_FREQ)

# Przeanalizowano dane w kolumnie RED_CAR.

hist(claims$CLM_FREQ)

sum(is.na(claims$CLM_FREQ))

claims$CLM_FREQ <- scale(claims$CLM_FREQ, center = TRUE, scale = TRUE)

#15. REVOKED - zmienna okreslajaca czy kierowcy odebrano prawo jazdy w ciagu ostatnich 7 lat. Zapisana jako "character": "Yes", "No". Zamieniona na zmienna boolowska (logiczna). 

claims$REVOKED <- ifelse(claims$REVOKED == "Yes", TRUE, FALSE)

sum(is.na(claims$REVOKED))

#16. MVR_PTS - liczba wykroczeñ drogowych. "intiger"

# Przeanalizowano dane w kolumnie MVR_PTS.

hist(claims$MVR_PTS)

sum(is.na(claims$MVR_PTS))

claims$MVR_PTS <- scale(claims$MVR_PTS, center = TRUE, scale = TRUE)

#17. CLM_AMT - wartosc zgloszen w biezacym okresie ubezpieczeniowym. Zapisane jako "character" z symbolem "$" zamiast "intiger". 

claims$CLM_AMT <- as.numeric(sub(pattern = "\\$", replace = "", x = claims$CLM_AMT))

class(claims$CLM_AMT)

hist(claims$OLDCLAIM)

sum(is.na(claims$CLM_AMT))

claims$CLM_AMT <- scale(claims$CLM_AMT, center = TRUE, scale = TRUE)

# Zmienna wykluczona z dalszej analizy gdy¿ jest bezpoœredni zwi¹zana ze zmienn¹ celu.

#18. CLM_DATE - data zg³oszenia  w biezacym okresie ubezpieczeniowym. Zapisana jako "character" zamiast "date", ponadto miesiace maja francuskie nazwy. Stoworzono funkcje fr_months upraszczajaca ten proces.

claims$CLM_DATE <- fr_months(claims$CLM_DATE)

# Przeanalizowano dane w kolumnie CLM$DATE.

hist(claims$CLM_DATE, 10)

# Zmienna wykluczona z dalszej analizy gdy¿ jest bezpoœredni zwi¹zana ze zmienn¹ celu.

#19. CLM_FLAG - dychotomiczna zmienna celu. Zmiena okreslaj¹ca czy dokonano zg³oszenia w bie¿acym okresie ubezpieczeniowym. Zapisana jako "character": "Yes", "No". Zamieniona na zmienna boolowska (logiczna). 

claims$CLM_FLAG <- as.factor(claims$CLM_FLAG)

# Przeanalizowano dane w kolumnie CLM_FLAG.

sum(claims$CLM_FLAG)/length(claims$CLM_FLAG)

#20. BIRTH - data urodzenia. Zapisana jako "character" zamiast "date", ponadto miesiace maja francuskie nazwy. Stoworzono funkcje fr_months upraszczajaca ten proces.

claims$BIRTH <- fr_months(claims$BIRTH)

sum(is.na(claims$BIRTH))

# Przeanalizowano dane w kolumnie BIRTH.

hist(claims$BIRTH,10)

#21. AGE - grupa wiekowa kierowcy. Zapisane jako "character" zamiast "factor" o szesciu uporz¹dkowanych poziomach "16-24", "25-40", "41-60", ">60".

claims$AGE <- factor(claims$AGE,ordered = TRUE, levels = c("16-24", "25-40", "41-60", ">60"))

summary(claims$AGE)

#22. AGE*GENDER -”integer”, the age of the driver merged with gender 

# Przeanalizowano dane w kolumnie AGE*GENDER

class(claims$`AGE*GENDER`)

summary(claims$`AGE*GENDER`)

#Zmienna zawiera same wartoœci "0". Zmienna wykluczona z dalszej analizy. Nazwa zmiennej zawiera znak specjalny, co powoduje problem dla programu Rstudio.

#23. HOMEKIDS - liczba dzieci. ”integer”.

# Przeanalizowano dane w kolumnie HOMEKIDS.

class(claims$HOMEKIDS)

summary(claims$HOMEKIDS)

hist(claims$HOMEKIDS)

sum(is.na(claims$HOMEKIDS))

claims$HOMEKIDS <- scale(claims$HOMEKIDS, center = TRUE, scale = TRUE)

#24. YOJ - liczba lat w obecnej pracy. ”integer”. 

# Przeanalizowano dane w kolumnie YOJ.

class(claims$YOJ)

summary(claims$YOJ)

hist(claims$YOJ)

sum(is.na(claims$YOJ))

claims$YOJ[is.na(claims$YOJ)] <- 0

claims$YOJ <- scale(claims$YOJ, center = TRUE, scale = TRUE)

#25. INCOME - wysokosc rocznego wynagrodzenia.  Zapisane jako "character" z symbolem "$" zamiast "intiger". 

claims$INCOME <- as.numeric(sub(pattern = "\\$", replace = "", x = claims$INCOME))

sum(is.na(claims$INCOME))

claims$INCOME[is.na(claims$INCOME)] <- 0

claims$INCOME <- scale(claims$INCOME, center = TRUE, scale = TRUE)

# Przeanalizowano dane w kolumnie INCOME.

class(claims$INCOME)

hist(claims$INCOME)

#26. GENDER - plec kierowcy. Zapisane jako "character" zamiast "factor" o dwóch poziomach "M", "F".

claims$GENDER <- as.factor(claims$GENDER)

# Przeanalizowano dane w kolumnie GENDER.

summary(claims$GENDER)

#27. MARRIED - Zmienna okreslaj¹ca czy kierowca ma ma³¿onka. Zapisana jako "character": "Yes", "No". Zamieniona na zmienna boolowska (logiczna). 

claims$MARRIED <- ifelse(claims$MARRIED == "Yes", TRUE, FALSE)

sum(is.na(claims$MARRIED))


# Przeanalizowano dane w kolumnie MARRIED.

sum(claims$MARRIED)/length(claims$MARRIED)

#28. PARENT1 - Zmienna okreslaj¹ca czy kierowca jest samotnym rodzicem. Zapisana jako "character": "Yes", "No". Zamieniona na zmienna boolowska (logiczna). 

claims$PARENT1 <- ifelse(claims$PARENT1 == "Yes", TRUE, FALSE)

sum(is.na(claims$PARENT1))

# Przeanalizowano dane w kolumnie PARENT1.

sum(claims$PARENT1)/length(claims$PARENT1)

#29. JOBCLASS - grupa zawodowa kierowcy. Zapisane jako "character" zamiast "factor" o dziewiêciu poziomach: ”Blue Collar”, ”Clerical”, ”Doctor”, ”Home Maker”, ”Lawyer”, ”Manager”, ”Professional”, ”Student”, "Unknown". 

class(claims$JOBCLASS)

sum(is.na(claims$JOBCLASS))

claims$JOBCLASS[is.na(claims$JOBCLASS)] <- "Unknown"

claims$JOBCLASS <- as.factor(claims$JOBCLASS)

# Przeanalizowano dane w kolumnie JOBCLASS.

summary(claims$JOBCLASS)

plot(claims$JOBCLASS)

#30. MAX_EDUC - wykszta³cenie kierowcy. Zapisane jako "character" zamiast "factor" o piêciu uporz¹dkowanych poziomach: "<High School", "High School", "Bachelors", "Masters", "PhD".

class(claims$MAX_EDUC)

claims$MAX_EDUC <- factor(claims$MAX_EDUC, ordered = TRUE, levels = c("<High School", "High School", "Bachelors", "Masters", "PhD"))

sum(is.na(claims$MAX_EDUC))

# Przeanalizowano dane w kolumnie MAX_EDUC

summary(claims$MAX_EDUC)

plot(claims$MAX_EDUC)

#31. HOME_VAL - wartosc domu ubezpieczonego. Zapisane jako "character" zamiast "intiger".

# Zastosowano funkcje sub w celu usuniecia znaku "$" z wartosci (zapis "\\" poprzedajacy symbol "$" informuje program, ¿e stanowi on zwykly znak, a nie wyra¿enie regularne).

claims$HOME_VAL <- as.numeric(sub(pattern = "\\$", replace = "", x = claims$HOME_VAL))

# Przeanalizowano dane w kolumnie MAX_EDUC

summary(claims$HOME_VAL)

hist(claims$HOME_VAL)

sum(is.na(claims$HOME_VAL))

claims$HOME_VAL[is.na(claims$HOME_VAL)] <- 0

claims$HOME_VAL <- scale(claims$HOME_VAL, center = TRUE, scale = TRUE)

#32. SAMEHOME - liczba lat w obecnym domu. ”integer”.

# Przeanalizowano dane w kolumnie SAMEHOME

class(claims$SAMEHOME)

hist(claims$SAMEHOME)

summary(claims$SAMEHOME)

#Blad. Jedna z wartosci wynosi -3. Obserwacja ta zostanie usuniêta.

claims <- claims[-which(claims$SAMEHOME==-3),]

sum(is.na(claims$SAMEHOME))

claims$SAMEHOME[is.na(claims$SAMEHOME)] <- 0

claims$SAMEHOME <- scale(claims$SAMEHOME, center = TRUE, scale = TRUE)


#33. DENSITY - typ obszaru zamieszkaia/pracy. Zapisane jako "character" zamiast "factor" o czterech uporz¹dkowanych poziomach: : ”Highly Rural”, ”Rural”, ”Urban”, ”Highly Urban”. 

class(claims$DENSITY)

claims$DENSITY <- factor(claims$DENSITY, ordered = TRUE, levels = c("Highly Rural", "Rural", "Urban", "Highly Urban"))

sum(is.na(claims$DENSITY))

# Przeanalizowano dane w kolumnie MAX_EDUC

summary(claims$DENSITY)

plot(claims$DENSITY)

#34. YEARQTR - zmienna to¿sama z PLCYDATE. 

# Zmienna wykluczona z dalszej analizy.

claims2 <- claims[,-c(1,6,17,18,22,34)]

summary(claims2)

# Sprawdzono korelacjê miêdzy zmiennymi.

cor_check <- claims2[, c("KIDSDRIV", "TRAVTIME", "BLUEBOOK", "RETAINED", "NPOLICY", "OLDCLAIM", "CLM_FREQ", "MVR_PTS", "HOMEKIDS", "YOJ", "INCOME", "HOME_VAL", "SAMEHOME")]

res <- cor(cor_check, method = "pearson")

cor_table <- round(res, 2)

#KIDSHOME I KIDSDRIVE, INCOME I BLUEBOOK, OLDCLAIM i CLM_FREQ, CLM_FREQ i MVR_PTS, income i home_val, 

# Regresja logistyczna

# Dokonano przetasowania danych w zbiorze.

set.seed(42)

rows <- sample(nrow(claims2))

claims3 <- claims2[rows,]

# Dokonano podzialu danych na zbior treningowy i testowy w proporcji 80:20

split <- round(nrow(claims3) * 0.80)
train <- claims3[1:split, ]
test <- claims3[(split+1):nrow(claims3), ]

# Zbudowano pierwszy model logitowy.

model <- glm(CLM_FLAG~., family = "binomial", train)

# Dokonano predyckji.

p <- predict(model, test, na.action = na.pass, type = "response")

summary(p)

# Zbudowano confusion matrix.

T_or_F <- ifelse(p>0.5,"Yes","No")

class(T_or_F)

class(test$CLM_FLAG)

p_class <- factor(T_or_F)

confusionMatrix(table(p_class, test[["CLM_FLAG"]]))

# Podsumowanie i ocena modelu.

AIC(model)

summary(model)

# Zbudowano drugi model logitowy. Zawierajacy jedynie zmienne istotne.

model <- glm(CLM_FLAG~KIDSDRIV+TRAVTIME+CAR_USE+BLUEBOOK+NPOLICY+CAR_TYPE+OLDCLAIM+CLM_FREQ+REVOKED+MVR_PTS+BIRTH+AGE+INCOME+ MARRIED+PARENT1+MAX_EDUC+HOME_VAL+DENSITY, family = "binomial", train)

# Dokonano predyckji.

p <- predict(model, test, na.action = na.pass, type = "response")

summary(p)

# Zbudowano confusion matrix.

T_or_F <- ifelse(p>0.5,"Yes","No")

class(T_or_F)

class(test$CLM_FLAG)

p_class <- factor(T_or_F)

confusionMatrix(table(p_class, test[["CLM_FLAG"]]))

# Podsumowanie i ocena modelu.

summary(model)

AIC(model)

# Model dokonuje trafnych przewidywan w 80,09%.
# true positive rate (sensitivity) wynosi: 92,81%.
# true negative rate (or specificity) wynosi: 44,48%.
# Model jest bardzo skuteczny w wykrywaniu kierowcow, ktorzy nie bêd¹ dokonywaæ zgloszenia. Nalezy obnizyc wartosc cut-off-u, aby wiekszyc swoistosc modelu.

# Zbudowano trzeci model logitowy. Zawierajacy jedynie zmienne istotne.

model <- glm(CLM_FLAG~KIDSDRIV+TRAVTIME+CAR_USE+BLUEBOOK+NPOLICY+CAR_TYPE+CLM_FREQ+REVOKED+MVR_PTS+AGE+ MARRIED+PARENT1+MAX_EDUC+HOME_VAL+DENSITY, family = "binomial", train)

# Dokonano predyckji.

p <- predict(model, test, na.action = na.pass, type = "response")

summary(p)

# Zbudowano confusion matrix z nizsz¹ wartoœci¹ cut-off-u.

T_or_F <- ifelse(p>0.30,"Yes","No")

class(T_or_F)
class(test$CLM_FLAG)
p_class <- factor(T_or_F)

confusionMatrix(table(p_class, test[["CLM_FLAG"]]))
summary(model)

AIC(model)
# Model dokonuje trafnych przewidywan w 76,25%.
# true positive rate (or sensitivity) wynosi: 77,68%.
# true negative rate (or specificity) wynosi: 72,22%.
# Zmieniaj¹c wartosc cut-off-u osiagnieto poziom maksymalizuj¹cy wra¿liwosc i swoistosc modelu.

# Zwizualiozwano zaleznosc miedzy wrazliwoscia a swoistoscia modelu na wykresie krzywej ROC.

colAUC(p, test$CLM_FLAG, plotROC = TRUE)

