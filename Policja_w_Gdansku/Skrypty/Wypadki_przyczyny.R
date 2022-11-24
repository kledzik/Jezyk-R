
# Analiza danych policyjnych o przyczynach wypadków w Gdańsku
# https://gdansk.policja.gov.pl/
# https://gdansk.policja.gov.pl/pm1/statystyki/statystyka-wypadkow


library(here)
library(readxl)

# Załadowanie danych ------------------------------------------------------

# Wyświetlenie folderu roboczego
here()

# Wczytanie (drugiego) arkusza "Wypadki przyczyny"
# z pliku XLSX (Excel), zawierającego dane o wypadkach.
przyczyny <- read_xlsx(here("Dane", "statystyki_gdanskiej_policji.xlsx"),
                       sheet = "Wypadki przyczyny")

# Konwersja tibble do data.frame
przyczyny <- data.frame(przyczyny)

str(przyczyny)
summary(przyczyny)
head(przyczyny)
tail(przyczyny)

# Unikalne miesiące
unique(przyczyny$Miesiąc)

# Analizowane lata
unique(przyczyny$Rok)

# Liczba analizowanych lat
length(unique(przyczyny$Rok))

# Unikalne przyczyny wypadków
unique(przyczyny$Przyczyny.wypadków)


# Niedostosowanie prędkości w podziale na lata ----------------------------

# Wykresy w postaci dwóch wierszy i jednej kolumny
par(mfrow = c(2, 1), cex = 0.6)

# Filtr dla niedostosowanej prędkości
filtr <- przyczyny$Przyczyny.wypadków == "niedostosowanie prędkości"

# Odfiltrowanie danych dla niedostosowanej prędkości
predkosc <- przyczyny[filtr, c("Rok", "Liczba")]

# Niedostosowanie prędkości w podziale na lata
suma_predkosc <- aggregate(predkosc$Liczba, by = list(predkosc$Rok), FUN = sum)

# Zmiana nazw kolumn tego data.frame
names(suma_predkosc) <- c("Rok", "Predkosc")

# Niedostosowanie prędkości w latach
barplot(suma_predkosc$Predkosc,
        names.arg = suma_predkosc$Rok,
        horiz = FALSE,
        las = 1,
        col = 2,
        main = "Niedostosowanie prędkości",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Nieustąpienie pierwszeństwa przejazdu w podziale na lata ----------------

# Filtr dla nieustąpienia pierwszeństwa przejazdu
filtr <- przyczyny$Przyczyny.wypadków == "nieustąpienie pierwszeństwa przejazdu"

# Odfiltrowanie danych dla nieustąpienia pierwszeństwa przejazdu
przejazd <- przyczyny[filtr, c("Rok", "Liczba")]

# Nieustąpienie pierwszeństwa przejazdu w podziale na lata
suma_przejazd <- aggregate(przejazd$Liczba, by = list(przejazd$Rok), FUN = sum)

# Zmiana nazw kolumn tego data.frame
names(suma_przejazd) <- c("Rok", "Przejazd")

# Nieustąpienie pierwszeństwa przejazdu w latach
barplot(suma_przejazd$Przejazd,
        names.arg = suma_przejazd$Rok,
        horiz = FALSE,
        las = 1,
        col = 3,
        main = "Nieustąpienie pierwszeństwa przejazdu",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Nieustąpienie pierwszeństwa pieszym w podziale na lata ------------------

# Filtr dla nieustąpienia pierwszeństwa pieszym
filtr <- przyczyny$Przyczyny.wypadków == "nieustąpienie pierwszeństwa pieszym"

# Odfiltrowanie danych dla nieustąpienia pierwszeństwa pieszym
piesi_pierwszenstwo <- przyczyny[filtr, c("Rok", "Liczba")]

# Nieustąpienie pierwszeństwa pieszym w podziale na lata
suma_piesi_pierwsz <- aggregate(piesi_pierwszenstwo$Liczba,
                                by = list(piesi_pierwszenstwo$Rok), FUN = sum)

# Zmiana nazw kolumn tego data.frame
names(suma_piesi_pierwsz) <- c("Rok", "Piesi")

# Nieustąpienie pierwszeństwa pieszym w latach
barplot(suma_piesi_pierwsz$Piesi,
        names.arg = suma_piesi_pierwsz$Rok,
        horiz = FALSE,
        las = 1,
        col = 4,
        main = "Nieustąpienie pierwszeństwa pieszym",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Przyczyny z winy pieszego w podziale na lata ----------------------------

# Filtr dla przyczyn z winy pieszego
filtr <- przyczyny$Przyczyny.wypadków == "przyczyny z winy pieszego"

# Odfiltrowanie danych dla przyczyn z winy pieszego
piesi_wina <- przyczyny[filtr, c("Rok", "Liczba")]

# Przyczyny z winy pieszego w podziale na lata
suma_piesi_wina <- aggregate(piesi_wina$Liczba, by = list(piesi_wina$Rok), FUN = sum)

# Zmiana nazw kolumn tego data.frame
names(suma_piesi_wina) <- c("Rok", "Piesi")

# Przyczyny z winy pieszego w latach
barplot(suma_piesi_wina$Piesi,
        names.arg = suma_piesi_wina$Rok,
        horiz = FALSE,
        las = 1,
        col = 5,
        main = "Przyczyny z winy pieszego",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)


# Mapa cieplna zmian na przestrzeni lat -----------------------------------

# Ramka danych utworzona z wcześniej zagregowanych danych
mapa <- data.frame(niedostosowanie.prędkości = suma_predkosc$Predkosc,
                   nieustąpienie.pierwszeństwa.przejazdu = suma_przejazd$Przejazd,
                   nieustąpienie.pierwszeństwa.pieszym = suma_piesi_pierwsz$Piesi,
                   przyczyny.z.winy.pieszego = suma_piesi_wina$Piesi,
                   row.names = suma_piesi_wina$Rok)

# Zamiana kropek w nazwach kolumn na spację
names(mapa) <- gsub("[.]", " ", names(mapa))

# Przekształcenie tej ramki danych
# do macierzy oraz jej transpozycja
mapa <- t(data.matrix(mapa))

# Mapa cieplna
heatmap(mapa,
        Rowv = NA,
        Colv = NA,
        scale = "row",
        cexRow = 0.85,
        cexCol = 1,
        col = colorRampPalette(c("white", "darkred"))(256))


# Mapa cieplna zmian na przestrzeni jednego roku --------------------------

# Odfiltrowanie wierszy dla wybranego roku
mapa <- przyczyny[przyczyny$Rok == 2021, ]

# Usunięcie kolumny "Id" i "Rok"
mapa <- mapa[, -c(1, 2)]

# Przekształcenie ramki danych z formy długiej na szeroką
mapa <- reshape(data = mapa, idvar = "Miesiąc", v.names = "Liczba",
                timevar = "Przyczyny.wypadków", direction = "wide")

# Usunięcie słowa "Liczba." z nazw kolumn
names(mapa) <- sub("Liczba.", "", names(mapa))

# Zmiana nazw wierszy na miesiące
row.names(mapa) <- mapa$Miesiąc

# Usunięcie kolumny "Miesiąc"
mapa <- mapa[, -1]

# Przekształcenie tej ramki danych
# do macierzy oraz jej transpozycja
mapa <- t(data.matrix(mapa))

# Mapa cieplna
heatmap(mapa,
        Rowv = NA,
        Colv = NA,
        scale = "row",
        cexRow = 0.85,
        cexCol = 1,
        col = colorRampPalette(c("white", "darkred"))(256))
