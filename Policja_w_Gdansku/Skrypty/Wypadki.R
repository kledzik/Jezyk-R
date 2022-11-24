
# Analiza danych policyjnych o wypadkach w Gdańsku
# https://gdansk.policja.gov.pl/
# https://gdansk.policja.gov.pl/pm1/statystyki/statystyka-wypadkow


library(here)
library(readxl)

# Załadowanie danych ------------------------------------------------------

# Wyświetlenie folderu roboczego
here()

# Wczytanie (pierwszego) arkusza "Wypadki ogólnie"
# z pliku XLSX (Excel), zawierającego dane o wypadkach.
wypadki <- read_xlsx(here("Dane", "statystyki_gdanskiej_policji.xlsx"),
                     sheet = "Wypadki")

# Konwersja tibble do data.frame
wypadki <- data.frame(wypadki)

str(wypadki)
summary(wypadki)
head(wypadki)
tail(wypadki)

# Unikalne miesiące
unique(wypadki$Miesiąc)

# Analizowane lata
unique(wypadki$Rok)

# Liczba analizowanych lat
length(unique(wypadki$Rok))


# Suma kolizji w podziale na lata -----------------------------------------

# Wykresy w postaci dwóch wierszy i jednej kolumny
par(mfrow = c(2, 1), cex = 0.6)

# Suma kolizji w podziale na lata
suma_kolizji <- tapply(wypadki$Kolizje, wypadki$Rok, FUN = sum)

# Konwersja otrzymanej tablicy do data.frame i zmiana nazw kolumn
suma_kolizji <- data.frame(Rok = row.names(suma_kolizji),
                           Kolizje = suma_kolizji,
                           row.names = NULL)

# Liczba kolizji w latach
barplot(suma_kolizji$Kolizje,
        names.arg = suma_kolizji$Rok,
        horiz = FALSE,
        las = 1,
        col = 2,
        main = "Liczba kolizji",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Suma wypadków w podziale na lata ----------------------------------------

# Suma wypadków w podziale na lata
suma_wypadkow <- tapply(wypadki$Wypadki, wypadki$Rok, FUN = sum)

# Konwersja otrzymanej tablicy do data.frame i zmiana nazw kolumn
suma_wypadkow <- data.frame(Rok = row.names(suma_wypadkow),
                            Wypadki = suma_wypadkow,
                            row.names = NULL)

# Liczba wypadków w latach
barplot(suma_wypadkow$Wypadki,
        names.arg = suma_wypadkow$Rok,
        horiz = FALSE,
        las = 1,
        col = 3,
        main = "Liczba wypadków",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Suma osób rannych w podziale na lata ------------------------------------

# Suma osób rannych w podziale na lata
suma_rannych <- tapply(wypadki$Ranni, wypadki$Rok, FUN = sum)

# Konwersja otrzymanej tablicy do data.frame i zmiana nazw kolumn
suma_rannych <- data.frame(Rok = row.names(suma_rannych),
                           Ranni = suma_rannych,
                           row.names = NULL)

# Liczba osób rannych w latach
barplot(suma_rannych$Ranni,
        names.arg = suma_rannych$Rok,
        horiz = FALSE,
        las = 1,
        col = 4,
        main = "Liczba rannych w wypadkach",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Suma osób zabitych w podziale na lata -----------------------------------

# Suma osób zabitych w podziale na lata
suma_zabitych <- tapply(wypadki$Zabici, wypadki$Rok, FUN = sum)

# Konwersja otrzymanej tablicy do data.frame i zmiana nazw kolumn
suma_zabitych <- data.frame(Rok = row.names(suma_zabitych),
                            Zabici = suma_zabitych,
                            row.names = NULL)

# Liczba osób zabitych w latach
barplot(suma_zabitych$Zabici,
        names.arg = suma_zabitych$Rok,
        horiz = FALSE,
        las = 1,
        col = 5,
        main = "Liczba zabitych w wypadkach",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Liczba rannych i zabitych per 10 wypadków, w podziale na lata -----------

# Liczba osób rannych i zabitych per 10 wypadków
suma_poszkodowanych <- 10 * (suma_zabitych$Zabici + suma_rannych$Ranni) /
  suma_wypadkow$Wypadki

# Liczba poszkodowanych per 10 wypadków w latach
barplot(suma_poszkodowanych,
        names.arg = suma_zabitych$Rok,
        horiz = FALSE,
        las = 1,
        col = 6,
        main = "Liczba poszkodowanych na 10 wypadków",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Śmiertelność poszkodowanych w wypadkach, w podziale na lata -------------

# Śmiertelność poszkodowanych w wypadkach, w podziale na lata
smiertelnosc_poszkodowanych <- 100 * suma_zabitych$Zabici /
  (suma_zabitych$Zabici + suma_rannych$Ranni)

# Procent osób zabitych w latach
barplot(smiertelnosc_poszkodowanych,
        names.arg = suma_zabitych$Rok,
        horiz = FALSE,
        las = 1,
        col = 7,
        main = "Procent zabitych wśród poszkodowanych",
        xlab = "Rok",
        ylab = "%",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 1)


# Suma nietrzeźwych kierowców w podziale na lata --------------------------

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)

# Suma nietrzeźwych kierowców w podziale na lata
suma_pijanych <- tapply(wypadki$Nietrzeźwi.kierowcy, wypadki$Rok, FUN = sum)

# Konwersja otrzymanej tablicy do data.frame i zmiana nazw kolumn
suma_pijanych <- data.frame(Rok = row.names(suma_pijanych),
                            Pijani = suma_pijanych,
                            row.names = NULL)

# Liczba nietrzeźwych kierowców w latach
barplot(suma_pijanych$Pijani,
        names.arg = suma_pijanych$Rok,
        horiz = FALSE,
        las = 1,
        col = 8,
        main = "Liczba nietrzeźwych kierowców",
        xlab = "Rok",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.8)


# Mapa cieplna zmian na przestrzeni lat -----------------------------------

# Ramka danych utworzona z wcześniej zagregowanych danych
mapa <- data.frame(kolizje = suma_kolizji$Kolizje,
                   wypadki = suma_wypadkow$Wypadki,
                   pijani.kierowcy = suma_pijanych$Pijani,
                   ranni = suma_rannych$Ranni,
                   zabici = suma_zabitych$Zabici,
                   row.names = suma_zabitych$Rok)

# Zamiana nazwy "pijani.kierowcy" na "pijani kierowcy"
names(mapa) <- sub("pijani.kierowcy", "pijani kierowcy", names(mapa))

# Przekształcenie tej ramki danych
# do macierzy oraz jej transpozycja
mapa <- t(data.matrix(mapa))

# Mapa cieplna
heatmap(mapa,
        Rowv = NA,
        Colv = NA,
        scale = "row",
        cexRow = 1.1,
        cexCol = 1,
        col = colorRampPalette(c("white", "darkred"))(256))


# Mapa cieplna zmian na przestrzeni jednego roku --------------------------

# Odfiltrowanie wierszy dla wybranego roku
mapa <- wypadki[wypadki$Rok == 2021, ]

# Zmiana nazw wierszy na miesiące
row.names(mapa) <- mapa$Miesiąc

# Wybranie kolumn i zmiana ich kolejności
mapa <- mapa[, c("Kolizje", "Wypadki", "Nietrzeźwi.kierowcy", "Ranni", "Zabici")]

# Nazwy kolumn z małej litery
names(mapa) <- tolower(names(mapa))

# Zamiana nazwy "nietrzeźwi.kierowcy" na "pijani kierowcy"
names(mapa) <- sub("nietrzeźwi.kierowcy", "pijani kierowcy", names(mapa))

# Przekształcenie tej ramki danych
# do macierzy oraz jej transpozycja
mapa <- t(data.matrix(mapa))

# Mapa cieplna
heatmap(mapa,
        Rowv = NA,
        Colv = NA,
        scale = "row",
        cexRow = 1.1,
        cexCol = 1,
        col = colorRampPalette(c("white", "brown"))(256))
