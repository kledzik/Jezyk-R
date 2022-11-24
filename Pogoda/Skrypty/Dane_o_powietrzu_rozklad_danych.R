
# Rozkład danych o powietrzu
#
# Dane IMGW
# https://danepubliczne.imgw.pl/apiinfo
#
# Przeliczanie współrzędnych geograficznych
# https://vbest.com.pl/gps/

library(jsonlite)

# Pobranie pliku JSON z danymi meteorologicznymi
powietrze <- fromJSON("https://danepubliczne.imgw.pl/api/data/synop")

# Zmiana typu danych z character na numeric
powietrze$temperatura <- as.numeric(powietrze$temperatura)
powietrze$predkosc_wiatru <- as.numeric(powietrze$predkosc_wiatru)
powietrze$wilgotnosc_wzgledna <- as.numeric(powietrze$wilgotnosc_wzgledna)
powietrze$suma_opadu <- as.numeric(powietrze$suma_opadu)

# Wykresy w postaci dwóch wierszy i dwóch kolumn
par(mfrow = c(2, 2), cex = 0.7)

# Wykres pudełkowy temperatury powietrza
boxplot(powietrze$temperatura,
        main = "Temperatura powietrza",
        ylab = "[°C]",
        las = 2,
        col = 2,
        varwidth = FALSE,
        cex.axis = 1)

# Wykres pudełkowy prędkości wiatru
boxplot(powietrze$predkosc_wiatru,
        main = "Prędkość wiatru",
        ylab = "[m/s]",
        las = 2,
        col = 3,
        varwidth = FALSE,
        cex.axis = 1)

# Wykres pudełkowy wilgotności względnej
boxplot(powietrze$wilgotnosc_wzgledna,
        main = "Wilgotność względna",
        ylab = "[%]",
        las = 2,
        col = 4,
        varwidth = FALSE,
        cex.axis = 1)

# Wykres pudełkowy sumy opadu
boxplot(powietrze$suma_opadu,
        main = "Suma opadu",
        ylab = "[mm]",
        las = 2,
        col = 7,
        varwidth = FALSE,
        cex.axis = 1)

# Histogram temperatury powietrza
hist(powietrze$temperatura,
     breaks = 10,
     col = 2,
     xlab = "[°C]",
     main = "Temperatura powietrza")

# Histogram prędkości wiatru
hist(powietrze$predkosc_wiatru,
     breaks = 10,
     col = 3,
     xlab = "[m/s]",
     main = "Prędkość wiatru")

# Histogram wilgotności względnej
hist(powietrze$wilgotnosc_wzgledna,
     breaks = 10,
     col = 4,
     xlab = "[%]",
     main = "Wilgotność względna")

# Histogram sumy opadu
hist(powietrze$suma_opadu,
     breaks = 10,
     col = 7,
     xlab = "[mm]",
     main = "Suma opadu")

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)
