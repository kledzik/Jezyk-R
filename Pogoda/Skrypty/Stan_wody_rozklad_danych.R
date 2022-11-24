
# Rozkład danych stanu wód i temperatury
#
# Dane IMGW
# https://danepubliczne.imgw.pl/apiinfo
# https://hydro.imgw.pl/

library(jsonlite)

# Pobranie pliku JSON z danymi hydrologicznymi
woda <- fromJSON("https://danepubliczne.imgw.pl/api/data/hydro/")

# Zmiana typu danych
woda$stan_wody <- as.integer(woda$stan_wody)
woda$temperatura_wody <- as.numeric(woda$temperatura_wody)

# Wykresy w postaci jednego wiersza i dwóch kolumn
par(mfrow = c(1, 2), cex = 0.7)

# Wykres pudełkowy dla poziomu wód
boxplot(woda$stan_wody,
        ylab = "Poziom wody [mm]",
        las = 2,
        col = 4,
        varwidth = FALSE,
        cex.axis = 0.9)

# Wykres pudełkowy dla temperatury wód
boxplot(woda$temperatura_wody,
        ylab = "Temperatura wody [°C]",
        las = 2,
        col = 2,
        varwidth = FALSE,
        cex.axis = 0.9)

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)

# Histogram poziomu wód
hist(woda$stan_wody,
     col = 4,
     xlab = "Poziom wody [mm]",
     main = "")

# Histogram temperatury wód
hist(woda$temperatura_wody,
     breaks = 30,
     col = 2,
     xlab = "Temperatura wody [°C]",
     main = "")

# Reprezentacja punktów pomiarowych
rug(woda$temperatura_wody, col = "red", lwd = 2.3)

# Zagęszczenie oznaczeń osi X
axis(side = 1, at = seq(-2, 16, 2), labels = seq(-2, 16, 2))

# Odrzucenie skrajnych wartości temperatury
filtr <-  woda$temperatura_wody[woda$temperatura_wody >
                                  -10 & woda$temperatura_wody < 30]

# Histogram temperatury wód
hist(filtr,
     breaks = 15,
     col = 2,
     xlab = "Temperatura wody [°C]",
     main = "")

# Reprezentacja punktów pomiarowych
rug(woda$temperatura_wody, col = "red", lwd = 2.3)

# Zagęszczenie oznaczeń osi X
axis(side = 1, at = seq(0, 20, 1), labels = seq(0, 20, 1))
