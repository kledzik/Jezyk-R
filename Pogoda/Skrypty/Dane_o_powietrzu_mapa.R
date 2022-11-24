
# Mapa danych o powietrzu
#
# Dane IMGW
# https://danepubliczne.imgw.pl/apiinfo
#
# Przeliczanie współrzędnych geograficznych
# https://vbest.com.pl/gps/


# Załadowanie danych ------------------------------------------------------

library(jsonlite)
library(mapview)
library(RColorBrewer)
library(readxl)
library(sf)

# Pobranie pliku JSON z danymi meteorologicznymi
powietrze <- fromJSON("https://danepubliczne.imgw.pl/api/data/synop")

str(powietrze)
summary(powietrze)
head(powietrze)
tail(powietrze)

# Wczytanie arkusza "StacjeMeteo" zawierającego
# dane o stacjach meteorologicznych w Polsce
stacje <- read_xlsx("Dane/stacje_pomiarowe.xlsx", sheet = "StacjeMeteo")

# Konwersja tibble do data.frame
stacje <- data.frame(stacje)


# Oczyszczenie i modyfikacja danych ---------------------------------------

# Zmiana typu danych z character na integer i numeric
powietrze$id_stacji <- as.integer(powietrze$id_stacji)
powietrze$temperatura <- as.numeric(powietrze$temperatura)
powietrze$predkosc_wiatru <- as.numeric(powietrze$predkosc_wiatru)
powietrze$wilgotnosc_wzgledna <- as.numeric(powietrze$wilgotnosc_wzgledna)
powietrze$suma_opadu <- as.numeric(powietrze$suma_opadu)
stacje$id_stacji <- as.integer(stacje$id_stacji)

# Połączenie obu ramek danych za pomocą kolumny "id_stacji"
powietrze <- merge(x = stacje, y = powietrze, by = "id_stacji")


# Mapa interaktywna "mapview" ---------------------------------------------

# Konwersja ramki danych do obiektu spatial
powietrze <- st_as_sf(powietrze, coords = c("longitude", "latitude"), crs = 4326)

# Zapobiegnięcie zmiany koloru tła mapy
mapviewOptions("basemaps.color.shuffle" = FALSE)

# Wygenerowanie mapy sumy opadu i wilgotności względnej
mapa <- mapview(powietrze,
                label = paste(powietrze$nazwa, powietrze$suma_opadu, "mm"),
                cex = 10,
                alpha.regions = 1,
                zcol = "suma_opadu",
                col.regions = brewer.pal(9, "Blues"),
                legend = TRUE,
                layer.name = "Suma opadu [mm]") +
  mapview(powietrze,
          label = paste(powietrze$nazwa, powietrze$wilgotnosc_wzgledna, "%"),
          cex = 10,
          alpha.regions = 1,
          zcol = "wilgotnosc_wzgledna",
          col.regions = brewer.pal(9, "Greens"),
          legend = TRUE,
          layer.name = "Wilgotność wzgl. [%]")

# Wyświetlenie mapy
mapa

# Zapisanie mapy do pliku HTML
mapshot(mapa, url = "Dokumenty/Opady_i_wilgotnosc_mapa.html")


# Wygenerowanie mapy prędkości wiatru i temperatury
mapa <- mapview(powietrze,
                label = paste(powietrze$nazwa, powietrze$predkosc_wiatru, "m/s"),
                cex = 10,
                alpha.regions = 1,
                zcol = "predkosc_wiatru",
                col.regions = brewer.pal(9, "Blues"),
                legend = TRUE,
                layer.name = "Prędkość wiatru [m.s]") +
  mapview(powietrze,
          label = paste(powietrze$nazwa, powietrze$temperatura, "°C"),
          cex = 10,
          alpha.regions = 1,
          zcol = "temperatura",
          col.regions = rev(brewer.pal(11, "Spectral")),
          legend = TRUE,
          layer.name = "Temp. powietrza [°C]")

# Wyświetlenie mapy
mapa

# Zapisanie mapy do pliku HTML
mapshot(mapa, url = "Dokumenty/Temperatura_i_wiatr_mapa.html")


# Usunięcie zawartości folderu "Dokumenty"
unlink("Dokumenty/*", recursive = TRUE)
