
# Mapy poziomu wód i temperatury
#
# Dane IMGW
# https://danepubliczne.imgw.pl/apiinfo
# https://hydro.imgw.pl/
#
# Współrzędne geograficzne i mapy
# https://boundingbox.klokantech.com/


# Załadowanie danych ------------------------------------------------------

library(glue)
library(htmltools)
library(jsonlite)
library(mapview)
library(RColorBrewer)
library(readxl)
library(sf)

# Pobranie pliku JSON z danymi hydrologicznymi
woda <- fromJSON("https://danepubliczne.imgw.pl/api/data/hydro/")

str(woda)
summary(woda)
head(woda)
tail(woda)

# Wczytanie arkusza "StacjeHydro" zawierającego
# dane o stacjach hydrologicznych w Polsce
stacje <- read_xlsx("Dane/stacje_pomiarowe.xlsx", sheet = "StacjeHydro")

# Konwersja tibble do data.frame
stacje <- data.frame(stacje)


# Oczyszczenie i modyfikacja danych ---------------------------------------

# Zmiana typu danych w kolumnach z character
# na integer, date i numeric

woda$id_stacji <- as.integer(woda$id_stacji)

woda$stan_wody_data_pomiaru <- as.Date(woda$stan_wody_data_pomiaru,
                                       format="%Y-%m-%d")

woda$stan_wody <- as.integer(woda$stan_wody)

woda$temperatura_wody_data_pomiaru <- as.Date(woda$temperatura_wody_data_pomiaru,
                                              format="%Y-%m-%d")

woda$temperatura_wody <- as.numeric(woda$temperatura_wody)

stacje$id_stacji <- as.integer(stacje$id_stacji)

# Połączenie obu ramek danych za pomocą kolumny "id_stacji"
woda <- merge(x = stacje, y = woda, by = "id_stacji")

# Pobranie kolumn z danymi o temperaturze do nowego data.frame
woda.temp <- woda[, c("id_stacji", "stacja", "rzeka", "temperatura_wody_data_pomiaru",
                      "temperatura_wody", "latitude", "longitude")]

# Usunięcie wierszy zawierających wartości NA
# w kolumnie z temperaturą i datą jej pomiaru
woda.temp <- woda.temp[(!is.na(woda.temp$temperatura_wody_data_pomiaru)) &
                         (!is.na(woda.temp$temperatura_wody)), ]

# Usunięcie wierszy o temperaturze mniejszej niż ... °C
woda.temp <- woda.temp[woda.temp$temperatura_wody >= 5, ]

# Zachowanie wierszy tylko z dzisiejszymi lub wczorajszymi pomiarami
woda.temp <- woda.temp[woda.temp$temperatura_wody_data_pomiaru >= Sys.Date() - 1, ]

# Pobranie kolumn z danymi o poziomie wód
woda.stan <- woda[, c("id_stacji", "stacja", "rzeka", "stan_wody_data_pomiaru",
                      "stan_wody", "latitude", "longitude")]

# Usunięcie wierszy zawierających wartości NA
# w kolumnie z poziomem wód i datą jego pomiaru
woda.stan <- woda.stan[(!is.na(woda.stan$stan_wody_data_pomiaru)) &
                         (!is.na(woda.stan$stan_wody)), ]

# Zachowanie wierszy tylko z dzisiejszymi lub wczorajszymi pomiarami
woda.stan <- woda.stan[woda.stan$stan_wody_data_pomiaru >= Sys.Date() - 1, ]


# Mapa interaktywna "mapview" ---------------------------------------------

# Konwersja ramki danych temperaturowych do obiektu spatial
woda.temp <- st_as_sf(woda.temp, coords = c("longitude", "latitude"), crs = 4326)

# Konwersja ramki danych z poziomem wód do obiektu spatial
woda.stan <- st_as_sf(woda.stan, coords = c("longitude", "latitude"), crs = 4326)

# Zakres skali kolorów od temp.min do temp.max
kolory.temp <- seq(from = min(woda.temp$temperatura_wody),
                   to = max(woda.temp$temperatura_wody), length.out = 10)

# Zaokrąglenie wartości
kolory.temp <- round(kolory.temp, digits = 2)

# Zakres skali kolorów od poziom.min do poziom.max
kolory.poz <- seq(from = min(woda.stan$stan_wody),
                  to = max(woda.stan$stan_wody), length.out = 10)

# Zaokrąglenie wartości
kolory.poz <- round(kolory.poz, digits = 0)

# Zapobiegnięcie zmiany koloru tła mapy
mapviewOptions("basemaps.color.shuffle" = FALSE)

# Wygenerowanie mapy
mapa <-
  mapview(woda.stan,
          popup = glue("<strong>Stacja</strong>: {woda.stan$stacja}<br/>
          <strong>Rzeka</strong>: {woda.stan$rzeka}") %>% lapply(HTML),
          label = paste(woda.stan$stan_wody, "mm", sep = " "),
          cex = 7,
          alpha.regions = 1,
          zcol = "stan_wody",
          col.regions = brewer.pal(9, "Blues"),
          at = kolory.poz,
          legend = TRUE,
          layer.name = "Poziom [mm]" ) +
  
  mapview(woda.temp,
          popup = glue("<strong>Stacja</strong>: {woda.temp$stacja}<br/>
          <strong>Rzeka</strong>: {woda.temp$rzeka}") %>% lapply(HTML),
          label = paste(woda.temp$temperatura_wody, "°C", sep = " "),
          cex = 7,
          alpha.regions = 1,
          zcol = "temperatura_wody",
          col.regions = brewer.pal(9, "Reds"),
          at = kolory.temp,
          legend = TRUE,
          layer.name = "Temperatura [°C]")

# Wyświetlenie mapy
mapa

# Zapisanie mapy do pliku HTML
mapshot(mapa, url = "Dokumenty/Stan_wody_mapa.html")

# Usunięcie zawartości folderu "Dokumenty"
unlink("Dokumenty/*", recursive = TRUE)
