
# Mapy temperatury powietrza w Gdyni
#
# https://otwartedane.gdynia.pl/pl/dataset/lista-aktualnych-danych-pogodowych
# https://otwartedane.gdynia.pl/pl/dataset/lista-stacji-pogodowych
# https://otwartedane.gdynia.pl/pl/dataset/osie-drog-publicznych


# Załadowanie danych ------------------------------------------------------

library(glue)
library(htmltools)
library(jsonlite)
library(mapview)
library(RColorBrewer)
library(sf)


# Utworzenie podfolderu "drogi" wewnątrz folderu "Dane"
dir.create("Dane/drogi")

# Pobranie pliku ZIP z kształtem dróg
download.file("https://otwartedane.gdynia.pl/pl/dataset/86d61404-8b4b-4ae4-94d8-e98629b31ccb/resource/fa75a29a-45ed-4502-b251-7a35dc9626c0/download/osie_drog_publicznych.zip",
              destfile = "Dane/drogi/osie_drog_publicznych.zip")

# Rozpakowanie pliku ZIP
unzip("Dane/drogi/osie_drog_publicznych.zip", exdir = "Dane/drogi")

# Wczytanie pliku z kształtem dróg w Gdyni. "quiet = TRUE"
# powoduje niewyświetlenie informacji o zaimportowanych danych
# przestrzennych.Podobnie działa funkcja read_sf()
drogi <- st_read("Dane/drogi/Osie Publiczne Gdynia Otwarte Dane2.shp", quiet = TRUE)

str(drogi)
summary(drogi)
head(drogi)
tail(drogi)

# Pobranie pliku JSON z danymi meteorologicznymi
meteo <- fromJSON("http://api.zdiz.gdynia.pl/ri/rest/weather_stations_data")

str(meteo)
summary(meteo)
head(meteo)
tail(meteo)

# Pobranie pliku JSON z danymi o stacjach meteorologicznych
stacje <- fromJSON("http://api.zdiz.gdynia.pl/ri/rest/weather_stations")

str(stacje)
summary(stacje)
head(stacje)
tail(stacje)


# Oczyszczenie i modyfikacja danych ---------------------------------------

# Pobranie numeru id stacji oraz temperatury
meteo <- meteo[, c("weatherStationId", "airTemperature")]

# Wydobycie współrzędnych geograficznych z listy
# "stacje", i przekształcenie ich w ramkę danych
wspolrzedne <- data.frame(do.call("rbind", stacje$weatherStations$location$coordinates))

# Zmiana nazw kolumn data.frame
names(wspolrzedne) <- c("longitude", "latitude")

# Utworzenie ramki danych z uporządkowanymi danymi
stacje <- data.frame(weatherStationId = stacje$weatherStations$id,
                     street = stacje$weatherStations$street,
                     longitude = wspolrzedne$longitude,
                     latitude = wspolrzedne$latitude)

# Połączenie obu ramek danych za pomocą kolumny z id
temperatura <- merge(x = stacje, y = meteo, by = "weatherStationId")

# Konwersja ramki danych temperaturowych do obiektu spatial
temperatura <- st_as_sf(temperatura, coords = c("longitude", "latitude"), crs = 4326)

# Zapobiegnięcie zmiany koloru tła mapy
mapviewOptions("basemaps.color.shuffle" = FALSE)

# Zakres skali kolorów od temp.min do temp.max
kolory.temp <- seq(from = min(temperatura$airTemperature),
                   to = max(temperatura$airTemperature), length.out = 6)

# Zaokrąglenie wartości
kolory.temp <- round(kolory.temp, digits = 2)

# Wygenerowanie mapy
mapa <- mapview(temperatura,
                popup = glue("<strong>Ulica</strong><br/>
                             {temperatura$street}") %>% lapply(HTML),
                label = paste(temperatura$airTemperature, "°C", sep = " "),
                cex = 10,
                alpha.regions = 1,
                zcol = "airTemperature",
                col.regions = brewer.pal(5, "Reds"), # "6" aby zmienić na zakres
                at = kolory.temp,
                legend = TRUE,
                layer.name = "Temperatura [°C]" ) +
  
  mapview(drogi,
          popup = glue("<strong>Kategoria drogi:</strong><br/>
                       {drogi$kategoria}") %>% lapply(HTML),
          label = drogi$nazwa_ulic,
          alpha = 0.4,
          color = "black",
          legend = FALSE,
          layer.name = "Drogi" )

# Wyświetlenie mapy
mapa

# Zapisanie mapy do pliku HTML
mapshot(mapa, url = "Dokumenty/Gdynia_mapa.html")


# Usunięcie zbędnych danych -----------------------------------------------

# Usunięcie podfolderu "drogi"
unlink("Dane/drogi", recursive = TRUE)

# Usunięcie zawartości folderu "Dokumenty"
unlink("Dokumenty/*", recursive = TRUE)
