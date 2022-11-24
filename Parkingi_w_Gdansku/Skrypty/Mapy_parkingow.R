
# Mapy parkingów w Gdańsku
# https://ckan.multimediagdansk.pl/dataset/otwarte-dane-tristar-w-gdansku
#
# Wyszukiwarka parkingów
# https://en.parkopedia.pl/
#
# Współrzędne geograficzne i mapy
# https://boundingbox.klokantech.com/
#
# Google API
# https://www.youtube.com/watch?v=Of_M4kcE9yM


# Załadowanie danych ------------------------------------------------------

library(ggplot2)
library(ggmap)
library(ggspatial)
library(jsonlite)
library(leaflet)
library(mapview)
library(sf)


# Pobranie pliku JSON z nazwami i adresami parkingów
parking_names <- fromJSON("https://ckan.multimediagdansk.pl/dataset/cb1e2708-aec1-4b21-9c8c-db2626ae31a6/resource/d361dff3-202b-402d-92a5-445d8ba6fd7f/download/parking-lots.json")

str(parking_names)
summary(parking_names)
head(parking_names)
tail(parking_names)


# Oczyszczenie i modyfikacja danych ---------------------------------------

# Pobranie z listy jej drugiego elementu, czyli ramki danych
# z nazwami i adresami parkingów, bez daty modyfikacji zbioru
parking_names <- parking_names[[2]]

# Skrócenie nazwy Europejskiego Centrum Solidarności
parking_names$name <- gsub("Europejskie Centrum Solidarności",
                           "E.C. Solidarności", parking_names$name)


# Mapa statyczna ----------------------------------------------------------

# Ramka danych "parking_names" zawiera
# "podramkę" ze współrzędnymi geograficznymi
str(parking_names)

# Pobranie tej "podramki" danych
geo_coord <- parking_names[, "location"]

# Pobranie mapy Gdańska
gdansk <- ggmap(get_map(c(18.575041, 54.336607, 18.676939, 54.430017)))

# Mapa podstawowa
gdansk +
  geom_point(aes(x = longitude, y = latitude), data = geo_coord)

# Mapa ze zmodyfikowanymi punktami
gdansk +
  geom_point(aes(x = longitude, y = latitude), data = geo_coord,
             color = "blue", size  = 6, alpha = 0.5)

# Mapa upiększona
mapa_statyczna <- gdansk +
  geom_point(aes(x = longitude, y = latitude), data  = geo_coord,
             color = "blue", size  = 6, alpha = 0.5) +
  annotation_north_arrow(location = "tr") +
  labs(x = "Długość", y = "Szerokość",
       title = "Parkingi w Gdańsku",
       subtitle = "wg danych systemu Tristar")
  # + theme_minimal()

# Wyświetlenie mapy
mapa_statyczna

# Zapisanie mapy do pliku PDF. Wymiary domyślnie w calach
ggsave("Dokumenty/mapa_statyczna.pdf", mapa_statyczna, height = 10, width = 7)

# Zapisanie mapy do pliku PNG
ggsave("Dokumenty/mapa_statyczna.png", mapa_statyczna)


# Mapa interaktywna "mapview" ---------------------------------------------

# Pobranie "podramki" danych
Parkingi <- parking_names[, "location"]

# Konwersja danych do formatu sf
Parkingi <- st_as_sf(Parkingi, coords = c("longitude", "latitude"), crs = 4326)

# Mapa podstawowa
mapview(Parkingi)

# Mapa z opisami znaczników po kliknięciu
# na nie myszą, i bez legendy
mapview(Parkingi,
        popup = paste("Adres:", parking_names$address),
        legend = FALSE,
        label = parking_names$name,
        layer.name = "Parkingi w Gdańsku")

# Zapisanie mapy do pliku HTML
mapshot(mapview(Parkingi,
                popup = paste("Adres:", parking_names$address),
                legend = FALSE,
                label = parking_names$name,
                layer.name = "Parkingi w Gdańsku"),
        url = "Dokumenty/mapa_mapview.html")


# Mapy interaktywne "leaflet" ---------------------------------------------

# Te mapy można zapisać do pliku HTML wg metody podanej wyżej

# Mapa podstawowa, ze znacznikami jak w Google
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = parking_names$location$longitude,
             lat = parking_names$location$latitude)

# Mapa ze znacznikami w postaci okręgów
# i opisami po kliknięciu na nie myszą
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = parking_names$location$longitude,
                   lat = parking_names$location$latitude,
                   popup = paste("Nazwa parkingu:", parking_names$name,
                                 "<br>",
                                 "Adres:", parking_names$address,
                                 "<br>",
                                 "Wjazd od strony:", parking_names$streetEntrance))

# Jak wyżej, dodatkowo z grupowaniem okręgów
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = parking_names$location$longitude,
                   lat = parking_names$location$latitude,
                   popup = paste("Nazwa parkingu:", parking_names$name,
                                 "<br>",
                                 "Adres:", parking_names$address,
                                 "<br>",
                                 "Wjazd od strony:", parking_names$streetEntrance),
                   clusterOptions = markerClusterOptions())

# Zapisanie mapy do pliku HTML
mapshot(leaflet() %>%
          addTiles() %>%
          addCircleMarkers(lng = parking_names$location$longitude,
                           lat = parking_names$location$latitude,
                           popup = paste("Nazwa parkingu:", parking_names$name,
                                         "<br>",
                                         "Adres:", parking_names$address,
                                         "<br>",
                                         "Wjazd od strony:", parking_names$streetEntrance),
                           clusterOptions = markerClusterOptions()),
        url = "Dokumenty/mapa_leaflet.html")

# Usunięcie zawartości folderu "Dokumenty"
unlink("Dokumenty/*", recursive = TRUE)
