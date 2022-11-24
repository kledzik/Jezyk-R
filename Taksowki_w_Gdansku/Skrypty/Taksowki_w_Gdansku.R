
# Wykaz taksówek z licencjami w Gdańsku
#
# https://ckan.multimediagdansk.pl/dataset/wykaz-taksowek-z-licencjami-w-gdansku
# https://motofakty.pl/tablice-rejestracyjne-2021-jak-poznac-z-ktorej-czesci-polski-jest-pojazd/ar/c4-16285509


# Załadowanie danych ------------------------------------------------------

library(ggplot2)
library(htmltools)
library(jsonlite)
library(mapview)
library(readxl)
library(sf)

# Pobranie pliku JSON z danymi o taksówkach
taxi <- fromJSON("https://files.cloudgdansk.pl/f/xml/wykaz-taksowek-z-licencjami.json")

str(taxi)
summary(taxi)
head(taxi$results)
tail(taxi$results)

# Przykładowe numery rejestracyjne
sample(taxi$results$numerRejestracyjny, 20)

# Rozkład długości numerów rejestracyjnych
table(nchar(taxi$results$numerRejestracyjny))

# Pobranie pierwszych trzech znaków
numery_rejestracyjne <- substr(taxi$results$numerRejestracyjny, start = 1, stop = 3)

# Usunięcie z nich cyfr, występujących jeden lub wiele razy
numery_rejestracyjne <- sub("[0-9]+", "", numery_rejestracyjne)

# Usunięcie znaków od spacji włącznie, aż do końca. Wzorzec
# to: spacja ( ), następnie dowolny znak (.) dowolną ilość
# razy (*) aż do końca ($) wyrażenia łańcuchowego.
prefiksy_rejestracyjne <- sub( " .*$", "", numery_rejestracyjne)

# Rozkład długości prefiksów literowych
table(nchar(prefiksy_rejestracyjne))

# Posortowany rozkład liczebności prefiksów
sort(table(prefiksy_rejestracyjne), decreasing = TRUE)
prefiksy_rejestracyjne <- sort(table(prefiksy_rejestracyjne), decreasing = TRUE)

# Odrzucenie mniej licznych prefiksów, dla wykresu barplot
prefiksy_rejestracyjne50 <- prefiksy_rejestracyjne[prefiksy_rejestracyjne > 50]

# Wykres popularnych prefiksów
barplot(prefiksy_rejestracyjne50,
        las = 1,
        col = rainbow(length(prefiksy_rejestracyjne50)),
        main = "Najpopularniejsze rejestracje taksówek w Gdańsku",
        cex.names = 0.8)

# Konwersja zbioru wszystkich prefiksów do data.frame
prefiksy_rejestracyjne <- data.frame(prefiks = rownames(prefiksy_rejestracyjne),
                                     ilosc = as.vector(prefiksy_rejestracyjne))

# Wczytanie arkusza "Miejscowości" z pliku XLSX
# zawierającego dane o prefiksach i miejscowościach
miasta <- read_xlsx("Dane/wspolrzedne_miast.xlsx", sheet = "Miasta")

# Konwersja tibble do data.frame
miasta <- data.frame(miasta)

# Pobranie danych niezawierających wartości "NA".
# Ale łatwiej jest użyć polecenie "omit()"
miasta <- miasta[(!is.na(miasta$latitude)) &
                   (!is.na(miasta$longitude)), ]

# Połączenie obu ramek danych za pomocą kolumny "prefiks"
miasta <- merge(x = miasta, y = prefiksy_rejestracyjne, by = "prefiks")

# Konwersja ramki danych do obiektu spatial
miasta <- st_as_sf(miasta, coords = c("longitude", "latitude"), crs = 4326)

# Zapobiegnięcie zmiany koloru tła mapy
mapviewOptions("basemaps.color.shuffle" = FALSE)

# Wygenerowanie mapy
mapa <- mapview(miasta,
                label = paste(miasta$miasto,
                              miasta$prefiks,
                              miasta$ilosc, sep = ", "),
                cex = 7,
                alpha.regions = 0.5,
                zcol = "ilosc",
                col.regions = "green",
                legend = FALSE,
                layer.name = "Liczba taksówek")

# Wyświetlenie mapy
mapa

# Zapisanie mapy do pliku HTML
mapshot(mapa, url = "Dokumenty/mapa.html")

# Usunięcie zawartości folderu "Dokumenty"
unlink("Dokumenty/*", recursive = TRUE)
