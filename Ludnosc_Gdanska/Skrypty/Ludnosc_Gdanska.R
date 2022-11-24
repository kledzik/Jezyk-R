
# Mapy zaludnienia dzielnic w Gdańsku
# https://ckan.multimediagdansk.pl/dataset/granice-dzielnic-w-gdansku
# https://www.gdansk.pl/gdansk-w-liczbach
# https://gcigdansk.sharepoint.com/:x:/s/UMG-OtwarteDane3.0/ERwCWKxr_JJGiF_7U0DlwOkBv5vrEWR3H8W8HHsxHFaJyA?rtime=mcw6bvjE2kg


# Załadowanie danych ------------------------------------------------------

library(leaflet)
library(leaflet.extras2)
library(leafsync)
library(mapview)
library(RColorBrewer)
library(readxl)
library(sf)


# Utworzenie podfolderu "dzielnice" wewnątrz folderu "Dane"
dir.create("Dane/dzielnice")

# Rozpakowanie pliku ZIP
unzip("Dane/dzielnice.zip", exdir = "Dane/dzielnice")

# Wczytanie pliku z kształtem dzielnic Gdańska
# "quiet = TRUE" powoduje niewyświetlenie informacji o zaimportowanych
# danych przestrzennych. Podobnie działa funkcja read_sf()
dzielnice <- st_read("Dane/dzielnice/dzielnice.shp", quiet = TRUE,
                     options = "ENCODING=UTF-8")

str(dzielnice)
summary(dzielnice)
head(dzielnice)
tail(dzielnice)

# Nazwy kolumn tego obiektu
names(dzielnice)

# Usunięcie pierwszego myślnika z nazwy Orunii-Św.Woj.-Lip.,
# aby wyeliminować błąd kolejności sortowania obu Orunii
dzielnice$DZIELNICY[11] <- "Orunia Św. Wojciech-Lipce"

# Posortowanie alfabetyczne nazw dzielnic
dzielnice <- dzielnice[order(dzielnice$DZIELNICY), ]

# Wczytanie arkusza "Mieszkańcy" z pliku XLSX
# zawierającego liczbę mieszkańców w dzielnicach
ludnosc <- read_xlsx("Dane/ludnosc_Gdanska.xlsx", sheet = "Mieszkańcy")

# Konwersja tibble do data.frame
ludnosc <- data.frame(ludnosc)

# Czy obie kolumny z nazwami dzielnic są identyczne
identical(dzielnice$DZIELNICY, ludnosc$Dzielnica)

# Uaktualnienie kolumny z liczbą ludności
dzielnice$L_MIESZK <- ludnosc$Rok2021


# Kształt i nazwy dzielnic ------------------------------------------------

# Prosta mapa kształtu dzielnic
plot(dzielnice$geometry)

# Mapa z nazwami dzielnic
nazwy_dzielnic <- mapview(dzielnice,
                          alpha.regions = 0.1,
                          color = "red",
                          lwd = 1,
                          label = dzielnice$DZIELNICY,
                          popup = FALSE,
                          legend = FALSE,
                          layer.name = "Podział Gdańska na dzielnice")

# Wyświetlenie mapy
nazwy_dzielnic

# Zapisanie mapy do pliku HTML
mapshot(nazwy_dzielnic, url = "Dokumenty/nazwy_dzielnic.html")


# Liczba mieszkańców ------------------------------------------------------

# Liczba mieszkańców
liczba_mieszk <- mapview(dzielnice,
                         alpha.regions = 1,
                         zcol = "L_MIESZK",
                         col.regions = brewer.pal(9, "Reds"), # "RdPu"
                         label = paste(dzielnice$DZIELNICY, dzielnice$L_MIESZK, sep = ":"),
                         popup = FALSE,
                         layer.name = "Liczba mieszkańców")

# Wyświetlenie mapy
liczba_mieszk

# Zapisanie mapy do pliku HTML
mapshot(liczba_mieszk, url = "Dokumenty/liczba_mieszkancow.html")


# Gęstość zaludnienia -----------------------------------------------------

# Obliczenie gęstości zaludnienia
dzielnice$GEST_ZAL <- round(dzielnice$L_MIESZK / dzielnice$POWIERZCHN)

# Gęstość zaludnienia
gestosc <- mapview(dzielnice,
                   alpha.regions = 1,
                   zcol = "GEST_ZAL",
                   col.regions = brewer.pal(9, "YlOrRd"), # "RdPu"
                   label = paste(dzielnice$DZIELNICY, dzielnice$GEST_ZAL, sep = ":"),
                   popup = FALSE,
                   layer.name = "Gęstość zaludnienia")

# Wyświetlenie mapy
gestosc

# Zapisanie mapy do pliku HTML
mapshot(gestosc, url = "Dokumenty/gestosc_zaludnienia.html")


# Migracja ludności -------------------------------------------------------

# Saldo migracji ludności
dzielnice$SALDO_MIGR <- ludnosc$Rok2021 - ludnosc$Rok2020

# Migracja ludności
migracja <- mapview(dzielnice,
                    alpha.regions = 1,
                    zcol = "SALDO_MIGR",
                    col.regions = brewer.pal(11, "RdYlGn"),
                    label = paste(dzielnice$DZIELNICY, dzielnice$SALDO_MIGR, sep = ":"),
                    popup = FALSE,
                    layer.name = "Migracja ludności")

# Wyświetlenie mapy
migracja

# Zapisanie mapy do pliku HTML
mapshot(migracja, url = "Dokumenty/migracja_ludnosci.html")

# Przycięcie max i min do wartości symetrycznych względem zera
dzielnice$SALDO_MIGR <- ifelse(dzielnice$SALDO_MIGR > abs(min(dzielnice$SALDO_MIGR)),
                               abs(min(dzielnice$SALDO_MIGR)),
                               ifelse(dzielnice$SALDO_MIGR < -max(dzielnice$SALDO_MIGR),
                                      -max(dzielnice$SALDO_MIGR), dzielnice$SALDO_MIGR))

# "Znormalizowanie" wartości migracji ludności
dzielnice$SALDO_MIGR <- round(dzielnice$SALDO_MIGR / max(dzielnice$SALDO_MIGR),
                              digits = 2)

# Zapobiegnięcie zmiany koloru tła mapy
mapviewOptions("basemaps.color.shuffle" = FALSE)

# "Znormalizowana" migracja ludności
migracja_znorm <- mapview(dzielnice,
                          alpha.regions = 1,
                          zcol = "SALDO_MIGR",
                          col.regions = brewer.pal(11, "RdYlGn"),
                          label = paste(dzielnice$DZIELNICY,
                                        dzielnice$SALDO_MIGR, sep = ":"),
                          popup = FALSE,
                          layer.name = "Migracja znorm.")

# Wyświetlenie mapy
migracja_znorm

# Zapisanie mapy do pliku HTML
mapshot(migracja_znorm, url = "Dokumenty/migracja_znorm_ludnosci.html")


# Osoby w wieku emerytalnym -----------------------------------------------

# Wczytanie arkusza "Emeryci" z pliku XLSX zawierającego
# liczbę osób w wieku produkcyjnym i poprodukcyjnym
emeryci <- read_xlsx("Dane/ludnosc_Gdanska.xlsx", sheet = "Emeryci")

## Kobiety emerytki w 2018 roku -------------------------------------------

# Liczba kobiet emerytek w 2018 roku
kobiety.emerytki.18 <- emeryci$Kobiety18_60do64 + emeryci$Kobiety18_pow.64

# Procent kobiet emerytek w 2018 roku, wśród kobiet dorosłych
kobiety.emerytki.18 <- 100 * kobiety.emerytki.18 / (emeryci$Kobiety18_18do59 +
                                                      kobiety.emerytki.18)

# Zaokrąglenie procentów kobiet emerytek w 2018 roku
kobiety.emerytki.18 <- round(kobiety.emerytki.18)

# Zapisanie obliczeń jako nowej kolumny w zbiorze "dzielnice"
dzielnice$kobiety.emerytki.18 <- kobiety.emerytki.18

# Procent kobiet emerytek w 2018 roku
kobiety.18por <- mapview(dzielnice,
                         alpha.regions = 1,
                         zcol = "kobiety.emerytki.18",
                         at = seq(from = 10, to = 50, by = 4),
                         col.regions = brewer.pal(9, "Blues"), # "RdPu"
                         label = paste(dzielnice$DZIELNICY,
                                       dzielnice$kobiety.emerytki.18, sep = ":"),
                         popup = FALSE,
                         layer.name = "% emerytek '18")

# Zapisanie mapy do pliku HTML
mapshot(kobiety.18por, url = "Dokumenty/procent_kobiet_emerytek_2018.html")


## Kobiety emerytki w 2021 roku -------------------------------------------

# Liczba kobiet emerytek w 2021 roku
kobiety.emerytki.21 <- emeryci$Kobiety21_60do64 + emeryci$Kobiety21_pow.64

# Procent kobiet emerytek w 2021 roku, wśród kobiet dorosłych
kobiety.emerytki.21 <- 100 * kobiety.emerytki.21 / (emeryci$Kobiety21_18do59 +
                                                      kobiety.emerytki.21)

# Zaokrąglenie procentów kobiet emerytek w 2021 roku
kobiety.emerytki.21 <- round(kobiety.emerytki.21)

# Zapisanie obliczeń jako nowej kolumny w zbiorze "dzielnice"
dzielnice$kobiety.emerytki.21 <- kobiety.emerytki.21

# Procent kobiet emerytek w 2021 roku
kobiety.21 <- mapview(dzielnice,
                      alpha.regions = 1,
                      zcol = "kobiety.emerytki.21",
                      col.regions = brewer.pal(9, "Blues"), # "RdPu"
                      label = paste(dzielnice$DZIELNICY,
                                    dzielnice$kobiety.emerytki.21, sep = ":"),
                      popup = FALSE,
                      layer.name = "% emerytek'21")

# Zapisanie mapy do pliku HTML
mapshot(kobiety.21, url = "Dokumenty/procent_kobiet_emerytek_2021.html")

# Procent kobiet emerytek w 2021 roku, do porównania
# z 2018 rokiem, ze skalą barw ujednoliconą z kobiety.18por
kobiety.21por <- mapview(dzielnice,
                         alpha.regions = 1,
                         zcol = "kobiety.emerytki.21",
                         at = seq(from = 10, to = 50, by = 4),
                         col.regions = brewer.pal(9, "Blues"), # "RdPu"
                         label = paste(dzielnice$DZIELNICY,
                                       dzielnice$kobiety.emerytki.21, sep = ":"),
                         popup = FALSE,
                         layer.name = "% emerytek'21")

# Wyświetlenie map na sobie, z suwakiem
kobiety.18por | kobiety.21por


## Mężczyźni emeryci w 2020 roku ------------------------------------------

# Liczba mężczyzn w wieku produkcyjnym w 2020 roku
mezczyzni.praca.20 <- emeryci$Mężczyźni20_18do59 + emeryci$Mężczyźni20_60do64

# Procent mężczyzn emerytów w 2020 roku, wśród mężczyzn dorosłych
mezczyzni.emeryci.20 <- 100 * emeryci$Mężczyźni20_pow.64 /
  (emeryci$Mężczyźni20_pow.64 + mezczyzni.praca.20)

# Zaokrąglenie procentów mężczyzn emerytów w 2020 roku
mezczyzni.emeryci.20 <- round(mezczyzni.emeryci.20)

# Zapisanie obliczeń jako nowej kolumny w zbiorze "dzielnice"
dzielnice$mezczyzni.emeryci.20 <- mezczyzni.emeryci.20


## Mężczyźni emeryci w 2021 roku ------------------------------------------

# Liczba mężczyzn w wieku produkcyjnym w 2021 roku
mezczyzni.praca.21 <- emeryci$Mężczyźni21_18do59 + emeryci$Mężczyźni21_60do64

# Procent mężczyzn emerytów w 2021 roku, wśród mężczyzn dorosłych
mezczyzni.emeryci.21 <- 100 * emeryci$Mężczyźni21_pow.64 /
  (emeryci$Mężczyźni21_pow.64 + mezczyzni.praca.21)

# Zaokrąglenie procentów mężczyzn emerytów w 2021 roku
mezczyzni.emeryci.21 <- round(mezczyzni.emeryci.21)

# Zapisanie obliczeń jako nowej kolumny w zbiorze "dzielnice"
dzielnice$mezczyzni.emeryci.21 <- mezczyzni.emeryci.21

# Procent mężczyzn emerytów w 2021 roku
mezczyzni.21 <- mapview(dzielnice,
                        alpha.regions = 1,
                        zcol = "mezczyzni.emeryci.21",
                        col.regions = brewer.pal(9, "Greens"), # "RdPu"
                        label = paste(dzielnice$DZIELNICY,
                                      dzielnice$mezczyzni.emeryci.21, sep = ":"),
                        popup = FALSE,
                        layer.name = "% emerytów'21")

# Zapisanie mapy do pliku HTML
mapshot(mezczyzni.21, url = "Dokumenty/procent_mezczyzn_emerytow_2021.html")

# Wyświetlenie obok siebie map dla kobiet i mężczyzn
sync(kobiety.21, mezczyzni.21)


# Usunięcie zbędnych danych -----------------------------------------------

# Usunięcie podfolderu "dzielnice"
unlink("Dane/dzielnice", recursive = TRUE)

# Usunięcie zawartości folderu "Dokumenty"
unlink("Dokumenty/*", recursive = TRUE)
