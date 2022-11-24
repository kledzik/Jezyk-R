
# Analiza danych o wolnych miejscach parkingowych w Gdańsku
# https://ckan.multimediagdansk.pl/dataset/otwarte-dane-tristar-w-gdansku
#
# Wyszukiwarka parkingów
# https://en.parkopedia.pl/


# Załadowanie danych ------------------------------------------------------

library(jsonlite)

# Pobranie pliku JSON z nazwami i adresami parkingów
parking_names <- fromJSON("https://ckan.multimediagdansk.pl/dataset/cb1e2708-aec1-4b21-9c8c-db2626ae31a6/resource/d361dff3-202b-402d-92a5-445d8ba6fd7f/download/parking-lots.json")

# Typ tego zbioru to lista
str(parking_names)
summary(parking_names)
head(parking_names)
tail(parking_names)

# Pobranie pliku JSON z ilością wolnych miejsc parkingowych
parking_spots <- fromJSON("https://ckan2.multimediagdansk.pl/parkingLots")

# Typ tego zbioru to lista
str(parking_spots)
summary(parking_spots)
head(parking_spots)
tail(parking_spots)


# Oczyszczenie i modyfikacja danych ---------------------------------------

# Pobranie z listy jej drugiego elementu, czyli ramki danych
# z nazwami i adresami parkingów, bez daty modyfikacji zbioru
parking_names <- parking_names[[2]]

# Zmiana nazw parkingów na krótsze
parking_names$name <- c("Galeria Bałtycka", "CH Manhattan", "PGE Arena",
                        "AmberExpo", "CH Madison", "Akademia Muzyczna",
                        "EC Solidarności", "CH Metropolia", "Forum Gdańsk",
                        "Błękitna", "Kapliczna", "Kaczyńskiego", "Czarny Dwór")

# Pobranie z listy jej drugiego elementu, czyli ramki danych
# z ilością wolnych miejsc parkingowych, bez daty modyfikacji zbioru
parking_spots <- parking_spots[[2]]

# Zmiana nazw kolumn
names(parking_spots) <- c("id", "spots", "updated")

# Złączenie obu ramek danych za pomocą kolumny
# "id". Jest to odpowiednik INNER JOIN w SQL
spots <- merge(x = parking_names, y = parking_spots, by = "id")


# Wykres ilości wolnych miejsc --------------------------------------------

# Wybranie tylko dwóch kolumn z nazwami i ilością
# miejsc. Można to też wykonać za pomocą wyrażenia
# subset(spots, select = c(name, spots))
p_spots <- spots[, c("name", "spots")]

# Zwiększenie szerokości lewego marginesu. Domyślne
# wartości to: par(mar = c(5, 4, 4, 2) + 0.1)
par(mar = c(5, 7, 4, 2) + 0.1)

# Wykres słupkowy ilości wolnych miejsc parkingowych
# z pominięciem danych dla PGE Arena
barplot(p_spots$spots[-10],
        names.arg = p_spots$name[-10],
        horiz = TRUE,
        las = 1,
        col = rainbow(nrow(p_spots) - 1),
        main = "Liczba wolnych miejsc parkingowych",
        xlab = "",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.7)


# Wykres procent wolnych miejsc -------------------------------------------

# Maksymalna ilość miejsc parkingowych
max_spots <- c("Galeria Bałtycka" = 1100,
               "ECS" = 287,
               "Metropolia" = 1230,
               "Forum Gdańsk" = 1030,
               "Błękitna" = 220,
               "Kapliczna" = 60,
               "Manhattan" = 360,
               "Kaczyńskiego" = 150,
               "Czarny Dwór" = 340,
               "PGE Arena" = 1914,
               "Amber Expo" = 510,
               "Madison" = 117,
               "Akademia Muzyczna" = 215)

# Procent wolnych miejsc parkingowych
p_spots$percent_spot <- p_spots$spots * 100 / max_spots

# Wektor parkingów dla których poprawnie działa Tristar
filtered <- c("Galeria Bałtycka",
              "EC Solidarności",
              "CH Metropolia",
              "Forum Gdańsk",
              "Błękitna",
              "Kapliczna",
              "Kaczyńskiego",
              "Czarny Dwór",
              "PGE Arena",
              "CH Madison")

# Odfiltrowanie danych dla tych parkingów
p_spots <- p_spots[p_spots$name %in% filtered, c("name", "percent_spot")]

# Usunięcie ze zbioru danych dla PGE Arena
p_spots <- subset(p_spots, name != "PGE Arena")

# Zwiększenie szerokości lewego marginesu. Domyślne
# wartości to: par(mar = c(5, 4, 4, 2) + 0.1)
par(mar = c(5, 6, 4, 2) + 0.1)

# Wykres słupkowy procent wolnych miejsc parkingowych
barplot(p_spots$percent_spot,
        names.arg = p_spots$name,
        horiz = TRUE,
        las = 1,
        col = rainbow(9),
        main = "Procent wolnych miejsc parkingowych",
        xlab = "%",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.7,
        xlim = c(0, 100))
