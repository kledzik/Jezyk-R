
# Analiza danych o utylizacji azbestu w Polsce
# https://www.bazaazbestowa.gov.pl/pl/
# https://www.bazaazbestowa.gov.pl/pl/usuwanie-azbestu/zestawienie-statystyczne
# https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/


library(ggplot2)
library(mapview)
library(RColorBrewer)
library(sf)
library(zip)


# 1. Załadowanie danych ---------------------------------------------------

# Utworzenie podfolderu "wojewodztwa" wewnątrz folderu "Dane"
dir.create("Dane/wojewodztwa")

# Pobranie pliku ZIP z kształtem województw
download.file("https://www.gis-support.pl/downloads/Wojewodztwa.zip",
              destfile = "Dane/wojewodztwa/Wojewodztwa.zip")

# Rozpakowanie pliku ZIP
unzip("Dane/wojewodztwa/Wojewodztwa.zip", exdir = "Dane/wojewodztwa")

# Wczytanie pliku z kształtem województw Polski
# "quiet = TRUE" powoduje niewyświetlenie informacji o zaimportowanych
# danych przestrzennych. Podobnie działa funkcja read_sf()
wojewodztwa <- st_read("Dane/wojewodztwa/Wojew�dztwa.shp", quiet = TRUE)

# Prawidłowe nazwy kolumn dla pliku z danymi o azbeście
cnames <- c("Lp.", "kod", "nazwa", "razem.zinwentaryzowane",
            "osoby.fizyczne.zinwentaryzowane", "osoby.prawne.zinwentaryzowane",
            "razem.unieszkodliwione", "osoby.fizyczne.unieszkodliwione",
            "osoby.prawne.unieszkodliwione", "razem.do.unieszkodliwienia",
            "osoby.fizyczne.do.unieszkodliwienia",
            "osoby.prawne.do.unieszkodliwienia")

# Wczytanie pliku CSV z danymi o azbeście
azbest <- read.table("Dane/export.csv", header = FALSE, sep = ";",
                     col.names = cnames, skip = 1, fileEncoding = "UTF-8")

str(azbest)
summary(azbest)
head(azbest)
tail(azbest)


# 2. Modyfikacja danych ---------------------------------------------------

# Konwersja nazw województw do małych liter
azbest$nazwa <- tolower(azbest$nazwa)

# Usunięcie pustych znaków z liczb zapisanych jako typ character
azbest$razem.zinwentaryzowane <- gsub("[ ]", "", azbest$razem.zinwentaryzowane)
azbest$razem.do.unieszkodliwienia <- gsub("[ ]", "", azbest$razem.do.unieszkodliwienia)
azbest$razem.unieszkodliwione <- gsub("[ ]", "", azbest$razem.unieszkodliwione)

# Konwersja liczb zapisanych jako typ character, do typu numeric
azbest$razem.zinwentaryzowane <- as.numeric(azbest$razem.zinwentaryzowane)
azbest$razem.do.unieszkodliwienia <- as.numeric(azbest$razem.do.unieszkodliwienia)
azbest$razem.unieszkodliwione <- as.numeric(azbest$razem.unieszkodliwione)

# Przeliczenie danych na tysiące ton
azbest$razem.zinwentaryzowane <- azbest$razem.zinwentaryzowane / 1000000
azbest$razem.do.unieszkodliwienia <- azbest$razem.do.unieszkodliwienia / 1000000
azbest$razem.unieszkodliwione <- azbest$razem.unieszkodliwione / 1000000

# Pobranie dwóch kolumn z danych o województwach
wojewodztwa <- wojewodztwa[, c("JPT_NAZWA_", "geometry")]

# Połączenie ramek danych za pomocą kolumn oznaczających
# nazwy. Kolejność złączenia jest istotna, aby zbiór wynikowy
# nadal posiadał klasę "sf", a nie samo "data.frame"
azbest <- merge(x = wojewodztwa, y = azbest,
                by.x = "JPT_NAZWA_", by.y = "nazwa")


# 3. Zinwentaryzowano, razem ----------------------------------------------

# Posortowanie danych malejąco względem kolumny "razem.zinwentaryzowane"
azbest <- azbest[order(azbest$razem.zinwentaryzowane, decreasing = TRUE), ]

# Zwiększenie szerokości lewego marginesu. Domyślne
# wartości to: par(mar = c(5, 4, 4, 2) + 0.1)
par(mar = c(5, 7.5, 4, 2) + 0.1)

# Azbest zinwentaryzowany, razem
barplot(azbest$razem.zinwentaryzowane,
        names.arg = azbest$JPT_NAZWA_,
        horiz = TRUE,
        las = 1,
        col = heat.colors(nrow(azbest)),
        main = "Azbest zinwentaryzowany, razem",
        xlab = "Masa [tysiące ton]",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.8)

# Mapa azbestu zinwentaryzowanego, razem
zinwentaryzowany <- mapview(azbest,
                            alpha.regions = 1,
                            zcol = "razem.zinwentaryzowane",
                            col.regions = brewer.pal(9, "Reds"),
                            label = paste(azbest$JPT_NAZWA_,
                                          round(azbest$razem.zinwentaryzowane),
                                          sep = ":"),
                            popup = FALSE,
                            layer.name = "Zinwentaryz., tys. ton")

# Wyświetlenie mapy
zinwentaryzowany


# 4. Do unieszkodliwienia, razem ------------------------------------------

# Posortowanie danych malejąco względem kolumny "razem.do.unieszkodliwienia"
azbest <- azbest[order(azbest$razem.do.unieszkodliwienia, decreasing = TRUE), ]

# Azbest pozostający w użyciu, do unieszkodliwienia
barplot(azbest$razem.do.unieszkodliwienia,
        names.arg = azbest$JPT_NAZWA_,
        horiz = TRUE,
        las = 1,
        col = heat.colors(nrow(azbest)),
        main = "Azbest pozostający w użyciu, razem",
        xlab = "Masa [tysiące ton]",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.8)

# Mapa azbestu pozostającego w użyciu, do unieszkodliwienia
w_uzyciu <- mapview(azbest,
                    alpha.regions = 1,
                    zcol = "razem.do.unieszkodliwienia",
                    col.regions = brewer.pal(9, "Reds"),
                    label = paste(azbest$JPT_NAZWA_,
                                  round(azbest$razem.do.unieszkodliwienia),
                                  sep = ":"),
                    popup = FALSE,
                    layer.name = "W użyciu, tys. ton")

# Wyświetlenie mapy
w_uzyciu


# 5. Procent unieszkodliwienia, razem -------------------------------------

# Wyznaczenie procentowego unieszkodliwienia odpadów
azbest$procent <- 100 * azbest$razem.unieszkodliwione /
  (azbest$razem.unieszkodliwione + azbest$razem.do.unieszkodliwienia)

# Posortowanie danych malejąco względem kolumny "procent"
azbest <- azbest[order(azbest$procent, decreasing = TRUE), ]

# Unieszkodliwiony azbest w procentach
barplot(azbest$procent,
        names.arg = azbest$JPT_NAZWA_,
        horiz = TRUE,
        las = 1,
        col = cm.colors(nrow(azbest)),
        main = "Azbest unieszkodliwiony, razem",
        xlab = "[%]",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.8)

# Przywrócenie domyślnych wielkości marginesów.
par(mar = c(5, 4, 4, 2) + 0.1)

# Mapa azbestu unieszkodliwionego
zutylizowany <- mapview(azbest,
                        alpha.regions = 1,
                        zcol = "procent",
                        col.regions = brewer.pal(9, "Greens"),
                        label = paste(azbest$JPT_NAZWA_,
                                      round(azbest$procent),
                                      sep = ":"),
                        popup = FALSE,
                        layer.name = "Zutylizowano, %")

# Wyświetlenie mapy
zutylizowany

# Mapa statyczna azbestu unieszkodliwionego
ggplot(azbest, aes(fill = procent)) +
  geom_sf() +
  scale_fill_gradient(low = "white",
                      high = "darkgreen") +
  labs(fill = "Zutylizowano, %",
       title = "Postęp utylizacji azbestu",
       subtitle = "w 2022 roku") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


# 6. Histogramy procentu utylizacji ---------------------------------------


## 6.1. Razem -------------------------------------------------------------

# Przedziały procentowego unieszkodliwienia odpadów
razem <- cut(azbest$procent,
             c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100))

# Ilość jednostek administracyjnych zakwalifikowanych
# do odpowiednich przedziałów procentowych
tbl <- table(razem)
tbl

# Rozkład procentowy postępu utylizacji azbestu
barplot(tbl,
        col = "wheat",
        main = "Rozkład procentowy stopnia utylizacji, razem",
        xlab = "[%]",
        ylab = "Liczba jednostek administracyjnych",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        ylim = c(0, max(tbl)))


## 6.2. Osoby fizyczne ----------------------------------------------------

# Wykresy w postaci jednego wiersza i dwóch kolumn
par(mfrow = c(1, 2), cex = 0.8)

# Usunięcie pustych znaków z liczb zapisanych jako typ character
azbest$osoby.fizyczne.unieszkodliwione <-
  gsub("[ ]", "", azbest$osoby.fizyczne.unieszkodliwione)
azbest$osoby.fizyczne.do.unieszkodliwienia <-
  gsub("[ ]", "", azbest$osoby.fizyczne.do.unieszkodliwienia)

# Konwersja liczb zapisanych jako typ character, do typu numeric
azbest$osoby.fizyczne.unieszkodliwione <-
  as.numeric(azbest$osoby.fizyczne.unieszkodliwione) 
azbest$osoby.fizyczne.do.unieszkodliwienia <-
  as.numeric(azbest$osoby.fizyczne.do.unieszkodliwienia)

# Wyznaczenie procentowego unieszkodliwienia odpadów
azbest$procent <- 100 * azbest$osoby.fizyczne.unieszkodliwione /
  (azbest$osoby.fizyczne.unieszkodliwione + azbest$osoby.fizyczne.do.unieszkodliwienia)

# Przedziały procentowego unieszkodliwienia odpadów
razem <- cut(azbest$procent,
             c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100))

# Ilość jednostek administracyjnych zakwalifikowanych
# do odpowiednich przedziałów procentowych
tbl <- table(razem)
tbl

# Rozkład procentowy postępu utylizacji azbestu
barplot(tbl,
        col = "lightgreen",
        main = "Rozkład procentowy, osoby fizyczne",
        xlab = "[%]",
        ylab = "Liczba jednostek administracyjnych",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        font.axis = 2,
        ylim = c(0, max(tbl)))


## 6.3. Osoby prawne ------------------------------------------------------

# Usunięcie pustych znaków z liczb zapisanych jako typ character
azbest$osoby.prawne.unieszkodliwione <-
  gsub("[ ]", "", azbest$osoby.prawne.unieszkodliwione)
azbest$osoby.prawne.do.unieszkodliwienia <-
  gsub("[ ]", "", azbest$osoby.prawne.do.unieszkodliwienia)

# Konwersja liczb zapisanych jako typ character, do typu numeric
azbest$osoby.prawne.unieszkodliwione <-
  as.numeric(azbest$osoby.prawne.unieszkodliwione) 
azbest$osoby.prawne.do.unieszkodliwienia <-
  as.numeric(azbest$osoby.prawne.do.unieszkodliwienia)

# Wyznaczenie procentowego unieszkodliwienia odpadów
azbest$procent <- 100 * azbest$osoby.prawne.unieszkodliwione /
  (azbest$osoby.prawne.unieszkodliwione + azbest$osoby.prawne.do.unieszkodliwienia)

# Przedziały procentowego unieszkodliwienia odpadów
razem <- cut(azbest$procent,
             c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100))

# Ilość jednostek administracyjnych zakwalifikowanych
# do odpowiednich przedziałów procentowych
tbl <- table(razem)
tbl

# Rozkład procentowy postępu utylizacji azbestu
barplot(tbl,
        col = "lightblue",
        main = "Rozkład procentowy, osoby prawne",
        xlab = "[%]",
        ylab = "Liczba jednostek administracyjnych",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        font.axis = 2,
        ylim = c(0, max(tbl)))

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)


# 7. Usunięcie zbędnych danych --------------------------------------------

# Usunięcie podfolderu "wojewodztwa"
unlink("Dane/wojewodztwa/", recursive = TRUE)
