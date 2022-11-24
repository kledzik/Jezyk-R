
# Analiza ruchu pasażerskiego na lotnisku San Francisco
# https://data.sfgov.org/Transportation/Air-Traffic-Passenger-Statistics/rkru-6vcg

library(lubridate)

# 1. Załadowanie danych ---------------------------------------------------

# UWAGA! W ścieżce do pobierania pliku ustawić opcję "bom=false"
# download.file("https://data.sfgov.org/api/views/rkru-6vcg/rows.csv?accessType
#=DOWNLOAD&bom=false&format=true&delimiter=%3B",
#              destfile = "Dane/air_traffic_passenger_statistics.csv")

download.file("https://data.sfgov.org/api/views/rkru-6vcg/rows.csv?accessType=DOWNLOAD",
              destfile = "Dane/air_traffic_passenger_statistics.csv")

filePath <- "Dane/air_traffic_passenger_statistics.csv"

air <- read.csv(filePath)

str(air)
summary(air)
summary(air$Activity.Period)
summary(air$Passenger.Count)
head(air)
tail(air)


# 2. Liczba pasażerów wszystkich linii w podziale na lata -----------------

# Wyodrębnienie roku z oryginalnej kolumny z datą
year <- substr(air$Activity.Period, start = 1, stop = 4)

# Wyodrębnienie miesięcy z oryginalnej kolumny z datą
month <- substr(air$Activity.Period, start = 5, stop = 6)

# Utworzenie dodatkowej kolumny z datami w postaci string
air$date_string <- paste(year, month, "01", sep = "-")
str(air$date_string)
head(air)

# Utworzenie dodatkowej kolumny z pełnymi datami w formacie ISO
air$date_iso <- ISOdate(year, month, "01")
str(air$date_iso)
head(air)

# Liczba pasażerów w podziale na lata, w milionach
group_by_year <- tapply(air$Passenger.Count,
                        list(year(air$date_string)), FUN = sum) / 1000000

plot(x = names(group_by_year),
     y = group_by_year,
     type = "o",
     main = "Liczba pasażerów w podziale na lata",
     xlab = "Rok",
     ylab = "[mln.]",
     col = 4,
     bg = 4,
     pch = 21,
     cex = 1.4)

# Linie lotnicze obsługujące loty na lotnisku San Francisco
unique(air$Operating.Airline)


# 3. Loty British Airways i Lufthansa na wszystkich trasach ---------------

# Zmienna oznaczająca rok poddany analizie
year_an <- "2022"

# Odfiltrowanie danych dla British Airways
filter_british <- air[air$Operating.Airline == "British Airways", ]

# Sumaryczna liczba pasażerów w pogrupowanych datach
group_by_year_month_british <- tapply(filter_british$Passenger.Count,
                                      list(year(filter_british$date_string),
                                           month(filter_british$date_string)),
                                      FUN = sum) / 1000

# Odfiltrowanie danych dla Lufthansa German Airlines
filter_lufthansa <- air[air$Operating.Airline == "Lufthansa German Airlines", ]

# Sumaryczna liczba pasażerów w pogrupowanych datach
group_by_year_month_lufthansa <- tapply(filter_lufthansa$Passenger.Count,
                                        list(year(filter_lufthansa$date_string),
                                             month(filter_lufthansa$date_string)),
                                        FUN = sum) / 1000

# Wartość minimalna na osi Y, pomniejszona o 10%
min_y <- min(c(group_by_year_month_british[year_an, ],
               group_by_year_month_british[year_an, ]), na.rm = TRUE)
min_y <- min_y - min_y * 0.1

# Wartość maksymalna na osi Y, powiększona o 10%
max_y <- max(c(group_by_year_month_british[year_an, ],
              group_by_year_month_british[year_an, ]), na.rm = TRUE)
max_y <- max_y + max_y * 0.1

plot(x = names(group_by_year_month_british[year_an, ]),
     y = group_by_year_month_british[year_an, ],
     ylim = c(min_y, max_y),
     type = "o",
     main = paste("Liczba pasażerów w", year_an, "roku"),
     xlab = "Miesiąc",
     ylab = "[tys.]",
     col = 3,
     bg = 3,
     pch = 21,
     cex = 1.4,
     lty = 1)

points(x = names(group_by_year_month_lufthansa[year_an, ]),
       y = group_by_year_month_lufthansa[year_an, ],
       col = 2,
       bg = 2,
       pch = 22,
       cex = 1.4)

lines(x = names(group_by_year_month_lufthansa[year_an, ]),
      y = group_by_year_month_lufthansa[year_an, ],
      col = 2,
      lty = 2)

legend("topright",
       leg = c("British Airways", "Lufthansa German Airlines"),
       col = c(3, 2),
       pch = c(19, 15),
       cex = 0.5,
       pt.cex = 1.3,
       lty = c(1, 2))


# 4. Loty do Europy, wszystkie lata i wszyscy przewoźnicy -----------------

# Czy są wartości NA w kodzie IATA
air[is.na(air$Operating.Airline.IATA.Code), "Operating.Airline"]

# Które z linii lotniczych nie mają kodu IATA
unique(air[air$Operating.Airline.IATA.Code == "", "Operating.Airline"])

# Pobranie danych dla lotów do Europy
airEU <- air[air$GEO.Region == "Europe" & air$Activity.Type.Code == "Enplaned", ]

# Różne metody sprawdzenia, czy kolumna "Operating..."
# oraz "Published" zawierają takie same wartości
all(airEU$Operating.Airline.IATA.Code == airEU$Published.Airline.IATA.Code)
setequal(airEU$Operating.Airline.IATA.Code, airEU$Published.Airline.IATA.Code)
min(airEU$Operating.Airline.IATA.Code == airEU$Published.Airline.IATA.Code)
identical(airEU$Operating.Airline.IATA.Code, airEU$Published.Airline.IATA.Code)

# Udział linii lotniczych do Europy w ruchu na lotnisku San Francisco
table(airEU$Operating.Airline.IATA.Code)
plot(table(airEU$Operating.Airline.IATA.Code),
     main = "Udział linii lotniczych do Europy, sort. wg IATA",
     xlab = "Kod IATA",
     ylab = "Zliczenia, jdn. umowne",
     col = "blue")

# J.w., posortowane według ilości, malejąco
sort(table(airEU$Operating.Airline.IATA.Code), decreasing = TRUE)
plot(sort(table(airEU$Operating.Airline.IATA.Code), decreasing = TRUE),
     main = "Udział linii lotniczych do Europy, sort. wg zliczeń",
     xlab = "Kod IATA",
     ylab = "Zliczenia, jdn. umowne",
     col = "blue")

# Nazwy linii lotniczych obsługujących loty do Europy, posortowane wg IATA
eu <- unique(airEU[, c("Operating.Airline.IATA.Code", "Operating.Airline")])
eu <- eu[order(eu$Operating.Airline.IATA.Code), ]
row.names(eu) <- seq(from = 1, to = nrow(eu), by = 1)
eu

# Które z kodów IATA powtarzają się, i dlatego są razem na wykresie
eu[duplicated(eu$Operating.Airline.IATA.Code), ]

# Ile jest takich powtórzonych kodów IATA
sum(table(eu$Operating.Airline.IATA.Code) - 1)

# Liczba pasażerów lecących do Europy w podziale na linie lotnicze
aggr <- aggregate(airEU$Passenger.Count, by = list(airEU$Operating.Airline), FUN = sum)
aggr$proc <- round(100 * aggr$x / sum(aggr$x), digits = 2)
aggr <- aggr[order(aggr$x, decreasing = TRUE), ]
colnames(aggr) <- c("Nazwa linii lotn.", "Liczba pasażerów w mln.", "Udział procentowy")
row.names(aggr) <- seq(from = 1, to = nrow(aggr), by = 1)
aggr

# Które z nazw linii powtarzają się, i dlatego są dodatkowo zagregowane
eu[duplicated(eu$Operating.Airline), ]

# Ile jest takich powtórzonych nazw
sum(table(eu$Operating.Airline) - 1)

# Liczba pasażerów lecących do Europy w podziale na linie lotnicze
tap <- tapply(airEU$Passenger.Count, list(airEU$Operating.Airline),
              FUN = sum) / 1000000
tap <- sort(tap, decreasing = TRUE)
tap

# Zwiększenie szerokości lewego marginesu. Domyślne
# wartości to: par(mar = c(5, 4, 4, 2) + 0.1)
par(mar = c(5, 8, 4, 2) + 0.1)

barplot(tap,
        names.arg = names(tap),
        col = heat.colors(nrow(tap)),
        horiz = TRUE,
        las = 1,
        main = "Liczba pasażerów lecących do Europy",
        xlab = "[mln.]",
        xlim = c(0, max(tap)),
        cex.names = 0.6,
        axisnames = TRUE)

# Usunięcie pliku "air_traffic_passenger_statistics.csv"
unlink(filePath, recursive = TRUE)
