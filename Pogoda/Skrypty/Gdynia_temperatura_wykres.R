
# Wykres zmian temperatury powietrza w Gdyni
#
# https://otwartedane.gdynia.pl/pl/dataset/lista-aktualnych-danych-pogodowych
# https://otwartedane.gdynia.pl/pl/dataset/lista-stacji-pogodowych


library(ggplot2)
library(jsonlite)

# Utworzenie podfolderu "temperatura_czas"
# wewnątrz folderu "Dane"
sciezka <- "Dane/temperatura_czas"

if (!dir.exists(sciezka)){
  dir.create(sciezka)
  } else {
    cat("Folder już istnieje!")
  }

# Zainicjowanie ramki danych
temperatura_czas <- data.frame()

# Zainicjowanie licznika pętli
i <- 1

# Nieskończona pętla pobierająca i zapisująca lokalnie dane
while(TRUE) {
  
  # Pobranie pliku JSON z danymi meteorologicznymi
  meteo <- fromJSON("http://api.zdiz.gdynia.pl/ri/rest/weather_stations_data")

  # Jeżeli pobrane dane nie są puste, to ...
  if (!is.null(nrow(meteo))) {
    
    # Pobranie numeru id stacji, temperatury i czasu pomiaru
    tmp <- meteo[, c("weatherStationId", "airTemperature", "measureTime")]
    
    # Zapisanie aktualnego stanu licznika pętli
    tmp$licznik <- i
    
    # Dołączenie nowo pobranych wierszy do poprzednich wierszy
    temperatura_czas <- rbind(temperatura_czas, tmp)
    
    # Zwiększenie stanu licznika
    i <- i + 1
    
    # Zapisanie zbiorczych danych do pliku RDS
    saveRDS(temperatura_czas, file = paste(sciezka, "temperatura_czas.RDS", sep = "/"))
    
    # Wstrzymanie pętli na 600 sekund, ponieważ
    # pomiary temperatury są rejestrowane co 10 min.
    Sys.sleep(600)
  }
}

# Zapisanie kopii bezpieczeństwa
saveRDS(temperatura_czas, file = "Dane/Archiwum/temperatura_czas.RDS")

# Usunięcie kolumny z licznikiem
temperatura_czas <- temperatura_czas[, -c(4)]

# Usunięcie zduplikowanych wierszy
temperatura_czas <- unique(temperatura_czas)

# Pobranie czasu pomiaru z kolumny z datą i czasem
temperatura_czas$measureTime <- format(as.POSIXct(temperatura_czas$measureTime),
                                       format = "%H:%M")

# Dane temperaturowe dla ul. Kwiatkowskiego w lesie
las <- temperatura_czas[temperatura_czas$weatherStationId == 7, ]

# Wykres zmian temperatury powietrza dla ul. Kwiatkowskiego w lesie
ggplot(data = las, aes(x = measureTime, y = airTemperature, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(n.breaks = 10) +
  labs(y = "Temperatura [°C]", x = "Godzina pomiaru",
       title = "Zmiany temperatury dla stacji pomiarowej na ul. Kwiatkowskiego w lesie") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6.5))

# Zamiana typu danych w kolumnie weatherStationId, na string
temperatura_czas$weatherStationId <- as.character(temperatura_czas$weatherStationId)

# Zmiana id stacji pomiarowej, na nazwy słowne
temperatura_czas$weatherStationId <-
  ifelse(temperatura_czas$weatherStationId == "5", "Karwiny",
         ifelse(temperatura_czas$weatherStationId == "6", "Chylonia",
                ifelse(temperatura_czas$weatherStationId == "7", "las",
                       ifelse(temperatura_czas$weatherStationId == "8", "Leszczynki",
                              ifelse(temperatura_czas$weatherStationId == "9", "centrum",
                                     "")))))

# Zbiorczy Wykres zmian temperatury powietrza
# dla wszystkich stacji pomiarowych w Gdyni
ggplot(data = temperatura_czas, aes(x = measureTime, y = airTemperature,
                                    color = weatherStationId,
                                    group = weatherStationId,
                                    shape = weatherStationId)) +
  geom_point(size = 2.5) +
  geom_line(size = 0.8) +
  scale_y_continuous(n.breaks = 10) +
  labs(y = "Temperatura [°C]", x = "Godzina pomiaru",
       title = "Zmiany temperatury w różnych rejonach Gdyni") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6.5)) +
  theme(legend.title = element_blank())

# Usunięcie wiersza z danymi z ulicy Kwiatkowskiego w
# lesie, aby ilość wykresów ładniej układała się na ekranie
temperatura_czas <- temperatura_czas[temperatura_czas$weatherStationId != "las", ]

# Wykresy zmian temperatury powietrza z rozbiciem
# na poszczególne stacje pomiarowych w Gdyni
ggplot(data = temperatura_czas, aes(x = measureTime, y = airTemperature,
                                    color = weatherStationId,
                                    group = weatherStationId)) +
  geom_point(size = 2) +
  geom_line(size = 0.8) +
  scale_y_continuous(n.breaks = 5) +
  labs(y = "Temperatura [°C]", x = "Godzina pomiaru") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  facet_wrap( ~ weatherStationId)

# Usunięcie podfolderu "temperatura_czas"
unlink(sciezka, recursive = TRUE)
