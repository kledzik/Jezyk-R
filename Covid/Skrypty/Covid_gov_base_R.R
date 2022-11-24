
# Analiza zachorowań i zgonów spowodowanych koronawirusem SARS-CoV-2


# 1. Załadowanie danych ---------------------------------------------------

# https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2
download.file("https://www.arcgis.com/sharing/rest/content/items/153a138859bb4c418156642b5b74925b/data",
              destfile = "Dane/covid.csv")

filePath <- "Dane/covid.csv"

covid <- read.table(filePath, header = TRUE, sep = ";", fileEncoding = "cp1250")

str(covid)
summary(covid)
head(covid)
tail(covid)

# Usunięcie pierwszego wiersza z danymi dla "Cały kraj"
covid <- covid[-1, ]
head(covid)


# 2. Liczba zachorowań na 10 tys. mieszkańców -----------------------------

# Posortowanie danych malejąco względem kolumny
# "liczba_wszystkich_zakazen_na_10_tys_mieszkancow"
covid <- covid[order(covid$liczba_wszystkich_zakazen_na_10_tys_mieszkancow,
                     decreasing = TRUE), ]

# Liczba słupków na wykresie jest równa liczbie województw
liczba_slupkow <- length(covid$wojewodztwo)

# Skrócenie nazw województw, bo nie mieściły się na wykresie
covid$wojewodztwo <- substr(covid$wojewodztwo, start = 1, stop = 7)
covid$wojewodztwo <- paste(covid$wojewodztwo, ".", sep = "")

barplot(covid$liczba_wszystkich_zakazen_na_10_tys_mieszkancow,
        names.arg = covid$wojewodztwo,
        horiz = TRUE,
        las = 1,
        col = heat.colors(nrow(covid)),
        main = "Liczba zachorowań na 10 tys. mieszkańców",
        cex.names = 0.8)


# 3. Liczba zachorowań i Liczba zgonów ------------------------------------

# Wykresy w postaci jednego wiersza i dwóch kolumn
par(mfrow = c(1, 2), cex = 0.8)

# Posortowanie danych malejąco względem kolumny "liczba_wszystkich_zakazen"
covid <- covid[order(covid$liczba_wszystkich_zakazen, decreasing = TRUE), ]

barplot(covid$liczba_wszystkich_zakazen,
        names.arg = covid$wojewodztwo,
        las = 3,
        col = heat.colors(nrow(covid)),
        main = "Liczba zachorowań",
        cex.names = 0.85,
        font.axis = 2)

# Posortowanie danych malejąco względem kolumny "zgony"
covid <- covid[order(covid$zgony, decreasing = TRUE), ]

barplot(covid$zgony,
        names.arg = covid$wojewodztwo,
        las = 3,
        col = heat.colors(nrow(covid)),
        main = "Liczba zgonów",
        cex.names = 0.85,
        font.axis = 2)

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)

# Usunięcie pliku "covid.csv"
unlink(filePath, recursive = TRUE)
