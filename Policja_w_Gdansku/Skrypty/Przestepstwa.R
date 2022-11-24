
# Analiza danych policyjnych o przestępstwach w Gdańsku
# https://gdansk.policja.gov.pl/
# https://gdansk.policja.gov.pl/pm1/statystyki/statystyka-przestepstw


library(here)
library(readxl)

# Załadowanie danych ------------------------------------------------------

# Wyświetlenie folderu roboczego
here()

# Wczytanie (trzeciego) arkusza "Przestępstwa"
# z pliku XLSX (Excel), zawierającego dane o przestępstwach
przestepstwa <- read_xlsx(here("Dane", "statystyki_gdanskiej_policji.xlsx"),
                          sheet = "Przestępstwa")

# Konwersja tibble do data.frame
przestepstwa <- data.frame(przestepstwa)

str(przestepstwa)
summary(przestepstwa)
head(przestepstwa)
tail(przestepstwa)

# Analizowane lata
unique(przestepstwa$Rok)

# Liczba analizowanych lat
length(unique(przestepstwa$Rok))


# Mapa cieplna zmian na przestrzeni lat -----------------------------------

# Wartości z kolumny "Rok" jako nazwy wierszy
row.names(przestepstwa) <- przestepstwa$Rok

# Usunięcie kolumny "Id" oraz "Rok"
przestepstwa <- przestepstwa[, -c(1, 2)]

# Zamiana liter w nazwach kolumn na małe
names(przestepstwa) <- tolower(names(przestepstwa))

# Zamiana kropek w nazwach kolumn na spację
names(przestepstwa) <- gsub("[.]", " ", names(przestepstwa))

# Przekształcenie tej ramki danych
# do macierzy oraz jej transpozycja
mapa <- t(data.matrix(przestepstwa))

# Mapa cieplna
heatmap(mapa,
        Rowv = NA,
        Colv = NA,
        scale = "row",
        cexRow = 0.95,
        cexCol = 1,
        col = colorRampPalette(c("white", "darkred"))(256))
