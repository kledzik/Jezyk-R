
# Analiza zachorowań i zgonów spowodowanych koronawirusem SARS-CoV-2

library(dplyr)
library(ggplot2)
library(gridExtra) # albo ggpubr, cowplot
library(readr)

# 1. Załadowanie danych ---------------------------------------------------

# https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2
download.file("https://www.arcgis.com/sharing/rest/content/items/153a138859bb4c418156642b5b74925b/data",
              destfile = "Dane/covid.csv")

filePath <- "Dane/covid.csv"

covid <- read_delim(filePath, delim = ";", locale = locale(encoding = "cp1250"))

covid %>% glimpse()
covid

# Usunięcie pierwszego wiersza z danymi dla "Cały kraj"
# covid %>% slice(-1)
# filter(covid, row_number() != 1)
covid <- covid %>% filter(row_number() != 1)
covid


# 2. Liczba zachorowań na 10 tys. mieszkańców -----------------------------

# Posortowanie danych malejąco względem kolumny
# "liczba_wszystkich_zakazen_na_10_tys_mieszkancow"
covid <- covid %>% arrange(desc(liczba_wszystkich_zakazen_na_10_tys_mieszkancow))

# Słupki na wykresie są posortowane wg liczby a nie województwa (funkcja reorder)
# oraz jest zamieniona miejscami oś pozioma z pionową (funkcja coord_flip)
ggplot(data = covid) +
  geom_bar(mapping = aes(x = reorder(wojewodztwo, -liczba_wszystkich_zakazen_na_10_tys_mieszkancow),
                         y = liczba_wszystkich_zakazen_na_10_tys_mieszkancow),
           stat = "identity", colour = "black",
           fill = heat.colors(nrow(covid))) +
  coord_flip() +
  labs(y = "", x = "", title = "Liczba zachorowań na 10 tys. mieszkańców")


# 3. Liczba zachorowań i Liczba zgonów ------------------------------------

# Posortowanie danych malejąco względem kolumny "liczba_wszystkich_zakazen"
covid <- covid %>% arrange(desc(liczba_wszystkich_zakazen))

disease <- ggplot(data = covid) +
  geom_bar(mapping = aes(x = reorder(wojewodztwo, -liczba_wszystkich_zakazen),
                         y = liczba_wszystkich_zakazen), stat = "identity",
           colour = "black", fill = heat.colors(nrow(covid))) +
  coord_flip() +
  theme_minimal() +
  labs(y = "", x = "", title = "Liczba zachorowań")

# Posortowanie danych malejąco względem kolumny "zgony"
covid <- covid %>% arrange(desc(zgony))

death <- ggplot(data = covid) +
  geom_bar(mapping = aes(x = reorder(wojewodztwo, -zgony), y = zgony),
           stat = "identity", colour = "black",
           fill = heat.colors(nrow(covid))) +
  coord_flip() +
  theme_minimal() +
  labs(y = "", x = "", title = "Liczba zgonów")

# Wykresy w postaci jednego wiersza i dwóch kolumn
grid.arrange(disease, death, ncol = 2, nrow = 1)

# Usunięcie pliku "covid.csv"
unlink(filePath, recursive = TRUE)
