
# Analiza zachorowań i zgonów spowodowanych koronawirusem
# SARS-CoV-2, oraz wykonanych szczepień

library(dplyr)
library(ggplot2)
library(readr)

# 1. Załadowanie danych ---------------------------------------------------

# https://ourworldindata.org/coronavirus/country/poland
download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv",
              destfile = "Dane/owid-covid-data.csv")

filePath <- "Dane/owid-covid-data.csv"

covid <- read_csv(filePath)

spec(covid)

glimpse(covid)

head(covid)

covid <- select(covid, location, date, new_cases, new_deaths, new_vaccinations)

head(covid)


# 2. Nowe zachorowania ----------------------------------------------------

# Odfiltrowanie danych dla "new_cases", z usunięciem wierszy z wartościami NA
cov_cases <- filter(covid, (location == "Poland")) #& (!is.na(new_cases)))

ggplot(data = cov_cases, mapping = aes(x = date, y = new_cases / 1000)) +
  geom_line(color = 8) +
  geom_point(color = 1) +
  theme_classic() +
  labs(y = "Liczba przypadków [tys.]", x = "Data", title = "Nowe zachorowania",
     subtitle = "Raport zakażeń koronawirusem (SARS-CoV-2)",
     caption = "Źródło: https://ourworldindata.org/coronavirus/country/poland")


# 3. Nowe zgony -----------------------------------------------------------

# Odfiltrowanie danych dla "new_deaths", z usunięciem wierszy z wartościami NA
cov_deaths <- filter(covid, (location == "Poland")) #& (!is.na(new_deaths)))

ggplot(data = cov_deaths, mapping = aes(x = date, y = new_deaths)) +
  geom_line(color = 8) +
  geom_point(color = 1) +
  theme_classic() +
  labs(y = "Liczba przypadków", x = "Data", title = "Nowe zgony",
       subtitle = "Raport zakażeń koronawirusem (SARS-CoV-2)",
       caption = "Źródło: https://ourworldindata.org/coronavirus/country/poland")


# 4. Nowe szczepienia -----------------------------------------------------

# Odfiltrowanie danych dla "new_vaccinations", z usunięciem wierszy z wartościami NA
cov_vaccinations <- filter(covid, (location == "Poland")) #& (!is.na(new_vaccinations)))

ggplot(data = cov_vaccinations, mapping = aes(x = date, y = new_vaccinations / 1000)) +
  geom_line(color = 8) +
  geom_point(color = 1) +
  theme_classic() +
  labs(y = "Liczba przypadków [tys.]", x = "Data", title = "Nowe szczepienia",
       subtitle = "Raport zakażeń koronawirusem (SARS-CoV-2)",
       caption = "Źródło: https://ourworldindata.org/coronavirus/country/poland")

# Usunięcie pliku "owid-covid-data.csv" (bo jest zbyt duży)
unlink(filePath, recursive = TRUE)
