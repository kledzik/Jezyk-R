
# Analiza zachorowań i zgonów spowodowanych koronawirusem
# SARS-CoV-2, oraz wykonanych szczepień


# 1. Załadowanie danych ---------------------------------------------------

# https://ourworldindata.org/coronavirus/country/poland
download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv",
              destfile = "Dane/owid-covid-data.csv")

filePath <- "Dane/owid-covid-data.csv"

covid <- read.csv(filePath)

str(covid)
summary(covid)
head(covid)
tail(covid)


# 2. Nowe zachorowania ----------------------------------------------------

# Odfiltrowanie danych dla "new_cases"
filter_cases <- (covid$location == "Poland")
cov_cases <- covid[filter_cases, "new_cases"] / 1000

plot(cov_cases,
     type = "o",
     main = "Nowe zachorowania",
     xlab = "Dni",
     ylab = "Liczba przypadków [tys.]",
     col = 8,
     bg = 1,
     pch = 21,
     cex = 0.75)

# Linia dla wartości 0
abline(h = 0, col = 8, lwd = 1)


# 3. Nowe zgony -----------------------------------------------------------

# Odfiltrowanie danych dla "new_deaths"
filter_deaths <- (covid$location == "Poland")
cov_deaths <- covid[filter_deaths, "new_deaths"]

plot(cov_deaths,
     type = "o",
     main = "Nowe zgony",
     xlab = "Dni",
     ylab = "Liczba przypadków",
     col = 8,
     bg = 1,
     pch = 21,
     cex = 0.75)

abline(h = 0, col = 8, lwd = 1)


# 4. Nowe szczepienia -----------------------------------------------------

# Odfiltrowanie danych dla "new_vaccinations"
filter_vaccinations <- (covid$location == "Poland")
cov_vaccinations <- covid[filter_vaccinations, "new_vaccinations"] / 1000

plot(cov_vaccinations, # albo cov_vaccinations[-c(0:300)]
     type = "o",
     main = "Nowe szczepienia",
     xlab = "Dni",
     ylab = "Liczba dawek [tys.]",
     col = 8,
     bg = 1,
     pch = 21,
     cex = 0.75)

abline(h = 0, col = 8, lwd = 1)


# 5. Znormalizowane zmiany ----------------------------------------------------

# Odfiltrowanie wygładzonych danych dla "new_cases_smoothed", "new_deaths_smoothed"
# oraz "new_vaccinations_smoothed", z zamianą wartości NA na 0
covid$new_cases_smoothed <- ifelse(is.na(covid$new_cases_smoothed),
                                    0, covid$new_cases_smoothed)

covid$new_deaths_smoothed <- ifelse(is.na(covid$new_deaths_smoothed),
                                    0, covid$new_deaths_smoothed)

covid$new_vaccinations_smoothed <- ifelse(is.na(covid$new_vaccinations_smoothed),
                                          0, covid$new_vaccinations_smoothed)

filter <- (covid$location == "Poland") 

cov <- covid[filter, c("new_cases_smoothed", "new_deaths_smoothed",
                       "new_vaccinations_smoothed")]

cov <- t(data.matrix(cov))

heatmap(cov,
        Rowv = NA,
        Colv = NA,
        scale = "row",
        cexRow = 1.4,
        cexCol = 1.4,
        labRow = c("zachorowania", "zgony", "szczepienia"),
        labCol = NA,
        main = "Znormalizowane zmiany",
        xlab = paste("od 2020-03-09", Sys.Date(), sep = " do "),
        col = colorRampPalette(c("whitesmoke", "darkred"))(256))

# Usunięcie pliku "owid-covid-data.csv" (bo jest zbyt duży)
unlink(filePath, recursive = TRUE)
