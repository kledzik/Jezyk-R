
# Analiza danych o znanych osobach
# https://www.kaggle.com/datasets/imoore/age-dataset


# Załadowanie danych ------------------------------------------------------

filePath <- "Dane/ageDataset.csv"

AgeDataset <- read.csv(filePath)

# Struktura zbioru
str(AgeDataset)

# Prosta statystyka
summary(AgeDataset)

# Początkowe wiersze
head(AgeDataset)

# Końcowe wiersze
tail(AgeDataset)


# Oczyszczenie danych -----------------------------------------------------

# Ilość powtórzonych (tzn. nadmiarowych) wierszy
cat("Ilość zdublowanych wierszy:", sum(duplicated(AgeDataset)))

# Powtórzone (tzn. nadmiarowe) wiersze
AgeDataset[duplicated(AgeDataset), ]

# Usunięcie powtórzonych (tzn. nadmiarowych) wierszy
AgeDataset <- AgeDataset[!duplicated(AgeDataset), ]

# Ilość wartości NA w danych typu numerycznego
cat("Ilość wartości NA:", sum(is.na(AgeDataset)))

# Wiersze zawierające wartości NA w danych typu numerycznego
AgeDataset[unique(which(is.na(AgeDataset), arr.ind = TRUE)[, 1]), ]

# Usunięcie wierszy zawierających wartości NA
# w danych typu numerycznego
AgeDataset <- na.omit(AgeDataset)

# Ilość wierszy zawierających zerową długość życia
cat("Ilość wierszy z długością życia = 0:",
    nrow(AgeDataset[AgeDataset$Age.of.death == 0, 0]))

# Usunięcie wierszy z zerową długością życia
AgeDataset <- AgeDataset[AgeDataset$Age.of.death != 0, ]

# Zamiana pustych wartości tekstowych na "unknown"
AgeDataset[AgeDataset == ''] <- "unknown"

# Usunięcie kolumny "Id"
AgeDataset <- AgeDataset[-1]


# Ilość brakujących danych w kolumnach tekstowych -------------------------

# Ilość wierszy głównego zbioru
max.rows <- nrow(AgeDataset)

# Zliczenie wartości "unknown" w kolumnach tekstowych, w procentach
gender <- 100 * sum(AgeDataset$Gender == 'unknown') / max.rows
country <- 100 * sum(AgeDataset$Country == 'unknown') / max.rows
occupation <- 100 * sum(AgeDataset$Occupation == 'unknown') / max.rows
manner <- 100 * sum(AgeDataset$Manner.of.death == 'unknown') / max.rows

# Posortowany wektor % wartości "unknown"
empty.data <- sort(c("Gender" = gender, "Country" = country,
                     "Occupation" = occupation, "Manner" = manner))

# Zaokrąglone % wartości "unknown"
round(empty.data, digits = 2)

# Wykres słupkowy % wartości "unknown"
barplot(empty.data,
        las = 1,
        col = seq(from = 1, to = length(empty.data), by = 1),
        main = "Ilość danych nieznanych",
        ylab = "%",
        cex.names = 0.9,
        ylim = c(0, 100))

# Linia ilości wierszy głównego zbioru, w tysiącach
abline(h = 100, col = "gray", lty = "longdash", lwd = 3)


# Liczebność płci ---------------------------------------------------------

# Unikalne oznaczenia płci
unique(AgeDataset$Gender)

# Liczebność w procentach poszczeglnych płci
gender.all <- 100 * table(AgeDataset$Gender) / max.rows

# Posortowana liczebność płci, w %
gender.all <- round(sort(gender.all), digits = 2)
gender.all

# Liczebność trzech najliczniejszych płci, w %
gender.all <- tail(gender.all, n = 3)
gender.all

# Wykres słupkowy trzech najliczniejszych płci, w %
barplot(gender.all,
        las = 1,
        col = c(2, "darkgray", 3),
        main = "Najliczniejsze płcie",
        ylab = "%",
        cex.names = 0.9)


# Liczebność zawodów, razem -----------------------------------------------

# Unikalne zawody, tylko część
head(unique(AgeDataset$Occupation), n = 500)

# Liczebność w procentach poszczególnych zawodów
occupation.all <- 100 * table(AgeDataset$Occupation) / max.rows

# Posortowana liczebność zawodów, w %
occupation.all <- round(sort(occupation.all), digits = 2)

# Liczebność 20 najliczniejszych zawodów, w %
occupation.all <- tail(occupation.all, n = 20)
occupation.all

# Zwiększenie szerokości lewego marginesu. Domyślne
# wartości to: par(mar = c(5, 4, 4, 2) + 0.1)
par(mar = c(5, 6, 4, 2) + 0.1)

# Wykres słupkowy 20 najliczniejszych zawodów, w %
barplot(occupation.all,
        horiz = TRUE,
        las = 1,
        col = rev(heat.colors(length(occupation.all))),
        main = "Najpopularniejsze zawody, razem",
        xlab = "%",
        cex.names = 0.7)

# Domyślne wielkości marginesów
par(mar = c(5, 4, 4, 2) + 0.1)


# Liczebność zawodów, mężczyźni -------------------------------------------

# Dane o wszystkich zawodach wykonywanych przez mężczyzn
male.occupation <- AgeDataset[AgeDataset$Gender == "Male", "Occupation"]

# Ilość wierszy zbioru z zawodami męskimi
max.rows <- NROW(male.occupation)

# Liczebność w procentach poszczególnych zawodów
male.occupation <- 100 * table(male.occupation) / max.rows

# Posortowana liczebność zawodów, w %
male.occupation <- round(sort(male.occupation), digits = 2)

# Liczebność 10 najliczniejszych zawodów, w %
male.occupation <- tail(male.occupation, n = 10)
male.occupation

# Wykres słupkowy 10 najliczniejszych zawodów, w %
barplot(male.occupation,
        horiz = FALSE,
        las = 2,
        col = 3,
        main = "Najpopularniejsze zawody męskie",
        ylab = "%",
        cex.names = 0.6)


# Liczebność zawodów, mężczyźni i kobiety ---------------------------------

# Dane o zawodach wykonywanych przez mężczyzn
male.occupation <- AgeDataset[AgeDataset$Gender == "Male", "Occupation"]

# Ilość wierszy zbioru z zawodami męskimi
max.rows <- length(male.occupation)

# Liczebność w procentach poszczególnych zawodów męskich
male.occupation <- 100 * table(male.occupation) / max.rows

# Posortowana ilościowo liczebność zawodów męskich, w %
male.occupation <- round(sort(male.occupation), digits = 2)

# Liczebność 10 najliczniejszych zawodów męskich, w %
male.occupation <- tail(male.occupation, n = 10)
male.occupation

# Nazwy najliczniejszych zawodów męskich
male.occupation.names <- names(male.occupation)

# Posortowana alfabetycznie liczebność zawodów męskich, w %
male.occupation <- male.occupation[order(male.occupation.names)]
male.occupation

# Dane o wszystkich zawodach wykonywanych przez kobiety
female.occupation <- AgeDataset[AgeDataset$Gender == "Female", "Occupation"]

# Dane o tych zawodach kobiecych, które pokrywają się z męskimi
female.occupation <- female.occupation[female.occupation %in% male.occupation.names]

# Liczebność w procentach poszczeglnych zawodów kobiecych z listy męskiej
female.occupation <- 100 * table(female.occupation) / max.rows

# Zaokrąglenie wartości
female.occupation <- round(female.occupation, digits = 2)
female.occupation

# Zestawienie 10 zawodów męskich i na ich tle zawodów kobiecych
male.female <- rbind(male.occupation, female.occupation)

# Kolory dla słupków na wykresie
bar.colors <- rownames(male.female)
bar.colors <- as.integer(factor(bar.colors)) + 1

# Wykres słupkowy 10 zawodów męskich i na ich tle zawodów kobiecych
barplot(male.female,
        las = 2,
        col = bar.colors,
        main = "Porównanie 10 zawodów męskich z kobiecymi",
        ylab = "%",
        cex.names = 0.6,
        legend = TRUE)

# Wykres słupkowy 10 zawodów męskich i na ich tle zawodów kobiecych
barplot(male.female,
        beside = TRUE,
        las = 2,
        col = bar.colors,
        main = "Porównanie 10 zawodów męskich z kobiecymi",
        ylab = "%",
        cex.names = 0.6,
        legend = FALSE)

legend("topright", rownames(male.female), col = bar.colors,
       pch = c(15, 15), bty = "n")


# Liczebność zawodów, kobiety ---------------------------------------------

# Dane o wszystkich zawodach wykonywanych przez kobiety
female.occupation <- AgeDataset[AgeDataset$Gender == "Female", "Occupation"]

# Ilość wierszy zbioru z zawodami kobiecymi
max.rows <- length(female.occupation)

# Liczebność w procentach poszczególnych zawodów
female.occupation <- 100 * table(female.occupation) / max.rows

# Posortowana liczebność zawodów, w %
female.occupation <- round(sort(female.occupation), digits = 2)

# Liczebność 10 najliczniejszych zawodów, w %
female.occupation <- tail(female.occupation, n = 10)
female.occupation

# Wykres słupkowy 10 najliczniejszych zawodów, w %
barplot(female.occupation,
        horiz = FALSE,
        las = 2,
        col = 2,
        main = "Najpopularniejsze zawody kobiece",
        ylab = "%",
        cex.names = 0.6)


# Długość życia mężczyzn i kobiet -----------------------------------------

# Odfiltrowanie długości życia dla mężczyzn i kobiet
filter.life <- AgeDataset[(AgeDataset$Gender == "Male") |
                            (AgeDataset$Gender == "Female"),
                          c("Gender", "Age.of.death")]

# Utworzenie listy długości życia wypełnionej
# danymi pobranymi dla obu płci
filter.life <- tapply(filter.life$Age.of.death, filter.life$Gender, FUN = cbind)

# Wykres pudełkowy poziomy. Szerokość pudełka
# to pierwiastek z ilości danych
boxplot(filter.life,
        main = "Długość życia",
        ylab = "Lata", 
        col = c(2, 3))

# Wykresy w postaci dwóch wierszy i jednej kolumny
par(mfrow = c(2, 1), cex = 0.7)

# Długość życia mężczyzn
life.length <- AgeDataset[AgeDataset$Gender == "Male", "Age.of.death"]

# Parametry boxplot dla mężczyzn
outliers <- boxplot.stats(life.length)
outliers$stats

# Mediana wieku mężczyzn
cat("Mediana wieku mężczyzn:", outliers$stats[3])

# Dolna granica wieku dla "outliers" - mężczyzn
# odstających od ogółu zbyt podeszłym wiekiem
max(outliers$stats)

# Outliers - mężczyźni
outliers <- AgeDataset[(AgeDataset$Gender == "Male") &
                         (AgeDataset$Age.of.death >= max(outliers$stats)), ]

# Zresetowanie numerowania wierszy w Outliers
row.names(outliers) <- NULL

# Outliers - mężczyźni
outliers

# Histogram długości życia mężczyzn
hist(life.length,
     breaks = 25,
     xlab = "Lata",
     col = 3,
     xlim = c(10, 100),
     main = "Długość życia mężczyzn",
     cex.axis = 1.2)

# Długość życia kobiet
life.length <- AgeDataset[AgeDataset$Gender == "Female", "Age.of.death"]

# Parametry boxplot dla kobiet
outliers <- boxplot.stats(life.length)
outliers$stats

# Mediana wieku kobiet
cat("Mediana wieku kobiet:", outliers$stats[3])

# Dolna granica wieku dla "outliers" - kobiet
# odstających od ogółu zbyt podeszłym wiekiem
max(outliers$stats)

# Outliers - kobiety
outliers <- AgeDataset[(AgeDataset$Gender == "Female") &
                         (AgeDataset$Age.of.death >= max(outliers$stats)), ]

# Zresetowanie numerowania wierszy w Outliers
row.names(outliers) <- NULL

# Outliers - kobiety
outliers

# Histogram długości życia kobiet
hist(life.length,
     breaks = 25,
     xlab = "Lata",
     col = 2,
     xlim = c(10, 100),
     main = "Długość życia kobiet",
     cex.axis = 1.2)

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)


# Przyczyny śmierci, razem ------------------------------------------------

# Ilość wierszy głównego zbioru
max.rows <- nrow(AgeDataset)

# Unikalne przyczyny śmierci
unique(AgeDataset$Manner.of.death)

# Przyczyny śmierci za wyjątkiem "unknown"
manner <- AgeDataset[AgeDataset$Manner.of.death != "unknown", "Manner.of.death"]

# Liczebność w procentach poszczególnych przyczyn
manner <- 100 * table(manner) / max.rows

# Posortowane przyczyny śmierci, w %
manner <- round(sort(manner), digits = 2)

# Liczebność 5 najliczniejszych przyczyn, w %
manner <- tail(manner, n = 5)
manner

# Wykres słupkowy 5 najliczniejszych przyczyn, w %
barplot(manner,
        horiz = FALSE,
        las = 1,
        col = seq(from = 1, to = length(manner), by = 1),
        main = "Najczęstsze przyczyny zgonów, razem",
        ylab = "%",
        cex.names = 0.8)


# Długość życia artystów i artystek popełniających samobójstwa ------------

# Wykresy w postaci dwóch wierszy i jednej kolumny
par(mfrow = c(2, 1), cex = 0.7)

# Odfiltrowanie danych dla mężczyzn artystów
filter <- (AgeDataset$Gender == "Male") & (AgeDataset$Occupation == 'Artist') &
  (AgeDataset$Manner.of.death == 'suicide')

# Długość życia mężczyzn artystów
life.length <- AgeDataset[filter, "Age.of.death"]

# Histogram długości życia mężczyzn artystów
hist(life.length,
     xlab = "Lata",
     xlim = c(10, 110),
     breaks = 10,
     col = 3,
     cex.axis = 1.2,
     main = "Długość życia mężczyzn artystów samobójców")

# Odfiltrowanie danych dla kobiet artystek
filter <- (AgeDataset$Gender == "Female") & (AgeDataset$Occupation == 'Artist') &
  (AgeDataset$Manner.of.death == 'suicide')

# Długość życia kobiet artystek
life.length <- AgeDataset[filter, "Age.of.death"]

# Histogram długości życia kobiet artystek
hist(life.length,
     xlab = "Lata",
     xlim = c(10, 110),
     breaks = 10,
     col = 2,
     cex.axis = 1.2,
     main = "Długość życia kobiet artystów samobójców")

# Przywrócenie domyślnych ustawień wyświetlania wykresów
par(mfrow = c(1, 1), cex = 1)


# Długości życia dla osób urodzonych po 1000 roku -------------------------

# Dane dla osób urodzonych po 1000 roku
life.length <- AgeDataset[AgeDataset$Birth.year > 1000,
                          c("Death.year", "Age.of.death")]

# Wyliczenie wartości średnich Age.of.death dla każdego roku
life.length <- aggregate(life.length$Age.of.death,
                         by = list(life.length$Death.year), FUN = mean)

# Szybsza metoda obliczenia tych wartości
# life.length <- tapply(life.length$Age.of.death,
#                       life.length$Death.year, FUN = mean)

# Zmiana nazw kolumn
names(life.length) <- c("Rok", "Wiek osób")

# Wykres wartości średnich Age.of.death dla każdego roku
plot(life.length,
     type = "p",
     pch = 19,
     cex = 1.2,
     col = rgb(0, 0, 0, 0.1),
     main = "Długość życia, razem",
     xlab = "Rok",
     ylab = "Lata")


# Liczebność obywatelstwa -------------------------------------------------

# https://www.geeksforgeeks.org/how-to-calculate-the-number-of-occurrences-of-a-character-in-each-row-of-r-dataframe/

# Zliczenie ilości średników rozdzielających państwa
# w kolumnie Country, z pominięciem wartości "unknown"
nationality <- regmatches(AgeDataset$Country[AgeDataset$Country != "unknown"],
            gregexpr(";", AgeDataset$Country[AgeDataset$Country != "unknown"]))

# Ta ilość plus jeden jest równa ilości państw
# których obywatelem była dana osoba
nationality <- lengths(nationality) + 1

# Rozkład ilości obywatelstw
table(nationality)
