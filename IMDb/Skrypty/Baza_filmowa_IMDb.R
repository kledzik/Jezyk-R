
# Analiza danych z bazy filmowej IMDb
# https://www.kaggle.com/datasets/digvijaysinhgohil/imdb-dataset-toprated-films-18982022


# Załadowanie danych ------------------------------------------------------

filePath <- "Dane/imdb_data.csv"

imdb <- read.csv(filePath)

str(imdb)
summary(imdb)
head(imdb)
tail(imdb)


# Czyszczenie kolumny Director --------------------------------------------

# Usunięcie przecinków z kolumny Director
imdb$Director <- gsub(",", "", imdb$Director)

# Podzielenie kolumny Director według wielkiej litery
# https://stackoverflow.com/questions/7988959/splitting-string-based-on-letters-case
imdb$Director <- gsub('([[:upper:]])', ' \\1', imdb$Director)

# Usunięcie pierwszego znaku spacji z kolumny Director
imdb$Director <- sub(" ", "", imdb$Director)

# Zwrócenie danych z kolumny Director, zawierających
# podwójną spację. Można też zastosować grep
imdb$Director[grepl("  ", imdb$Director)]


# Czyszczenie kolumny Stars -----------------------------------------------

# Sprawdzenie gdzie w kolumnie Stars są wiersze
# zaczynające się od przecinka i spacji
grep("^, ", imdb$Stars)

# Wypisanie pierwszych wierszy tych danych
head(imdb$Stars[grepl("^, ", imdb$Stars)])

# Jeżeli wykryto takie dane (wiersze), to początkowe
# przecinki i spacje są z nich usuwane. A jeżeli nie ma
# takich wierszy, to kolumna pozostaje niezmieniona
imdb$Stars <- ifelse(!grepl("^, ", imdb$Stars), imdb$Stars, sub(", ", "", imdb$Stars))

# Podzielenie kolumny Stars według wielkiej litery
imdb$Stars <- gsub('([[:upper:]])', ' \\1', imdb$Stars)

# Usunięcie pierwszego znaku spacji z kolumny Stars
imdb$Stars <- sub(" ", "", imdb$Stars)

# Ilość wierszy zawierających podwójną
# spację w kolumnie Stars
length(grep("  ", imdb$Stars))

# Zwrócenie początkowych wierszy z kolumny
# Stars, zawierających podwójną spację
head(imdb$Stars[grepl("  ", imdb$Stars)])

# Zamiana podwójnych spacji na pojedyncze w kolumnie Stars
imdb$Stars <- gsub("  ", " ", imdb$Stars)


# Czyszczenie kolumny Duration --------------------------------------------

# Usunięcie przyrostka min z kolumny Duration
imdb$Duration <- sub("min", "", imdb$Duration)

# Zamiana typu kolumny Duration na integer
# z pominięciem ostrzeżenia o wartościach NA
suppressWarnings(imdb$Duration <- as.integer(imdb$Duration))

# Ilość wierszy zawierających NA w kolumnie Duration
nrow(imdb[is.na(imdb$Duration), 0])

# Jeżeli są takie wiersze, to NA jest zamieniane na zero.
# Ale można je usunąć, w zależności od potrzeby.
if (nrow(imdb[is.na(imdb$Duration), 0]) != 0) {
  imdb$Duration[is.na(imdb$Duration)] <- 0
}


# Czyszczenie kolumny Category --------------------------------------------

# Usunięcie nadmiarowych słów Drama, zniekształcających
# statystyki. Słowo Drama pozostawiono tylko w tych
# wierszach, w których jest jedynym oznaczeniem filmu.

# Sprawdzenie gdzie w kolumnie Category są wiersze
# zawierające wyrażenie "Drama," na początku napisu
grep("^Drama,", imdb$Category)

# Wypisanie pierwszych wierszy tych danych
head(imdb$Category[grepl("^Drama,", imdb$Category)])

# Jeżeli wykryto wiersze z wyrażeniem "Drama,"
# na początku napisu, to jest ono usuwane
imdb$Category <- ifelse(!grepl("^Drama,", imdb$Category), imdb$Category,
                        imdb$Category <- sub("Drama,", "", imdb$Category))

# Sprawdzenie gdzie w kolumnie Category są wiersze
# zawierające słowo Drama wewnątrz napisu
grep(",Drama,", imdb$Category)

# Wypisanie pierwszych wierszy tych danych
head(imdb$Category[grepl(",Drama,", imdb$Category)])

# Jeżeli wykryto wiersze z Drama w środku napisu,
# to jest ona zamieniana na przecinek
imdb$Category <- ifelse(!grepl(",Drama,", imdb$Category), imdb$Category,
                        imdb$Category <- sub(",Drama,", ",", imdb$Category))

# Sprawdzenie gdzie w kolumnie Category są wiersze
# zawierające wyrażenie ",Drama" na końcu napisu
grep(",Drama$", imdb$Category)

# Wypisanie pierwszych wierszy tych danych
head(imdb$Category[grepl(",Drama$", imdb$Category)])

# Jeżeli wykryto wiersze z Drama na końcu
# napisu, to jest ona usuwana
imdb$Category <- ifelse(!grepl(",Drama$", imdb$Category), imdb$Category,
                        imdb$Category <- sub(",Drama", "", imdb$Category))


# Obliczenie udziału procentowego filmów z każdego gatunku ----------------

# Podzielenie danych w kolumnie Category ze względu
# na separator przecinek
categories <- strsplit(imdb$Category, split = ",")

# ...i przekształcenie ich w wektor
categories <- unlist(categories)

# Ilość filmów zakwalifikowanych do danego gatunku
table(categories)

# Posortowany udział procentowy filmów w danych gatunkach
categories_sorted <- sort(100 * table(categories) / nrow(imdb))
categories_sorted

# Zwiększenie zakresu osi X
xlim_max <- ceiling(max(categories_sorted) / 25) * 25

# Wykres udziału procentowego filmów z danego gatunku
barplot(categories_sorted,
        horiz = TRUE,
        las = 1,
        col = rev(heat.colors(nrow(categories_sorted))),
        main = "Udział filmów z danego gatunku",
        xlab = "[%]",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.7,
        xlim = c(0, xlim_max))


# Obliczenie średniej długości filmów z każdego gatunku -------------------

# ....................................................................................
# Przekształcenie wyrazów oddzielonych przecinkami w kolumnie Category w oddzielne
# wiersze, i dołączenie kolumny Duration, można zrobić też w poniższy sposób
# https://stackoverflow.com/questions/15347282/split-delimited-strings-in-a-column-and-insert-as-new-rows
#
# Podzielenie danych w kolumnie Category ze względu na separator przecinek
# categories <- strsplit(imdb$Category, split = ",")
# Przekształcenie categories w oddzielne wiersze i dołączenie kolumny Duration
# category_df <- data.frame(Category = unlist(categories),
#                           Duration = rep(imdb$Duration, sapply(categories, length)))
# ....................................................................................

# Przekształcenie wyrazów oddzielonych przecinkami w kolumnie
# Category w oddzielne wiersze, i dołączenie kolumny Duration
# https://itecnote.com/tecnote/r-split-comma-separated-strings-in-a-column-into-separate-rows/
category_df <- stack(setNames(strsplit(imdb$Category, ','), imdb$Duration))

# Zmiana nazw kolumn tego data frame
names(category_df) <- c("Category", "Duration")

# Konwersja kolumny Duration z typu factor do integer.
# Można to zrobić w nieco bardziej efektywny sposób:
# as.integer(levels(category_df$Duration))[category_df$Duration]
category_df$Duration <- as.integer(as.character(category_df$Duration))

# Wyliczenie wartości średnich Duration dla każdego gatunku

# ....................................................................................
# Można to też zrobić za pomocą funkcji tapply
# mean_duration <- tapply(category_df$Duration, category_df$Category,
#                         FUN = mean, na.rm = TRUE)
#
# Konwersja otrzymanej tablicy do data.frame i zmiana nazw kolumn
# mean_duration <- data.frame(Category = row.names(mean_duration),
#                             Average_duration = mean_duration)
# Usunięcie nazw wierszy
# row.names(mean_duration) <- NULL
#
# Albo
# mean_duration <- cbind(Category = rownames(mean_duration),
#                        data.frame(Average_duration = mean_duration,
#                                   row.names = NULL))
# ....................................................................................

mean_duration <- aggregate(category_df$Duration, by = list(category_df$Category),
                           FUN = mean, na.rm = TRUE)

# Zmiana nazw kolumn tego data frame
names(mean_duration) <- c("Category", "Average_duration")

# Zaokrąglenie długości filmów
mean_duration$Average_duration <- round(mean_duration$Average_duration, digits = 1)

# Posortowanie średnich długości utworów w danym gatunku
mean_duration <- mean_duration[order(mean_duration$Average_duration,
                                     decreasing = FALSE), ]
mean_duration

# Zwiększenie zakresu osi X
xlim_max <- ceiling(max(mean_duration$Average_duration) / 160) * 160

# Wykres średnich długości utworów z danego gatunku
barplot(mean_duration$Average_duration,
        names.arg = mean_duration$Category,
        horiz = TRUE,
        las = 1,
        col = rev(heat.colors(nrow(mean_duration))),
        main = "Średnie długości utworów",
        xlab = "min.",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.7,
        xlim = c(0, xlim_max))

# Unikalne gatunki filmów posortowane alfabetycznie
categories_uniq <- sort(unique(category_df$Category))
categories_uniq

# ....................................................................................
# Ten cały blok kodu można zastąpić wyrażeniem z tapply z linii nr 271
#
# # Utworzenie pustej listy długości filmów
# duration_list <- list()
# 
# # Wypełnienie listy długości filmów danymi pobranymi dla
# # określonych gatunków wyrażenie seq_along(categories_uniq)
# # można zastąpić takim 1:length(categories_uniq)
# for (i in seq_along(categories_uniq)) {
#   duration_list[[i]] <- category_df$Duration[category_df$Category == categories_uniq[i]]
# }
# 
# # Przypisanie do kolumn unikalnych nazw gatunków
# names(duration_list) <- categories_uniq
# duration_list
# ....................................................................................

# Utworzenie listy długości filmów wypełnionej
# danymi pobranymi dla (unikalnych) gatunków
duration_list <- tapply(category_df$Duration, category_df$Category, FUN = cbind)

# Wykres pudełkowy poziomy. Szerokość pudełka
# to pierwiastek z ilości danych
boxplot(duration_list,
        ylab = "Długość [min.]",
        las = 2,
        col = rainbow(nrow(duration_list)),
        varwidth = TRUE,
        cex.axis = 0.8)

# # Wykres pudełkowy można zrobić też w poniższy sposób
# boxplot(Duration ~ Category, data = category_df, ylab = "Długość [min.]",
#         las = 2, col = rainbow(length(categories_uniq)), varwidth = TRUE)


# Obliczenie średniej, mediany i ich rozrzutu -----------------------------

# Wyliczenie wartości średnich Duration
# dla każdego gatunku (inny sposób)
mean_duration <- tapply(category_df$Duration, category_df$Category, FUN = mean)

# Wyliczenie wartości mediany Duration dla każdego gatunku
median_duration <- tapply(category_df$Duration, category_df$Category, FUN = median)

# Wartość bezwzględna z rozrzutu tych wartości
difference_mm <- abs(mean_duration - median_duration)

# Posortowanie rozrzutu
difference_mm <- sort(difference_mm, decreasing = TRUE)

# Pobranie nazwy gatunku o największym rozrzucie
difference_name <- rownames(head(difference_mm, n = 1))

# ...i odpowiadających mu wartości z kolumny Duration
values <- category_df$Duration[category_df$Category == difference_name]

# Histogram
hist(values, xlab = "Długość [min.]", main = "Histogram czasu trwania utworów")

# Punkty pomiarowe poniżej histogramu
rug(values, col = "red", lwd = 2.3)

# Linia dla wartości mediany
abline(v = median(values), col = "magenta", lwd = 4)

# Linia dla wartości średniej
abline(v = mean(values), col = "orange", lwd = 4)


# Obliczenie średniego rankingu filmów z każdego gatunku ------------------

# Przekształcenie wyrazów oddzielonych przecinkami w kolumnie
# Category w oddzielne wiersze, i dołączenie kolumny IMDb.Rating
category_df <- stack(setNames(strsplit(imdb$Category, ','), imdb$IMDb.Rating))

# Zmiana nazw kolumn tego data frame
names(category_df) <- c("Category", "Rating")

# Konwersja kolumny Rating z typu factor do integer.
# Można to zrobić w nieco bardziej efektywny sposób:
# as.numeric(levels(Category_df$Rating))[Category_df$Rating]
category_df$Rating <- as.numeric(as.character(category_df$Rating))

# Wyliczenie wartości średnich Rating dla każdego gatunku
mean_rating <- aggregate(category_df$Rating, by = list(category_df$Category),
                         FUN = mean, na.rm = TRUE)

# Zmiana nazw kolumn tego data frame
names(mean_rating) <- c("Category", "Average_rating")

# Zaokrąglenie ocen rankingowych
mean_rating$Average_rating <- round(mean_rating$Average_rating, digits = 3)

# Posortowanie średnich ocen utworów w danym gatunku
mean_rating <- mean_rating[order(mean_rating$Average_rating, decreasing = FALSE), ]
mean_rating

# Wykres średnich ocen utworów w danym gatunku
barplot(mean_rating$Average_rating,
        names.arg = mean_rating$Category,
        horiz = TRUE,
        las = 1,
        col = rev(heat.colors(nrow(mean_rating))),
        main = "Średnie oceny utworów",
        xlab = "Punkty",
        col.axis = "darkblue",
        col.lab = "darkblue",
        col.main = "darkblue",
        cex.names = 0.7)
