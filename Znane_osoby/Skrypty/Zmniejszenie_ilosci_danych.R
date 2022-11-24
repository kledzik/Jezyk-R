
# Skrypt pobiera 5000 losowych wierszy z oryginalnego, dużego
# pliku AgeDataset.csv, i zapisuje je do oddzielnego pliku CSV


filePath <- "Dane/ageDataset.csv"

AgeDataset <- read.csv(filePath)

set.seed(1)

sample_data <- AgeDataset[sample(nrow(AgeDataset), 5000), ]

write.csv(sample_data, file = "Dane_wyjsciowe/ageDataset_little_data.csv",
          row.names = FALSE)
