library("tidyverse")

data <- read.csv2("DBW_DB_2024-04-19_1404.csv")
data$opis_okres <- as.Date(paste(substring(data$opis_okres, 1, 4), substring(data$opis_okres, 7, 8), "01", sep = "-"))
important_columns <- data[c("nazwa_pozycja_2", "opis_okres", "wartosc")]
split_data <- important_columns %>% split(important_columns$nazwa_pozycja_2)
for(product in split_data){
    plot(product$opis_okres, product$wartosc, main = product$nazwa_pozycja_2[1], xlab = "Okres", ylab = "Cena")
}
