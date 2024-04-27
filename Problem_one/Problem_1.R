library("tidyverse")

data <- read.csv2("food_statistics.csv")
data$opis_okres <- as.Date(paste(substring(data$opis_okres, 1, 4), substring(data$opis_okres, 7, 8), "01", sep = "-"))
important_columns <- data[c("nazwa_pozycja_2", "opis_okres", "wartosc", "jednostka_terytorialna")]
masovian_voievoidship <- important_columns[important_columns$jednostka_terytorialna == "MAZOWIECKIE",]
poland <- important_columns[important_columns$jednostka_terytorialna == "POLSKA",]
split_localization <- function(localization){
    return (localization |> split(localization$nazwa_pozycja_2))
}
group_localization <- function(localization){
    return (localization |> group_by(nazwa_pozycja_2))
}
split_poland <- split_localization(poland)
split_masovian <- split_localization(masovian_voievoidship)
groups_pl <- group_localization(poland)
groups_masovian <- group_localization(masovian_voievoidship)

plot_prices <- function(price_list){
    for(product in price_list){
        plot(product$opis_okres, product$wartosc, main = paste("Cena:", product$nazwa_pozycja_2[1], "; Lokalizacja:", product$jednostka_terytorialna[1]), xlab = "Okres", ylab = "Cena")
    }
}


calculate_inflation <- function(prices){
    return (100 * (tail(prices, n = 1) - prices[1]) / prices[1])
}
print_inflation <- function(df){
    df |> group_by(nazwa_pozycja_2) |> summarize(inflation = calculate_inflation(wartosc))
}
print_inflation(groups_masovian)
print_inflation(groups_pl)
plot_prices(split_masovian)
plot_prices(split_poland)

