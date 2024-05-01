library("tidyverse")

data <- read.csv2("food_statistics.csv")
data$opis_okres <- as.Date(paste(substring(data$opis_okres, 1, 4), substring(data$opis_okres, 7, 8), "01", sep = "-"))
months <- data |> select(opis_okres) |> distinct()
important_columns <- data |> select(nazwa_pozycja_2, opis_okres, wartosc, jednostka_terytorialna)
masovian_voievoidship <- important_columns |> filter(jednostka_terytorialna == "MAZOWIECKIE")
poland <- important_columns |> filter(jednostka_terytorialna == "POLSKA")

group_localization <- function(localization){
    return (localization |> group_by(nazwa_pozycja_2))
}

groups_pl <- group_localization(poland)
groups_masovian <- group_localization(masovian_voievoidship)

calculate_inflation <- function(prices){
    return (100 * (tail(prices, n = 1) - prices[1]) / prices[1])
}

inflation_masovian <- calculate_inflation(groups_masovian$wartosc)
inflation_pl <- calculate_inflation(groups_pl$wartosc)


print_inflation <- function(df){
    df |> group_by(nazwa_pozycja_2) |> summarize(inflation = calculate_inflation(wartosc))
}



print_inflation(groups_masovian)
print_inflation(groups_pl)



create_plot_for_every_group <- function(group){
    list_of_products <- split(group, group$nazwa_pozycja_2)
    for(product in list_of_products){
        print(product  |> ggplot(aes(x = opis_okres, y = wartosc)) + 
                  geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 3)) +
                  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
                  labs(title = "Cena produktu w ostatnim roku", subtitle = paste(product$nazwa_pozycja_2, "\n", "lokalizacja: ", product$jednostka_terytorialna, sep = ""), x = "Okres", y = "Cena"))
    }
}



create_plot_for_every_group(masovian_voievoidship)
create_plot_for_every_group(groups_pl)

         