library("tidyverse")

data <- read.csv2("food_statistics.csv")
data$opis_okres <- as.Date(paste(substring(data$opis_okres, 1, 4), substring(data$opis_okres, 7, 8), "01", sep = "-"))
months <- data |> select(opis_okres) |> distinct()
data <- data |> select(nazwa_pozycja_2, opis_okres, wartosc, jednostka_terytorialna)



grouped_data <- data |> group_by(jednostka_terytorialna, nazwa_pozycja_2)

calculate_inflation <- function(prices){
    return (100 * (tail(prices, n = 1) - prices[1]) / prices[1])
}


print_inflation <- function(grouped_data){
    grouped_data |> summarize(inflation = calculate_inflation(wartosc))
}


plot_additional_metrics <- function(df, metric){
    df |>
        summarise(metryka = metric(wartosc)) |>
        ggplot(aes(x = nazwa_pozycja_2, y = metryka)) + geom_point()
}



grouped_data |> summarise(średnia = mean(wartosc))



print_inflation(grouped_data)


create_plot_for_every_group <- function(group, polynomial_degree = 3){
    list_of_products <- group_split(group)
    for(product in list_of_products){
        print(product  |> ggplot(
                            aes(x = opis_okres, y = wartosc)) + 
                            geom_point() +
                            geom_smooth(method = "lm", formula = y ~ poly(x, polynomial_degree), se = FALSE) +
                            scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
                            labs(title = "Cena produktu w ostatnim roku", subtitle = paste(product$nazwa_pozycja_2, "\n", "lokalizacja: ", product$jednostka_terytorialna, sep = ""), x = "Okres", y = "Cena"))
    }
}

create_plot_for_every_group(grouped_data)

         