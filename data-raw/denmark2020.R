# formats Juul paper data and exports it
# https://www.nature.com/articles/s41567-020-01121-y

library(dplyr)
library(readr)
library(purrr)
devtools::load_all()

ens <- read_csv("data-raw/ensemble.csv") |>
    select(-...1) |>
    as.list()
n <- length(ens[[1]])

denmark2020fc <- create_forecast(list(time=0:(n-1), real=ens), name="Denmark 5 May to 1 October 2020", forecast_time=0)

hst <- read_csv("data-raw/historic.csv") |> select(-...1)

m <- -length(hst[[1]])

denmark2020obs <- tibble(time=m:-1, obs=hst[[1]])

usethis::use_data(denmark2020fc)
usethis::use_data(denmark2020obs)

# NULL |> graph_ensemble(fc) |> graph_observations(obs)