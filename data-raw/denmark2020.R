# formats Juul paper data and exports it
# https://www.nature.com/articles/s41567-020-01121-y

library(dplyr)
library(readr)
library(purrr)
devtools::load_all()

denmark2020ens <- read_csv("data-raw/ensemble.csv") |>
    select(-...1) |>
    as.list() |> unname()
n <- length(denmark2020ens[[1]])

denmark2020fc <- create_forecast(list(time=0:(n-1), ensemble=denmark2020ens), name="COVID-19 Denmark May 5 to October 1 2020", forecast_time=0)

hst <- read_csv("data-raw/historic.csv") |> select(-...1)

m <- -length(hst[[1]])

denmark2020obs <- tibble(time=m:-1, obs=hst[[1]])

usethis::use_data(denmark2020ens, overwrite=TRUE)
usethis::use_data(denmark2020fc, overwrite=TRUE)
usethis::use_data(denmark2020obs, overwrite=TRUE)

# NULL |> plot_ensemble(fc) |> plot_observations(obs)