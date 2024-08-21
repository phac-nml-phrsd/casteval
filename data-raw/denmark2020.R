# formats Juul paper data and exports it
# https://www.nature.com/articles/s41567-020-01121-y

library(dplyr)
library(readr)
library(purrr)
library(lubridate)
devtools::load_all()

denmark2020ens <- read_csv("data-raw/ensemble.csv") |>
    select(-...1) |>
    as.list() |> unname()
n <- length(denmark2020ens[[1]])

times <- seq(ymd("2020-05-05"), ymd("2020-10-01"), by="days")
denmark2020fc <- create_forecast(list(time=times, vals=denmark2020ens), name="COVID-19 Denmark May 5 to October 1 2020", forecast_time=ymd("2020-05-05"))

hst <- read_csv("data-raw/historic.csv") |> select(-...1)

m <- -length(hst[[1]])

#denmark2020obs <- tibble(time=m:-1, obs=hst[[1]])

# a bunch of trial and error logic to make the observations look ok for demonstration purposes
denmark2020obs <- read_csv("data-raw/statista.csv") |>
    mutate(time=mdy(time), val_obs=rev(val_obs)) |>
    filter(time <= ymd("2020-10-01")) |>
    mutate(val_obs=rev(val_obs))

usethis::use_data(denmark2020ens, overwrite=TRUE)
usethis::use_data(denmark2020fc, overwrite=TRUE)
usethis::use_data(denmark2020obs, overwrite=TRUE)

# NULL |> plot_ensemble(fc) |> plot_observations(obs)