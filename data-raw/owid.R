library(dplyr)
library(readr)
library(purrr)
library(lubridate)
devtools::load_all()

owid <- read_csv("data-raw/owid-covid-data.csv")

denmark2020owid <- owid |> filter(location=="Denmark") |> select(date, new_cases) |> mutate(date=as.numeric(date)-as.numeric(ymd("2020-05-01")))

denmark2020owid <- denmark2020owid |> rename(time=date, obs=new_cases)

usethis::use_data(denmark2020owid, overwrite=TRUE)