# formats Juul paper data and exports it
# https://www.nature.com/articles/s41567-020-01121-y

library(dplyr)
library(readr)
library(purrr)

ens <- read_csv("data-raw/ensemble.csv") |> select(-...1)
n <- ncol(ens)
ens <- ens |> mutate(raw = c_across(1:n))
    
hst <- read_csv("data-raw/historic.csv") |> select(-...1)

