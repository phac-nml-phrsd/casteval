# formats Juul paper data and exports it
# https://www.nature.com/articles/s41567-020-01121-y

library(dplyr)
library(readr)

ens <- read_csv("data-raw/ensemble.csv") |> select(-...1)
hst <- read_csv("data-raw/historic.csv") |> select(-...1)

