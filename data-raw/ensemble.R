library(dplyr)
library(purrr)
devtools::load_all()
x <- 1:100
ys <- 1:2000 |> map(\(r) dplyr::tibble(time=x, raw=sample(100) * r))
print("hi")
fcst <- create_forecast(ys)