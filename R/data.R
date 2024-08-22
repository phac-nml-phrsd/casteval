#' Denmark 2020 COVID-19 forecast object
#'
#' A `{casteval}` forecast object containing data from a Danish COVID-19 forecasting effort,
#' concerning 5 May to 1 October 2020.
#' 
#' @seealso [denmark2020df], [denmark2020obs], [denmark2020ens]
#' @source <https://www.nature.com/articles/s41567-020-01121-y>
#' @source <https://github.com/jonassjuul/curvestat>
"denmark2020fc"

#' Denmark 2020 COVID-19 forecast data frame
#' 
#' The data frame that belongs to `denmark2020fc`, exported for convenience.
#' 
#' @seealso [denmark2020fc], [denmark2020obs], [denmark2020ens]
#' @source <https://www.nature.com/articles/s41567-020-01121-y>
#' @source <https://github.com/jonassjuul/curvestat>
"denmark2020df"

#' Denmark 2020 COVID-19 observations
#'
#' An observations data frame containing made-up daily hospitalization data for demonstration purposes.
#' Meant to be used in conjunction with `denmark2020fc` and `denmark2020ens`.
#'
#' @seealso [denmark2020fc], [denmark2020df], [denmark2020ens]
"denmark2020obs"

#' Denmark 2020 COVID-19 forecast ensemble
#'
#' The data used to create `denmark2020fc`, as a list of realization vectors.
#'
#' @seealso [denmark2020fc], [denmark2020df], [denmark2020obs]
#' @source <https://www.nature.com/articles/s41567-020-01121-y>
#' @source <https://github.com/jonassjuul/curvestat>
"denmark2020ens"

#' Grouping example forecast 1
#'
#' An example forecast data frame for demonstrating (and testing) `{casteval}`'s grouping functionality.
#' Contains mean-and-quantiles data.
#' 
#' @seealso [groups2], [groups_obs]
"groups1"

#' Grouping example observations
#' 
#' An example observations data frame for demonstrating (and testing) `{casteval}`'s grouping functionality.
#' 
#' @seealso [groups1], [groups2]
"groups_obs"

#' Grouping example forecast 2
#'
#' An example forecast data frame for demonstrating (and testing) `{casteval}`'s grouping functionality.
#' Contains raw data.
#' 
#' @seealso [groups1], [groups_obs]
"groups2"