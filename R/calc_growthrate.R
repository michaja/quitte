#' Calculates the growth rate in '%/yr' for variables
#'
#' @param x anything with an as.quitte method (data.frame, quitte or magclass object, mif file, ...)
#' @param only.new If `FALSE` (the default), add new variables to existing ones.
#'   If variable names already exist in the data.frame, they are replaced.
#'   If `TRUE`, return only new variables.
#' @param filter.function A function used to filter data before calculating growth rates.
#'   If instead a character vector is passed, only variables matching this vector are used.
#'
#' @author Oliver Richters
#'
#' GDPgrowth <- calc_growthrate(quitte_example_data, only.new = TRUE, filter.function = "GDP|PPP")
#' alldata <- calc_growthrate(quitte_example_data, only.new = FALSE,
#'                            filter.function = function(x) filter(x, grepl("GDP", .data$variable)))
#' }
#'
#' @importFrom dplyr filter group_by lag mutate rename select ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return data as a quitte object
#' @export
calc_growthrate <- function(x, only.new = FALSE, filter.function = identity) {
  # transform character vector into filter function
  if (is.character(filter.function)) {
    vars <- filter.function
    filter.function <- function(x) return(filter(x, .data$variable %in% vars))
  }
  # remove na and filter data
  xg <- as.quitte(x, na.rm = TRUE) %>%
    filter.function()
  # rename variables
  levels(xg$variable) <- paste(levels(xg$variable), "[Growth Rate]")
  # calculate growth rates
  xg <- xg %>%
    group_by(.data$model, .data$scenario, .data$region, .data$variable) %>%
    mutate(diffyear = .data$period - lag(.data$period, order_by = .data$period),
           growthrate = 100 * ((.data$value/lag(.data$value, order_by = .data$period))^(1/.data$diffyear) - 1),
           unit = factor("%/yr")) %>%
    filter(is.finite(.data$growthrate)) %>%
    select(-"value", -"diffyear") %>%
    rename(value = "growthrate") %>%
    ungroup() %>%
    droplevels()
  # return data
  if (isTRUE(only.new)) {
    return(xg)
  } else {
    return(rbind(filter(x, ! .data$variable %in% levels(xg$variable)), xg))
  }
}
