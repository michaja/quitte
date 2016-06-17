#'
#' Sum over one dimension of a data frame
#'
#' \code{sum_total()} is a short-hand function to calculate and insert the sum
#' of a category in a data frame.
#'
#' @param data a data frame
#' @param group column for which the sum is to be calculated
#' @param value column of the numbers to be summed
#' @param name entry in column \code{group} for the sum; defaults to
#'        \code{"Total"}
#' @return a data frame
#' @author Michaja Pehl
#' @examples
#' require(dplyr)
#' data <- expand.grid(UPPER = LETTERS[1:2], lower = letters[24:26],
#'                     numbers = 1:2) %>%
#'     mutate(value = 1:n()) %>%
#'     group_by(UPPER)
#' data
#' data %>%
#'     sum_total(lower)
#'
#' @export
#' @importFrom dplyr '%>%' ungroup group_by_ summarise_ mutate_ select_ arrange_
#'                   groups
#' @importFrom lazyeval interp
#'
sum_total <- function(data, group, value = NA, name = "Total") {

    .group = deparse(substitute(group))
    .value = deparse(substitute(value))

    if ("NA" == .value)
        .value = "value"

    # guardians
    if (!is.data.frame(data))
        stop("only works with data frames")

    if (!(.group %in% colnames(data)))
        stop("No column '", .group, "' in data frame")

    if (!(.value %in% colnames(data)))
        stop("No column '", .value, "' in data frame")

    .colnames <- colnames(data)
    .groups <- setdiff(.colnames, c(.group, .value))

    .groups.old <- c()
    for (i in 1:length(groups(data)))
        .groups.old <- c(.groups.old, deparse(groups(data)[[i]]))

    summarise.list <- list(interp(~sum(value), value = as.name(.value)))
    names(summarise.list) <- .value

    mutate.list <- list(interp(~as.character(name)))
    names(mutate.list) <- .group

    .data <- rbind(
        data %>%
            ungroup(),
        data %>%
            group_by_(.dots = .groups, add = TRUE) %>%
            summarise_(.dots = summarise.list) %>%
            mutate_(.dots = mutate.list) %>%
            select_(.dots = .colnames)
    ) %>%
        arrange_(.dots = c(.groups, .group))

    if (length(groups(data)) > 0)
        .data <- .data %>%
            group_by_(.groups.old)

    return(.data)
}
