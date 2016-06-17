#' Order data frame factor levels
#'
#' @param df a data frame
#' @param ... name-value pairs assigning level order to factors
#' @param drop.extra.levels if \code{TRUE} (default) levels not present in the
#'                          factor are silently dropped
#' @return a data frame
#' @author Michaja Pehl
#' @examples
#' require(dplyr)
#' df <- data.frame(name = letters[1:3], value = 1:3)
#' df %>% getElement("name")
#' df %>% order.levels(name = c("c", "a", "b", "x")) %>% getElement("name")
#' @export

order.levels <- function(df, ..., drop.extra.levels = TRUE) {

    # Guardians
    if (!is.data.frame(df))
        stop("only works on data frames")

    dots <- list(...)
    dot.names <- names(dots)

    for (column in dot.names) {
        if (!is.factor(getElement(df, column)))
            stop(paste(column, "is not a factor"))

        have.levels <- levels(getElement(df, column))
        want.levels <- getElement(dots, column)

        if (drop.extra.levels)
            want.levels <- intersect(want.levels, have.levels)

        df[, column] <- factor(getElement(df, column), levels = want.levels)
    }

    return(df)
}
