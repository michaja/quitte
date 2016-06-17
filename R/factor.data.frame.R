#' (Re-) Factorise factor and character columns in data frame
#'
#' \code{factor.data.frame()} turns character columns in a data frame into
#' factor columns and refactorises factor columns, silently dropping unused
#' levels.
#'
#' @param df a data frame
#' @return a data frame
#' @author Michaja Pehl
#' @examples
#' df <- data.frame(
#'     character = letters[1:5],
#'     factor = as.factor(LETTERS[1:5]),
#'     value = 1:5,
#'     stringsAsFactors = FALSE)
#' df <- df[1:3,]
#' str(df)
#' str(factor.data.frame(df))
#' @export

factor.data.frame <- function(df) {
    for (c in 1:ncol(df))
        if (is.factor(df[[c]]) | typeof(df[[c]]) == "character") {
            df[[c]] <- factor(df[[c]])

            if ("" %in% levels(df[[c]]))
                levels(df[[c]])[levels(df[[c]]) == ""] <- NA
        }
    return(df)
}
