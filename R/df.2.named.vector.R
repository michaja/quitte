
#' Data frame to named vector
#'
#' Turns the two first columns of a data frame into a named vector, taking the
#' values from the second and the names from the first column.
#'
#' @param .data a data frame with at least two columns
#'
#' @return a named vector
#' @export
#'
#' @examples
#' data <- data.frame(names = c("one", "two", "three"), values = 1:3)
#' data
#' df.2.named.vector(data)

df.2.named.vector <- function(.data) {
    x <- as.character(.data[,2])
    names(x) <- as.character(.data[,1])

    return(x)
}
