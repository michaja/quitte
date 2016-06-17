#' Convert a vector of stings to a data frame
#'
#' \code{inline.data.frame()} converts a vector of strings that contain
#' separated items into a data frame.
#'
#' @param table vector of strings
#' @param sep   item separator within strings
#' @param quote quote character for masking separators
#'
#' @return a data frame
#'
#' @author Michaja Pehl
#'
#' @examples
#' inline.data.frame(c("letters; numbers", "A; 1", "B; 2"))
#'
#' @importFrom dplyr '%>%'
#' @export

inline.data.frame <- function(table, sep = ";", quote = "") {
    paste(table, sep = "\n") %>%
        textConnection() %>%
        read.table(header = TRUE, sep = sep, quote = quote,
                   comment.char = "", strip.white = TRUE,
                   stringsAsFactors = FALSE)
}
