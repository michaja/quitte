#' read IAMC-style .csv files into quitte data frame
#'
#' @usage read.quitte(file, sep = ";", quote = "",
#'             na.strings = c("UNDF", "NA", "N/A", "n_a"),
#'             convert.periods = FALSE)
#' @param file IAMC-style .csv file
#' @param sep column separator; defaults to ";"
#' @param quote quote characters; empty by default
#' @param na.strings entries to interpret as NA; defaults to
#'                   c("UNDF", "NA", "N/A", "n_a")
#' @param convert.periods if TRUE, periods are converted to POSIXct, defaults
#'                        to FALSE
#' @return a quitte data frame
#' @author Michaja Pehl
#' @examples
#' \dontrun{
#' mif <- read.quitte("some/data/file.mif")
#' csv <- read.quitte("some/data/file.csv", sep = ",", quote = '"')
#' }
#'
#' @export
#' @importFrom tidyr gather_
#' @importFrom dplyr '%>%' tbl_df
#'


read.quitte <- function(file,
                        sep = ";",
                        quote = "",
                        na.strings = c("UNDF", "NA", "N/A", "n_a"),
                        convert.periods = FALSE) {

    # Check the header for correct names, periods all in one block and no
    # additional columns after the periods
    # FIXME: relax last condition to conform to .mif standard
    header <- read.table(file, header = TRUE, sep = sep, quote = quote,
                         na.strings = na.strings, nrows = 1,
                         check.names = FALSE, strip.white = TRUE) %>%
        colnames() %>%
        tolower()

    default.columns  <- c("model", "scenario", "region", "variable", "unit")
    # FIXME: relax to handle other than 4-digit periods
    period.columns   <- grep("^[0-9]{4}$", header)


    # Check if the last column of df is of class logical, i.e. if it was a
    # propper .mif file with the pointless trailing semi-colon.
    useless.last.column <- tail(header, 1) == ""

    if (useless.last.column)
        header <- header[-length(header)]

    if (min(header[1:5] == default.columns) != 1)
        stop("missing default columns in header of file ", file)

    if (tail(period.columns, n = 1) != length(header))
        stop("unallowed extra columns in header of file ", file)

    if (length(setdiff(min(period.columns):max(period.columns),
                       period.columns)) > 0)
        stop("period columns not all in one block in header of file ", file)

    periods <- header[period.columns]

    colClasses <- c(rep("factor", period.columns[1] - 1),
                     rep("numeric", length(period.columns)))
    if (useless.last.column)
        colClasses <- c(colClasses, "NULL")

    # read actual data
    data <- read.table(file, header = TRUE, sep = sep, quote = quote,
                       na.strings = na.strings, colClasses = colClasses,
                       check.names = FALSE, strip.white = TRUE) %>%
        tbl_df()

    colnames(data) <- tolower(colnames(data))

    # convert to long format
    data <- data %>%
        gather_("period", "value", periods)

    # convert periods
    if (convert.periods) {
        ISOyear <- make.ISOyear(seq(2005, 2150, by = 5))
        data$period <- ISOyear(data$period)
    } else {
        data$period <- as.numeric(as.character(data$period))
    }

    return(tbl_df(data))
}

