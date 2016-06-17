#'
#' Stacked bar plot with variable bar width
#'
#' \code{ggplot_bar_vts()} creates a bar plot with variable bar widths and is
#' intended for plotting time series with variable time-step width.
#'
#' @usage ggplot_bar_vts(data, mapping = aes(x = period, y = value, fill = variable),
#'                ts = NULL, gaps = 0.1)
#'
#' @param data a data frame
#' @param mapping \code{aes()} mapping with aesthetics \code{x}, \code{y}, and
#'                \code{fill}
#' @param ts data frame linking periods to time steps widths and positons;
#'           defaults to REMIND values
#' @param gaps gaps between bars as a fraction of the lowest bar width;
#'             defaults 0.1 (* 5 years = 0.5 years)
#' @return a \code{ggplot()} like object
#' @details If \code{ts} is \code{NULL}, values for the use with REMIND data are
#'          used. If other values are needed, pass a data frame with the columns
#'          period (coinciding with the column assigned to the x aesthetic in
#'          \code{data}), .ts (containing the desired width of the bars), and
#'          .positon (containing the center point of the bars).
#'          Use further \code{ggplot2} functions like scales, themes and facets
#'          as usual. Automatically splits positive and negative values for
#'          plotting above and below zero bars.
#' @author Michaja Pehl
#' @examples
#' require(dplyr)
#' require(tidyr)
#' require(ggplot2)
#' data.frame(period = c(seq(2005, 2060, by = 5), seq(2070, 2100, by = 10))) %>%
#'     mutate(linear = (period - 2005) / 10,
#'            logarithmic = log10(period - 2000),
#'            negative = -(logarithmic / (linear + 1))) %>%
#'     gather(variable, value, -period) %>%
#'     ggplot_bar_vts()
#' @export
#' @importFrom dplyr '%>%' mutate inner_join filter_
#' @importFrom ggplot2 ggplot aes geom_bar xlab
#' @importFrom lubridate year
#'

ggplot_bar_vts <- function(data,
                           mapping = aes_string(x = "period", y = "value",
                                                fill = "variable"),
                           ts = NULL,
                           gaps = 0.1) {

    # guardians
    if (!is.data.frame(data))
        stop("Only works with data frames")

    if (is.null(getElement(mapping, "x")) | is.null(getElement(mapping, "y")))
        stop("Need mapping of at least x and y")

    # get names of columns to work on
    x     <- deparse(getElement(mapping, "x"))
    y     <- deparse(getElement(mapping, "y"))

    # default Remind period to time-step mapping
    if (is.null(ts))
        ts <- data.frame(
            period    = c(2005,   2010, 2015,    2020, 2025, 2030, 2035, 2040,
                          2045,   2050, 2055,    2060, 2070, 2080, 2090, 2100,
                          2110,   2130, 2150),
            .ts       = c(   5,      5,    5,       5,    5,    5,    5,    5,
                             5,      5,    5,     7.5,   10,   10,   10,   10,
                             15,    20,   27),
            .position = c(2005,   2010, 2015,    2020, 2025, 2030, 2035, 2040,
                          2045,   2050, 2055, 2061.25, 2070, 2080, 2090, 2100,
                          2112.5, 2130, 2153.5)
        )


    # convert POSIXct to years
    if (any(class(getElement(data, "period")) == "POSIXct"))
        data <- data %>% mutate_(period = interp(~ lubridate::year(period),
                                                 period = as.name("period")))

    # join data with time-step information
    .by <- "period"
    names(.by) <- x

    data <- inner_join(data, ts, by = .by) %>%
        mutate_(.ts = interp(~ .ts - min(getElement(ts, ".ts")) * gaps,
                             .ts = as.name(".ts"), gaps = as.name("gaps")))

    if (dim(data)[1] == 0)
        stop(paste0("data$", x, " did not match any periods from ts"))

    # bars are not generally centered at the mid-point of their period
    mapping$x <- substitute(.position)

    # return the plot object
    p <- ggplot(data = data, mapping = mapping) +
        geom_bar(data = data %>% filter_(paste(y, ">= 0")),
                 mapping = aes_string(width = ".ts"), stat = "identity") +
        geom_bar(data = data %>% filter_(paste(y, "<  0")),
                 mapping = aes_string(width = ".ts"), stat = "identity") +
        xlab(x)

    return(p)
}
