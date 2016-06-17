#'
#' Stacked and dodged bar plot
#'
#' \code{ggplot_bar_stacked_dodged()} creates a bar plot with both stacked
#' (different categories atop each other) and dodged (different categories along
#' the x-axis) positioning. Useful for showing more dimensions.
#'
#' @param data a data frame
#' @param mapping \code{aes()} mapping with aesthetics \code{x}, \code{y},
#'                \code{fill} and \code{dodge}.
#' @return a \code{ggplot()} like object
#' @details The \code{fill} dimension will be stacked and the \code{dodge}
#'          dimension spread along the x-axis. Use further \code{ggplot2}
#'          functions like scales, themes and facets as usual. Automatically
#'          splits positive and negative values for plotting above and below
#'          zero bars.
#' @author Michaja Pehl
#' @examples
#' require(dplyr)
#' require(ggplot2)
#' expand.grid(x = as.factor(c("left", "center", "right")),
#'                   stack = as.factor(c("bottom", "middle", "top")),
#'                   category = letters[1:4]) %>%
#'     mutate(value = abs(rnorm(n()))) %>%
#'     ggplot_bar_stacked_dodged(aes(x = x, y = value, fill = stack,
#'                                   dodge = category)) +
#'     guides(fill = guide_legend(reverse = TRUE))
#'
#' @export
#' @importFrom dplyr '%>%' select_ filter_ distinct arrange_
#' @importFrom tidyr unite_
#' @importFrom lazyeval interp
#' @importFrom ggplot2 ggplot geom_bar aes_string scale_x_continuous
#'

ggplot_bar_stacked_dodged <- function(data, mapping) {

    # get names from aesthetics mapping
    category <- deparse(mapping$x)
    y        <- deparse(mapping$y)
    fill     <- deparse(mapping$fill)
    dodge    <- deparse(mapping$dodge)

    # guardians
    if (is.null(category))
        stop("requires aesthetics mpping 'x'")
    if (is.null(y))
        stop("requires aesthetics mpping 'y'")
    if (is.null(fill))
        stop("requires aesthetics mpping 'fill'")
    if (is.null(dodge))
        stop("requires aesthetics mpping 'dodge'")
    if (!is.data.frame(data))
        stop("requires data frame")

    # generate temporary data frame in which 'category' and 'dodge' columns are
    # combined to one column which can be used on the x axis
    tmp <- data.frame(
        data %>%
            select_(.dots = setdiff(colnames(data), c(y, dodge))) %>%
            # there is no gap after the last category on the x axis
            filter_(interp(~ val != var, val = as.name(category),
                           var = as.character(tail(unique(data[[category]]),
                                                   1)))) %>%
            distinct(),
        "gap",
        0)

    # fix column names
    colnames(tmp)[dim(tmp)[2] - c(1, 0)] <- c(dodge, y)

    unite.name <- paste(category, dodge, sep = ".")

    tmp <- rbind(data, tmp) %>%
        unite_(col    = unite.name,
               from   = c(category, dodge),
               sep    = ".",
               remove = FALSE) %>%
        arrange_(.dots = c(category, dodge, fill))

    unite.col <- which(colnames(tmp) == unite.name)

    tmp[unite.col] <- as.numeric(
        factor(getElement(tmp, unite.col),
               levels = unique(getElement(tmp, unite.col))))

    # information for x axis scale
    x.breaks <- seq(
        from       = (length(unique(getElement(data, dodge))) + 1) / 2,
        by         = length(unique(getElement(data, dodge))) + 1,
        length.out = length(unique(getElement(data, category))))
    labels.category <- as.character(unique(getElement(data, category)))


    ggplot(data = tmp, # pass data along for future use
           mapping = aes_string(x = unite.name, y = y, fill = fill)) +
        # plot positive and negative values individually
        geom_bar(data = tmp %>% filter_(interp(~ val >= 0, val = as.name(y))),
                 stat = "identity") +
        geom_bar(data = tmp %>% filter_(interp(~ val < 0, val = as.name(y))),
                 stat = "identity") +
        scale_x_continuous(breaks = x.breaks, labels = labels.category)
}
