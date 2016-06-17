#' Calculate probs
#'
#' @param probs a named vector of values between 0 and 1
#'

calc_quantiles_ <- function(.data,
                              value = "value",
                              probs = c(min    = 1,
                                        p25    = 0.25,
                                        median = 0.5,
                                        p75    = 0.75,
                                        max    = 1)) {

    .data %>%
        do(data.frame(quantile = factor(x = names(probs),
                                        levels = names(probs)),
                      value = quantile(x = getElement(., value),
                                       probs = as.numeric(probs),
                                       names = FALSE))) %>%
        return()
}

calc_quantiles <- function(.data, value = NULL, probs = c(min    = 1,
                                                          p25    = 0.25,
                                                          median = 0.5,
                                                          p75    = 0.75,
                                                          max    = 1)) {
    if (is.null(value)) {
        value <- "value"
    } else {
        value <- substitute(value)
    }

    return(value)
    # calc_quantiles_(.data, deparse(substitute(value)), probs)
}
