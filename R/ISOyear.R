#' speedily converting years to POSIXct values
#'
#' \code{make.ISOyear()} creates a closure for speedily converting years to
#' POSIXct values. Initialise the closure with year values you know you will
#' need. Additional values will be added automatically.
#'
#' @param years vector of year numbers to initialise closure with; defaults to
#'              1900 to 2200 in five year steps
#' @return function taking an integer year and returning a POSIXct value, based
#'         on July 2, 12am GMT as the middle of the year
#' @author Michaja Pehl
#' @examples
#'      ISOyear <- make.ISOyear()
#'      ISOyear(c(2005, 2010, 2100, 1900))
#'
#' @export
#'

make.ISOyear <- function(years = seq(from = 1900, to = 2200, by = 5)) {

  ISOyear <- data.frame(key = years, value = ISOdate(years, 7, 2))

  f <- function(keys) {
    rows <- match(keys, ISOyear$key)
    if (any(is.na(rows))) {
      new.keys <- keys[is.na(rows)]
      ISOyear <<- rbind(ISOyear, data.frame(key = new.keys,
                                            value = ISOdate(new.keys, 7, 2)))
      rows <- match(keys, ISOyear$key)
    }

    return(ISOyear[rows,"value"])
  }

  return(f)
}

