#'
#' Read item from \code{.gdx} file as quitte data frame
#'
#' \code{read.gdx()} is a wrapper function for \code{\link[gdxrrw]{rgdx}} that
#' returns a quitte data frame.
#'
#' @param gdxName path to \code{.gdx} file
#' @param requestList.name name of item to read
#' @param fields fields to read from variables and equations (any of \code{lo},
#'               \code{l}, \code{m}, and \code{up}); ignored for parameters
#' @param colNames string vector of column names to override dimension names
#' @return quitte data frame
#' @author Michaja Pehl
#'
#' @export
#' @importFrom gdxrrw igdx rgdx gdxInfo
#' @importFrom dplyr tbl_df
#'

read.gdx <- function(gdxName, requestList.name, fields = "l", colNames = NULL) {

    gdxName <- path.expand(gdxName)

    # if reading variable or equation, read specific fields (e.g. "lo", "m")
    info <- gdxInfo(gdxName, dump = FALSE, returnList = TRUE)
    read.fields <- tolower(requestList.name) %in% tolower(c(info$variables,
                                                            info$equations))

    # read the first (or only) field
    if (read.fields) {
        requestList <- list(name = requestList.name, field = fields[[1]])
        item <- rgdx(gdxName, requestList, squeeze = FALSE)
    } else {
        requestList <- list(name = requestList.name)
        item <- rgdx(gdxName, requestList, squeeze = TRUE)
    }

    # if item is a scalar, return a named vector
    if (0 == item$dim) {
        data <- as.vector(item$val)
        names(data) <- requestList.name
        return(data)
    }

    # convert dimension info
    data <- list()
    for (d in 1:(item$dim)) {
        val <- item$val[,d]
        uel <- item$uels[[d]]

        if (all(grepl("^[0-9]+$", uel)))
            uel <- as.numeric(uel)

        data[[d]] <- c(uel[val])
    }

    # add first (or only) field
    if (dim(item$val)[[2]] > d) {
        d <- d + 1
        data[[d]] <- c(item$val[,d])
    }

    # read additional fields
    if (read.fields) {
        for (field in fields[-1]) {
            d <- d + 1
            requestList <- list(name = requestList.name, field = field)
            item <- rgdx(gdxName, requestList, squeeze = FALSE)
            data[[d]] <- c(item$val[,item$dim+1])
        }
    }

    if (read.fields) {
        field.names <- sub("^l$", "value", fields)
    } else {
        field.names <- "value"
    }

    if (is.null(colNames)) {
        if (length(data) > length(item$domains)) {
            names(data) <- c(item$domains, field.names)
        } else {
            names(data) <- c(item$domains)
        }
    } else {
        names(data) <- colNames
    }

    return(tbl_df(data.frame(data)))
}
