#'
#' Initialise external gdx libraries
#'
#' Load the external gdx libraries that are required to interface with gdx data.
#'
#' @param libname character string giving the library directory where the
#'        package defining the namespace was found
#' @param character string giving the name of the package
#' @return TRUE if successful
#' @author Michaja Pehl
#'
#' @importFrom gdxrrw igdx
#'

.onLoad <- function(libname, pkgname) {

    done <- FALSE

    if ("Windows" == getElement(Sys.info(), "sysname")) {
        path <- strsplit(Sys.getenv("PATH"), ";")[[1]]
        path <- grep("gams", path, value = TRUE, ignore.case = TRUE)
        path <- grep("%", path, value = TRUE, invert = TRUE)

        for (p in path)
            if (done <- gdxrrw::igdx(p, silent = TRUE))
                break
    } else {
        done <- gdxrrw::igdx(system("which gams | xargs dirname",
                                    intern = TRUE),
                             silent = TRUE)
    }

    if (!done)
        stop("Could not load gdx libraries")

    return(done)
}
