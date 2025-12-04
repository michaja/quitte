#' Read IAMC-style .csv or .xlsx files, or object from rds file.
#'
#' Reads IAMC-style .csv or .xlsx files or object from rds file into a quitte data frame.
#'
#' @param file Path of IAMC-style .csv, .xlsx, or rds file or vector of paths to read.
#' @param sep Column separator, defaults to ";" in read_mif_header().
#' @param quote Quote characters, empty by default.
#' @param na.strings Entries to interpret as NA; defaults to
#'     `c("UNDF", "NA", "N/A", "n_a")`
#' @param convert.periods If `TRUE`, periods are converted to
#'     [`POSIXct`][base::DateTimeClasses].  If `FALSE` (the default), periods
#'     are numerical.
#' @param check.duplicates If `TRUE` a duplicates check will be performed on the
#'     data.  For time- and memory-critical applications this can be switched
#'     off.
#' @param factors Return columns as factors (`TRUE`, the default) or not.
#' @param drop.na Should `NA` values be dropped from the `quitte`?
#' @param comment A character which at line start signifies the optional comment
#'   header with metadata at the head of `file`.  The comment header, if
#'   present, is returned as a `comment_header` attribute.  If multiple files
#'   are read, the `comment_header` attribute is a list of comment headers with
#'   file paths as names.
#' @param filter.function A function used to filter data during read.  See
#'     Details.
#' @param chunk_size Number of lines to read at a time.  Defaults to 200000.
#'     (REMIND .mif files have between 55000 and 105000 lines for H12 and EU21
#'     regional settings, respectively.)
#'
#' @author Michaja Pehl
#'
#' @examples
#' \dontrun{
#' read.quitte(c("some/data/file.mif", "some/other/data/file.mif"))
#' read.quitte("some/data/file.csv", sep = ",", quote = '"')
#' }
#'
#' @importFrom dplyr bind_rows distinct first last mutate relocate select
#' @importFrom forcats as_factor
#' @importFrom magrittr %>%
#' @importFrom rlang .data is_empty
#' @importFrom readr DataFrameCallback problems read_delim_chunked read_lines
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom utils read.table
#'
#' @export
read.quitte <- function(file,
                        sep = NULL,
                        quote = "",
                        na.strings = c("UNDF", "NA", "N/A", "n_a"),
                        convert.periods = FALSE,
                        check.duplicates = TRUE,
                        factors = TRUE,
                        drop.na = FALSE,
                        comment = '#',
                        filter.function = identity,
                        chunk_size = 200000L) {

    if (!length(file))
        stop('\'file\' is empty.')

    if (!(   is.function(filter.function)
          && 1 == length(formals(filter.function))))
        stop('`filter.function` must be a function taking only one argument.')

    .read.quitte <- function(f, sep, quote, na.strings, convert.periods,
                             drop.na, comment, filter.function, chunk_size) {

        default.columns  <- c("model", "scenario", "region", "variable", "unit")

        if (grepl("\\.xlsx?$", f)) {
            data <- read_excel(path = f, sheet = if ('data' %in% excel_sheets(f)) 'data' else 1, guess_max = 21474836)
            if (! any(grepl("^[0-9]{4}$", colnames(data)))) {
                warning("File ", f, " contains no data, returning empty data.frame.")
                return(as.quitte(NULL))
            }
            data <- pivot_longer(data, matches("^[0-9]{4}$"), names_to = 'period', values_drop_na = drop.na)
            missing.default.columns <- default.columns[! default.columns %in% tolower(colnames(data))]
            if (length(missing.default.columns) > 0) {
                warning(f, " misses default columns: ", paste(missing.default.columns, collapse = ", "))
            }
            return(filter.function(as.quitte(data)))
        }
        if (grepl("\\.rds$", f)) {
            return(filter.function(as.quitte(readRDS(f))))
        }

        # Check the header for correct names, periods all in one block and no
        # additional columns after the periods

        # read header ----
        # read the header, comment_header, and useless.last.column from f and
        # assign them to the environment
        foo <- read_mif_header(f, sep, comment)
        header              <- foo$header
        comment_header      <- foo$comment_header
        useless.last.column <- foo$useless.last.column
        if (is.null(sep)) sep <- foo$sep

        # FIXME: relax to handle other than 4-digit periods
        period.columns <- grep("^[A-Za-z]?[0-9]{4}$", header)

        if (!all(header[1:5] %in% default.columns))
            stop("missing default columns in header of file ", f, ": ",
                 paste(setdiff(default.columns, header[1:5]), collapse = ", "))

        if (length(period.columns) == 0) {
            warning("No column name found that could be understood as a 4-digit year in file ", f,
                    ". Returning empty data.frame.")
            return(as.quitte(NULL))
        }

        if (last(period.columns) != length(header))
            stop("unallowed extra columns in header of file ", f)

        if (!all(seq_range(range(period.columns)) == period.columns))
            stop("period columns not all in one block in header of file ", f)

        periods <- header[period.columns]

        colClasses <- paste(c(rep('c', period.columns[1] - 1),
                              rep('n',   length(period.columns)),
                              ifelse(useless.last.column, '-', '')),
                            collapse = '')

        # read data ----

        # the callback function accepts a chunk of data, `x`, pivots the periods
        # to long format (dropping NAs if required), converts the periods to
        # integer or POSIXct values as required, and applies the
        # `filter.function`.
        chunk_callback <- DataFrameCallback$new(
            (function(FilterF, convert.periods, drop.na) {

                function(x, pos) {
                    if ('problems' %in% names(attributes(x))) {
                        p <- problems(x)
                    }
                    else {
                        p <- NULL
                    }

                    x <- x %>%
                        relocate(all_of(default.columns)) %>%
                        # convert to long format
                        pivot_longer(all_of(periods), names_to = 'period',
                                     values_drop_na = drop.na) %>%
                        # convert periods
                        mutate(period = gsub('^[A-Za-z]?', '', .data$period),
                               period = if (convert.periods) {
                                   ISOyear(.data$period)
                               } else {
                                   as.integer(as.character(.data$period))
                               }) %>%
                        # apply filter
                        FilterF()

                    if (!is.null(p))
                        attr(x, 'problems') <- p

                    return(x)
                }
            })(filter.function, convert.periods, drop.na)
        )

        data <- suppressWarnings(
            read_delim_chunked(
                file = f, callback = chunk_callback, delim = sep,
                chunk_size = chunk_size, quote = quote, col_names = header,
                col_types = colClasses, na = na.strings, comment = comment,
                trim_ws = TRUE, skip = length(comment_header) + 1)
        )

        # catch any parsing problems
        data_problems <- if (nrow(problems(data))) {
            problems(data) %>%
                mutate(file = f)
        }

        # re-attach parsing problems
        attr(data, 'problems') <- data_problems
        attr(data, 'comment_header') <- comment_header

        return(data)
    }

    # read quitte for all supplied files ----
    quitte <- tibble()
    quitte_problems <- tibble()
    comment_header <- list()
    for (f in file) {
        data <- .read.quitte(f, sep, quote, na.strings, convert.periods,
                             drop.na, comment, filter.function, chunk_size)
        quitte <- bind_rows(quitte, data)
        quitte_problems <- bind_rows(quitte_problems, attr(data, 'problems'))
        comment_header <- c(comment_header,
                            setNames(list(attr(data, 'comment_header')), f))
    }

    if (factors) {
        quitte <- quitte %>%
            # preserve order of scenarios for order of .mif files
            mutate(scenario = as_factor(.data$scenario)) %>%
            factor.data.frame()
    }

    # check for duplicate entries, ignoring values
    if (check.duplicates) {
        reportDuplicates(quitte, action = "warn")
    }

    # apply quitte "class" attribute
    class(quitte) <- c('quitte', class(quitte))

    # pass parsing problems along and issue warning
    attr(quitte, 'problems') <- if (nrow(quitte_problems)) {
        quitte_problems
    }

    if (nrow(quitte_problems)) {
        if (interactive()) {
            warning('One or more parsing issues, call `readr::problems()` for ',
                    'details.',
                    call. = FALSE)
        } else {
            warning.length <- getOption('warning.length')
            options(warning.length = 8170)
            warning('One or more parsing issues:\n',
                    quitte_problems %>%
                        as.data.frame() %>%
                        format() %>%
                        capture.output() %>%
                        paste(collapse = '\n'))
            options(warning.length = warning.length)
        }
    }

    # pass comment_header along as attribute, if any
    if (!is_empty(unlist(comment_header))) {
        if (1 == length(file)) {
            comment_header <- unlist(comment_header, use.names = FALSE)
        }
        attr(quitte, 'comment_header') <- comment_header
    }

    return(quitte)
}
