
f <- function(data, ...) {
    eval(substitute(alist(...)))
}
