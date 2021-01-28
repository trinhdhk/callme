#' Methods for R6Caller
#' @rdname R6Class
#' @method print R6Caller
#' @export
print.R6Caller <- function(x,...){
  has_print_method <- is.function(environment(x)$print)
  if (has_print_method) print(environment(x), ...)
  else {
    cat("Callable() R6 object\n")
    print(environment(x), ...)
  }
  invisible(x)
}

#' @method print Caller
#' @export
print.Caller <- function(x, .hide_target=TRUE, ...){
  # obj <- get("x", envir = environment(x))
  cat("Callable object at ")
  print(environment(x), ...)
  obj <- as.list.environment(environment(x), all.names = !.hide_target)
  print(obj)
  invisible(x)
}

#' @export
`$.Caller` <- function(x, y){
  obj <- environment(x)
  # obj <- get("x", envir = environment(x))
  y <- match(y, names(obj), nomatch = NULL)
  `[[`(obj, names(obj)[y])
}

#' @export
`$<-.Caller` <- function(x, y, value){
  obj <- environment(x)
  # obj <- get("x", envir = environment(x))
  assign("x", `[[<-`(obj, y, value), envir = environment(x))
  x
}

#' @export
`[.Caller` <- function(x, ...){
  obj <- environment(x)
  # obj <- get("x", envir = environment(x))
  `[`(obj, ...)
}

#' @export
`[<-.Caller` <- function(x, y, value){
  obj <- environment(x)
  # obj <- get("x", envir = environment(x))
  assign("x", `[<-`(obj, y, value), envir = environment(x))
  x
}

#' @export
`[[.Caller` <- function(x, ...){
  obj <- environment(x)
  # obj <- get("x", envir = environment(x))
  `[[`(obj, ...)
}

#' @export
`[[<-.Caller` <- function(x, ...){
  obj <- environment(x)
  # obj <- get("x", envir = environment(x))
  assign("x", `[[<-`(obj, ...), envir = environment(x))
  x
}

#' @export
names.Caller <- function(x){
  names(environment(x))
}

#' @export
`names<-.Caller` <- function(x, value){
  obj <- environment(x)
  # obj <- get("x", envir = environment(x))
  assign("x", `names<-`(obj, value), envir = environment(x))
}

#' @export
with.Caller <- function(x, expr, ...){
  eval(substitute(expr), envir=environment(x), enclos = parent.frame())
}

#' @export
decallable <- function(x){
  UseMethod("decallable")
}

#'@export
decallable.Caller <- function(x){
  environment(x)
}

#'@export
as.list.Caller <- function(x, all.names=TRUE){
  as.list.environment(decallable(x), all.names = all.names)
}
