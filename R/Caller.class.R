#' Methods for R6Caller object
#' @rdname R6Caller
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

#' Methods for Caller object
#' @rdname Caller
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

#' @rdname Caller
#' @method $ Caller
#' @export
`$.Caller` <- function(x, y){
  env <- environment(x)
  # obj <- get("x", envir = environment(x))
  # y <- match(y, names(obj), nomatch = NULL)
  # `[[`(obj, names(obj)[y])
  get(y, envir=env)
}

#' @rdname Caller
#' @method $<- Caller
#' @export
`$<-.Caller` <- function(x, y, value){
  env <- environment(x)
  assign(y, value, envir = env)
  x
}

#' @rdname Caller
#' @method [ Caller
#' @export
`[.Caller` <- function(x, y, ...){
  env <- environment(x)
  `[`(env, y, ...)
}

#' @rdname Caller
#' @method [<- Caller
#' @export
`[<-.Caller` <- function(x, y, ..., value){
  env <- environment(x)
  `[<-`(env, y, ..., value)
  x
}

#' @rdname Caller
#' @method [[ Caller
#' @export
`[[.Caller` <- function(x, y,...){
  env <- environment(x)
  `[[`(env, y, ...)
}

#' @rdname Caller
#' @method [[<- Caller
#' @export
`[[<-.Caller` <- function(x, y, ..., value){
  env <- environment(x)
  `[[<-`(env, y, ..., value)
  x
}

#' @rdname Caller
#' @method names Caller
#' @export
names.Caller <- function(x){
  ls(environment(x), all.names = !getOption("hide.dots", default = TRUE))
}

#' @rdname Caller
#' @method names<- Caller
#' @export
`names<-.Caller` <- function(x, value){
  obj <- environment(x)
  names(obj) <- value
  x
}

#' @rdname Caller
#' @method with Caller
#' @details
#' The \code{with} method provides a convenient way to expose the inner environment that Caller points to,
#'  without the need to de-callable it.
#' This is mainly used for the case where subsetting methods are sufficient
#' @export
with.Caller <- function(x, expr, ...){
  eval(substitute(expr), envir=environment(x), enclos = parent.frame())
}

#' De-callable a caller
#' @description This function decalls a Caller object, exposing what it hides
#' @param x a caller
#' @return a de-callable object
#' @export
decallable <- function(x){
  UseMethod("decallable")
}

#'@export
decallable.Caller <- function(x){
  environment(x)
}

#' @rdname Caller
#' @param all.names default to TRUE, whether to extract all names, see \link[base]{as.list.environment}
#' @method as.list Caller
#' @export
as.list.Caller <- function(x, all.names=TRUE){
  as.list.environment(decallable(x), all.names = all.names)
}
