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
print.Caller <- function(x, hide.dots=getOption("hide.dots", default = TRUE), ...){
  # obj <- get("x", envir = environment(x))
  cat("Callable object at ")
  print(environment(x), ...)
  obj <- as.list.environment(environment(x), all.names = !hide.dots)
  print(obj)
  invisible(x)
}

#' @rdname Caller
#' @method $ Caller
#' @export
`$.Caller` <- function(x, y){
  env <- environment(x)
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
  `[<-`(env, y, ..., value=value)
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
  `[[<-`(env, y, ..., value=value)
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
#' This is mainly used for the case where subsetting methods are insufficient
#' @export
with.Caller <- function(x, expr, ...){
  eval(substitute(expr), envir=environment(x), enclos = parent.frame())
}

#' De-callable a caller
#' @description This function decalls a Caller object, exposing what it hides
#' @param x a caller
#' @return a list or environment
#' @export
decallable <- function(x, ...){
  UseMethod("decallable")
}

#'@export
decallable.Caller <- function(x){
  if (attr(x, "origin_state")$mode=="list") decall_to_list(x)
  else decall_to_environment(x)
}

decall_to_list <- function(x){
  origin <- as.list.Caller(x, all.names = TRUE)
  class(origin) <- class(x)[-1]
  mode(origin) <- attr(x, "origin_state")$mode
  attributes(origin) <- attr(x, "origin_state")$attr
  origin
}

decall_to_environment <- function(x, ...){
  origin <- environment(x)
  origin
}

#' @rdname Caller
#' @param all.names default to TRUE, whether to extract all names, see \link[base]{as.list.environment}
#' @method as.list Caller
#' @export
as.list.Caller <- function(x, all.names=TRUE){
  as.list.environment(environment(x), all.names = all.names)
}

#' @rdname Caller
#' @export
as.environment.Caller <- function(x){
  environment(x)
}

#' @rdname Caller
#' @method as.data.frame Caller
#' @export
as.data.frame.Caller <- function(x, ...){
  as.data.frame(as.list(x), ...)
}

#' Get call target from a Caller
#' @rdname call_target
#' @description call_target returns the name of the targetted function.
#' @param x an object of class "Caller"
#' @return For \code{call_target}: a character object
#' @export
call_target <- function(x){
  attr(x, "call_target")
}

#' @rdname call_target
#' @description get_call_target returns the function itself.
#' @return For \code{get_call_target}: a function
#' @export
get_call_target <- function(x){
  x[[attr(x, "call_target")]]
}
