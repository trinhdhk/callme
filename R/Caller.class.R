#' Methods for R6Caller object
#' @rdname R6_Caller
#' @method print R6_Caller
#' @export
print.R6_Caller <- function(x,...){
  has_print_method <- is.function(target(x)$print)
  if (has_print_method) print(target(x), ...)
  else {
    cat("Callable() R6 object\n")
    print(target(x), ...)
  }
  invisible(x)
}

#' Methods for Caller object
#' @rdname Caller
#' @method print Caller
#' @export
print.Caller <- function(x, hide.dots=getOption("hide.dots", default = TRUE), ...){
  # obj <- get("x", envir = environment(x))
  cat("Callable object targetting ")
  print(target(x), ...)
  obj <- as.list(x, all.names = !hide.dots)
  print(obj)
  invisible(x)
}

#' @rdname Caller
#' @method $ Caller
#' @export
`$.Caller` <- function(x, y){
  env <- target(x)
  y <- grep(paste0("^",y), ls(env, all.names = TRUE), value = TRUE)
  get(y, envir=env)
}

#' @rdname Caller
#' @method $<- Caller
#' @export
`$<-.Caller` <- function(x, y, value){
  env <- target(x)
  assign(y, value, envir = env)
  x
}

#' @rdname Caller
#' @method [ Caller
#' @export
`[.Caller` <- function(x, y, ...){
  # env <- environment(x)
  `[`(as.list(x), y, ...)
}

#' @rdname Caller
#' @method [<- Caller
#' @export
`[<-.Caller` <- function(x, y, ..., value){
  make_callable_env(`[<-`(as.list(x), y, ..., value),
                    caller_env = environment(x),
                    call_target = call_target(x),
                    reset = TRUE)
}

#' @rdname Caller
#' @method [[ Caller
#' @export
`[[.Caller` <- function(x, y,...){
  # env <- environment(x)
  `[[`(as.list(x), y, ...)
}

#' @rdname Caller
#' @method [[<- Caller
#' @export
`[[<-.Caller` <- function(x, y, value){
  # browser()
  env <- target(x)
  names.x <- names(as.list(x))
  for (.y in y){
    if (is.numeric(.y)) .y <- names.x[[.y]]
    assign(.y, value, envir = env)
  }
  x
}

#' @rdname Caller
#' @method names Caller
#' @export
names.Caller <- function(x){
  rev(ls(target(x), all.names = !getOption("hide.dots", default = TRUE), sorted=FALSE))
}

#' @rdname Caller
#' @method names<- list_Caller
#' @export
`names<-.list_Caller` <- function(x, value){
  if (length(value) < length(as.list(x)) && getOption("hide.dots", default = TRUE))
    value <- c(value, call_target(x))
  make_callable_env(`names<-`(as.list(x, all.names = TRUE), value),
                    caller_env  = environment(x),
                    call_target = call_target(x),
                    reset = TRUE)
}

#' @rdname Caller
#' @method names<- Caller
#' @export
`names<-.Caller` <- function(x, value, ...){
  # pass to the method within x
  `names<-`(target(x), value, ...)
}

#' @rdname Caller
#' @method with Caller
#' @details
#' The \code{with} method provides a convenient way to expose the inner environment that Caller points to,
#'  without the need to de-callable it.
#' This is mainly used for the case where subsetting methods are insufficient
#' @export
with.Caller <- function(x, expr, ...){
  eval(substitute(expr), envir=target(x), enclos = parent.frame())
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
  origin <- target(x)
  origin
}

#' @rdname Caller
#' @param all.names default to TRUE, whether to extract all names, see \link[base]{as.list.environment}
#' @method as.list Caller
#' @export
as.list.Caller <- function(x, all.names=TRUE){
  rev(as.list.environment(target(x), all.names = all.names, sorted = FALSE))
}

#' @rdname Caller
#' @export
as.environment.Caller <- function(x){
  target(x)
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
  get("call_target", meta(x))
}

#' @rdname call_target
#' @description get_call_target returns the function itself.
#' @return For \code{get_call_target}: a function
#' @export
get_call_target <- function(x){
  x[[get("call_target", meta(x))]]
}

target <- function(x){
  get(".__target_env__", environment(x))
}

meta <- function(x){
  get(".__meta_lock__", environment(x))
}
