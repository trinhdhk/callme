#' Generate a callable subsettable object
#' @description Function to create a callable object that wrap around x
#' @param x an subsettable object
#' @param call_target the target binding for the caller; must be function.
#' @return a "Caller" object
#' @export
make_callable <- function(x, call_target = ".call"){
  if (any(grepl("^$", names(x)))) stop("x must be a named list.")
  if (!exists(call_target, x)) stop("No call_target found in x.")

  # get the call target
  fn_target <- get(call_target, x)
  if (is.character(fn_target)) fn_target <- get(fn_target)
  if (mode(fn_target)!="function") stop("call_target must be function.")
  args <- if (is.primitive(fn_target)) rlang::pairlist2(...=) else rlang::fn_fmls(fn_target)

  # build the wrapper
  if (mode(x) != "environment") caller_env <- list2env(x)
  else {
    caller_env <- rlang::env_clone(x, rlang::current_env())
    class(caller_env) <- class(x)
  }
  # browser()
  caller <-
    rlang::new_function(
      args,
      rlang::expr({
        args <- as.list(match.call())[-1]
        call_fn <- get(!!{{call_target}}, rlang::current_env())
        # call_fn <- get(attr(sys.function(), "call_target"), environment(sys.function()))
        do.call(call_fn, args)
      }), env = caller_env)

  class(caller) <- c("Caller", class(x))
  attr(caller, "origin_state") <- list(attr = attributes(x), mode = mode(x))
  attr(caller, "call_target") <- call_target
  caller
}





