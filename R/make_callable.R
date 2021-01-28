#' Generate a callable subsettable object
#' @description Function to create a callable object that wrap around x
#' @param x an subsettable object
#' @param call_target the target binding for the caller; must be function.
#' @return a "Caller" object
#' @export
make_callable <- function(x, call_target = ".call"){
  if (!exists(call_target, x)) stop("No call_target found in x.")

  # get the call target
  fn_target <- get(call_target, x)
  if (mode(fn_target)!="function") stop("call_target must be function.")

  args <- rlang::fn_fmls(fn_target)

  # build the wrapper
  # names(x[call_target]) <- ".call"


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
        call_fn <- !!{{fn_target}}
        # call_fn <- get(attr(sys.function(), "call_target"), environment(sys.function()))
        do.call(call_fn, args)
      }), env = caller_env)

  class(caller) <- c("Caller", class(x))
  attr(caller, "call_target") <- call_target
  caller
}





