#' Generate a callable subsettable object
#' @description Function to create a callable object that wrap around x
#' @param x an subsettable object
#' @param call_target the target binding for the caller; must be function.
#' @return a "Caller" object
#' @export
make_callable <- function(x, call_target = ".call"){
  # check the purity of inputs
  if (any(grepl("^$", names(x)))) stop("x must be a named list.")
  if (!mode(x) %in% c("environment", "list")) x <- as.list(x)
  if (!exists(call_target, x)) stop("No call_target found in x.")

  # build the binding environment
  caller_env <- new.env(hash = FALSE)

  # call the make function
  make_callable_env(x, caller_env, call_target = call_target)
}

make_bind_env <- function(x, envir, reset = FALSE){
  if (mode(x) == "list") {
    if (!exists(".__target_env__", envir = envir))
      assign('.__target_env__', new.env(hash = FALSE, parent = emptyenv()), envir = envir)
    if (reset) rm(list=ls(envir$.__target_env__, all.names = TRUE), envir = envir$.__target_env__)
    list2env(x, envir = envir$.__target_env__, parent = envir)
  } else {
    assign('.__target_env__', x, envir = envir)
  }
}

make_caller <- function(x, args, caller_env, call_target){
  # get the call target
  fn_target <- get(call_target, x)
  if (is.character(fn_target)) fn_target <- get(fn_target)
  if (mode(fn_target)!="function") stop("call_target must be function.")
  # copy the arguments from the targetted function
  args <- if (is.primitive(fn_target)) rlang::pairlist2(...=) else rlang::fn_fmls(fn_target)

  caller <- rlang::new_function(
    args,
    rlang::expr({
      args <- as.list(match.call())[-1]
      call_target <-
        if (length(attr(sys.function(), "call_target"))) attr(sys.function(), "call_target")
      else !!{{call_target}}
      target_env <- environment(sys.function())$.__target_env__
      call_fn <- get(call_target, target_env)
      do.call(call_fn, args)
    }), env = caller_env)
  class(caller) <- c(paste(class(x), "Caller", sep="_"), "Caller")
  attr(caller, "call_target") <- call_target
  caller
}

make_callable_env <- function(x, caller_env, call_target = attr(x, "call_target"), reset = FALSE){
  # -- if x is list, expose x to a new environment (list2env)
  # -- otherwise set x as target
  make_bind_env(x, caller_env, reset = reset)

  # create the caller
  make_caller(x, args, caller_env, call_target)
}
