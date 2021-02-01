#' Generate a callable subsettable object
#' @description Function to create a callable object that wrap around x
#' @param x an subsettable object
#' @param call_target the target binding for the caller; must be function.
#' @return a "Caller" object
#' @export
make_callable <- function(x, call_target = ".call", self){
  if (any(grepl("^$", names(x)))) stop("x must be a named list.")
  if (!exists(call_target, x)) stop("No call_target found in x.")

  # get the call target
  fn_target <- get(call_target, x)
  if (is.character(fn_target)) fn_target <- get(fn_target)
  if (mode(fn_target)!="function") stop("call_target must be function.")
  args <- if (is.primitive(fn_target)) rlang::pairlist2(...=) else rlang::fn_fmls(fn_target)

  # build the wrapper
  envs <- set_caller_env(x, fn_target)
  caller <-
    rlang::new_function(
      args,
      rlang::expr({
        `%||%` <- rlang::`%||%`
        args <- as.list(match.call())[-1]
        target_env <- attr(sys.function(), "target_env") %||% !!{{envs$target_env}}
        call_fn <- get(!!{{call_target}}, target_env)
        do.call(call_fn, args)
      }), env = envs$caller_env)

  class(caller) <- c(paste(class(x),"Caller", sep="_"), "Caller")
  attr(caller, "origin_state") <- list(attr = attributes(x), mode = mode(x))
  attr(caller, "call_target") <- call_target
  attr(caller, "target_env") <- envs$target_env
  caller
}

#' Helper methods to derive the suitable function
#' @param x the object
#' @param fn_target Target function. Must have in each method.
set_caller_env <- function(x, fn_target){
  UseMethod("set_caller_env")
}

#' @method set_caller_env list
set_caller_env.list <- function(x, ...){
  env <- list2env(x)
  list(target_env = env, caller_env = env)
}

#' @method set_caller_env environment
#' @method set_caller_env R6
#' @method set_caller_env R6ClassGenerator
set_caller_env.environment <-
  set_caller_env.R6 <-
  set_caller_env.R6ClassGenerator <-
  function(x, fn_target){
  list(
    target_env = environment(fn_target),
    caller_env = x
  )
}


