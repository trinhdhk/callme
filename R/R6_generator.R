#' R6Class - just callable
#' @description An extension to R6::R6Class, make it callable
#' @param classname,public,private,active,inherit,lock_objects,class,portable,lock_class,cloneable See \link[R6]{R6Class} for more details
#' @param ... everything else passed to R6::R6Class
#' @param callable_object_target if is NULL, R6 object created is not callable. Default will point to a public method name ".call". Be assured that the object created will have that target set as public method, otherwise will return an error when trigger.
#' @return A function of class ["R6Caller", "Caller"] which itself points to an <R6ClassGenerator>
#' @export
R6CallClass <-
  function(classname = NULL,
           public = list(), private = NULL, active = NULL,
           inherit = NULL, lock_objects = TRUE, class = TRUE,
           portable = TRUE, lock_class = FALSE, cloneable = TRUE,
           # parent_env = parent.frame(),
           callable_object_target = ".call",...){

    uncallable_generator <-
      R6::R6Class(classname = classname,
                  public = public,
                  private = private,
                  active = active,
                  inherit = inherit,
                  lock_objects = lock_objects,
                  class = class,
                  portable = portable,
                  lock_class = lock_class,
                  cloneable = cloneable,
                  # parent_env = parent_env,
                  ...)

    # browser()
    if (length(callable_object_target) >= 1){
      if (length(callable_object_target) > 1){
        warning("Multiple targets found! First one is used.")
        callable_object_target <- callable_object_target[[1]]
      }
      uncallable_generator <-
        make_callable_object_methods(uncallable_generator,
                                     call_target = callable_object_target)
    }
    callable_generator <- make_callable(uncallable_generator, "new")
    class(callable_generator) <- c(
      "R6Caller",
      class(callable_generator))
    callable_generator
  }

make_callable_object_methods <- function(R6ClassGenerator, call_target = ".call"){
  stopifnot(inherits(R6ClassGenerator, "R6ClassGenerator"))
  is_lock <- bindingIsLocked("new", R6ClassGenerator)
  if (is_lock) unlockBinding("new", R6ClassGenerator)
  R6ClassGenerator$decallable_new <- R6ClassGenerator$new
  callable_new <-
    rlang::new_function(
      rlang::pairlist2(...=),
      rlang::expr(
        {
          uncallable <- decallable_new(...)
          make_callable_clone_method <- !!{{make_callable_clone_method}}
          make_callable <- !!{{make_callable}}
          uncallable <-
            make_callable_clone_method(uncallable,
                                       call_target = !!{{call_target}})
          callable <- make_callable(uncallable, call_target = !!{{call_target}})
          class(callable) <- c(
            class(uncallable)[-length(class(uncallable))],
            "R6Caller", "Caller",
            class(uncallable)[length(class(uncallable))]
          )
          callable
        }
      ),
      env = environment(R6ClassGenerator$decallable_new)
    )

  R6ClassGenerator$new <- callable_new
  if (is_lock) lockBinding("new", R6ClassGenerator)
  R6ClassGenerator
}

make_callable_clone_method <- function(R6Object, call_target = ".call"){
  stopifnot(inherits(R6Object,"R6"))
  is_lock <- rlang::env_is_locked(R6Object)
  if (is_lock) rlang::env_unlock(R6Object)
  unlockBinding("clone", R6Object)
  R6Object$decallable_clone <- R6Object$clone
  callable_clone <-
    rlang::new_function(
      args = rlang::pairlist2(...=),
      rlang::expr({
        uncallable <- self$decallable_clone(...)
        callable <- make_callable(uncallable, call_target = !!{{call_target}})

        class(callable) <- c(
          class(uncallable)[-length(class(uncallable))],
          "R6Caller", "Caller",
          class(uncallable)[length(class(uncallable))]
        )
        callable
      }),
      env = environment(R6Object$decallable_clone)
    )
  R6Object$clone <- callable_clone
  lockBinding("clone", R6Object)
  if (is_lock) rlang::env_lock(R6Object)
  R6Object
}
