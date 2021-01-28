#' MyCallClass <- R6CallClass(classname = "MyCallClass",
#'                            public = list(
#'                              initialize = function(){
#'                                cat("Hello")
#'                                self
#'                              },
#'                              # print = function() {
#'                              #   cat("I am R6Call")
#'                              #   invisible(self)
#'                              # },
#'                              call = function(x){
#'                                cat("hello", x)
#'                                invisible(self)
#'                              }
#'                            ), callable_object_target = "call")
#'
#' MyCallClass2 <- R6CallClass(classname = "MyCallClass",
#'                        public = list(
#'                          initialize = function(){
#'                            cat("Hello")
#'                            self
#'                          },
#'                          print = function() {
#'                            cat("I am R6Call")
#'                            invisible(self)
#'                          },
#'                          call = function(x){
#'                            cat("hello", x)
#'                            invisible(self)
#'                          }
#'                        ))
#'
#' MyCallClass
#' mycallobj = MyCallClass()
#' mycallobj$clone()
#' mycallobj(3)
#' mycallobj$x
#'
#' R6:::generator_funs$clone_method
#'
#' #' Generate R6 callable Generator
#' #'
#'
#' print.R6callGenerator <- function(x, ...){
#'   R6_obj <- get("R6_generator", envir = environment(x))
#'   print(R6_obj, ...)
#'   invisible(x)
#' }
