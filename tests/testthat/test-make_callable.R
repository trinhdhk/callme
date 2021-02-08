test_that("make_callable is okay for list and environment", {
  # only named list is allowed
  expect_error(make_callable(c(3, 4, .callme=5)))
  expect_error(make_callable(c(3, 4, .callme=print)))

  # custom call_target
  x <- list(a=3, b=4, .callme=print)
  expect_error(make_callable(x))
  call_list <- make_callable(x, ".callme")
  call_env  <- make_callable(list2env(x), call_target = ".callme")
  expect_equal(call_list("4"), call_env("4"))
  expect_invisible(call_list("4"))
  expect_invisible(call_env("4"))
  options(hide.dots = T)
  expect_named(call_list, c("a", "b"))
  expect_named(call_env, c("a", "b"))
  options(hide.dots = F)
  expect_named(call_list, c("a", "b", ".callme"))
  expect_named(call_env, c("a", "b", ".callme"))
})

test_that("R6CallClass is okay", {
  expect_error(
    MyCallClass <- R6CallClass(classname = "MyCallClass",
                               public = list(
                                 initialize = function(seed) {
                                   set.seed(private$.seed <- seed)
                                   private$.secret <- rnorm(1,0,1)
                                 },
                                 print = function() print("I am printed as R6"),
                                 .callme = function() {
                                   writeLines("I am called")
                                   writeLines("And this is my private:")
                                   print(private$.secret)
                                 }
                               ),
                               private = list(
                                 .secret = numeric(), .seed = integer()
                               ),
                               active = list(
                                 seed = function() private$.seed
                               ))
  )

  MyCallClass <- R6CallClass(classname = "MyCallClass",
                             public = list(
                               initialize = function(seed) {
                                 set.seed(private$.seed <- seed)
                                 private$.secret <- rnorm(1,0,1)
                                 },
                               print = function() print("I am printed as R6"),
                               .callme = function() {
                                 private$.secret
                               }
                             ),
                             private = list(
                               .secret = double(), .seed = integer()
                             ),
                             active = list(
                               seed = function() private$.seed
                             ),
                             callable_object_target = ".callme")
  mycallobj <- MyCallClass(1920)
  expect_identical(mycallobj$seed, 1920)
  expect_type(mycallobj(), "double")
  expect_identical(mycallobj(), mycallobj$.callme())
})
