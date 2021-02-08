test_that("callable methods works", {
  l <- list(o=3, r=5, d=4, e=1, r2=list(0), .call=print)
  mycall <- make_callable(l)
  env1 <- environment(mycall)$.__target_env__
  options(hide.dots = T)
  expect_equal(names(mycall), c("o", "r", "d", "e", "r2"))
  expect_equal(as.list(mycall), l)
  expect_equal(mycall[[1]], mycall$o)
  expect_equal(mycall[1], list(o = 3))
  mycall[[1]] <- "a"
  expect_equal(mycall[[1]], "a")
  env2 <- environment(mycall)$.__target_env__
  mycall[1] <- list(d="3")
  expect_equal(mycall[[1]], "3")
  names(mycall)[1] <- "o"
  expect_equal(names(mycall), c("o", "r", "d", "e", "r2"))
  env3 <- environment(mycall)$.__target_env__
  expect_identical(env1, env2)
  expect_identical(env1, env3)
})
