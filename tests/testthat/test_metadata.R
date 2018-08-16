context("metadata")

load(system.file('tests/testthat/test-files/test1.rda', package = "EngeMetadata"))

test_that("check that .processKeys outputs the expected result", {
  expect_identical(.processKeys(c("A.x", "A.y")), "A")
})

test_that("check that .layout outputs the expected result", {
  expect_true(is_tibble(.layout(384)))
})

test_that("check that .resolve works with nothing to resolve", {
  #setup input data
  wells <- pull(meta[[3]], wells_in_plate)
  layout <- .layout(wells)
  base <- map_dfr(1:nrow(layout), function(x) meta[[3]]) %>%
    bind_cols(layout, .)
  bind1 <- full_join(base, meta[[2]], by = "Column")
  keys1 <- .processKeys(colnames(bind1))
  
  
  #expected
  expected <- data.frame()
  
  #output
  output <- map_dfc(keys1,  ~.resolve(bind1, .x))
  
  #test
  expect_identical(expected, output)
})

test_that("check that .resolve works with something to resolve", {
  #setup input data
  wells <- pull(meta[[3]], wells_in_plate)
  layout <- .layout(wells)
  base <- map_dfr(1:nrow(layout), function(x) meta[[3]]) %>%
    bind_cols(layout, .)
  resolved1 <- full_join(base, meta[[2]], by = "Column")
  bind2 <- full_join(resolved1, meta[[1]], by = "Well")
  keys2 <- .processKeys(colnames(bind2))
  
  #expected
  expected.sc.mult <- 48L
  expected.sc.new <- 8L
  expected.sc.sng <- 120L
  expected.sc.NA <- 208L
  expected.oc.X <- 61L
  expected.oc.Y <- 96L
  expected.oc.Z <- 3L
  expected.oc.NA <- 224L
  
  #output
  output <- map_dfc(keys2,  ~.resolve(bind2, .x))
  output.sc <- table(output$Sort_condition, useNA = "ifany")
  output.oc <- table(output$other_condition, useNA = "ifany")
  
  #test
  expect_identical(unname(output.sc[1]), expected.sc.mult)
  expect_identical(unname(output.sc[2]), expected.sc.new)
  expect_identical(unname(output.sc[3]), expected.sc.sng)
  expect_identical(unname(output.sc[4]), expected.sc.NA)
  
  expect_identical(unname(output.oc[1]), expected.oc.X)
  expect_identical(unname(output.oc[2]), expected.oc.Y)
  expect_identical(unname(output.oc[3]), expected.oc.Z)
  expect_identical(unname(output.oc[4]), expected.oc.NA)
})
