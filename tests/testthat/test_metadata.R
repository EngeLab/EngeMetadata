context("testDatadata")

test_that("check that .processKeys outputs the expected result", {
  expect_identical(.processKeys(c("A.x", "A.y")), "A")
})

test_that("check that .layout outputs the expected result", {
  expect_true(is_tibble(.layout(384)))
  expect_true(is_tibble(.layout(96)))
})

test_that("check that .resolve works with nothing to resolve", {
  #setup input data
  wells <- pull(testData[[3]], wells_in_plate)
  layout <- .layout(wells)
  base <- map_dfr(1:nrow(layout), function(x) testData[[3]]) %>%
    bind_cols(layout, .)
  bind1 <- full_join(base, testData[[2]], by = "Column")
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
  wells <- pull(testData[[3]], wells_in_plate)
  layout <- .layout(wells)
  base <- map_dfr(1:nrow(layout), function(x) testData[[3]]) %>%
    bind_cols(layout, .)
  resolved1 <- full_join(base, testData[[2]], by = "Column")
  bind2 <- full_join(resolved1, testData[[1]], by = "Well")
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
  
  expect_identical(.resolve(bind2, list()), bind2)
})

test_that("check that checkMeta errors when expected", {
  #check that unique_key exists
  tmp <- list(testData[[1]], testData[[2]], select(testData[[3]], -unique_key))
  expect_error(checkMeta(tmp), regexp = "unique_key key is missing from Plate sheet.")
  
  #check that file name matches unique key
  tmp <- testData
  names(tmp) <- rep("A", length(tmp))
  expect_error(checkMeta(tmp), regexp = "File name and unique_key do not match.")
  
  #check that the wells_in_plate variable is present in the Plate sheet
  tmp <- list(testData[[1]], testData[[2]], select(testData[[3]], -wells_in_plate))
  names(tmp) <- rep("test1", 3)
  expect_error(checkMeta(tmp), regexp = "wells_in_plate key missing from Plate sheet")
  
  #check that wells_in_plate is wither 384 or 96
  tmp <- list(testData[[1]], testData[[2]], mutate(testData[[3]], wells_in_plate = 12))
  names(tmp) <- rep("test1", 3)
  expect_error(checkMeta(tmp), "wells_in_plate key must equal 384 or 96")
  
  #check that the Column key in the Columns sheet is present.
  tmp <- list(testData[[1]], select(testData[[2]], -Column), testData[[3]])
  names(tmp) <- rep("test1", 3)
  expect_error(checkMeta(tmp), regexp = "The Column key in the Columns sheet is missing.")
  
  #check that the Column key in the Columns sheet is correct.
  tmp <- list(testData[[1]], mutate(testData[[2]], Column = c(NA, Column[-1])), testData[[3]])
  names(tmp) <- rep("test1", 3)
  expect_error(checkMeta(tmp), regexp = "The Column key in the Columns sheet is malformated.*")
  
  #check that the Well key in the Wells sheet is present.
  tmp <- list(select(testData[[1]], -Well), testData[[2]], testData[[3]])
  names(tmp) <- rep("test1", 3)
  expect_error(checkMeta(tmp), regexp = "The Well key in the Wells sheet is missing.")
  
  #check that the Wells key in the Wells sheet is correct.
  tmp <- list(mutate(testData[[1]], Well = c(NA, Well[-1])), testData[[2]], testData[[3]])
  names(tmp) <- rep("test1", 3)
  expect_error(checkMeta(tmp), regexp = "The Well key in the Wells sheet is malformated.*")
})

test_that("check that .datesFormat outputs the expected result", {
  expect_is(pull(testData[[3]], sample_recieve_date), "Date")
  
  #no date present
  tmp <- list(testData[[1]], testData[[2]], select(testData[[3]], -sample_recieve_date))
  expect_identical(tmp, .datesFormat(tmp))
  
  #malformatted dates
  tmp <- list(testData[[1]], testData[[2]], mutate(testData[[3]], sample_recieve_date = "2018102"))
  expect_warning(.datesFormat(tmp))
  
  tmp <- list(testData[[1]], testData[[2]], mutate(testData[[3]], sample_recieve_date = "2018-20-02"))
  expect_warning(.datesFormat(tmp))
})

test_that("check that resolvePlateMeta outputs the expected result", {
  output <- resolvePlateMeta(testData)
  expect_true(is.tibble(output))
  
  #check that all conflicts are resolved
  expect_false(all(str_detect(colnames(output), "\\.x$")))
  expect_false(all(str_detect(colnames(output), "\\.y$")))
  
  #check all columns present
  expected <- map(testData, colnames) %>% unlist() %>% unique()
  expect_true(all(expected %in% colnames(output)))
  
  #check dimensions
  expect_identical(nrow(output), 384L)
  expect_identical(ncol(output), 15L)
})
