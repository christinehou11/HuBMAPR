test_that("'.query_entity()' works", {
  test <- .query_entity("Dataset", size = 10, from = 2)
  expect_true(tibble::is_tibble(test))
})

test_that("'.query_match()' works", {
  uuid <- "e695c48cb6be99479daefaf372743f65"
  test <- .query_match(uuid, "hits.hits[]._source")
  expect_true(tibble::is_tibble(test))
  expect_true(nrow(test) == 1L && ncol(test) >= 1L)
})

test_that("'.default_columns()' works", {
  test1 <- .default_columns("Dataset")
  test2 <- .default_columns("Sample")
  expect_true(tibble::is_tibble(test1) &&
                tibble::is_tibble(test2))
  expect_true(ncol(test1) >= 1L && nrow(test1) == 13L)
  expect_true(ncol(test2) >= 1L && nrow(test2) == 7L)
  expect_true("organ" %in% test1$columns && "organ" %in% test2$columns)
  
  test3 <- .default_columns("Donor", "character")
  expect_true(is.character(test3))
  expect_true(length(test3) == 8L)
})