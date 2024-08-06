test_that("'datasets()' works with small size and default arguments", {
  test_datasets <- datasets(size = 20)
  expect_true(tibble::is_tibble(test_datasets))
  expect_true(nrow(test_datasets) > 0L)
  expect_true(all(names(test_datasets) %in% datasets_default_columns("character")))
})

test_that("'dataset_detail()' works for specific dataset", {
  test_uuid <- datasets(size = 100, from = 88) |>
    utils::head(1) |>
    dplyr::pull("uuid")
  test_dataset <- dataset_detail(test_uuid)
  expect_true(tibble::is_tibble(test_dataset))
  expect_true(nrow(test_dataset) == 1L && ncol(test_dataset) >= 1L)
  expect_equal(test_dataset$uuid, test_uuid)
})

test_that("'dataset_detail()' works for uuid that does not exist", {
  test_uuid <- "feaaaaaf2bbbbb26ccccccb12ddd3a82"
  test_dataset <- dataset_detail(test_uuid)
  expect_true(tibble::is_tibble(test_dataset))
  expect_true(nrow(test_dataset) == 0L)
})

test_that("'datasets_default_columns()' works", {
  
  ## testing tibble output
  tbl <- datasets_default_columns()
  expect_true(tibble::is_tibble(tbl))
  expect_true(nrow(tbl) > 0L && all("columns" %in% names(tbl)))
})

test_that("'dataset_derived()' works", {
  
  # derived dataset yes
  test_uuid <- "2c77b1cdf33dbed3dbfb74e4b578300e"
  tbl <- dataset_derived(test_uuid)
  expect_true(tibble::is_tibble(tbl))
  expect_true(nrow(tbl) > 0L)
  
  
})
