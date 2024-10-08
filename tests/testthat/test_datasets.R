test_that("'datasets()' works", {
  test_datasets <- datasets()
  expect_true(tibble::is_tibble(test_datasets))
  expect_true(nrow(test_datasets) > 0L)
  expect_true(all(names(test_datasets) %in% datasets_default_columns("character")))
})

test_that("'dataset_detail()' works for specific dataset", {
  test_uuid <- datasets() |>
    utils::head(1) |>
    dplyr::pull("uuid")
  test_dataset <- dataset_detail(test_uuid)
  expect_true(tibble::is_tibble(test_dataset))
  expect_true(nrow(test_dataset) == 1L && ncol(test_dataset) >= 1L)
  expect_equal(test_dataset$uuid, test_uuid)
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

  # derived dataset no
  test_uuid <- "3acdb3ed962b2087fbe325514b098101"
  tbl <- dataset_derived(test_uuid)
  expect_true(is.null(tbl))
})

test_that("'dataset_metadata()' works", {

  test_uuid <- "993bb1d6fa02e2755fd69613bb9d6e08"
  tbl <- dataset_metadata(test_uuid)
  expect_true(tibble::is_tibble(tbl))
  expect_true(nrow(tbl) > 0L &&
                all(c("Key", "Value") %in% names(tbl)))
  
})