test_that("'collections()' works with small size and default arguments", {
  test_collections <- collections(size = 3, from = 3)
  expect_true(tibble::is_tibble(test_collections))
  expect_true(nrow(test_collections) > 0L)
  expect_true(all(names(test_collections) %in% collections_default_columns("character")))
})

test_that("'collections_default_columns()' works", {
  
  ## testing tibble output
  tbl <- collections_default_columns()
  expect_true(tibble::is_tibble(tbl))
  expect_true(nrow(tbl) > 0L && all("columns" %in% names(tbl)))
})

test_that("'collection_contacts()', 'collection_contributors()', and 'collection_datasets()' work with specific uuid", {
  test_uuid <- "eb063a0ed8a68117b17345415ab4dd3c"
  
  # contacts
  test_contacts <- collection_contacts(test_uuid)
  expect_true(tibble::is_tibble(test_contacts))
  expect_true(nrow(test_contacts) > 0L)
  
  # datasets
  test_datasets <- collection_datasets(test_uuid)
  expect_true(tibble::is_tibble(test_datasets))
  expect_true(nrow(test_datasets) > 0L)
  
  # contributors
  test_contributors <- collection_contributors(test_uuid)
  expect_true(tibble::is_tibble(test_contributors))
  expect_true(nrow(test_contributors) > 0L) 
})