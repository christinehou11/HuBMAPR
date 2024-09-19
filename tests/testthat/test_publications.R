test_that("'publications()' works", {
  test_publications <- collections()
  expect_true(tibble::is_tibble(test_publications))
  expect_true(nrow(test_publications) > 0L)
  expect_true(all(names(test_publications) %in% publications_default_columns("character")))
})

test_that("'publications_default_columns()' works", {

  ## testing tibble output
  tbl <- publications_default_columns()
  expect_true(tibble::is_tibble(tbl))
  expect_true(nrow(tbl) > 0L && all("columns" %in% names(tbl)))
})

test_that("'publication_authors()', 'publication_information()' and 'publication_data()' work with specific uuid", {
  test_uuid <- "3c7273660cdf9ab91a7901533b2cd9a5"

  # authors
  test_authors <- publication_authors(test_uuid)
  expect_true(tibble::is_tibble(test_authors))
  expect_true(nrow(test_authors) > 0L)
  
  # info
  test_info <- publication_information(test_uuid)
  expect_true(is(test_info, "publication_information"))
  expect_output(print(test_info))
  
  # datasets
  test_datasets <- publication_data(test_uuid)
  expect_true(tibble::is_tibble(test_datasets))
  expect_true(nrow(test_datasets) > 0L)
})
