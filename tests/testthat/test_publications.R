test_that("'publications()' works with small size and default arguments", {
  test_publications <- collections(size = 3)
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

test_that("'publication_authors()' works with specific uuid", {
  test_uuid <- "3c7273660cdf9ab91a7901533b2cd9a5"
  
  # authors
  test_authors <- publication_authors(test_uuid)
  expect_true(tibble::is_tibble(test_authors))
  expect_true(nrow(test_authors) > 0L)
  
})