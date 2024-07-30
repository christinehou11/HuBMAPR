test_that("'samples()' works with small size and default arguments", {
  test_samples <- samples(size = 4)
  expect_true(tibble::is_tibble(test_samples))
  expect_true(nrow(test_samples) > 0L)
  expect_true(all(names(test_samples) %in% samples_default_columns("character")))
})

test_that("'sample_detail()' work for specific sample", {
  test_uuid <- "038e4fcefc00f7ecc537b5d8462e6404"
  test_sample <- sample_detail(test_uuid)
  expect_true(tibble::is_tibble(test_sample))
  expect_true(nrow(test_sample) == 1L && ncol(test_sample) >= 23L)
  expect_equal(test_sample$uuid, test_uuid)
})

test_that("'sample_detail()' works for uuid that does not exist", {
  test_uuid <- "feaaaaaf2bbbbb26ccccccb12ddd3a82"
  test_sample <- sample_detail(test_uuid)
  expect_true(tibble::is_tibble(test_sample))
  expect_true(nrow(test_sample) == 0L)
})

test_that("'samples_default_columns()' works", {
  
  ## testing tibble output
  tbl <- samples_default_columns()
  expect_true(tibble::is_tibble(tbl))
  expect_true(nrow(tbl) > 0L && all("columns" %in% names(tbl)))
})

test_that("'sample_derived()' works for uuid without derived samples", {
  test_uuid <- "1e69e0533d19740b03f1a6c3703c4435"
  test_sample <- sample_derived(test_uuid, "Sample")
  expect_true(tibble::is_tibble(test_sample))
  expect_true(nrow(test_sample) == 0L && 
                all(names(test_sample) %in% samples_default_columns("character")))
})
