test_that("'donors()' works", {
  test_donors <- donors()
  expect_true(tibble::is_tibble(test_donors))
  expect_true(nrow(test_donors) > 0L)
  expect_true(all(names(test_donors) %in% donors_default_columns("character")))
})

test_that("'donor_detail()' work for specific donor", {
  test_uuid <- "0abacde2443881351ff6e9930a706c83"
  test_donor <- donor_detail(test_uuid)
  expect_true(tibble::is_tibble(test_donor))
  expect_true(nrow(test_donor) == 1L && ncol(test_donor) >= 23L)
  expect_equal(test_donor$uuid, test_uuid)
})

test_that("'donor_detail()' works for uuid that does not exist", {
  test_uuid <- "feaaaaaf2bbbbb26ccccccb12ddd3a82"
  test_donor <- donor_detail(test_uuid)
  expect_true(tibble::is_tibble(test_donor))
  expect_true(nrow(test_donor) == 0L)
})

test_that("'donors_default_columns()' works", {
  
  ## testing tibble output
  tbl <- donors_default_columns()
  expect_true(tibble::is_tibble(tbl))
  expect_true(nrow(tbl) > 0L && all("columns" %in% names(tbl)))
})

test_that("'donor_derived()' works", {
  
  test_uuid <- "3b5d057daf1e84d746d01a16acf4f0bb"
  
  # Derived Samples
  test_sample <- donor_derived(test_uuid, "Sample")
  expect_true(tibble::is_tibble(test_sample))
  expect_true(nrow(test_sample) > 0L && 
              all(c("organ", "derived_dataset_count") %in% names(test_sample)))
  
  # Derived Datasets
  test_dataset <- donor_derived(test_uuid, "Dataset")
  expect_true(tibble::is_tibble(test_dataset))
  expect_true(nrow(test_dataset) > 0L &&  
                "derived_dataset_count" %in% names(test_sample))
  
})