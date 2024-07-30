test_that("'donors()' works with small size and default arguments", {
  test_donors <- donors(size = 4)
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

test_that("'donor_derived()' works with specific entity type", {
  test_uuid <- "3b5d057daf1e84d746d01a16acf4f0bb"
  test_donor <- donor_derived(test_uuid, "Sample")
  expect_true(tibble::is_tibble(test_donor))
  expect_true(nrow(test_donor) > 0L && 
                all(names(test_donor) %in% samples_default_columns("character")))
})