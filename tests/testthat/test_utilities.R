test_that("'.is_scalar()' works", {
  ## scalar
  expect_false(.is_scalar(logical()))
  expect_true(.is_scalar(logical(1)))
  expect_false(.is_scalar(logical(2)))
  
  ## NA values
  expect_false(.is_scalar(NA))
})

test_that("'.is_scalar_*()' works", {
  expect_true(.is_scalar_character("foo"))
  expect_true(.is_scalar_logical(TRUE))
})

test_that("'.is_uuid()' works", {
  expect_true(.is_uuid('c09f875545a64694d70a28091ffbcf8b'))
  expect_false(.is_uuid('c09f875545a64694d70a28091ffbcf8'))
})

test_that("'.is_size()' works", {
  expect_true(.is_size(23))
  expect_false(.is_size(''))
})

test_that("'.to_numeric()' works", {
  expect_equal(.to_numeric("1"), 1)
})

test_that("'.update_preferred_term()' works", {
  expect_equal(.update_preferred_term("", "Female"), "Female")
  expect_equal(.update_preferred_term("Sex", "Female"), "Sex")
})

test_that("'.keep_numeric_values()' works", {
  expect_equal(.keep_numeric_values("Plaque total 0; Tangle total 6.5; Braak score III"),
               "")
  expect_equal(.keep_numeric_values("3/3"), "")
  expect_equal(.keep_numeric_values("90"), "90")
})

test_that("'.title_to_organ()' works", {
  expect_equal(.title_to_organ('MALDI data from the kidney (left) of a 56-year-old white female'),
               "Kidney (Left)")
  expect_equal(.title_to_organ('RNAseq data from the Brain of a 23-year-old white male'),
               "Brain")
  
})