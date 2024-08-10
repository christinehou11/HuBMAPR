test_that("'organ()' works", {
  organs <- organ()
  expect_true(tibble::is_tibble(organs))
  expect_true(nrow(organs) > 0L)
  expect_true(all(names(organs) %in% c("abbreviation", "name")))
})
