test_that("'uuid_provenance()' works", {

  # donor no provenance
  test_donor_uuid <- "d7c90f1fe1ae702df845e29da648854a"
  test_donor_ls <- uuid_provenance(test_donor_uuid)
  expect_true(rlang::is_empty(test_donor_ls))

  # sample provenance yes
  test_sample_uuid <- "6874561561f77469bf3ac02d0c1ba598"
  test_sample_ls <- uuid_provenance(test_sample_uuid)
  expect_false(is.null(test_sample_ls))
  expect_true(length(test_sample_ls) == 3L)

  # dataset provenance yes
  test_dataset_uuid <- "57b475b8ad39fa2cf07ede0cf192b2f6"
  test_dataset_ls <- uuid_provenance(test_dataset_uuid)
  expect_false(is.null(test_dataset_ls))
  expect_true(length(test_dataset_ls) == 4L)
})
