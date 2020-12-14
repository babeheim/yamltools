

test_that("yaml_loads fails on invalid yamls", {
  expect_false(yaml_loads("invalid_yamls/faulty_indentation.yaml",
    silent = TRUE))
  expect_false(yaml_loads("invalid_yamls/faulty_square_brackets.yaml",
    silent = TRUE))
  expect_false(yaml_loads("invalid_yamls/faulty_quotations.yaml",
    silent = TRUE))
  expect_false(yaml_loads("invalid_yamls/faulty_quotations_ii.yaml",
    silent = TRUE))
  expect_false(yaml_loads("invalid_yamls/duplicated_key.yaml",
    silent = TRUE))
})


# test_validate_yamls.r

test_that("valid yamls all pass validate_yamls", {

  out <- validate_yamls("batch_yaml_flat", report = TRUE, silent = TRUE)
  expect_true(length(out) == 0)
  out <- validate_yamls("batch_yaml_flat_variable", report = TRUE, silent = TRUE)
  expect_true(length(out) == 0)
  out <- validate_yamls("batch_yaml_n_subtables", report = TRUE, silent = TRUE)
  expect_true(length(out) == 0)
  out <- validate_yamls("batch_yaml_one_subtable", report = TRUE, silent = TRUE)
  expect_true(length(out) == 0)
  out <- validate_yamls("single_yaml_to_json", report = TRUE, silent = TRUE)
  expect_true(length(out) == 0)

})

test_that("invalid yamls are detected", {
  out <- validate_yamls("invalid_yamls", report = TRUE, silent = TRUE)
  expect_true(length(out) > 0)
})

