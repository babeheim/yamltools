

test_that("works on all of them", {
  my_files <- list.files('./single_yaml_to_json', pattern="\\.yml|\\.yaml", full.names=TRUE)
  for(i in 1:length(my_files)){
    yaml_to_json(my_files[i])
  }
  my_files <- list.files('./single_yaml_to_json', pattern="\\.json", full.names=TRUE)
  expect_true(length(my_files)==4)
})

test_that("flat array", {
  my_file <- "./single_yaml_to_json/flat_array.yml" 
  yaml_to_json(my_file)
  my_file <- "./single_yaml_to_json/flat_array.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  expect_true(all(dim(x)==c(3,3)))
})

test_that("flat array with unique keys", {
  my_file <- "./single_yaml_to_json/flat_array_unique_keys.yaml" 
  yaml_to_json(my_file)
  my_file <- "./single_yaml_to_json/flat_array_unique_keys.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  expect_true(all(dim(x)==c(3,5)))
})

test_that("one_subtable", {
  my_file <- "./single_yaml_to_json/one_subtable.yaml" 
  yaml_to_json(my_file)
  my_file <- "./single_yaml_to_json/one_subtable.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  expect_true(all(dim(x)==c(3,2)))
  expect_true(length(x$entries)==3)
})

test_that("n_subtables", {
  my_file <- "./single_yaml_to_json/n_subtables.yaml" 
  yaml_to_json(my_file)
  my_file <- "./single_yaml_to_json/n_subtables.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  expect_true(all(dim(x)==c(3,4)))
  expect_true(length(x$residents)==3)
  expect_true(length(x$visitors)==3)
})

test_that("can combine simple yamls into one dataframe", {
  my_files <- list.files('./batch_yaml_flat', pattern="*.yml", full.names=TRUE)
  lapply(my_files, yaml_to_json)
  my_files <- gsub("\\.*yml", ".json", my_files)
  output <- lapply(my_files, function(z) read_json(z, simplifyVector=TRUE))
  expect_true(all(unlist(lapply(output, class)) == "data.frame"))
  # rbind.pages combines a list of dataframes
  output <- jsonlite::rbind.pages(output)
  expect_true(all(dim(output) == c(27,3)))
})

# is rbind.pages saavy enough if they have different shapes tho? yes! goodbye rbind_plus!
# apparently plry::rbind.fill does this job too

test_that("can combine variable-column yamls into one dataframe", {
  my_files <- list.files('./batch_yaml_flat_variable', pattern="*.yml", full.names=TRUE)
  lapply(my_files, yaml_to_json)
  my_files <- gsub("\\.*yml", ".json", my_files)
  output <- lapply(my_files, function(z) read_json(z, simplifyVector=TRUE))
  expect_true(all(unlist(lapply(output, class)) == "data.frame"))
  # rbind.pages combines a list of dataframes
  output <- jsonlite::rbind.pages(output)
  expect_true(all(dim(output) == c(27,4)))
})

