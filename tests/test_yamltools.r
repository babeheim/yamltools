rm(list=ls())

test_that("works on all of them", {
  my_files <- list.files('./single_yaml_to_json', pattern="\\.yml|\\.yaml", full.names=TRUE)
  for(i in 1:length(my_files)){
    x <- read_yaml(my_files[i], to.json=TRUE)
    newname <- gsub("\\.yaml|\\.yml", ".json", my_files[i])
    writeLines(x, newname)
  }
  my_files <- list.files('./single_yaml_to_json', pattern="\\.json", full.names=TRUE)
  expect_true(length(my_files)==5)
})

test_that("flat array", {
  my_file <- "./single_yaml_to_json/flat_array.yml" 
  x <- read_yaml(my_file, to.json=TRUE)
  newname <- gsub("\\.yaml|\\.yml", ".json", my_file)
  writeLines(x, newname) 
  my_file <- "./single_yaml_to_json/flat_array.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  x <- vectorize(x)
  expect_true(all(dim(x)==c(3,3)))
})

test_that("flat array with unique keys", {
  my_file <- "./single_yaml_to_json/flat_array_unique_keys.yaml" 
  x <- read_yaml(my_file, to.json=TRUE)
  newname <- gsub("\\.yaml|\\.yml", ".json", my_file)
  writeLines(x, newname) 
  my_file <- "./single_yaml_to_json/flat_array_unique_keys.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  x <- vectorize(x)
  expect_true(all(dim(x)==c(3,5)))
})

test_that("one_subtable", {
  my_file <- "./single_yaml_to_json/one_subtable.yaml" 
  x <- read_yaml(my_file, to.json=TRUE)
  newname <- gsub("\\.yaml|\\.yml", ".json", my_file)
  writeLines(x, newname) 
  my_file <- "./single_yaml_to_json/one_subtable.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  x <- vectorize(x)
  expect_true(all(dim(x)==c(3,2)))
  expect_true(length(x$entries)==3)
})

test_that("n_subtables", {
  my_file <- "./single_yaml_to_json/n_subtables.yaml" 
  x <- read_yaml(my_file, to.json=TRUE)
  newname <- gsub("\\.yaml|\\.yml", ".json", my_file)
  writeLines(x, newname) 
  my_file <- "./single_yaml_to_json/n_subtables.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  expect_true(all(dim(x)==c(3,4)))
  x <- vectorize(x)
  expect_true(length(x$residents)==3)
  expect_true(length(x$visitors)==3)
})

test_that("can handle two levels deep", {
  my_file <- "./single_yaml_to_json/one_subtable_two_levels.yaml" 
  x <- read_yaml(my_file, to.json=TRUE)
  newname <- gsub("\\.yaml|\\.yml", ".json", my_file)
  writeLines(x, newname) 
  my_file <- "./single_yaml_to_json/one_subtable_two_levels.json" 
  x <- read_json(my_file, simplifyVector=TRUE)
  x <- vectorize(x)
  expect_true(all(dim(x)==c(3,2)))
  expect_true(length(x$entries)==3)
})

test_that("can combine flat array yamls with identical structures into one dataframe", {
  my_files <- list.files('./batch_yaml_flat', pattern="*.yml", full.names=TRUE)
  for(i in 1:length(my_files)){
    x <- read_yaml(my_files[i], to.json=TRUE)
    newname <- gsub("\\.yaml|\\.yml", ".json", my_files[i])
    writeLines(x, newname) 
  }
  my_files <- gsub("\\.*yml", ".json", my_files)
  output <- lapply(my_files, function(z) read_json(z, simplifyVector=TRUE))
  expect_true(all(unlist(lapply(output, class)) == "data.frame"))
  # rbind.pages combines a list of dataframes
  output <- jsonlite::rbind_pages(output)
  output <- vectorize(output)
  expect_true(all(dim(output) == c(27,3)))
})

# is rbind.pages saavy enough if they have different shapes tho? yes! goodbye rbind_plus!
# apparently plry::rbind.fill does this job too

test_that("can combine flat-array yamls with unique keys into one dataframe", {
  my_files <- list.files('./batch_yaml_flat_variable', pattern="*.yml", full.names=TRUE)
  for(i in 1:length(my_files)){
    x <- read_yaml(my_files[i], to.json=TRUE)
    newname <- gsub("\\.yaml|\\.yml", ".json", my_files[i])
    writeLines(x, newname) 
  }
  my_files <- gsub("\\.yml", ".json", my_files)
  output <- lapply(my_files, function(z) read_json(z, simplifyVector=TRUE))
  expect_true(all(unlist(lapply(output, class)) == "data.frame"))
  # rbind.pages combines a list of dataframes
  output <- jsonlite::rbind_pages(output)
  output <- vectorize(output)
  expect_true(all(dim(output) == c(27,4)))
})

# now multiple files with structures!

test_that("can combine one-subtable yamls into one dataframe", {
  my_files <- list.files('./batch_yaml_one_subtable', pattern="*.yaml", full.names=TRUE)
  for(i in 1:length(my_files)){
    x <- read_yaml(my_files[i], to.json=TRUE)
    newname <- gsub("\\.yaml|\\.yml", ".json", my_files[i])
    writeLines(x, newname) 
  }
  my_files <- gsub("\\.yaml", ".json", my_files)
  output <- lapply(my_files, function(z) read_json(z, simplifyVector=TRUE))
  expect_true(all(unlist(lapply(output, class)) == "data.frame"))
  # rbind.pages combines a list of dataframes
  output <- jsonlite::rbind_pages(output)
  output <- vectorize(output)
  expect_true(all(dim(output) == c(21,2)))
})

test_that("can combine n-subtable yamls into one dataframe", {
  my_files <- list.files('./batch_yaml_n_subtables', pattern="*.yaml", full.names=TRUE)
  for(i in 1:length(my_files)){
    x <- read_yaml(my_files[i], to.json=TRUE)
    newname <- gsub("\\.yaml|\\.yml", ".json", my_files[i])
    writeLines(x, newname) 
  }
  my_files <- gsub("\\.yaml", ".json", my_files)
  output <- lapply(my_files, function(z) read_json(z, simplifyVector=TRUE))
  expect_true(all(unlist(lapply(output, class)) == "data.frame"))
  # rbind.pages combines a list of dataframes
  output <- jsonlite::rbind_pages(output)
  output <- vectorize(output)
  expect_true(all(dim(output) == c(21,4)))
})





