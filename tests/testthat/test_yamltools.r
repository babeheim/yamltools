

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

test_that("flat_array.yml loads as a data frame", {
  "single_yaml_to_json/flat_array.yml" %>% read_yaml() %>% bind_rows() -> x
  expect_true(all(dim(x)==c(3, 3)))
})

test_that("a flat array with some unique keys loads", {
  "single_yaml_to_json/flat_array_unique_keys.yaml" %>%
    read_yaml() %>% bind_rows() -> x
  expect_true(all(dim(x)==c(3, 5)))
})

test_that("a file with one_subtable loads", {
  "single_yaml_to_json/one_subtable.yaml" %>% read_yaml() -> data_list
  subtables <- "entries"
  db <- extract_subtables(data_list, subtables, "interview_num")
  data_list %>% lapply(list.remove, subtables) %>%
    bind_rows() %>% as.data.frame() -> db$main
  expect_true(all(dim(db$main) == c(3,3)))
  expect_true(all(dim(db$entries) == c(9,3)))
})

test_that("a file with many subtables parses", {
  "single_yaml_to_json/n_subtables.yaml" %>% read_yaml() -> data_list
  schema <- list(
    name = "interviews",
    primary_key = "interview_num",
    subtables = list(
      list(
        name = "residents"
      ),
      list(
        name = "visitors"
      )
    )
  )
  schema$subtables %>% list.mapv(name) -> subtables
  db <- extract_subtables(data_list, subtables, "interview_num")
  data_list %>% lapply(list.remove, subtables) %>%
    bind_rows() %>% as.data.frame() -> db$main
  expect_true(nrow(db$residents)==9)
  expect_true(nrow(db$visitors)==9)
})

# test_that("one file with two sub-levels parses", {

#   "single_yaml_to_json/one_subtable_two_levels.yaml" %>% read_yaml() -> data_list

  # schema <- list(
  #   name = "main",
  #   primary_key = "interview_num",
  #   subtables = list(
  #     list(
  #       name = "entries",
  #       primary_key = "entry_num",
  #       subtables = list(
  #         list(
  #           name = "passengers"
  #         )
  #       )
  #     )
  #   )
  # )

#   db <- extract_subtables(data_list, subtables, "interview_num")
#   schema$subtables %>% list.mapv(name) -> subtables
#   data_list %>% lapply(list.remove, subtables) %>%
#     bind_rows() %>% as.data.frame() -> main

#   # what happens here??

#   expect_true(all(dim(x)==c(3,2)))
#   expect_true(length(x$entries)==3)
#   expect_true(all(dim(x$entries[[1]]$passengers[[1]]) == c(3,2)))
# })


# combining files:

test_that("can combine flat array yamls with identical structures into one dataframe", {
  my_files <- list.files('./batch_yaml_flat', pattern = "\\.yaml|\\.yml", full.names = TRUE)
  my_files %>% map(read_yaml) %>%
    flatten() %>% bind_rows() %>% as.data.frame() -> output
  expect_true(all(dim(output) == c(27,3)))
})

test_that("can combine flat-array yamls with unique keys into one dataframe", {
  my_files <- list.files('./batch_yaml_flat_variable', pattern = "\\.yaml|\\.yml", full.names = TRUE)
  my_files %>% map(read_yaml) %>%
    flatten() %>% bind_rows() %>% as.data.frame() -> output
  expect_true(all(dim(output) == c(27,4)))
})


# a critical lesson:
# all files must have the same hierarchy
# its okay if one file contains multiple objects in an array
# provided all files are arrays with one or more objects
# then just flatten as a way to concatenate!

# scrape_yamls must be able to detect what kind of sets of files it has then:
# arrays or objects, but not both!

# would be useful to detect object or array or what? json-parser useful?

is_json_object <- function(l) {
  class(l) == "list" & !is.null(names(l))
}

# now multiple files with structures!

test_that("can combine one-subtable yamls into one dataframe", {
  my_files <- list.files('./batch_yaml_one_subtable', pattern="*.yaml", full.names=TRUE)
  my_files %>% map(read_yaml) %>% flatten() -> data_list
  subtables <- "entries"
  data_list %>% map(list.remove, subtables) %>%
    bind_rows() %>% as.data.frame() -> output
  expect_true(all(dim(output) == c(21,1)))
})

test_that("can combine n-subtable yamls into one dataframe", {
  my_files <- list.files('./batch_yaml_n_subtables', pattern="*.yaml", full.names=TRUE)
  my_files %>% map(read_yaml) %>% flatten() -> data_list
  subtables <- c("residents", "visitors")
  data_list %>% map(list.remove, subtables) %>%
    bind_rows() %>% as.data.frame() -> output
  expect_true(all(dim(output) == c(21,2)))
})


# new tests:

# detect when the data_list entries are different structures 

test_that("we can differentiate JSON objects, JSON arrays and irregulars R list form", {

  array <- list(
    1,
    2,
    3
  )

  object <- list(
    name = "B",
    age = 38
  )

  irregular <- list(
    name = "B",
    age = 38,
    5
  )

  expect_true(list_is_array(array))
  expect_false(list_is_array(object))
  expect_false(list_is_array(irregular))

  expect_false(list_is_object(array))
  expect_true(list_is_object(object))
  expect_false(list_is_object(irregular))

  expect_false(list_is_irregular(array))
  expect_false(list_is_irregular(object))
  expect_true(list_is_irregular(irregular))

})


test_that("we can differentiate arrays of JSON objects, JSON arrays and mixtures in R list form", {

  arrays <- list(
    list(
      list(
        name = "J",
        age = 38
      ),
      list(
        name = "J",
        age = 38
      ),
      list(
        name = "J",
        age = 38
      )
    ),
    list(
      list(
        name = "J",
        age = 38
      ),
      list(
        name = "J",
        age = 38
      ),
      list(
        name = "J",
        age = 38
      )
    )
  )

  objects <- list(
    list(
      name = "B",
      age = 38
    ),
    list(
      name = "J",
      age = 38
    )
  )

  mixture <- list(
    list(
      name = "B",
      age = 38
    ),
    list(
      list(
        name = "B",
        age = 38
      ),
      list(
        name = "B",
        age = 38
      ),
      list(
        name = "B",
        age = 38
      )
    )
  )

  expect_true(list_all_objects(objects))
  expect_false(list_all_objects(arrays))
  expect_false(list_all_objects(mixture))

  expect_false(list_all_arrays(objects))
  expect_true(list_all_arrays(arrays))
  expect_false(list_all_arrays(mixture))

  expect_true(list_regular(objects))
  expect_true(list_regular(arrays))
  expect_false(list_regular(mixture))

})

# use unlist_plus to eliminate all unnecessary lists
# but check that against the auto-load features

# detect and extract the names object's subtables automatically

# subtables <- c("children", "readings")

# list.select(int_obj, children) %>% flatten() %>%
#   flatten() %>% bind_rows()
# list.select(int_obj, readings) %>% flatten() %>%
#   flatten() %>% bind_rows()

# cause a keyed object to inherit one, some,
# or all of the keys of a parent object

array_array <- list(
  list(
    1,
    2,
    3
  ),
  list(
    4,
    5,
    6
  )
)

dat_obj <- list(
  "house_id" = 10,
  "gps" = list(
    "latitude" = 10,
    "longitude" = 0
  ),
  "residents" = list(
    list(
      name = "A",
      age = 10
    )
  )
)

object_array <- list(
  dat_obj,
  dat_obj
)

# takes a list, either object or array
identify_values <- function(x) {
  class_list <- lapply(x, class)
  for (i in 1:length(class_list)) {
    if (class_list[[i]] == "list") {
      if (list_is_array(x[[i]])) {
        class_list[[i]] <- "array"
      }
      if (list_is_object(x[[i]])) {
        class_list[[i]] <- "object"
      }
      if (list_is_irregular(x[[i]])) {
        class_list[[i]] <- "irregular"
        warning("irregular list detected")
      }
    }
  }
  return(class_list)
}

# four key situations:
# - array with only string, numeric, or logical values  e.g. becomes a subtable
# - array with lists, but only objects e.g. anything that becomes a table or subtable, each entry becomes a row
# - object with lists that are arrays e.g. pretty much any subtable connected to a parent
# - object with lists that are objects e.g. a grouped set of object-level variables

# cause a keyed array of strings, numerics or logicals
# to become an array of objects, each taking as key the
# key of the array

# (recursively) cause a keyed array of objects to inherit one some
# or all of the keys of a parent object
# (as a heuristic rule it could take the *first* key
# in the parent object, but that's a hack)

# recursively step through an arbitrary nested list,
# either depth first or top first, and pull out the
# tables of each level recursively.

# purge an array of objects of its lists, so we can list.stack them fine

purge_subtables <- function(jar) {
  subtables <- name_subtables(jar)
  if (length(subtables) > 0) jar %>% lapply(list.remove, subtables) -> jar
  return(jar)
}

extract_subtables <- function(jar, subtables) {
  outlist <- list()
  for (j in 1:length(subtables)) {
    jar %>% purrr::map(subtables[j]) %>% unpack_jars() %>% purge_subtables() %>%
      list.stack() -> outlist[[subtables[j]]]
  }
  return(outlist)
}

unpack_jars <- function(target_list) {
  target_list %>% identify_values() %>% list.all(. == "array") -> needs_unpacking
  if (needs_unpacking) target_list <- flatten(target_list)
  return(target_list)
}

# ah, map produces a jar sometimes
# and an array of jars other times!

dat_obj <- list(
  "house_id" = 10,
  "gps" = list(
    "latitude" = 10,
    "longitude" = 0
  ),
  "residents" = list(
    list(
      name = "A",
      age = 10,
      pets = list(
        list(
          name = "D",
          species = "dog"
        ),
        list(
          name = "F",
          species = "cat"
        )
      )
    ),
    list(
      name = "B",
      age = 12
    )
  )
)

object_array <- list(
  dat_obj,
  dat_obj,
  dat_obj
)

# at this point we have to assume all keys are present...in other words, no mutate here!
subtables <- name_subtables(object_array)
object_array %>% unpack_jars() %>% purge_subtables() %>% list.stack()
object_array %>% extract_subtables(subtables)

for (i in 1:length(subtables)) {
  object_array2 <- purrr::map(object_array, subtables[i]) %>% unpack_jars()
  subtables2 <- name_subtables(object_array2)
  object_array2 %>% purge_subtables() %>% list.stack()
  extract_subtables(object_array2, subtables2)
}

# almost there!

# for an array of objects go into objects, and for every array or object therein identify
# "sublists"
# if there is an array of objcets inside this list, recurse the above operation downwards
# if i can get this working, arbitrarily (close!) the only thing left is identifying the key


int_obj <- list(
  list(
    name = "B",
    age = 38,
    children = list(
      list(
        name = "E",
        age = 10,
        pets = list(
          list(
            name = "F",
            species = "fish"
          ),
          list(
            name = "D",
            species = "dog"
          )
        )
      ),
      list(
        name = "A",
        age = 8,
        pets = list(
          list(
            name = "C",
            species = "cat"
          )
        )
      )
    ),
    readings = list(
      list(
        time = "18:00",
        reading = 100
      ),
      list(
        time = "13:00",
        reading = 50
      ),
      list(
        time = "13:50",
        reading = 501
      )
    )
  ),
  list(
    name = "B",
    age = 38,
    children = list(
      list(
        name = "E",
        age = 10
      ),
      list(
        name = "A",
        age = 8
      )
    ),
    readings = list(
      list(
        time = "18:00",
        reading = 100
      ),
      list(
        time = "13:00",
        reading = 50
      ),
      list(
        time = "13:50",
        reading = 501
      )
    )
  )
)
