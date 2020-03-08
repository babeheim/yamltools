

# detect when the data_list entries are different structures 

# lets start with JSON's recursive building blocks, the array and object:

simple_jar <- list(
  1,
  TRUE,
  "a",
  NA
)

simple_job <- list(
  name = "B",
  age = 38
)

irregular_list <- list(
  name = "B",
  age = 38,
  5
)

test_that("we can differentiate JSON arrays (i.e. unnamed lists, or 'jars'),
  JSON objects (i.e. named list, or 'jobs') and irregular R lists, ", {

  expect_false(is_deep_list(simple_jar))
  expect_false(is_deep_list(simple_job))
  expect_false(is_deep_list(irregular_list))

  expect_true(is_jar(simple_jar))
  expect_false(is_jar(simple_job))
  expect_false(is_jar(irregular_list))

  expect_false(is_job(simple_jar))
  expect_true(is_job(simple_job))
  expect_false(is_job(irregular_list))

  expect_false(is_irregular_list(simple_jar))
  expect_false(is_irregular_list(simple_job))
  expect_true(is_irregular_list(irregular_list))

})

# we might call all these 'depth-0' since they do not
# contain either jars or jobs as their values (in R, no lists!)

# irregular lists should never, ever happen in our ecodata workflow
# if we detect one it is an immediate and full stop

# I think there's a case to be made that jars should not count towards depth

# a depth-1 jar, then, contains either a jar or a job

simple_job_jar <- list(
  simple_job,
  simple_job
)

simple_jar_jar <- list(
  simple_jar,
  simple_jar
)

mixed_jar1 <- list(
  simple_job,
  simple_jar
)

mixed_jar2 <- list(
  1,
  simple_jar
)

mixed_jar3 <- list(
  simple_job,
  1
)

# we can detect these using some new tools:

test_that("we can differentiate all of these", {

  expect_true(is_jar_of_jobs(simple_job_jar))
  expect_false(is_jar_of_jobs(simple_jar_jar))
  expect_false(is_jar_of_jobs(mixed_jar1))
  expect_false(is_jar_of_jobs(mixed_jar2))
  expect_false(is_jar_of_jobs(mixed_jar3))

  expect_false(is_jar_of_jars(simple_job_jar))
  expect_true(is_jar_of_jars(simple_jar_jar))
  expect_false(is_jar_of_jars(mixed_jar1))
  expect_false(is_jar_of_jars(mixed_jar2))
  expect_false(is_jar_of_jars(mixed_jar3))

  expect_false(is_mixed_jar(simple_job_jar))
  expect_false(is_mixed_jar(simple_jar_jar))
  expect_true(is_mixed_jar(mixed_jar1))
  expect_true(is_mixed_jar(mixed_jar2))
  expect_true(is_mixed_jar(mixed_jar3))

})

# job jars are our standard situation, binding these job jars together is our standard task
# luckily, there are a number of tools for this:
# in rlist, list.stack()
# in dplyr, bind_rows()

test_that("we can combine job jars into data frames", {

  individuals <- bind_rows(simple_job_jar)
  expect_true(nrow(individuals) == 2)
  expect_true(ncol(individuals) == 2)

  individuals <- list.stack(simple_job_jar)
  expect_true(nrow(individuals) == 2)
  expect_true(ncol(individuals) == 2)

})

# a jar jar is only really encountered when we have multiple input files (which load as jars)
# that themselves contain jars of jobs
# its legit but we solve but flattening

test_that("we can combine jar jars", {

  readings <- flatten(simple_jar_jar)
  expect_true(is_jar(readings))
  expect_false(is_deep_list(readings))
  expect_true(length(readings) == 8)

  # I also have a function that will detect if its a jar of jars, and if so, unpack it
  readings <- unpack_jar_jar(simple_jar_jar)
  expect_true(is_jar(readings))
  expect_false(is_deep_list(readings))
  expect_true(length(readings) == 8)
  # this has the advantage of being usable, even if we don't know what's in the jar

  # note we can also use rlist::list.flatten here, but this flattens *all* sublevels
  # so does not generalize as a solution
  readings <- list.flatten(simple_jar_jar)
  expect_true(is_jar(readings))
  expect_false(is_deep_list(readings))
  expect_true(length(readings) == 8)

})

# in practice, not all jar jars will contain jars, sometimes it will be nulls too!

test_that("we know what to do with a jar of jars and NULLs", {

  null_jar_jar <- list(
    simple_jar,
    NULL,
    simple_jar
  )

  readings <- unpack_jar_jar(null_jar_jar)
  expect_true(is_jar(readings))
  expect_false(is_deep_list(readings))
  expect_true(length(readings) == 8)

})

# in practice, we should not encounter mixed jars except with NULLs
# still need to think this thru...
# as this implies a mistake in our data organization
# **if detected, we should return an error**

test_that("we can differentiate jars of JSON arrays ('jar jars'),
  jars of JSON objects ('job jars'),
  jars holding a mixture of jobs and jars", {

  expect_true(is_deep_list(simple_job_jar))
  expect_true(is_deep_list(simple_jar_jar))
  expect_true(is_deep_list(mixed_jar1))
  expect_true(is_deep_list(mixed_jar2))
  expect_true(is_deep_list(mixed_jar3))

  expect_true(is_jar_of_jobs(simple_job_jar))
  expect_false(is_jar_of_jobs(simple_jar_jar))
  expect_false(is_jar_of_jobs(mixed_jar1))
  expect_false(is_jar_of_jobs(mixed_jar2))
  expect_false(is_jar_of_jobs(mixed_jar3))

  expect_false(is_jar_of_jars(simple_job_jar))
  expect_true(is_jar_of_jars(simple_jar_jar))
  expect_false(is_jar_of_jars(mixed_jar1))
  expect_false(is_jar_of_jars(mixed_jar2))
  expect_false(is_jar_of_jars(mixed_jar3))

  expect_false(is_mixed_jar(simple_job_jar))
  expect_false(is_mixed_jar(simple_jar_jar))
  expect_true(is_mixed_jar(mixed_jar1))
  expect_true(is_mixed_jar(mixed_jar2))
  expect_true(is_mixed_jar(mixed_jar3))

})

# its also possible to have depth-1 jobs, but only if they contain a depth-0 jar or job

observation_job <- list(
  obs_id = "1",
  date = "2020-03-20",
  biography = simple_job,
  answers = simple_jar
)

# when we jar a depth-1 jobs, we need to make subtables for each jar or job field and the `answers` field. `biography` will be one-row-per-job, but the `answers` table will be one row per entry in the jar

# we can identify these:
name_job_lists(observation_job)

# and individually select them
subtables <- name_job_lists(observation_job)
observation_job[[subtables[1]]]
observation_job[[subtables[2]]]

# and remove them
remove_job_lists(observation_job)

# this means that we can quickly combine job jars into data frames, at least at the 'top level'

observations_jar <- list(
  observation_job,
  observation_job
)
# (what depth is this tho?)

test_that("we can remove subjobs and subjars and combine job jars at a depth of 1", {

  observations_jar %>% lapply(remove_job_lists) %>%
    bind_rows() %>% as.data.frame() -> observations

  expect_true(nrow(observations) == 2)
  expect_true(ncol(observations) == 2)

  observations_jar %>% lapply(remove_job_lists) %>%
    list.stack() -> observations

  expect_true(nrow(observations) == 2)
  expect_true(ncol(observations) == 2)

  observations_jar %>% stack_jobs() -> observations
  expect_true(nrow(observations) == 2)
  expect_true(ncol(observations) == 2)

})

# what about the sublevels? 
# the two standard functoins are list.map and map
# they only really differ in that one uses character strings, which makes
# purrr much better because I want that

test_that("we can extract the biography job from the jar of observation jobs", {

  observations_jar %>% purrr::map("biography") %>%
    bind_rows() %>% as.data.frame() -> biography

  expect_true(nrow(biography) == 2)

  # since 'biography' is a job, map returns a jar of jobs
  # but a more general solution requires we check to see if we need to unpack:

  observations_jar %>% purrr::map("biography") %>%
    unpack_jar_jar() %>% bind_rows() %>% as.data.frame()  -> biography

  expect_true(nrow(biography) == 2)

  # if we were worried about sublevels inside 'biography', we could also 
  # tell it to purge them using the same function as above

  observations_jar %>% purrr::map("biography") %>%
    unpack_jar_jar() %>% lapply(remove_job_lists) %>%
    bind_rows() %>% as.data.frame() -> biography

  expect_true(nrow(biography) == 2)

  # all these operations are combined into "extract_subtable"

  observations_jar %>% extract_subtable("biography") -> biography

  expect_true(nrow(biography) == 2)

})


# Also, note that I have a solution for jars of jobs ("biography"), but not for
# jars of simple values ("answers")
# I think I'm going to have to *convert* all jars of simple values to jars of jobs,
# then proceed as above
# this is an outstanding problem...

# observations_jar %>% purrr::map("answers") %>% ????


# here's some more depth-1 examples

anthropometrics_job <- list(
  height = 10,
  weight = 5,
  bodyfat = 0.3,
  measurement_system = "metric"
)

lucky_number_jar <- list(
  3,
  42,
  17
)

# depth-1
resident_job <- list(
  name = "A",
  age = 38,
  lucky_numbers = lucky_number_jar,
  anthropometrics = anthropometrics_job
)

residents_jar <- list(
  resident_job,
  resident_job
)

test_that("we can extract our data from the jar of residents", {

  db <- list()

  residents_jar %>% stack_jobs() -> db$residents
  expect_true(nrow(db$residents) == 2)
  expect_true(ncol(db$residents) == 2)

  residents_jar %>% extract_subtable("anthropometrics") -> db$anthropometrics
  expect_true(nrow(db$anthropometrics) == 2)
  expect_true(ncol(db$anthropometrics) == 4)

})

# now a depth-2 job:

# so far, we've never dealth with a jar of jobs being a value inside a jar of jobs
# but this is by far the most common kind of subtable we have to deal with
# extract_subtable can handle this too, by unpacking first!

# depth0
gps_job <- list(
  "latitude" = 10,
  "longitude" = 0
)

# depth0
resident1_job <- list(
  name = "B",
  age = 38
)

resident2_job <- list(
  name = "T",
  age = 38
)

resident3_job <- list(
  name = "E",
  age = 10
)

# depth1
residents_jar <- list(
  resident1_job,
  resident2_job,
  resident3_job
)

# depth 2 job, because it contains the depth1 jar
household_job <- list(
  house_id = 10,
  date = "2020-03-07",
  gps = gps_job,
  residents = residents_jar
)

# a jar of depth-2 is thus depth-3
households_jar <- list(
  household_job,
  household_job,
  household_job,
  household_job,
  household_job
)

test_that("we can unpack the jar of households", {

  db <- list()

  households_jar %>% stack_jobs() -> db$households
  expect_true(nrow(db$households) == 5)

  households_jar %>% extract_subtable("gps") -> db$gps
  expect_true(nrow(db$gps) == 5)

  households_jar %>% extract_subtable("residents") -> db$residents
  expect_true(nrow(db$residents) == 15)
  
  # we can also do all the above automatically
  households_jar %>% unpack_jar("households") -> db
  expect_true(nrow(db$households) == 5)
  expect_true(nrow(db$gps) == 5)
  expect_true(nrow(db$residents) == 15)

})

# I still wonder what to call depth here
# say you have a resident
# does a set of residents have a depth of 2?
# vs a set of numbers?
# it will all make sense soon i guess....
# i really like 'the jar of x's', where x is the name of the individual job
# "the jar of interviews"
# "we unpack the jar of jars of residents, becomes a sinlge jar of residents"
# its *not* the same thing as combining them, b/c there's one less jar involved!
# jars are not just sets...the 'set of sets of residents' is ambiguous
# 

## ok
# obviously i need to solve the 'jar of strings' problem
# probable solution is to convert to a jar of jobs in all cases
# when i write the 'inherit_parent_index' function

# the other problem is, now that i can do this, what happens when I want to do it recursively?
# lets be clear about what i can do:
# a jar of households, where each household contains a jar of residents
# a jar of residents, where each resident contains a jar of pets
# THAT is what I want to recurse on...jar recursion...

gps_job <- list(
  "latitude" = 10,
  "longitude" = 0
)

resident1_pets_jar <- list(
  list(
    name = "F",
    species = "fish"
  ),
  list(
    name = "D",
    species = "dog"
  )
)

resident2_pets_jar <-  list(
  list(
    name = "C",
    species = "cat"
  )
)

resident1_job <- list(
  name = "B",
  age = 38,
  pets = resident1_pets_jar
)

resident2_job <- list(
  name = "T",
  age = 38,
  pets = resident2_pets_jar
)

resident3_job <- list(
  name = "E",
  age = 10
)

residents_jar <- list(
  resident1_job,
  resident2_job,
  resident3_job
)

household_job <- list(
  house_id = 10,
  date = "2020-03-07",
  gps = gps_job,
  residents = residents_jar
)

households_jar <- list(
  household_job,
  household_job,
  household_job,
  household_job,
  household_job
)

# lets start with what I already know how to do...

test_that("we can extract household-level and resident-level data from the jar of households", {

  households_jar %>% unpack_jar("households") -> db
  expect_true(nrow(db$households) == 5)
  expect_true(nrow(db$gps) == 5)
  expect_true(nrow(db$residents) == 15)

})

# to get at the pets data, we need another jar, this time of residents

test_that("we can extract pet-level data from the jar of households", {

  households_jar %>% map("residents") %>% unpack_jar_jar() -> residents_jar
  residents_jar %>% unpack_jar("residents") -> db
  expect_true(nrow(db$pets) == 15)

})

# This is great, but I need a way to do this automatically and therefore recursively
# jars!!

# depth2 jar with variable subtables

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






# use unlist_plus to eliminate all unnecessary lists
# but check that against the auto-load features

# detect and extract the names object's subtables automatically

# cause a keyed object to inherit one, some,
# or all of the keys of a parent object

# four key situations:
# - array with only string, numeric, or logical values  e.g. becomes a subtable
# - array with lists, but only objects e.g.
# anything that becomes a table or subtable, each entry becomes a row
# - object with lists that are arrays e.g.
# pretty much any subtable connected to a parent
# - object with lists that are objects e.g.
# a grouped set of object-level variables

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


# ah, map produces a jar sometimes
# and an array of jars other times!


# at this point we have to assume all keys are present
# ...in other words, no mutate here!

# subtables <- name_subtables(object_array)
# object_array %>% unpack_jars() %>% purge_subtables() %>% list.stack()
# object_array %>% extract_subtables(subtables)

# for (i in 1:length(subtables)) {
#   object_array2 <- purrr::map(object_array, subtables[i]) %>% unpack_jars()
#   subtables2 <- name_subtables(object_array2)
#   object_array2 %>% purge_subtables() %>% list.stack()
#   extract_subtables(object_array2, subtables2)
# }

# almost there!

# for an array of objects go into objects,
# and for every array or object therein identify
# "sublists"
# if there is an array of objcets inside this list,
# recurse the above operation downwards
# if i can get this working, arbitrarily (close!)
# the only thing left is identifying the key




# loading data from files

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

# test_that("a file with one_subtable loads", {
#   "single_yaml_to_json/one_subtable.yaml" %>% read_yaml() -> data_list
#   subtables <- "entries"
#   db <- extract_subtables(data_list, subtables)
#   data_list %>% lapply(list.remove, subtables) %>%
#     bind_rows() %>% as.data.frame() -> db$main
#   expect_true(all(dim(db$main) == c(3,3)))
#   expect_true(all(dim(db$entries) == c(9,2)))
# })

# test_that("a file with many subtables parses", {
#   "single_yaml_to_json/n_subtables.yaml" %>% read_yaml() -> data_list
#   schema <- list(
#     name = "interviews",
#     primary_key = "interview_num",
#     subtables = list(
#       list(
#         name = "residents"
#       ),
#       list(
#         name = "visitors"
#       )
#     )
#   )
#   schema$subtables %>% list.mapv(name) -> subtables
#   db <- extract_subtables(data_list, subtables)
#   data_list %>% lapply(list.remove, subtables) %>%
#     bind_rows() %>% as.data.frame() -> db$main
#   expect_true(nrow(db$residents)==9)
#   expect_true(nrow(db$visitors)==9)
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

