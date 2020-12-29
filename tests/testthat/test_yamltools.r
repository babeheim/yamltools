

# detect when the data_list entries are different structures 

# there's a few issues with the rlist JSON files used as examples, i need to be able to load these!
# or diagnose why they are bad
# friends.json is a job of jobs, but its really supposed to be a jar of jobs...need to "dename" it!

# lets start with JSON's recursive building blocks, the JSON array (jar) and JSON object (job):

simple_jar <- list(
  1,
  10,
  5,
  3
)

simple_job <- list(
  name = "B",
  age = 38
)

# in R, these are called named and unnamed lists.
# However, not all lists in R meet this, because the language allows
# partially-named lists, which here I'll call irregulars
# irregular lists should never, ever happen in our ecodata workflow
# if we detect one it is an immediate and full stop
# we cannot write these in JSON either

irregular_list <- list(
  name = "B",
  age = 38,
  5
)

# my yamltools package has a set of tools for identifying different list types

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

# jars can have many things as values, including jobs and other jars.
# however, all values except for those are what i call 'simple' values
# numeric, logical, character and NULL class

difftype_jar <- list(
  1,
  TRUE,
  "a",
  NULL
)

# a jar of jobs is one common JSON value we deal with
# another is a jar of other jars, of indeterminate underlying structure

simple_job_jar <- list(
  simple_job,
  simple_job
)

simple_jar_jar <- list(
  simple_jar,
  simple_jar
)

# we can also imagine a jar with heterogeneous contents, which we'll called 'mixed'
# while mixed jars can be written in JSON, they should never happen in our workflow
# and indicate a malformed data structure

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

test_that("we can differentiate jars of JSON arrays ('jar jars'),
  jars of JSON objects ('job jars'),
  jars holding a mixture of jobs and jars", {

  expect_false(is_jar_of_jobs(simple_jar))
  expect_true(is_jar_of_jobs(simple_job_jar))
  expect_false(is_jar_of_jobs(simple_jar_jar))
  expect_false(is_jar_of_jobs(mixed_jar1))
  expect_false(is_jar_of_jobs(mixed_jar2))
  expect_false(is_jar_of_jobs(mixed_jar3))

  expect_false(is_jar_of_jobs(simple_jar))
  expect_false(is_jar_of_jars(simple_job_jar))
  expect_true(is_jar_of_jars(simple_jar_jar))
  expect_false(is_jar_of_jars(mixed_jar1))
  expect_false(is_jar_of_jars(mixed_jar2))
  expect_false(is_jar_of_jars(mixed_jar3))

  expect_false(is_jar_of_jobs(simple_jar))
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


# often we will encounter jobs which have a variable number of keys in them

simple_job1 <- list(
  name = "B",
  age = 38
)
simple_job2 <- list(
  name = "B",
  age = 38,
  height = 100
)

job_jar21 <- list(
  simple_job2,
  simple_job1
)

# if we use bind_rows, this is okay

test_that("we can combine jobs with variable numbers of keys", {
  expect_silent(bind_rows(job_jar21))

  # It turns out list.stack does a terrible job on these without fill = TRUE:
  expect_error(list.stack(job_jar21))
  expect_silent(list.stack(job_jar21, fill = TRUE))
})

## we also need to worry about situations in which NULL characters appear as the value of jobs
# bind_rows will fail in this situation, unless we first convert the NULL to NA

test_that("I know how to handle nulls inside objects", {

  null_job <- list(
    name = "a",
    age = NULL
  )

  null_job_jar <- list(
    simple_job,
    null_job
  )

  expect_error(null_job_jar %>% bind_rows())
  expect_warning(null_job_jar %>% list.stack())

  na_job_jar <- convert_missing_to_NA(null_job_jar)
  expect_silent(na_job_jar %>% bind_rows())
  expect_silent(na_job_jar %>% list.stack())

})

# jars of jobs might also appear as a "subjar" when we map them out of a higher-level jar of jobs
# specifically, if a job is a value inside another job
# this is a case in which we would want to inherit the parent index

test_that("we can inherit the index in a jar of jobs", {

  # imagine this is the output of a map(subjar) call
  simple_job_jar %>% prepend_parent_index() %>% list.stack() -> individuals
  expect_true(nrow(individuals) == 2)
  expect_true(ncol(individuals) == 3)

})

# this is exactly the kind of situation in which we have to worry about NULL entries though

test_that("jars of jobs can handle NULL entries", {

  null_job_jar <- list(
    simple_job,
    NULL,
    simple_job
  )

  # importantly, we should only be replacing NULL in jobs, not jars
  # so convert_missing_to_NA will not affect this NULL
  expect_identical(null_job_jar, convert_missing_to_NA(null_job_jar))

  null_job_jar %>% prepend_parent_index() %>% list.stack() -> individuals
  expect_true(nrow(individuals) == 2)
  expect_true(ncol(individuals) == 3)

})

# so much for jars of jobs
# what about jars of simple values?
## in our workflow a jar of simple values must become a subtable
# or be turned into a concatenated string

# we accomplish the former by converting it into a jar of jobs

test_that("we can convert simple jars to jars of jobs", {

  # this is accomplished by convert_simple_values_to_jobs
  x <- 5
  x2 <- convert_simple_values_to_jobs(x)
  expect_true(is_job(x2))

  x <- "test"
  x2 <- convert_simple_values_to_jobs(x)
  expect_true(is_job(x2))

  x <- TRUE
  x2 <- convert_simple_values_to_jobs(x)
  expect_true(is_job(x2))

  x <- NULL
  x2 <- convert_simple_values_to_jobs(x)
  expect_true(is_job(x2))

  # we can apply this operation to a whole jar of simple values by convert_simple_jar
  jar_of_jobs <- convert_simple_jar(simple_jar)
  expect_true(is_jar_of_jobs(jar_of_jobs))

  jar_of_jobs <- convert_simple_jar(difftype_jar)
  expect_true(is_jar_of_jobs(jar_of_jobs))

})

# the final kind of jar is a jar of jars
# this is a legit occurance in our work
# and is the normal consequence of mapping out a subjar

test_that("we can combine a jar of jars into one jar", {

  readings <- flatten(simple_jar_jar)
  expect_true(is_jar(readings))
  expect_false(is_deep_list(readings))
  expect_true(length(readings) == 8)

  # I also have a function that will detect if its a jar of jars, and if so, unpack it
  readings <- unpack_jar_of_jars(simple_jar_jar)
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


test_that("a jar of jars still behaves even with nulls inside", {

  null_jar_jar <- list(
    simple_jar,
    NULL,
    simple_jar
  )

  expect_true(length(null_jar_jar) == 3)
  expect_true(is_jar(null_jar_jar))
  expect_true(is_jar_of_jars(null_jar_jar))
  expect_true(is_deep_list(null_jar_jar))

  # we can combine jars of jars into one single jar with unpack_jar_of_jars
  readings <- unpack_jar_of_jars(null_jar_jar)
  expect_true(length(readings) == 8)
  expect_true(is_jar(readings))
  expect_false(is_deep_list(readings))

  # this still works if we want to unpack a jar made up of jars of jobs and NULLs

  null_jar_jar2 <- list(
    simple_job_jar,
    NULL,
    simple_job_jar
  )

  expect_true(length(null_jar_jar2) == 3)
  expect_true(is_jar(null_jar_jar2))
  expect_true(is_jar_of_jars(null_jar_jar2))
  expect_true(is_deep_list(null_jar_jar2))

  jar_of_jobs <- unpack_jar_of_jars(null_jar_jar2)
  expect_true(length(jar_of_jobs) == 4)
  expect_true(is_jar(jar_of_jobs))
  expect_true(is_deep_list(jar_of_jobs)) # the only difference here

  # what about a jar of jars, each of which is a jar of jars?

  jar_jar_jar <- list(
    simple_jar_jar,
    simple_jar_jar
  )

  expect_true(length(jar_jar_jar) == 2)
  expect_true(is_jar(jar_jar_jar))
  expect_true(is_jar_of_jars(jar_jar_jar))
  expect_true(is_deep_list(jar_jar_jar))

  expect_warning(jar_of_jars <- unpack_jar_of_jars(jar_jar_jar))
  # note that this situation should technically never happen, so I've added a warning
  expect_true(is_jar_of_jars(jar_of_jars))

  # though we allow some NULL in the above operations
  # a perverse jar would be filled with NULL

  null_jar <- list(
    NULL,
    NULL
  )

  expect_true(length(null_jar) == 2)
  expect_true(is_jar(null_jar))
  expect_false(is_jar_of_jars(null_jar))
  expect_false(is_deep_list(null_jar))

  # our unpack operation should be saavy enough not to do anything
  expect_identical(unpack_jar_of_jars(null_jar), null_jar)

})

# having converted the jar of jars, we then either have a jar of jobs or jar of simple objects
# so we can handle it as above

# in practice, not all jars of jars will contain jars, sometimes it will be nulls too!
# when we do the mapping, for example, not every job in our array will have that job or jar inside it
# the solution here is to use list.clean(), which has been incorporated into
# unpack_jar_of_jars

# in practice, we should not encounter other kinds of mixed jars
# as this implies a mistake in our data organization
# **if detected, we should return an error**

# there's one issue: how do we inherit parent_index values to jars of jars?
# especially if they have NULLs?

# here's the four situations I have to solve, in increasing complexity:

test_that("we can prepend the parent table index into various kinds of jars", {

  simple_jar_jar <- list(
    simple_jar,
    simple_jar
  )

  # our prepend function does nothing unless its working on a jar of jars
  # or, occasionally, a jar of jobs
  
  # doesn't work on jars of jars
  expect_identical(simple_jar_jar, prepend_parent_index(simple_jar_jar))

  simple_jar_jar %>% lapply(convert_simple_jar) %>%
    prepend_parent_index() %>% unpack_jar_of_jars() %>% list.stack() -> good

  expect_equal(good$parent_table_index, c(1, 1, 1, 1, 2, 2, 2, 2))
  expect_true(nrow(good) == 8)
  expect_true(ncol(good) == 2)

  # if its a jar of simple jars, we cannot use lapply
  simple_jar_jar %>% lapply(convert_simple_jar) %>%
    lapply(prepend_parent_index) %>% unpack_jar_of_jars() %>% list.stack() -> bad
  expect_false(identical(good$parent_table_index, bad$parent_table_index))

  jar_of_job_jars <- list(
    list(
      simple_job,
      simple_job
    ),
    list(
      simple_job
    ),
    list(
      simple_job,
      simple_job,
      simple_job
    )
  )

  jar_of_job_jars %>% lapply(convert_simple_jar) %>%
    prepend_parent_index() %>% unpack_jar_of_jars() %>% list.stack() -> dat

  expect_equal(dat$parent_table_index, c(1, 1, 2, 3, 3, 3))
  expect_true(nrow(dat) == 6)
  expect_true(ncol(dat) == 3)

  null_jar_jar <- list(
    simple_jar,
    NULL,
    simple_jar
  )

  null_jar_jar %>% lapply(convert_simple_jar) %>%
    prepend_parent_index() %>% unpack_jar_of_jars() %>% list.stack() -> good

  expect_equal(good$parent_table_index, c(1, 1, 1, 1, 3, 3, 3, 3))
  expect_true(nrow(good) == 8)
  expect_true(ncol(good) == 2)

  jar_of_job_jars_nulls <- list(
    list(
      simple_job,
      simple_job
    ),
    NULL,
    list(
      simple_job
    ),
    NULL,
    list(
      simple_job,
      simple_job,
      simple_job
    )
  )

  jar_of_job_jars_nulls %>% lapply(convert_simple_jar) %>%
    prepend_parent_index() %>% unpack_jar_of_jars() %>% list.stack() -> dat

  expect_equal(dat$parent_table_index, c(1, 1, 3, 5, 5, 5))
  expect_true(nrow(dat) == 6)
  expect_true(ncol(dat) == 3)

})


## applied examples

# its also possible to have depth-1 jobs, but only if they contain a depth-0 jar or job

observation_job <- list(
  obs_id = "1",
  date = "2020-03-20",
  biography = simple_job,
  answers = simple_jar
)

# when we jar a depth-1 jobs, we need to make subtables for each jar or job field and the `answers` field. `biography` will be one-row-per-job, but the `answers` table will be one row per entry in the jar

test_that("we can identify the sublists inside our jar of observations", {
  expect_identical(name_job_lists(observation_job), c("biography", "answers"))
})

test_that("we can remove sublists inside our job", {
  remove_job_lists(observation_job) -> reduced_observation_job
  expect_false(is_deep_list(reduced_observation_job))
  expect_true(length(reduced_observation_job) == 2)
})

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
    unpack_jar_of_jars() %>% bind_rows() %>% as.data.frame()  -> biography

  expect_true(nrow(biography) == 2)

  # if we were worried about sublevels inside 'biography', we could also 
  # tell it to purge them using the same function as above

  observations_jar %>% purrr::map("biography") %>%
    unpack_jar_of_jars() %>% remove_jar_sublists() %>%
    bind_rows() %>% as.data.frame() -> biography

  expect_true(nrow(biography) == 2)

  # however, im wondering if i shouldn't just define this like so:
  observations_jar %>% extract_jar("biography") %>% stack_jobs() -> observations

  # we can also do the same for the "answers" jar, which contains simple values
  # simply by converting it into a job
  observations_jar %>% extract_jar("answers") %>% stack_jobs() -> answers
  expect_true(nrow(answers) == 8)

  # and we can do both levels simultaneously with unpack_jar()

  db <- unpack_jar(observations_jar, label = "observations")

})

# this is where we need to start worrrying about the inheritance of information
# across levels
# by unpacking the jar, we have lost the ability to say which values came from which observations
# likewise with the biography

# so we need to tell it...


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

  expect_equal(name_jar_sublists(residents_jar), c("anthropometrics", "lucky_numbers"))

  residents_jar %>% extract_jar("anthropometrics") %>% stack_jobs() -> db$anthropometrics
  expect_true(nrow(db$anthropometrics) == 2)
  expect_true(ncol(db$anthropometrics) == 5)

  residents_jar %>% extract_jar("lucky_numbers") %>% stack_jobs() -> db$lucky_numbers
  expect_true(nrow(db$lucky_numbers) == 6)
  expect_true(ncol(db$lucky_numbers) == 2)

  # can we do all this at once?
  residents_jar %>% unpack_jar("residents") -> db

  expect_identical(names(db), c("residents", "anthropometrics", "lucky_numbers"))
  expect_true(nrow(db$residents) == 2)
  expect_true(ncol(db$residents) == 2)
  expect_true(nrow(db$anthropometrics) == 2)
  expect_true(ncol(db$anthropometrics) == 6)
  expect_true(nrow(db$lucky_numbers) == 6)
  expect_true(ncol(db$lucky_numbers) == 3)

  residents <- residents_jar
  db <- unpack_jar(residents)

  expect_identical(names(db), c("residents", "anthropometrics", "lucky_numbers"))
  expect_true(nrow(db$residents) == 2)
  expect_true(ncol(db$residents) == 2)
  expect_true(nrow(db$anthropometrics) == 2)
  expect_true(ncol(db$anthropometrics) == 6)
  expect_true(nrow(db$lucky_numbers) == 6)
  expect_true(ncol(db$lucky_numbers) == 3)

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
  expect_true(nrow(db$pets) == 15)

})

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

test_that("a complex interview can be tamed!", {
  int_obj %>% unpack_jar("interviews") -> db
  expect_true(nrow(db$interviews) == 2)
  expect_true(nrow(db$children) == 4)
  expect_true(nrow(db$pets) == 3)
  expect_true(nrow(db$readings) == 6)

  interviews <- int_obj

  db <- unpack_jar(interviews)
  expect_true(nrow(db$interviews) == 2)
  expect_true(nrow(db$children) == 4)
  expect_true(nrow(db$pets) == 3)
  expect_true(nrow(db$readings) == 6)

})

# it works!




# some weird situations
# 1. the job as only one entry
# 2. the jobs in the jar have a variable number of entries
# 3. some of the values are NA
# 4. the same key has different types across jobs, e.g. logical and character, logical and numeric, etc.



# flat array with identical keys
list(
  list(
    date = "2017-01-01",
    amount = -3,
    notes = "blah"
  ),
  list(
    date = "2017-01-02",
    amount = -3,
    notes = "blah"
  ),
  list(
    date = "2017-01-03",
    amount = -3,
    notes = "blah"
  )
) %>% unpack_jar("transactions") -> transactions

# flat array, some unique keys
list(
  list(
    date = "2017-01-01",
    amount = -3,
    notes = "blah"
  ),
  list(
    date = "2017-01-02",
    amount = -3,
    notes = "blah",
    time = 1200
  ),
  list(
    date = "2017-01-03",
    amount = -3,
    notes = "blah",
    location = "home"
  )
) %>% unpack_jar("transactions") -> transactions

# array with one subtable, `entries`
list(
  list(
    interview_num = 1,
    date = "2017-01-01",
    name = "tom",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  ),
  list(
    interview_num = 2,
    date = "2017-01-02",
    name = "tom",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  ),
  list(
    interview_num = 3,
    date = "2017-01-03",
    name = "alice",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  )
) %>% unpack_jar("interviews") -> db



# lets go simpler first

list(
  list(
    interview_num = 1,   # parent key of 'entries'
    date = "2017-01-01",
    name = "tom",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  )
) %>% unpack_jar("interviews") -> db


# becomes

list(
  list(
    interview_num = 1,
    date = "2017-01-01",
    name = "tom",
    entries = list(
      list(
        interview_num = 1,
        amount = -3,
        notes = "blah"
      ),
      list(
        interview_num = 1,
        amount = -5,
        notes = "blah2"
      ),
      list(
        interview_num = 1,
        amount = -10,
        notes = "blah blah"
      )
    )
  )
) %>% unpack_jar("interviews") -> db

# becomes

list(
  list(
    interview_num = 1,
    amount = -3,
    notes = "blah"
  ),
  list(
    interview_num = 1,
    amount = -5,
    notes = "blah2"
  ),
  list(
    interview_num = 1,
    amount = -10,
    notes = "blah blah"
  )
) %>% unpack_jar("interviews") -> db



# more on missing values

# what if the missingn value isn't a NULL but a character(0), etc?
# what if every value in a job is missing??

missing_job <- list(
  name = NULL,
  age = 18
)

missing_job2 <- list(
  name = character(0),
  age = 18
)


data_list <- list(
  list(
    name = character(0),
    age = 18
  ),
  list(
    name = "B",
    age = 28
  )
)

data_list2 <- list(
  list(
    name = character(0),
    age = 18,
    lucky_numbers = list(
      1, 2, integer(0), 20
    ),
    anthropometrics = list(
      height = 5,
      width = numeric(0)
    ),
    pets = list(
      list(
        name = character(0),
        age = integer(0)
      ),
      list(
        name = "a",
        age = 10
      )
    )
  ),
  list(
    name = "B",
    age = 28,
    lucky_numbers = list(
      1, 2, integer(0), 20
    ),
    anthropometrics = list(
      height = 5,
      width = 10
    ),
    pets = list(
      list(
        name = "A",
        age = integer(0)
      ),
      list(
        name = "a",
        age = 10
      )
    )
  )
)

test_that("we can convert missing values", {

  expect_silent(convert_missing_to_null(missing_job))
  expect_silent(convert_missing_to_NA(missing_job))

# note that missing_job[[1]] addresses NULL but if you assign it to NULL it disappears
# also true if you called missing_job$name!
# another part of R's charm...ugh
# see here: https://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r
# the solution is to do x[i] <- list(NULL), which preserves the name!!

  expect_silent(convert_missing_to_null(missing_job2))
  expect_silent(convert_missing_to_NA(missing_job2))

  expect_silent(convert_missing_to_null(data_list))
  expect_silent(convert_missing_to_NA(data_list))

  expect_silent(convert_missing_to_null(data_list2))
  expect_silent(convert_missing_to_NA(data_list2))

  expect_silent(data_list2 %>% convert_missing_to_NA() %>% unpack_jar("people"))

})


# loading data from files

# how to handle empty entries? what to do with these character(0) entries? that's a problem...


test_that("flat_array.yml loads as a data frame", {
  "single_yaml_to_json/flat_array.yml" %>% read_yaml() %>% bind_rows() -> x
  expect_true(all(dim(x)==c(3, 3)))
})

test_that("a flat array with some unique keys loads", {
  "single_yaml_to_json/flat_array_unique_keys.yaml" %>%
    read_yaml() %>% bind_rows() -> x
  expect_true(all(dim(x)==c(3, 5)))
})

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
