
# just use list.clean()

# takes a list, either object or array
classify_json_values <- function(x) {
  class_list <- lapply(x, class)
  for (i in 1:length(class_list)) {
    if (class_list[[i]] == "list") {
      if (is_jar(x[[i]])) class_list[[i]] <- "jar"
      if (is_job(x[[i]])) class_list[[i]] <- "job"
      if (is_irregular_list(x[[i]])) {
        class_list[[i]] <- "irregular"
        warning("irregular list detected")
      }
    }
  }
  return(class_list)
}

is_job <- function(l) class(l) == "list" & !is.null(names(l)) & !any(names(l) == "")
is_jar <- function(l) class(l) == "list" & is.null(names(l))
is_irregular_list <- function(z) !is_job(z) & !is_jar(z)

is_jar_or_null <- function(l) is_jar(l) | is.null(l)
is_job_or_null <- function(l) is_job(l) | is.null(l)

is_jar_of_jobs <- function(x) {
  x %>% lapply(is_job_or_null) %>% unlist() %>% all() -> all_jobs_or_nulls
  x %>% lapply(is.null) %>% unlist() %>% all() -> all_nulls
  all_jobs_or_nulls & !all_nulls
}

is_jar_of_jars <- function(x) {
  x %>% lapply(is_jar_or_null) %>% unlist() %>% all() -> all_jars_or_nulls
  x %>% lapply(is.null) %>% unlist() %>% all() -> all_nulls
  all_jars_or_nulls & !all_nulls
}

is_deep_list <- function(x) any(lapply(x, class) == "list")
# some entries can be NULL in a jar of jars, but not all
# i need to somehow know

is_mixed_jar <- function(x) is_deep_list(x) & !is_jar_of_jars(x) & !is_jar_of_jobs(x)

# seems like a recursive use of flatten could improve?
unlist_plus <- function(z) {
  if (is.list(z)) {
    if (length(z) == 1) {
      if (!is.list(z[[1]])) {
        out <- unlist(z)
      } else {
        z[[1]] <- unlist_plus(z[[1]])
        out <- z
      }
    } else if (length(z) > 1) {
      for (i in 1:length(z)) {
        z[[i]] <- unlist_plus(z[[i]])
      }
      out <- z
    } else {
      out <- z
    }
  } else {
    out <- z
  }
  return(out)
}


# some weird situations
# 1. the job as only one entry
# 2. the jobs in the jar have a variable number of entries
# 3. some of the values are NA
# 4. the same key has different types across jobs, e.g. logical and character, logical and numeric, etc.

name_job_lists <- function(job) names(job)[which(lapply(job, class) == "list")]

remove_job_lists <- function(job) {
  job_lists <- name_job_lists(job)
  if (length(job_lists) > 0) job <- list.remove(job, job_lists)
  return(job)
}

name_jar_sublists <- function(jar) {
  jar %>% lapply(name_job_lists) %>% unlist() %>% unique() %>% sort()
}

remove_jar_sublists <- function(jar) {
  jar %>% lapply(remove_job_lists)
}

# i think this is more efficient, but should have same functionality
remove_jar_sublists_fast <- function(jar) {
  subtables <- name_jar_sublists(jar)
  if (length(subtables) > 0) jar %>% lapply(list.remove, subtables) -> jar
  return(jar)
}

stack_jobs <- function(jar) {
  jar %>% remove_jar_sublists() %>% list.stack()
}

# these are needed to extract jars

convert_simple_values_to_jobs <- function(value, column_name = "value") {
  out <- list(value)
  names(out) <- column_name
  return(out)
}

convert_simple_jar <- function(jar, column_name = "value") {
  if (is_jar(jar) & !is_deep_list(jar)) {
    jar %>% lapply(convert_simple_values_to_jobs, column_name = column_name) -> jar
  }
  return(jar)
}

prepend_parent_index <- function(jar) {
  if (is_jar(jar)) {
    if (is_jar_of_jars(jar)) {
      for (i in 1:length(jar)) {
        if (is_jar_of_jobs(jar[[i]])) {
          jar[[i]] <- lapply(jar[[i]],
            function(z) list.prepend(z, parent_table_index = i))
        }
      }
    }
    # this below is only an edge case, happens sometimes but can happen
    if (is_jar_of_jobs(jar)) {
      for (i in 1:length(jar)) {
        if (is_job(jar[[i]])) {
          jar[[i]] <- list.prepend(jar[[i]], parent_table_index = i)
        }
      }
    }
  }
  return(jar)
}

unpack_jar_of_jars <- function(jar) {
  output_jar <- jar
  if (is_jar_of_jars(jar)) {
    jar %>% classify_json_values() %>% list.all(. %in% c("jar", "NULL")) -> needs_unpacking
    if (needs_unpacking) jar %>% list.clean() %>% flatten() -> output_jar
    if (is_jar_of_jars(output_jar)) warning("output is also a jar of jars; that shouldn't happen!")
  }
  return(output_jar)
}

# i need to pass the parent_index_key as a new variable here
# and then I'm basically done!

extract_jar2 <- function(jar, subjar) {
  jar %>% purrr::map(subjar) %>% lapply(convert_simple_jar) %>%
    prepend_parent_index() %>% unpack_jar_of_jars()
}

extract_jar <- function(jar, subjar) {
  jar %>% purrr::map(subjar) %>% unpack_jar_of_jars() %>% convert_simple_jar()
}

unpack_jar_double <- function(jar, label = NA, out = list()) {
  if (is.na(label)) label <- deparse(substitute(jar))
  label <- deparse(substitute(jar))
  jar %>% stack_jobs() -> out[[label]]
  subtables <- name_jar_sublists(jar)
  if (length(subtables) > 0) {
    for (i in 1:length(subtables)) {
      jar %>% extract_jar(subtables[i]) %>%
        stack_jobs() -> out[[subtables[i]]]
    }
  }
  return(out)
}

unpack_jar <- function(jar, label = NA, out = list()) {
  if (is.na(label)) label <- deparse(substitute(jar))
  jar %>% stack_jobs() -> out[[label]]
  subtables <- name_jar_sublists(jar)
  if (length(subtables) > 0) {
    for (i in 1:length(subtables)) {
      jar %>% extract_jar(subtables[i]) %>%
         unpack_jar(label = subtables[i], out = out) -> out
      out[[subtables[i]]] %>% mutate(parent_table = label) -> out[[subtables[i]]]
    }
  }
  return(out)
}





####################

# we need these two to extract subtables
# no, we don't. extract_subtables is deprecated!

extract_subtable <- function(jar, subtable) {
  jar %>% purrr::map(subtable) %>%
  unpack_jar_of_jars() %>% lapply(remove_job_lists) %>%
  list.stack()
}

unpack_double_jar <- function(jar, top_name = "interviews") {
  db <- list()
  jar %>% stack_jobs() -> db[[top_name]]
  # gotta fix this
  subtables <- name_jar_sublists(jar)
  for (i in 1:length(subtables)) {
    jar %>% extract_subtable(subtables[[i]]) -> db[[subtables[[i]]]]
  }
  return(db)
}


flatten_entry <- function(data, entry) {
  if (any(is.numeric(entry))) stop("cannot be numeric b/c those change!")
  if (length(entry) == 1) {
    if (is.character(entry)) entry <- which(names(data) == entry)
    data <- append(data, data[[entry]], entry)
    data[[entry]] <- NULL
  } else {
    for (j in 1:length(entry)) {
      data <- flatten_entry(data, entry[j])
    }
  }
  return(data)
}

purge_sublist_names <- function(data, sublists) {
  for (j in sublists) {
    names(data[[j]]) <- NULL
  }
  return(data)
}



extract_subtables_old2 <- function(jar, subtables) {
  outlist <- list()
  for (j in 1:length(subtables)) {
    jar %>% purrr::map(subtables[j]) %>% unpack_jar_of_jars() %>%
    remove_jar_sublists() %>% list.stack() -> outlist[[subtables[j]]]
  }
  return(outlist)
}

extract_subtables_old <- function(jar, subtables) {
  outlist <- list()
  for (j in 1:length(subtables)) {
    out <- list()
    for (i in 1:length(jar)) {
      jar[[i]][[subtables[j]]] %>%
        list.clean() %>%
        bind_rows() %>%
        as.data.frame() -> out[[i]] 
    }
    out %>% bind_rows() -> outlist[[subtables[j]]]
  }
  return(outlist)
}

################

validate_yamls <- function(path) {
  my_yamls <- list.files(path, pattern="\\.yml|\\.yaml", full.names = TRUE)
  for (i in 1:length(my_yamls)) {
    print(my_yamls[i])
    try(read_yaml(my_yamls[i]), silent = FALSE)
  }
}

yaml_loads <- function(path, silent = FALSE) {
  res <- try(read_yaml(path), silent = silent)
  failed <- class(res)=="try-error"
  return(!failed)
}

validate_keys <- function(data, pattern, nchar = NA) {
  keys <- grep(pattern, names(data))
  if(length(keys) == 0) is_valid <- FALSE
  if(length(keys) > 0){
    is_valid <- TRUE
    for(i in 1:length(keys)){
      this_value <- data[keys[i]]
      if(is.na(this_value)) this_value <- ""
      this_value <- gsub("'", "", this_value)
      this_value <- gsub("\"", "", this_value)
      if(is.na(nchar)) proper_length <- nchar(this_value) > 0
      if(!is.na(nchar)) proper_length <- nchar(this_value) == nchar
      is_valid <- is_valid & proper_length
    }
  }
  return(is_valid)
}

bad_transcriber <- function(data) {
  transcriber <- data$transcriber
  transcriber <- gsub("'", "", transcriber)
  out <- (is.na(transcriber) | length(transcriber)==0)
  return(out)
}

bad_reviewer <- function(data) {
  reviewer <- data$reviewer
  reviewer <- gsub("'", "", reviewer)
  out <- (is.na(reviewer) | length(reviewer)==0)
  return(out)
}

bad_hash <- function(data, hash_length = 7) {
  hash <- gsub("'", "", data$pdf_hash)
  out <- nchar(hash) != hash_length
  return(out)
}

bad_dates <- function(data) {
  date_check <- grep("date$", names(data))
  valid_date <- TRUE
  for(i in 1:length(date_check)){
    date_value <- data[[date_check[i]]]
    valid_date <- valid_date & !is.na(as.Date(date_value, "%Y-%m-%d"))
  }
  out <- !valid_date
  return(out)
}

bad_stamp <- function(data) {
  stamp <- gsub("'", "", data$stamp_number)
  out <- nchar(stamp) != 6 ### ?
  return(out)
}

