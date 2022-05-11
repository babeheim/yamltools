
read_yaml2 <- function (file, prep = TRUE) {
  x <- readLines(file, warn = FALSE)
  if (prep) {
    x <- gsub("\"", "'", x)
    x <- gsub(":  ", ": ", x)
    for (i in 1:length(x)) {
      if (
           length(grep(": ", x[i])) > 0 &
           length(grep(": $", x[i])) == 0
         ) {
        x[i] <- sub(": ", ": \"", x[i])
        x[i] <- sub("$", "\"", x[i])
      }
    }
    if (x[length(x)] != "") x <- c(x, "")
  }
  output <- yaml.load(paste(x, collapse = "\n"))
  return(output)
}


convert_to_characters <- function(data) {
  out <- data
  for (i in 1:length(data)) {
    if (class(data[[i]]) == "list") {
      out[[i]] <- convert_to_characters(data[[i]])
    } else {
      out[[i]] <- as.character(data[[i]])
    }
  }
  return(out)
}


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

# list.stack is my replacement for bind_rows!
stack_jobs <- function(jar) {
  jar %>% remove_jar_sublists() %>% convert_missing_to_NA() %>% list.stack(fill = TRUE)
}

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

# i really need to work on this language...sigh...
unpack_jar_of_jars <- function(jar) {
  output_jar <- jar
  if (is_jar_of_jars(jar)) {
    jar %>% classify_json_values() %>% list.all(. %in% c("jar", "NULL")) -> needs_unpacking
    if (needs_unpacking) jar %>% list.clean() %>% flatten() -> output_jar
    if (is_jar_of_jars(output_jar)) warning("output is also a jar of jars; that shouldn't happen!")
  }
  return(output_jar)
}

extract_jar <- function(jar, subjar) {
  jar %>% purrr::map(subjar) %>% lapply(convert_simple_jar) %>%
    prepend_parent_index() %>% unpack_jar_of_jars()
}

# this could be useful for debugging purposes...
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

convert_missing_to_null <- function(data) {
  for (i in 1:length(data)) {
    if (class(data[[i]]) == "list") {
      data[[i]] <- convert_missing_to_null(data[[i]])
    } else {
      if (length(data[[i]]) == 0 & class(data[[i]]) != "NULL") {
        data[i] <- list(NULL)
        # data[[i]] <- 999
      }
    }
  }
  return(data)
}

convert_missing_to_NA <- function(data) {
  for (i in 1:length(data)) {
    if (class(data[[i]]) == "list") {
      data[[i]] <- convert_missing_to_NA(data[[i]])
    } else {
      if (length(data[[i]]) == 0 & !is.null(names(data[i]))) {
        data[[i]] <- NA
      }
    }
  }
  return(data)
}

# how do you ensure you do an operation over every simple value in a deeply nested list?

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

scrape_yamls <- function(path, top_name = "interviews", recursive = FALSE) {

  my_yamls <- list.files(path, pattern = "*\\yaml$|*\\yml$",
    recursive = recursive, full.names = TRUE)

  my_yamls %>% lapply(yaml_loads) %>% all() -> all_load

  db <- list()

  if (all_load) {

    data_list <- list()

    for (i in 1:length(my_yamls)) {
      my_yamls[i] %>%
        read_yaml2() %>%
        compact() %>%
        map(compact) %>%
        convert_to_characters() -> data_list[[i]]
        print(my_yamls[i])
    }

    data_list %>% convert_missing_to_null() %>% 
      unpack_jar_of_jars() %>% unpack_jar(top_name) -> db

  }

  return(db)

}



scrape_yamls_old <- function() {

  my_yamls <- list.files("./2_yaml", pattern = "*\\yaml$",
    recursive = TRUE, full.names = TRUE)

  my_yamls %>% lapply(yaml_loads) %>% all() -> all_load

  db <- list()

  if (all_load) {

    dir_init("./3_json")

    my_jsons <- gsub("2_yaml", "3_json", my_yamls)
    my_jsons <- gsub("yaml", "json", my_jsons)

    for (i in 1:length(my_yamls)) {
      my_yamls[i] %>%
        read_yaml2() %>%
        compact() %>%
        map(compact) %>%
        convert_to_characters() %>%
        write_json(my_jsons[i], pretty = TRUE,
          auto_unbox = TRUE)
        print(my_jsons[i])
    }

    my_jsons %>% map(read_json, auto_unbox = TRUE) %>% unpack_list() -> db

  }

  return(db)

}


################

validate_yamls <- function(path, recursive = TRUE, silent = FALSE, report = FALSE) {
  my_yamls <- list.files(path, pattern="\\.yml|\\.yaml|\\.yaml.txt",
    full.names = TRUE, recursive = recursive)
  if (length(my_yamls) == 0) stop("no yamls detected at this path!")
  out <- list()
  for (i in 1:length(my_yamls)) {
    print(my_yamls[i])
    out[[i]] <- try(read_yaml2(my_yamls[i]), silent = silent)
  }
  errors <- unlist(lapply(out, class)) == "try-error"
  if (report == TRUE) return(out[errors])
}

yaml_loads <- function(file, silent = TRUE) {
  res <- try(read_yaml2(file), silent = silent)
  out <- class(res) != "try-error"
  return(out)
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

