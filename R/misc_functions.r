

name_job_lists <- function(job) names(job)[which(lapply(job, class) == "list")]

name_subtables <- function(jar) {
  jar %>% lapply(name_job_lists) %>% unlist() %>% unique() %>% sort()
}

list_is_object <- function(z) !is.null(names(z)) & !any(names(z) == "")
list_is_array <- function(z) is.null(names(z))
list_is_irregular <- function(z) !list_is_object(z) & !list_is_array(z)

list_all_objects <- function(x) x %>% lapply(list_is_object) %>% unlist() %>% all()
list_all_arrays <- function(x) x %>% lapply(list_is_array) %>% unlist() %>% all()
list_regular <- function(x) list_all_arrays(x) | list_all_objects(x)


validate_yamls <- function(path) {
  my_yamls <- list.files(path, pattern="\\.yml|\\.yaml", full.names = TRUE)
  for (i in 1:length(my_yamls)) {
    print(my_yamls[i])
    try(read_yaml(my_yamls[i]), silent = FALSE)
  }
}

extract_subtables <- function(jar, subtables) {
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

# just use list.clean()

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

extract_subtable_old <- function(data_list, subtable_name) {
  out <- list()
  for (i in 1:length(data_list)) {
    data_list[[i]][[subtable_name]] %>% map(purrr::flatten) %>%
    bind_rows() %>% mutate(instanceID = data_list[[i]]$meta$instanceID) %>%
    as.data.frame() -> out[[i]] 
  }
  out %>% bind_rows -> out
  return(out)
}

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