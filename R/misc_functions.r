
yaml_loads <- function(path, quote_everything = TRUE) {
  x <- readLines(path)
  x <- gsub("\"", "'", x)
  if(quote_everything){
    for(i in 1:length(x)){
      if (length(grep(": ", x[i])) > 0 & length(grep(": $", x[i])) == 0) {        
        line_key <- substr(x[i], 1, regexpr(": ", x[i]) - 1)
        line_value <- substr(x[i], regexpr(": ", x[i]) + 1 , nchar(x[i]))
        line_value <- gsub("^\\s+|\\s+$", "", line_value) 
        line_value <- paste0("\"", line_value, "\"")
        line_value <- gsub("\"'|'\"", "\"", line_value) 
        x[i] <- paste0(line_key, ": ", line_value)
      }
    }
  }
  res <- try(new <- yaml.load(paste(x, collapse="\n")), TRUE)
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


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

# the yaml package just wrote one of these...maybe just use that?!
read_yaml <- function (file, to.json = FALSE) {
  x <- readLines(file, warn = FALSE)
  x <- gsub("\"", "'", x)
  x <- gsub(":  ", ": ", x)
  for (i in 1:length(x)) {
    if (length(grep(": ", x[i])) > 0 & length(grep(": $", x[i])) == 0) {        
      line_key <- substr(x[i], 1, regexpr(": ", x[i]) - 1)
      line_value <- substr(x[i], regexpr(": ", x[i]) + 1 , nchar(x[i]))
      line_value <- gsub("^\\s+|\\s+$", "", line_value) 
      line_value <- paste0("\"", line_value, "\"")
      line_value <- gsub("\"'|'\"", "\"", line_value) 
      x[i] <- paste0(line_key, ": ", line_value)
    }
  }
  if (x[length(x)] != "") x <- c(x, "")
  output <- yaml.load(paste(x, collapse = "\n"))
  if (to.json) output <- jsonlite::toJSON(output, pretty = TRUE)
  return(output)
}

# bug: input_list is a list of length 1 b/c all other colimsn are empty

vectorize <- function (input_list) {
  output_list <- as.data.frame(input_list)
  for (i in 1:ncol(output_list)) {
    if (class(output_list[, i]) == "list") {
      if (all(lapply(input_list[, i], class) != "data.frame")) {
        empty <- which(lapply(input_list[, i], class) == 
          "list")
        if (length(empty) > 0) 
          output_list[empty, i] <- NA
        output_list[, i] <- unlist(nullToNA(output_list[, 
          i]))
      }
    }
  }
  return(output_list)
}

rbind_list <- function( input_list ){
  df1 <- vectorize(input_list[[1]])
  df1 <- do.call(data.frame, df1)
  for(i in 2:length(input_list)){
    if (!is.null(input_list[[i]])){
      df2 <- vectorize(input_list[[i]])
      df2 <- do.call(data.frame, df2)
      n1 <- colnames(df1)
      n2 <- colnames(df2)
      if( any(!n1 %in% n2) ){
        new_cols <- n1[!n1 %in% n2]
        df2 <- cbind(df2, matrix("", nrow=nrow(df2), ncol=length(new_cols)))
        colnames(df2) <- c(n2, new_cols)
      }
      if( any(!n2 %in% n1) ){
        new_cols <- n2[!n2 %in% n1]
        df1 <- cbind(df1, matrix("", nrow=nrow(df1), ncol=length(new_cols)))
        colnames(df1) <- c(n1, new_cols)
      }
      df1 <- rbind(df1, df2)
    }
  }
  df1 <- as.data.frame(df1, stringsAsFactors=FALSE)
  return(df1)
}
