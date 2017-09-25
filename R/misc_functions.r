
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}


yaml_to_json <- function(file){
  x <- readLines( file )
  x <- gsub("\"", "'", x)
  for(i in 1:length(x)){
    if( length(grep(": ", x[i])) > 0 & length(grep(": $", x[i])) == 0 ){
      x[i] <- gsub(": ", ": \"", x[i])
      x[i] <- gsub("$", "\"", x[i])
    }
  }
  if(x[length(x)] != "") x <- c(x, "")
  new <- yaml.load(paste(x, collapse="\n"))
  output <- jsonlite::toJSON(new, pretty=TRUE)
  newname <- gsub("\\.yaml|\\.yml", ".json", file)
  writeLines(output, newname)
}

# rbind_plus <- function( df1, df2 ){
#   df1 <- do.call(data.frame, df1)
#   df2 <- do.call(data.frame, df2)
#   n1 <- colnames(df1)
#   n2 <- colnames(df2)
#   if( any(!n1 %in% n2) ){
#     new_cols <- n1[!n1 %in% n2]
#     df2 <- cbind(df2, matrix("", nrow=nrow(df2), ncol=length(new_cols)))
#     colnames(df2) <- c(n2, new_cols)
#   }
#   if( any(!n2 %in% n1) ){
#     new_cols <- n2[!n2 %in% n1]
#     df1 <- cbind(df1, matrix("", nrow=nrow(df1), ncol=length(new_cols)))
#     colnames(df1) <- c(n1, new_cols)
#   }
#   output <- rbind(df1, df2)
#   output
# }

vectorize <- function( input_list ){
  for(i in 1:ncol(input_list)){
    if(class(input_list[,i])=="list"){
      empty <- which(lapply(input_list[,i], class)=="list")
      if(length(empty)>0) input_list[empty,i] <- NA
      input_list[,i] <- unlist(nullToNA(input_list[,i]))
    }
  }
  input_list
}

list_to_dataframe <- function( input_list ){
    column_names <- unique(unlist(lapply(input_list, names)))
    output <- matrix( "", nrow = length(input_list), ncol=length(column_names) )
    colnames(output) <- column_names
    output <- as.data.frame( output )
    for(j in 1:ncol(output)) output[,j] <- as.character(output[,j]) 
    for(i in 1:length(input_list)){
        new_record <- unlist(input_list[[i]])
        output[i, names(new_record)] <- new_record 
    }
    output
}
