
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

read_yaml <- function(file, to.json=FALSE){
  x <- readLines( file , warn=FALSE )
  x <- gsub("\"", "'", x)
  for(i in 1:length(x)){
    if( length(grep(": ", x[i])) > 0 & length(grep(": $", x[i])) == 0 ){
      x[i] <- gsub(": ", ": \"", x[i])
      x[i] <- gsub("$", "\"", x[i])
    }
  }
  if(x[length(x)] != "") x <- c(x, "")
  output <- yaml.load(paste(x, collapse="\n"))
  if(to.json) output <- jsonlite::toJSON(output, pretty=TRUE)
  return(output)
}

vectorize <- function( input_list ){
  for(i in 1:ncol(input_list)){
    if(class(input_list[,i])=="list"){
      if(class(input_list[,i][[1]])!="data.frame"){
        empty <- which(lapply(input_list[,i], class)=="list")
        if(length(empty)>0) input_list[empty,i] <- NA
        input_list[,i] <- unlist(nullToNA(input_list[,i]))
      }
    }
  }
  input_list
}
