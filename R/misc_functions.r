
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

read_yaml <- function(file, to.json=FALSE){
  x <- readLines( file , warn=FALSE )
  x <- gsub("\"", "'", x)
  x <- gsub(":  ", ": ", x)
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

# bug: input_list is a list of length 1 b/c all other colimsn are empty
vectorize <- function( input_list ){
  output_list <- as.data.frame(input_list)
  for(i in 1:ncol(output_list)){
    if(class(output_list[,i])=="list"){
      if(class(input_list[,i][[1]])!="data.frame"){
        empty <- which(lapply(input_list[,i], class)=="list")
        if(length(empty)>0) output_list[empty,i] <- NA
        output_list[,i] <- unlist(nullToNA(output_list[,i]))
      }
    }
  }
  return(output_list)
}

rbind_list <- function( input_list ){
  df1 <- vectorize(input_list[[1]])
  df1 <- do.call(data.frame, df1)
  for(i in 2:length(input_list)){
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
  df1 <- as.data.frame(df1, stringsAsFactors=FALSE)
  return(df1)
}
