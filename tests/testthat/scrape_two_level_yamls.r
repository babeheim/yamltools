
rm(list = ls())

library(yamltools)
library(yaml)
library(rlist)
library(jsonlite)
library(yaml)
library(dplyr)
library(purrr)
library(openxlsx)

dir_init <- function(path, verbose=FALSE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(verbose){
    if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
    if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
  }
  if(dir.exists(path)) unlink(path, recursive=TRUE)
  dir.create(path)
}

extract_subtable <- function(data_list, subtable_name) {
  out <- list()
  for (i in 1:length(data_list)) {
    data_list[[i]][[subtable_name]] %>% map(purrr::flatten) %>%
    bind_rows() %>% mutate(pdf_hash = data_list[[i]]$pdf_hash) %>%
    as.data.frame() -> out[[i]] 
  }
  out %>% bind_rows -> out
  return(out)
}

extract_fish_subtable <- function(data_list, subtable_name) {
  out <- list()
  for (i in 1:length(data_list)) {
    data_list[[i]][[subtable_name]] %>% map(purrr::flatten) %>%
    bind_rows() %>% mutate(fisher_name = data_list[[i]]$fisher_name) %>%
    as.data.frame() -> out[[i]] 
  }
  out %>% bind_rows -> out
  return(out)
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

####################################
# read and prep the YAML into JSON #
####################################

dir_init("./3_json")

my_yamls <- list.files("./2_yaml", pattern = "*\\yaml$",
  recursive = TRUE, full.names = TRUE)

my_jsons <- gsub("2_yaml", "3_json", my_yamls)
my_jsons <- gsub("yaml", "json", my_jsons)

for (i in 1:length(my_yamls)) {
  my_yamls[i] %>%
    yamltools::read_yaml() %>%
    compact() %>%
    map(compact) %>%
    convert_to_characters() %>%
    write_json(my_jsons[i], pretty = TRUE,
      auto_unbox = TRUE)
    print(my_jsons[i])
}

#######################################
# combine JSON into relational tables #
#######################################


my_jsons %>% map(read_json, auto_unbox = TRUE) -> data_list

sublists <- c(
  "fishers"
)

data_list %>%
  map(list.remove, sublists) %>%
  bind_rows() %>%
  as.data.frame() -> interviews

# a little bit of cleanup b/c a rush job

fix_fisher_names <- function(fisher_entry) {
  if(length(fisher_entry$fisher_name) == 0) fisher_entry$fisher_name <- ""
  fisher_entry
}

interviews$departure_date <- gsub("change_if_entry_is_present", "", interviews$departure_date)
interviews$return_date[interviews$departure_date != ""] <- ""

dat <- list(
  interviews = interviews
)

fishers <- list()
fish <- list()

for (i in 1:length(data_list)) {

  data_list[[i]]$fishers %>% map(fix_fisher_names) -> data_list[[i]]$fishers

  data_list[[i]]$fishers %>%
    map(list.remove, "fish") %>%
    compact() %>%
    map(compact) %>%
    bind_rows() %>%
    as.data.frame() -> fishers[[i]]
  fishers[[i]]$pdf_hash <- data_list[[i]]$pdf_hash

  data_list[[i]]$fishers %>% extract_fish_subtable("fish") -> fish[[i]]
  fish[[i]]$pdf_hash <- data_list[[i]]$pdf_hash

}

dat$fishers <- bind_rows(fishers)
link <- match(dat$fishers$pdf_hash, dat$interviews$pdf_hash)
dat$fishers$stamp_number <- dat$interviews$stamp_number[link]
dat$fishers$departure_date <- dat$interviews$departure_date[link]
dat$fishers$return_date <- dat$interviews$return_date[link]

dat$fish <- bind_rows(fish)

dir_init("./4_csv")

write.csv(dat$interviews, "./4_csv/interviews.csv", row.names = FALSE)
write.csv(dat$fishers, "./4_csv/fishers.csv", row.names = FALSE)
write.csv(dat$fish, "./4_csv/fish.csv", row.names = FALSE)

