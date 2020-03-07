
# as a yaml, this is what we need:
# name         : interviews
# primary_key  : "interview_number"
# subtables:
# - name       : "entries"
#   primary_key: "entry_num"
#   subtables:
#   - name     : "passengers"

extract_subtables <- function(data_list, subtables, parent_key) {
  outlist <- list()
  for (j in 1:length(subtables)) {
    out <- list()
    keyname <- paste0(parent_key, "_pk")
    for (i in 1:length(data_list)) {
      data_list[[i]][[subtables[j]]] %>% map(purrr::flatten) %>%
        bind_rows() %>% mutate(!!keyname := data_list[[i]][parent_key]) %>%
        as.data.frame() -> out[[i]] 
    }
    out %>% bind_rows() -> outlist[[subtables[j]]]
  }
  return(outlist)
}

extract_subtable_names <- function(data_obj) {
  subtables <- NA
  for (i in 1:length(data_obj$subtables)) {
    subtables[i] <- data_obj$subtables[[i]]$name
  }
  return(subtables)
}


schema <- list(
  name = "interviews",
  primary_key = "interview_num",
  subtables = list(
    list(
      name = "entries",
      primary_key = "entry_num",
      subtables = list(
        list(
          name = "passengers"
        )
      )
    )
  )
)

data_list %>% map(rlist::list.remove, subtables) %>%
    bind_rows() %>% as.data.frame()

"single_yaml_to_json/one_subtable_two_levels.yaml" %>% read_yaml() -> data_list

subtables <- extract_subtable_names(schema)
x1 <- extract_subtables(data_list, subtables, "interview_num")
for (i in 1:length(subtables)) {
  subtables <- extract_subtable_names(schema$subtables[[i]])
}

db <- extract_subtables(data_list, subtables, "interview_num")
data_list %>% map(rlist::list.remove, subtables) %>%
  bind_rows() %>% as.data.frame() -> db$main


scrape_yamls <- function(data_list, schema) {

}

# for each object J in an unnamed list:
# 1. get the `name`s of its subtables from the schema
# 2. list.remove those and make a table out of the data on that level
# 3. store the `primary_key` value as the `parent_key` 
# 4. store the subtable names as `subtables`
# 5. run the extract_subtables argument with these parameters
# 6. repeat steps 1 to 5 on each of the `subtables` of object J

# also combine with keys to flatten on that level as a matter of course!
# lists of lists are best indication its a subtable
# lists of non-lists, e.g. numbers or characters, indicates we should just concatenate!

# if no parent_key supplied, just go with the first one that looks unique?