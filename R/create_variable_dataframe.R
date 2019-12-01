

create_var_dataframe <- function(...) {
  #Filter the data dictionary so that it only contains variables that are in this year of the dataset
  variable_dictionary %>%
                          select(r_var_name,
                                 dataset_var_name  = Yearchar) %>%
                          filter(dataset_var_name != "") %>%
                          mutate(dataset_var_name  = tolower(dataset_var_name))
}



find_all_vars <- function(...) {

dataset_no_rows_vars <- names(dataset_no_rows) %>% tolower()

tibble(r_var_name = dataset_no_rows_vars,
       dataset_var_name = dataset_no_rows_vars) %>%
                   filter(!(dataset_var_name %in% var_dataframe$dataset_var_name)) %>%
                   bind_rows(var_dataframe)
}

find_all_vars(dataset_no_rows,variable_dictionary)

find_all_vars_append <- function(...) {

  var_dataframe        <- create_var_dataframe()

  dataset_no_rows_vars <- names(dataset_no_rows) %>% tolower()

  tibble(r_var_name = dataset_no_rows_vars,
         dataset_var_name = dataset_no_rows_vars) %>%
    filter(!(dataset_var_name %in% var_dataframe$dataset_var_name)) %>%
    mutate(r_var_name = paste0(r_var_name,"_",Yearchar)) %>%
    bind_rows(var_dataframe)
}

