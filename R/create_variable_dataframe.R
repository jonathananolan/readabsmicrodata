#' Create dataframe with variables and their labels
#'
#' Import SAS microdata from various ABS datasets, and combine them into a single file with variables given more memorable names.
#' @param survey A string containing the survey you want to import
#' @param file A string containing the survey file of interest - usually household, income unit, or person.
#' @param refyears A list of numbers containing the years you want to import. If blank all years imported.
#' @param all_of_latest_year If TRUE, will import every variable in the most recent dataset. It will also then look at previous years datasets and import variables that have the same name and similar variable labels as the variables in the most recent dataset
#' @param additional_variables A tibble with at least two columns "r_var_name" and the year of interest. Adds additional variables of interest on top of those that are already in the data dictionary. See vignette for more details
#' @param variable_dictionary_v Version of the preloaded dictionary you wish to use
#' @param grattandata Mark true if you want to use the SAS files in the Grattan Institute's data warehouse.
#' @param file_names Only to be used if grattandata is false. A tibble with three columns: "Year","Filename" and "Formats", and a row for each year of the dataset you want to import.
#' @param variable_dictionary Only to be used if you do not want to use the pre-loaded dictionary. A tibble in the prescribed format in the Vignette.
#' @keywords SIH
#' @export
#' @examples
#' @importFrom haven read_sas
#' @import dplyr
#' @import purrr
#' @importFrom rio factorize
#' @importFrom tibble enframe
#' @importFrom stringr str_replace_all



create_var_dataframe <- function(...) {
  #Filter the data dictionary so that it only contains variables that are in this year of the dataset
  variable_dictionary %>%
                          select(r_var_name,
                                 dataset_var_name  = Yearchar) %>%
                          filter(dataset_var_name != "") %>%
                          mutate(dataset_var_name  = tolower(dataset_var_name))
}



find_all_vars <- function(...) {

dataset_no_rows_vars <- dataset_no_rows %>% rename_all(tolower())

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

