#' Create list of variables to attempt to import
#' @keywords internal
#' @export
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom rio factorize
#' @importFrom purr map_dfr

#Extract variable names and labels from the most recent year so that we can match it to previous years


create_tibble_to_compare_dataframes <- function(year_extracted,
                                                ...) {
year_extracted = 2015
  dataset_no_rows <- import_year_all(year_extracted,
                                     filter_cols = FALSE,
                                     grattandata = grattandata,
                                     file_names = file_names,
                                     nrow = 0)


  #some silly little things that make a variable label seem different when in reality it's the same
  replacements=c("head of the income unit"="reference person of income unit",
                 "fis"="",
                 "unique household number"="unique household number - allocated to all members in the household",
                 "household identifier"="unique household number - allocated to all members in the household",
                 "unique household number - unique number allocated to all members of the household"="unique household number - allocated to all members in the household",
                 "experimental"="",
                 "hes only"="",
                 "in the"="in",
                 "previous"="prev",
                 "previous"="prev",
                 "allowance"="payment",
                 "financial"="fin",
                 "iu"="income unit",
                 "hh"="household",
                 "01 to 60"="",
                 "one"="1",
                 "two"="2",
                 "three"="3",
                 "four"="4",
                 "five"="5",
                 "six"="6",
                 "one"="1",
                 "two"="2",
                 "three"="3",
                 "four"="4",
                 "five"="5",
                 "six"="6",
                 "wage and salary"="employee income",
                 "one parent"="lone parent",
                 "children"="child",
                 "and over"="",
                 "to"="",
                 "income unit has"="",
                 "household has"="",
                 "imputaion"="imputaion",
                 "dependants"="dependents",
                 "the head of the"="reference person of",
                 "spouse"="partner",
                 "spouse"="partner",
                 "of income unit"="",
                 "of the income unit"="",
                 "income unit has"="",
                 "[ ]a[ ]"="",
                 "hhld"="household",
                 "job"="work",
                 "<u+0096>"="",
                 "(modelled)"="",
                 "weight - person"="person",
                 "level identifier - household level"="household level",
                 "weight - hh"="household",
                 "weight - iu"="income unit",
                 "sih replicate weight - person "="sih replicate weight",
                 "(sih)"="",
                 "(hes)"="",
                 "or equivalent"="",
                 "[^[:alnum:]]"="",
                 "[[:space:]]"="")



  latest_variable_dictionary <- variable_dictionary %>% select(year_extracted)
extract_labels_df(dataset_no_rows) %>% mutate(string_value = tolower(value),
                                              string_value = str_replace_all(string_value,replacements),
                                              string_value = paste0(var_name,string_value),
                                              year = year_extracted) %>%
    filter(!var_name %in% latest_variable_dictionary)

}

create_variable_dictionary <- function(...) {
  #Find the labels for each variable in the history of the dataset.

  map_df(years_HH_c,create_tibble_to_compare_dataframes) %>%
    group_by(string_value) %>%
    mutate(latest_year = max(year)) }

