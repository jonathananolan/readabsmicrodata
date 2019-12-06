#' Extract column labels
#' @keywords internal
#' @export
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom rio factorize
#' @importFrom purr map_dfr

extract_labels_col <- function(df, col) {

  labels <- attr(df[[col]], "label") %>%
    enframe() %>%
    mutate(var_name = col, name=as.character(name),value=as.character(value))

  labels
}

extract_labels_df <- function(df) {
  df_labels <- map_dfr(colnames(df),
                       ~ extract_labels_col(df = df, col = .))
  tibble(var_name = names(df)) %>%
    filter(!(var_name %in% df_labels$var_name)) %>%
    bind_rows(df_labels)
}


create_df_with_var_labels_one_year <- function(year_extracted,
                                               grattandata,
                                               file_names,
                                               ...) {

  dataset_no_rows <- import_year(year_extracted,
                                 filter_cols = FALSE,
                                 grattandata = grattandata,
                                 file_names = file_names,
                                 file = file,
                                 survey = survey,
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
                 "business" = "bus",
                 "investment" = "inv",
                 "-" = "",
                 "[^[:alnum:]]"="",
                 "[[:space:]]"="")



  extract_labels_df(dataset_no_rows) %>%mutate(string_value = tolower(value),
                                               string_value = str_replace_all(string_value,replacements),
                                               string_value = substr(string_value,1,30),
                                               string_value = paste0(var_name,string_value),
                                               year = year_extracted)

}

create_df_with_var_labels_multiple_years <- function(variable_dictionary,
                                                     years_HH,
                                                     grattandata,
                                                     file_names,
                                                     file,
                                                     survey,
                                                     ...) {
  #Find the labels for each variable in the history of the dataset.

  map_df(as.character(years_HH), ~create_df_with_var_labels_one_year(.,grattandata = grattandata,
                                                                     variable_dictionary = variable_dictionary,
                                                                     file_names = file_names,
                                                                     file = file,
                                                                     survey = survey
                                                                    )) %>%
    group_by(string_value) %>%
    mutate(latest_year = max(year)) %>%
    ungroup() %>%
    group_by(var_name) %>%
    mutate(latest_year_var = max(year)) %>%
    ungroup()
}

create_df_with_var_labels_and_dictionary_labels <- function(variable_dictionary,
                                                            years_HH,
                                                            grattandata,
                                                            file_names,
                                                            file,
                                                            survey,
                                                            ...) {
  variable_dictionary_long <- variable_dictionary %>%
    select(r_var_name,matches("1|2")) %>%
    rowid_to_column(var = "rowid") %>%
    gather("year","var_name",-r_var_name,-rowid) %>%
    filter(var_name != "") %>%
    mutate(var_name  = tolower(var_name))

  create_df_with_var_labels_multiple_years(years_HH = years_HH,
                                           grattandata = grattandata,
                                           file_names = file_names,
                                           file = file,
                                           survey = survey) %>%
    left_join(variable_dictionary_long, by = c("var_name", "year") )%>%
    mutate(dictionary_var = if_else(is.na(r_var_name),0,1),
           var = case_when(!is.na(r_var_name) ~ r_var_name,
                           TRUE ~ paste0(var_name,"_",latest_year))) %>%
    arrange(rowid,desc(latest_year_var),var_name,desc(year))
  }

