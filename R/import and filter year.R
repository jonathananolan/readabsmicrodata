#' Extract column labels
#' @keywords internal
#' @export
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom rio factorize
#' @importFrom purr map_dfr


import_and_filter_year <- function(year,
                                   variable_dictionary_with_new_names,
                                   grattandata,
                                   file,
                                   file_names,
                                   survey,
                                   ...)  {



  #Create a list of value labels so that you can add in variables that have the same name and label as the most recent year of the dataset
  if (import_vars == "all") {

    Varnamesfiltered<- variable_dictionary_with_new_names %>%
      filter (year == (!! year)) %>%
      select(var,var_name)

  #Create a list of variables and labels to filter to
  key.names <- Varnamesfiltered$var %>% trimws()
  key.vars <- Varnamesfiltered$var_name %>% trimws()

  import_year(year,
              col_select = Varnamesfiltered$var_name,
              grattandata = grattandata,
              file_names = file_names,
              file = file,
              survey = survey) %>%
    factorize() %>% rename_at(vars(key.vars), ~ key.names) } }
