#' Import a single year of your chosen dataset
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @keywords internal
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom rio factorize
#' @import grattandata



# Function that can import one year of the SIH
import_year <- function(year_num,
                            filter_vars = NULL,
                            nrow = Inf,
                            survey,
                            file,
                            file_names,
                            grattandata,
                            data_dir,
                            ...) {

  filename <- file_names %>%
    filter(Year == year_num,
           survey_name == survey,
           file_name == file) %>%
    pull(Filename)

  formats <- file_names %>%
    filter(Year == year_num,
           survey_name == survey,
           file_name == file) %>%
    mutate_if(is.character, list(~ na_if(., ""))) %>%
    pull(Formats)

  if (is.na(formats)) {
    formats <- NULL
  }


  if (grattandata == TRUE) {
    filename_for_read_sas <- find_filename(filename)
    catalog_file_for_read_sas <- paste0(dirname(filename_for_read_sas), "/", formats)

  } else {
    filename_for_read_sas<- find_filename_non_grattan(filename = filename,
                                                      data_warehouse_path = data_dir)
    catalog_file_for_read_sas <- paste0(dirname(filename_for_read_sas), "/", formats)
      }

  read_sas(filename_for_read_sas,
           catalog_file = catalog_file_for_read_sas,
           n_max = nrow) %>%
    rename_all(tolower)
}


import_year_filtered<- function(year_num,
                     variable_dictionary_with_new_names,
                     nrow = Inf,
                     survey,
                     file,
                     file_names,
                     grattandata,
                     data_dir) {

  filtered_vars_lower <- variable_dictionary_with_new_names %>% filter(year == year_num) %>% pull(var_name)
  new_vars_lower <- variable_dictionary_with_new_names %>% filter(year == year_num) %>% pull(var)

  filtered_vars_upper <- filtered_vars_lower %>% toupper()
  filtered_vars <- c(filtered_vars_upper,
                     filtered_vars_lower)

  import_year(year_num,
              filter_vars = filter_vars,
              nrow = Inf,
              survey = survey,
              file = file,
              file_names = file_names,
              grattandata = grattandata,
              data_dir = data_dir) %>%
              factorize() %>%
    rename_at(vars(filtered_vars_lower), ~ new_vars_lower) %>%
    mutate(year =year_num )

      }


