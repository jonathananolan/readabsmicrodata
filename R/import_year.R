#' Import a single year of your chosen dataset
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @keywords internal
#' @export
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom rio factorize



# Function that can import one year of the SIH
import_year_all <- function(year_num,
                            filter_cols = TRUE,
                            nrow = Inf,
                            ...) {
  if (grattandata == TRUE) {
    filename <- file_names %>%
      filter(Year == year_num,
             survey == (!! survey),
             file == (!! file)) %>%
      pull(Filename)

    formats <- file_names %>%
      filter(Year == year_num,
             survey == (!! survey),
             file == (!! file)) %>%
      mutate_if(is.character, list(~ na_if(., ""))) %>%
      pull(Formats)

    if (is.na(formats)) {
      formats <- NULL
    }

    filename_for_read_sas <- find_filename(filename)

    catalog_file_for_read_sas <- paste0(dirname(filename_for_read_sas), "/", formats)

    read_sas(filename_for_read_sas,
      catalog_file = catalog_file_for_read_sas,
      n_max = nrow) %>%
      factorize() %>% rename_all(tolower)

  } else {
    print("Grattan data is false - but this function only works with Grattan data at the moment.")
  }
}

import_one_year <- function(year, ...) {
  yearchar <- as.character(year)
  year_num <- year


  latest_variable_dictionary <- variable_dictionary %>%
    select(r_var_name,
      var_name = yearchar
    ) %>%
    filter(var_name != "") %>%
    mutate(var_name = tolower(var_name))

  if (!is.null(label_dictionary)) {
    latest_label_dictionary <- label_dictionary %>%
      filter(year == year_num) %>%
      ungroup() %>%
      select(
        r_var_name,
        var_name
      )

    latest_variable_dictionary <- latest_variable_dictionary %>% bind_rows(latest_label_dictionary)
  }

  variables_to_import <- latest_variable_dictionary %>%
    mutate(var_name_upper = toupper(var_name)) %>%
    gather("type", "var_name", var_name, var_name_upper) %>%
    pull(var_name)

  dataset_one_year <- import_year_filtered(n = Inf)



  names(dataset_one_year) <- tolower(names(dataset_one_year))

  # Create a list of variables and labels to filter to
  key.names <- latest_variable_dictionary$r_var_name %>% trimws()
  key.vars <- latest_variable_dictionary$var_name %>%
    trimws() %>%
    tolower()

  dataset_one_year <- dataset_one_year[, key.vars]
  colnames(dataset_one_year) <- key.names

  dataset_one_year <- dataset_one_year %>%
    mutate(refyear = year_num)
  # inconsistent data types
  dataset_one_year$id_hh <- as.character(dataset_one_year$id_hh)

  dataset_one_year
}


