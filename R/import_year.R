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
import_year <- function(year_num,
                            filter_cols = TRUE,
                            nrow = Inf,
                            vars = NULL,
                            survey,
                            file,
                            file_names,
                            grattandata,
                            ...) {

  if (grattandata == TRUE) {
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

    filename_for_read_sas <- find_filename(filename)

    catalog_file_for_read_sas <- paste0(dirname(filename_for_read_sas), "/", formats)

    read_sas(filename_for_read_sas,
      catalog_file = catalog_file_for_read_sas,
      n_max = nrow) %>%
      rename_all(tolower)

  } else {
    print("Grattan data is false - but this function only works with Grattan data at the moment.")
  }
}





