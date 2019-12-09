#' Import data dictionary
#'
#' Import SAS microdata from various ABS datasets, and combine them into a single file with variables given more memorable names.
#' @param survey A string containing the survey you want to import. Usually "hes" or "sih"
#' @param file A string containing the survey file of interest - usually "household", "income unit", or "person".
#' @param additional_variables A tibble with at least two columns "r_var_name" and the year of interest. Adds additional variables of interest on top of those that are already in the data dictionary. See vignette for more details
#' @param dictionary_file Only to be used if you do not want to use the pre-loaded dictionary. A tibble in the prescribed format in the Vignette.
#' @keywords
#' @export
#' @examples
#' @import dplyr



import_dictionary <- function(survey,
                              file,
                              dictionary_file = NULL,
                              additional_variables = NULL,
                              variable_dictionary_v = 1){

  # We walso want to import a data dictionary that matches up earlier years of the SIH.
  # The reason we need a data dictionary is that sometimes the ABS changes the varaible names and labels (e.g. exp13 becomes exp 12).
  # Manually matching up is the only way to make this process easier.
  if (is.null(dictionary_file)) {
    variable_dictionary <- read.csv(system.file("extdata",
                                                 paste0(survey,"_",file,"_dictionary_v_",variable_dictionary_v,".csv"),
                                                 package = "readabsmicrodata",
                                                 mustWork = TRUE),
                                     stringsAsFactors = FALSE,
                                     check.names      = FALSE)
  } else {
    variable_dictionary <- dictionary_file
  }

  if(!is.null(additional_variables)) {
    variable_dictionary<- bind_rows(variable_dictionary,additional_variables)
  } else {}

  variable_dictionary
}
