#' Import SIH data
#'
#' Import SAS microdata from various ABS datasets, and combine them into a single file with variables given more memorable names.
#' @param survey A string containing the survey you want to import. Usually "hes" or "sih"
#' @param file A string containing the survey file of interest - usually "household", "income unit", or "person".
#' @param data_dir a string containing the file path to a directory that contains SAS files for your dataset. Within each subfolder of the directory there must also be a formats file.
#' @param refyears A vector containing the years you want to import. If blank all years in the dictionary file are imported.
#' @param create_html_dictionary Mark TRUE if you want to create a HTML data dictionary to inspect the labels for each variable.
#' @param additional_variables A tibble with at least two columns "r_var_name" and the year of interest. Adds additional variables of interest on top of those that are already in the data dictionary. See vignette for more details
#' @param variable_dictionary_v Version of the preloaded dictionary you wish to use
#' @param grattandata Mark true if you want to use the SAS files in the Grattan Institute's data warehouse.
#' @param file_names Only to be used if grattandata is false. A tibble in the prescribed format in the Vignette.
#' @param dictionary_file Only to be used if you do not want to use the pre-loaded dictionary. A tibble in the prescribed format in the Vignette.
#' @keywords SIH
#' @export
#' @examples
#' @importFrom haven read_sas
#' @import dplyr
#' @import purrr
#' @importFrom rio factorize
#' @importFrom data.table rbindlist


read_abs_microdata <- function(survey = "sih",
                               file = "household",
                               refyears = NULL,
                               data_dir = getwd(),
                               import_vars = "all",
                               additional_variables = NULL,
                               variable_dictionary_v = "1",
                               grattandata = FALSE,
                               file_name_file = NULL,
                               dictionary_file = NULL,
                               create_html_dictionary = TRUE
                               ) {

  print("Importing survey filenames")
  #We want to import in the filenames of our SAS files. If Grattandata is true then the filenames are stored in the package, otherwise the
  #user needs to provide their own filenames to the 'file_names' argument to the function.
  if (is.null(file_name_file)) {
    file_names <- read.csv(system.file("extdata",
                                       paste0("filenames.csv"),
                                       package = "readabsmicrodata",
                                       mustWork = TRUE),
                           stringsAsFactors=FALSE)
  } else {
    file_names <- file_name_file
  }


  print("Importing survey data dictionary")
  # We walso want to import a data dictionary that matches up earlier years of the SIH.
  # The reason we need a data dictionary is that sometimes the ABS changes the varaible names and labels (e.g. exp13 becomes exp 12).
  # Manually matching up is the only way to make this process easier.



  variable_dictionary <- import_dictionary(survey = survey,
                                           file = file,
                                           dictionary_file = dictionary_file,
                                           additional_variables = additional_variables,
                                           variable_dictionary_v = variable_dictionary_v)

 years_HH <- create_refyear_list(refyears  = refyears,
                                 file_names = file_names,
                                 variable_dictionary = variable_dictionary,
                                 survey = survey,
                                 file = file)

 variable_dictionary_with_new_names<- create_df_with_var_labels_and_dictionary_labels(variable_dictionary = variable_dictionary,
                                                                                      years_HH = years_HH,
                                                                                      grattandata = grattandata,
                                                                                      file_names = file_names,
                                                                                      survey = survey,
                                                                                      file = file,
                                                                                      data_dir = data_dir
                                                                                      )

   if (create_html_dictionary == TRUE) {
     print("Creating HTML data dictionary")
     create_data_dictionary(data_dictionary_url = paste0(survey,"_",file,"_dictionary.html"),
                            variable_dictionary = variable_dictionary,
                            years_HH = years_HH,
                            grattandata = grattandata,
                            file_names = file_names,
                            survey = survey,
                            file = file,
                            variable_dictionary_with_new_names = variable_dictionary_with_new_names)
     }

 print("Importing data")
 data<- map(years_HH,~import_year_filtered(.,year_num,
                                    variable_dictionary_with_new_names = variable_dictionary_with_new_names,
                                    survey = survey,
                                    file = file,
                                    file_names = file_names,
                                    grattandata = grattandata,
                                    data_dir = data_dir))

rbindlist(data,fill = TRUE) %>% mutate(survey = (!! survey),
                                         file = (!! file ))
  }




# library(tidyverse)
# library(data.table)
# library(rio)
# library(Hmisc)
# library(haven)
# library(grattandata)
# library(kableExtra)



