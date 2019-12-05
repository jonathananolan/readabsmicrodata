#' Import SIH data
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


read_abs_microdata <- function(survey = "sih",
                               file = "household",
                               refyears = NULL,
                               all_of_latest_year = TRUE,
                               additional_variables = NULL,
                               variable_dictionary_v = "1",
                               grattandata = FALSE,
                               file_names = NULL,
                               variable_dictionary = NULL) {


   library(rio)
   library(tidyverse)
   library(grattandata)
   library(haven)
   Years=2015
   refyears = NULL
   file_names = read.csv("inst/extdata/filenames.csv", stringsAsFactors=FALSE)
     = read.csv("inst/extdata/sih_household_dictionary_v_1.csv", stringsAsFactors=FALSE,check.names = FALSE)
   Yearchar <- as.character(Years)
   Year <- as.numeric(Yearchar)
   year = 2015
   grattandata = TRUE
   variable_dictionary = NULL
   all_of_latest_year = TRUE
   survey = "sih"
   file = "household"
   variable_dictionary_v = 1
   additional_variables = NULL


  #We want to import in the filenames of our SAS files. If Grattandata is true then the filenames are stored in the package, otherwise the
  #user needs to provide their own filenames to the 'file_names' argument to the function.
  if (grattandata == TRUE) {
    file_names <- read.csv(system.file("extdata",
                                       paste0(survey,"_",file,"_filenames.csv"),
                                       package = "readabsmicrodata",
                                       mustWork = TRUE),
                           stringsAsFactors=FALSE)
                           } else {}

  # We walso want to import a data dictionary that matches up earlier years of the SIH.
  # The reason we need a data dictionary is that sometimes the ABS changes the varaible names and labels (e.g. exp13 becomes exp 12).
  # Manually matching up is the only way to make this process easier.
  if (is.null(variable_dictionary)) {
    variable_dictionary  <- read.csv(system.file("extdata",
                                                 paste0(survey,"_",file,"_dictionary_v_",variable_dictionary_v,".csv"),
                                                 package = "readabsmicrodata",
                                                 mustWork = TRUE),
                                     stringsAsFactors = FALSE,
                                     check.names      = FALSE)
  } else {}


  #This process imports every variable from the most recent year, but also all the variables of previous year
  #but only if the label and the variable name exactly match.

  all_variables <- create_var_list(latest_year = latest_year,
                                   variable_dictionary = variable_dictionary ,
                                   years_HH = years_HH,
                                   all_of_latest_year = all_of_latest_year,
                                   grattandata = grattandata,
                                   file_names = file_names,
                                   additional_variables = additional_variables)

  ### HH ###





  # file_names<- import("inst/extdata/SIHfilenames.csv")
  # variable_dictionary<-read.csv("inst/extdata/SIHkeyvariablenames.csv", stringsAsFactors=FALSE)

  map_df(years_HH,import_and_filter_year)

}


