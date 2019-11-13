#' Inflate HES data
#'
#' This function allows you to inflate HES data. It will only inflate variables listed as 'inflate' in the filename
#' @param data_directory A directory with all the required files to run this function.  /HESvariables contains two files: HESkeyvariablenames.csv which is a list of the variables you want in your dataset and filenames.csv contains the filenames of each DTA file in your data folder.
#' @keywords HES
#' @export
#' @examples
#' @import dplyr
#' @import purrr
#' @importFrom grattandata read_microdata
#' @importFrom grattan weighted_ntile cpi_inflator


inflate_HES<- function(data,to_year){

  var.names  <- read.csv(system.file("extdata", "HESkeyvariablenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE) %>%
    filter(inflate=="Yes") %>% pull(r_var_name)

  data %>%  mutate(fy = paste0(refyear,"-",substring(as.character(refyear+1),3,4))) %>%
    mutate_at(.vars = var.names,
              .funs = list(~ cpi_inflator(.,fy,to_year)))
}

#' Equivalise all dollar value HES data
#'
#' This function allows you to inflate HES data. It will only inflate variables listed as 'inflate' in the filename
#' @param data_directory A directory with all the required files to run this function.  /HESvariables contains two files: HESkeyvariablenames.csv which is a list of the variables you want in your dataset and filenames.csv contains the filenames of each DTA file in your data folder.
#' @keywords HES
#' @export
#' @examples
#' @import dplyr
#' @import purrr

equivalise_HES<- function(data){

  var.names  <- read.csv(system.file("extdata", "HESkeyvariablenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE) %>%
    filter(inflate=="Yes") %>% pull(r_var_name)

  data %>%  mutate(fy = paste0(refyear,"-",substring(as.character(refyear+1),3,4))) %>%
    mutate_at(.vars = var.names,
              .funs = list(~ ./hh_equiv))
}

#' add cohorts
#'
#' This function allows you to inflate HES data. It will only inflate variables listed as 'inflate' in the filename
#' @param data_directory A directory with all the required files to run this function.  /HESvariables contains two files: HESkeyvariablenames.csv which is a list of the variables you want in your dataset and filenames.csv contains the filenames of each DTA file in your data folder.
#' @keywords HES
#' @export
#' @examples
#' @import dplyr
#' @import purrr
#'
#'

add_cohorts_HES <- function(data){
  cohorts <- read_csv( system.file("extdata", "cohorts.csv", package = "readabsmicrodata", mustWork = TRUE)) %>%
    gather("refyear","cohort",-age_type,-age) %>%
    mutate(refyear = as.numeric(refyear))

  cohort_5  <- cohorts %>% filter(age_type == "age_5") %>%  rename(cohort_5y  = cohort,age_5  = age) %>% select(-age_type)
  cohort_10 <- cohorts %>% filter(age_type == "age_10") %>% rename(cohort_10y = cohort,age_10 = age) %>% select(-age_type)
  cohort_20 <- cohorts %>% filter(age_type == "age_20") %>% rename(cohort_20y = cohort,age_20 = age) %>% select(-age_type)

  data %>% left_join(cohort_5) %>%
    left_join(cohort_10) %>%
    left_join(cohort_20)
}
