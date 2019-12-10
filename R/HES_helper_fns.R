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


inflate_survey <- function(data,to_year) {

  var_names <- import_dictionary(survey = first(data$survey),
                                file    = first(data$file),
                                dictionary_file = NULL,
                                additional_variables = NULL,
                                variable_dictionary_v = 1) %>%
               filter(inflate=="Yes") %>%
              pull(r_var_name)

  data %>%  mutate(financial_year = paste0(year,"-",substring(as.character(year+1),3,4))) %>%
    mutate_at(.vars = vars(one_of(var_names)),
              .funs = list(~ cpi_inflator(.,financial_year,to_year)))
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
#' @importFrom stringr str_remove

equivalise_survey<- function(data){

  var_names <- import_dictionary(survey = first(data$survey),
                                 file    = first(data$file),
                                 dictionary_file = NULL,
                                 additional_variables = NULL,
                                 variable_dictionary_v = 1) %>%
    filter(inflate=="Yes") %>%
    pull(r_var_name)


  if(first(data$survey) == "hes") {
    data <- data %>% mutate(persons_under_15 = d1age+
                                               d2age+
                                               d3age+
                                               d4age) }


    data <- data %>% mutate(persons_15_plus  = as.numeric(substr(persons_15_plus,1,1)),
                            persons_under_15 = as.numeric(substr(persons_under_15,1,1)),
                            persons_under_15 = if_else(is.na(persons_under_15),0,persons_under_15),
                            equiv_hh = if_else(is.na(equiv_hh),
                                               (1 + 0.5 * (persons_15_plus - 1) + 0.3 * (persons_under_15)),
                                               equiv_hh)) %>%
      mutate_at(.vars = var_names,
                .funs = list(~ ./equiv_hh))
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

fix_old_vars<- function(data){

  data<- data %>%
           mutate_at(.vars = vars(ends_with("_10000")),
                     .funs = list(~./10000))  %>%
           mutate_at(.vars = vars(ends_with("_cents")),
                     .funs = list(~./100))

  data<-data %>%
    split.default(str_remove(names(.), "_cents")) %>%
    map_df(~ coalesce(!!! .x)) %>%
    #or use
    # map_df(reduce, coalesce) %>%
    bind_cols(., select(data, ends_with("_cents"))) %>%
    select(-ends_with("_cents"))


  data<-data %>%
    split.default(str_remove(names(.), "_10000")) %>%
    map_df(~ coalesce(!!! .x)) %>%
    #or use
    # map_df(reduce, coalesce) %>%
    bind_cols(., select(data, ends_with("_10000"))) %>%
    select(-ends_with("_10000"))

  data
}

#' add ages
#'
#' This function allows you to inflate HES data. It will only inflate variables listed as 'inflate' in the filename
#' @param data_directory A directory with all the required files to run this function.  /HESvariables contains two files: HESkeyvariablenames.csv which is a list of the variables you want in your dataset and filenames.csv contains the filenames of each DTA file in your data folder.
#' @keywords HES
#' @export
#' @examples
#' @import dplyr
#' @importFrom stringi stri_extract_last_regex

add_ages <- function(data){

  age_cutter <- function(data,year_range){
    varname = paste0("age_",year_range)

    breaks_seq = seq(5,105,year_range)

    label_interval <- function(breaks) {
      paste0( breaks[1:length(breaks) - 1], "-", breaks[2:length(breaks)]-1)
    }

    data %>% mutate(!!varname := cut(age_approx,
                                     breaks = breaks_seq,
                                     labels = label_interval(breaks_seq),
                                     right  = FALSE))
  }


data %>% mutate(age_approx = as.numeric(stri_extract_last_regex(age, "\\d{2}"))) %>%
                    age_cutter(5) %>%
    age_cutter(10) %>%
    age_cutter(20)

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

add_cohorts <- function(data){
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
