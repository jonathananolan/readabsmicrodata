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
#' @importFrom stringr str_remove str_replace_all

equivalise_survey<- function(data){

  var_names <- import_dictionary(survey = first(data$survey),
                                 file    = first(data$file),
                                 dictionary_file = NULL,
                                 additional_variables = NULL,
                                 variable_dictionary_v = 1) %>%
    filter(inflate=="Yes") %>%
    pull(r_var_name)


  if(first(data$survey) == "hes") {

    converter <- c("One" = "1", "Two" = "2", "Three" = "3", "Four" = "3","Five" = 5, "Six" = 6, "Seven" = 7, "Eight" = 8, Not="0")
    data <- data %>%
                      mutate_at(.vars = vars(starts_with("children_number_")),
                                .funs = list(~  str_replace_all(.,converter))) %>%
                      mutate_at(.vars = vars(starts_with("children_number_")),
                                .funs = list(~suppressWarnings(as.numeric(substr(.,1,1))))) %>%
                      mutate_at(.vars = vars(starts_with("children_number_")),
                                .funs = list(~if_else(is.na(.),0,.))) %>%
                      mutate(persons_under_15 = children_number_1+
                                                children_number_2+
                                                children_number_3+
                                                children_number_4,
                             size_hh = str_replace_all(size_hh,converter),
                             persons_15_plus = if_else(is.na(persons_15_plus),
                                                       as.numeric(substr(size_hh,1,1))-persons_under_15,
                                                       as.numeric(substr(persons_15_plus,1,1))))

              }


  data %>% mutate(persons_15_plus  = suppressWarnings(as.numeric(substr(persons_15_plus,1,1))),
                  persons_under_15 = suppressWarnings(as.numeric(substr(persons_under_15,1,1))),
                  persons_under_15 = if_else(is.na(persons_under_15),0,persons_under_15),
                  equiv_hh = if_else(is.na(equiv_hh),
                                     (1 + 0.5 * (persons_15_plus - 1) + 0.3 * (persons_under_15)),
                                     equiv_hh)) %>%
      mutate_at(.vars = var_names,
                .funs = list(~ ./equiv_hh))
  }

#' Fix old vars
#'
#' Scale old versions of the HES and the SIH with outdated variables.
#' @param data a tibble created by read_abs_microdata()
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
  cohort_5y_breaks   = c(-Inf,seq(1924,1984,5),Inf)
  cohort_5y_top      = c(seq(1924,1984,5),Inf)
  cohort_5y_bottom   = c(-Inf,cohort_5y_top[2:13]-5,1984)
  cohort_5y_labels   = paste0(cohort_5y_bottom,"-",cohort_5y_top-1)

  cohort_10y_breaks   = c(-Inf,seq(1924,1984,10),Inf)
  cohort_10y_top      = c(seq(1924,1984,10),Inf)
  cohort_10y_bottom   = c(-Inf,cohort_10y_top[2:7]-10,1985)
  cohort_10y_labels   = paste0(cohort_10y_bottom,"-",cohort_10y_top-1)

  cohort_20y_breaks   = c(-Inf,seq(1924,1984,20),Inf)
  cohort_20y_top      = c(seq(1924,1984,20),Inf)
  cohort_20y_bottom   = c(-Inf,cohort_20y_top[2:4]-20,1985)
  cohort_20y_labels   = paste0(cohort_20y_bottom,"-",cohort_20y_top-1)

  print("Note that for the 2009 dataset, years were supplied by the ABS in 5 year")
        print(        "groups and yet this survey was conducted 6 years after the 2003 survey.")
print(  "For cohort group, the year of birth is adjusted one year down so that every")
print(        "person belongs to the cohort 1 year older than their actual age.")
print(        "This means for those on the edge of each DOB cohort, their allocation will be")
print(        "slightly incorrect. Because the 2015 microdata supplied individual ages,")
        print(        "this adjustment did not need to be made in 2015.")

data %>% mutate(year_for_cohorts = if_else(year == 2009, 2008, year),
                         year_of_birth_approx = case_when(grepl("and over",age) ~ NA_real_,
                                                          grepl("or older",age) ~ NA_real_,
                                                          TRUE ~ year_for_cohorts - age_approx),
                         cohort_5y = as.character(cut(year_of_birth_approx,
                                                      breaks = cohort_5y_breaks,
                                                      labels = cohort_5y_labels,
                                                      right = FALSE)),
                         cohort_10y = as.character(cut(year_of_birth_approx,
                                                       breaks = cohort_10y_breaks,
                                                       labels = cohort_10y_labels,
                                                       right = FALSE)),
                         cohort_20y = as.character(cut(year_of_birth_approx,
                                                       breaks = cohort_20y_breaks,
                                                       labels = cohort_20y_labels,
                                                       right = FALSE)))
}
