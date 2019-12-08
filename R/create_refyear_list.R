#otherwise it imports the list provided by the data dictionary


#' Create refyear list
#'
#' Figure out what years we want to analyse. If refyears is null it will try and import every year
#' Otherwise it imports the list provided by the data dictionary
#' @keywords SIH
#' @import dplyr
#' @import purrr


create_refyear_list <- function (refyears=NULL,
                                file_names,
                                variable_dictionary,
                                survey,
                                file,
                                ...) {


  if (is.null(refyears)) {
    file_name_refyears <- file_names %>%
                            filter(survey_name == survey,
                                     file_name == file) %>%
                           select(Year) %>%
      as_tibble()

    dictionary_refyears <- variable_dictionary %>%
                           select(matches("1|2")) %>%
                           names() %>% as.numeric() %>%
                           tibble(Year =.)


    dictionary_missing <- anti_join(file_name_refyears,dictionary_refyears, by = "Year") %>% pull(Year)

    if(!is_empty(dictionary_missing)){
      print(paste0("The following years are in your dictionary file but not in your file name file: ",paste(dictionary_missing,sep = ", ")))
    }

    file_names_missing <- anti_join(dictionary_refyears,file_name_refyears, by = "Year") %>% pull(Year)

     if(!is_empty(file_names_missing)){
      print(paste0("The following years are in your filename file but not in your dictionary file: ", paste(file_names_missing,sep = ", ")))
     }

    inner_join(file_name_refyears,dictionary_refyears, by = "Year") %>% pull(Year)

  } else {
    refyears
  }
}

