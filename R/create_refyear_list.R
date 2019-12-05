#Figure out what years we want to analyse. If refyears is null it will try and import every year,
#otherwise it imports the list provided by the data dictionary
create_refyear_list <- function() {
  if (is.null(refyears)) {
    file_name_refyears <- file_names %>%  filter(survey == (!! survey),
                           file == (!! file)) %>%
                           select(Year) %>%
      as_tibble()

    dictionary_refyears <- variable_dictionary %>%
                           select(matches("1|2")) %>%
                           names() %>% as.numeric() %>%
                           tibble(Year =.)


    file_names_missing <- anti_join(file_name_refyears,dictionary_refyears, by = "Year") %>% pull(Year)

    if(!is_empty(dictionary_missing)){
      print(paste0("The following years are in your dictionary file but not in your file name file:",file_names_missing))
    }

    file_names_missing <- anti_join(dictionary_refyears,file_name_refyears, by = "Year")

     if(!is_empty(file_names_missing)){
      print(paste0("The following years are in your dictionary file but not in your file name file:", file_names_missing))
     }

    inner_join(file_name_refyears,dictionary_refyears, by = "Year") %>% pull(Year)

  } else {
    refyears
  }
}
