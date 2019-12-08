#' Create html data dictionary
#' @keywords internal
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom rio factorize
#' @importFrom kableExtra kable
#' @import knitr
#' @importFrom markdown markdownToHTML

create_data_dictionary <- function(data_dictionary_url,
                                   variable_dictionary,
                                   years_HH,
                                   grattandata,
                                   file_names,
                                   survey,
                                   file,
                                   variable_dictionary_with_new_names,
                                   ...) {



distinct_vars <- variable_dictionary_with_new_names  %>% distinct(var) %>% pull(var)

list_of_tables<- map(distinct_vars,~create_kable_for_one_variable(.,variable_dictionary_with_new_names = variable_dictionary_with_new_names))

body <- c('---',
                   'title: "test"',
                   'output: html_document',
                   '---',
                   '',
                   '## `r survey`, `r file` file',
                   '## Variables in the dataset',
                   'New variable names have been created for each variable in the dataset.',
                   '',
                   'Variable names are first assigned using the data dictionary.',
                   '',
                   'Where there is no variable name in the dictionary, the original name is used, along with the most recent year in which the variable name and label are identical to the current year.')


for (i in 1:length(list_of_tables)) {

  text<- c(paste0('### `r list_of_tables[[',i,']][1]`'),
           '',
           paste0('Most recently apeared in the dataset in `r list_of_tables[[',i,']][2]`'),
           '```{r echo=FALSE}',
           paste0('list_of_tables[[',i,']][3] %>% kable()'),
           '```')
  body <- c(body,text)
}
markdownToHTML(text = knitr::knit(text = body), output = data_dictionary_url)
browseURL(data_dictionary_url)
 }



create_kable_for_one_variable <- function(var_filtered,
                                          variable_dictionary_with_new_names) {

  filtered_var_list <- variable_dictionary_with_new_names %>% filter(var == var_filtered) %>% ungroup()

  late_year<- filtered_var_list %>% filter(year==max(year)) %>% pull(year)

  filtered_var_list_table <- filtered_var_list %>% select(`Survey variable name` = var_name,
                                                          Year = year,
                                                          `Variable description` = value) %>%
    arrange(desc(Year))

  list(var_filtered,late_year,filtered_var_list_table)}

