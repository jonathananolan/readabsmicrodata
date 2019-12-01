ih_importer_hh <- function(year,...)  {
  #year=2017
  #grattandata=TRUE
  #library(grattandata)
  #library(tidyverse)
  #library(rio)
  #variable_dictionary <- import("inst/extdata/SIHkeyvariablenames.csv")
  Yearchar <- as.character(year)
  SIH_raw  <- import_year(year)


  #Create a list of value labels so that you can add in variables that have the same name and label as the most recent year of the dataset
  if (all_of_latest_year== TRUE) {
    col_names <-  extract_labels_df(SIH_raw) %>%
      mutate(value = tolower(value),
             value = str_replace_all(value,replacements),
             string_value = paste0(var_name,value)) %>%
      filter(string_value %in% label_vars$string_value,
             !(var_name %in% eval(parse(text=paste0("Varnamesfiltered_raw$`",Yearchar,"`"))))) %>%
      select(var_name) %>%
      rename(r_var_name = var_name) %>%
      mutate(!!(paste0(year)) := r_var_name)

    Varnamesfiltered <- bind_rows(Varnamesfiltered_raw,col_names) }

  #Create a list of variables and labels to filter to
  key.names <- Varnamesfiltered$r_var_name %>% trimws()
  key.vars <- eval(parse(text=paste0("Varnamesfiltered$`",Yearchar,"`"))) %>% trimws() %>% tolower()

  ##check if all the variable names are correct
  ##test<-data.frame(key.vars, key.vars %in% names(SIH_raw))

  #enforce upper case variable names as is the ABS's style.
  SIH_raw <- SIH_raw[,key.vars]
  colnames(SIH_raw) <- key.names

  SIH_raw <- SIH_raw %>% mutate(refyear = year)
  #inconsistent data types
  SIH_raw$hh_id <- as.character(SIH_raw$hh_id)

  if ("persons_15_over" %in% names(SIH_raw)) {
    SIH_raw$persons_15_over <- as.character(SIH_raw$persons_15_over) }
  if ("persons_under_15" %in% names(SIH_raw)) {
    SIH_raw$persons_under_15 <- as.character(SIH_raw$persons_under_15) }
  SIH_raw


}
