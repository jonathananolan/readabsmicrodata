#' Import SIH data
#'
#' This function allows you to import HES microdata from various HES years, and combine them into a single tidy dataset.
#' @param data_directory A directory with all the required files to run this function. /data contains all the HES DTA files. /HESvariables contains two files: HESkeyvariablenames.csv which is a list of the variables you want in your dataset and filenames.csv contains the filenames of each DTA file in your data folder.
#' @keywords HES
#' @export
#' @examples
#' @importFrom haven read_sas
#' @import dplyr
#' @import purrr
#' @importFrom sjlabelled as_label

import_SIH <- function(data_directory=getwd(),grattandata=FALSE) {
  ### Read in data ----
  #  library(tidyverse)

  var_names  <- read.csv(system.file("extdata", "SIHkeyvariablenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)
  file_names <- read.csv(system.file("extdata", "SIHfilenames.csv",        package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)


  # Years=2005
  # data_directory="/Users/jnolan1/Dropbox (Grattan Institute)/Housing affordability/Data and analysis/Financial_stress_long"
  # file_names = read.csv("/Users/jnolan1/Documents/GitHub/attractingreport/readabsmicrodata/inst/extdata/SIHfilenames.csv", stringsAsFactors=FALSE)
  # var_names = read.csv("/Users/jnolan1/Documents/GitHub/attractingreport/readabsmicrodata/inst/extdata/SIHkeyvariablenames.csv", stringsAsFactors=FALSE)
  # Yearchar <- as.character(Years)
  # Year <- as.numeric(Yearchar)



  ### HH ###

  sih_importer_hh <- function(year,...)  {
    #year=2015
    #  grattandata=TRUE
    #  library(grattandata)
    #  library(tidyverse)
    #  library(rio)
    #  var_names <- import("inst/extdata/SIHkeyvariablenames.csv")
    Yearchar = as.character(year)
    if(grattandata==TRUE) {
      filename <- file_names %>% filter(Year==year) %>%
        mutate(full_path=paste0(Filename)) %>%
        pull(full_path)

      formats <- file_names %>% filter(Year==year) %>%
        mutate_if(is.character, list(~na_if(., ""))) %>%
        pull(Formats)

      if(is.na(formats)) {
       formats <- NULL}

      SIH_raw <- read_microdata(filename,catalog_file = formats )

      if(!is.null(formats)) {
        SIH_raw <- SIH_raw %>% as_label()}
    }

    #change column names to a common one.
    Columname <- paste0("SIH", Yearchar)
    Varnamesfiltered <- filter(var_names, eval(parse(text=Columname))!="")

    ##check if all the variable names are correct
    ##test<-data.frame(key.vars, key.vars %in% names(SIH_raw))

    key.names <- Varnamesfiltered$r_var_name %>% trimws()
    key.vars <- eval(parse(text=paste0("Varnamesfiltered$SIH",Yearchar)))%>% trimws()
    names(SIH_raw) <- toupper(names(SIH_raw))
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

  years_HH <- file_names %>% filter(!(Year %in% c(1990,2000,1986))) %>%  pull(Year) %>% rev()

    # file_names<- import("inst/extdata/SIHfilenames.csv")
    # var_names<-read.csv("inst/extdata/SIHkeyvariablenames.csv", stringsAsFactors=FALSE)

map_df(years_HH,sih_importer_hh)

}

