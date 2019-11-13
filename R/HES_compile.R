#' Import HES data
#'
#' This function allows you to import HES microdata from various HES years, and combine them into a single tidy dataset.
#' @param data_directory A directory with all the required files to run this function. /data contains all the HES DTA files. /HESvariables contains two files: HESkeyvariablenames.csv which is a list of the variables you want in your dataset and filenames.csv contains the filenames of each DTA file in your data folder.
#' @keywords HES
#' @export
#' @examples
#' @importFrom foreign read.dta
#' @import dplyr
#' @import purrr
#' @importFrom grattandata read_microdata


import_HES<- function(data_directory=getwd()){

### Read in data ----

#var_names<-read.csv(paste0(data_directory,"/HESvariables/HESkeyvariablenames.csv"), stringsAsFactors=FALSE)
#file_names<-read.csv(paste0(data_directory,"/HESvariables/filenames.csv"), stringsAsFactors=FALSE)

 var_names  <- read.csv(system.file("extdata", "HESkeyvariablenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)
 file_names <- read.csv(system.file("extdata", "filenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)

 # var_names <- read.csv("inst/extdata/HESkeyvariablenames.csv", stringsAsFactors=FALSE)
 # file_names <- read.csv("inst/extdata/filenames.csv", stringsAsFactors=FALSE)


years <- file_names %>% pull(Year)

hes_importer <- function(Years,...)  {
   # Years=2015
    Yearchar <- as.character(Years)
    Year <- as.numeric(Yearchar)

    # Read in the data
    HES_raw <-  read_microdata(file_names$Filename[file_names$Year==Year],haven = FALSE, convert.factors = TRUE)

    #library(grattandata)
    #library(tidyverse)
    # HES_raw   <- read_microdata("HES15BH.dta")
    #change column names to a common one.
    Columname <- paste0("HES", Yearchar)
    Varnamesfiltered <- var_names %>% filter(r_var_name!="",
                                             eval(parse(text=Columname))!="")

    ##check if all the variable names are correct
    ##test<-data.frame(key.vars, key.vars %in% names(HES_raw))

    key.names <- Varnamesfiltered$r_var_name
    key.vars <- eval(parse(text=paste0("Varnamesfiltered$HES",Yearchar))) %>% trimws()

    names(HES_raw) <- toupper(names(HES_raw))
    #Code to check for variables that are in the CSV file but not in the original HES dataset
    error_check <- tibble(x=key.vars) %>% filter(!(x %in% names(HES_raw)))
    if (nrow(error_check)>0) {
    print(paste0("For year",Yearchar, "the variables:","Are in the csv file but not in the dataset"))}

    HES_raw <- HES_raw[,key.vars]
    colnames(HES_raw) <- key.names

   HES_raw %>%
      mutate(refyear = Year) %>%
      mutate_at(vars(one_of('hhsize')), as.character) %>%
      mutate_at(vars(one_of('persons_15_over')), as.character) %>%
      mutate_if(is.factor, as.character)
}

# hes2015<- hes_importer(2015)
# hes1998<- hes_importer(1988)
# hes1993<- hes_importer(1993)
# hes1998<- hes_importer(1998)
# hes2003<- hes_importer(2003)
map_df(years,hes_importer) }
