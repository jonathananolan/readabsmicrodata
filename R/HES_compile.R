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

import_HES<- function(data_directory=getwd()){

### Read in data ----

#var.names<-read.csv(paste0(data_directory,"/HESvariables/HESkeyvariablenames.csv"), stringsAsFactors=FALSE)
#file.names<-read.csv(paste0(data_directory,"/HESvariables/filenames.csv"), stringsAsFactors=FALSE)

 var.names  <- read.csv(system.file("extdata", "HESkeyvariablenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)
 file.names <- read.csv( system.file("extdata", "filenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)



years <- file.names %>% pull(Year)

hes_importer <- function(Years,...)  {
   #Years=1998
    Yearchar <- as.character(Years)
    Year <- as.numeric(Yearchar)

    # Read in the data
    HES.raw <-  read.dta(paste0(data_directory,"/",file.names$Filename[file.names$Year==Year]))

    #change column names to a common one.
    Columname <- paste0("HES", Yearchar)
    Varnamesfiltered <- filter(var.names, eval(parse(text=Columname))!="")

    ##check if all the variable names are correct
    ##test<-data.frame(key.vars, key.vars %in% names(HES.raw))

    key.names <- Varnamesfiltered$r.var.name
    key.vars <- eval(parse(text=paste0("Varnamesfiltered$HES",Yearchar))) %>% trimws()
    names(HES.raw) <- toupper(names(HES.raw))
    #Code to check for variables that are in the CSV file but not in the original HES dataset
    #tibble(x=key.vars) %>% filter(!(x %in% names(HES.raw)))
    HES.raw <- HES.raw[,key.vars]
    colnames(HES.raw) <- key.names

    HES.raw %>% mutate(refyear = Year) %>%
      mutate_at(vars(one_of('hhsize')), as.character) %>%
      mutate_at(vars(one_of('persons.15.over')), as.character) %>%
      mutate_if(is.factor, as.character)
}#end for loop

map_df(years,hes_importer) }
