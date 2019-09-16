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

import_SIH <- function(data_directory=getwd()) {
    ### Read in data ----

    #var.names<-read.csv(paste0(data_directory,"/HESvariables/HESkeyvariablenames.csv"), stringsAsFactors=FALSE)
    #file.names<-read.csv(paste0(data_directory,"/HESvariables/filenames.csv"), stringsAsFactors=FALSE)

var.names  <- read.csv(system.file("extdata", "SIHkeyvariablenames.csv", package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)
file.names <- read.csv(system.file("extdata", "SIHfilenames.csv",        package = "readabsmicrodata", mustWork = TRUE), stringsAsFactors=FALSE)


# Years=2005
# data_directory="/Users/jnolan1/Dropbox (Grattan Institute)/Housing affordability/Data and analysis/Financial_stress_long"
# file.names = read.csv("/Users/jnolan1/Documents/GitHub/attractingreport/readabsmicrodata/inst/extdata/SIHfilenames.csv", stringsAsFactors=FALSE)
# var.names = read.csv("/Users/jnolan1/Documents/GitHub/attractingreport/readabsmicrodata/inst/extdata/SIHkeyvariablenames.csv", stringsAsFactors=FALSE)
# Yearchar <- as.character(Years)
# Year <- as.numeric(Yearchar)



years_HH <- c("1995","2000","2003","2005","2007","2009","2011","2013","2015")
years_IU <- c("1986","1990")




### HH ###

sih_importer_hh <- function(Years,...)  {
    Yearchar <- as.character(Years)
    Year <- as.numeric(Yearchar)
    # Read in the data
    SIH.raw <-  read_sas(paste0(data_directory,"/",file.names$Filename[file.names$Year==Year]))

    #change column names to a common one.
    Columname <- paste0("SIH", Yearchar)
    Varnamesfiltered <- filter(var.names, eval(parse(text=Columname))!="")

    ##check if all the variable names are correct
    ##test<-data.frame(key.vars, key.vars %in% names(SIH.raw))

    key.names <- Varnamesfiltered$r.var.name
    key.vars <- eval(parse(text=paste0("Varnamesfiltered$SIH",Yearchar)))
    names(SIH.raw) <- toupper(names(SIH.raw))
    SIH.raw <- SIH.raw[,key.vars]
    colnames(SIH.raw) <- key.names

    SIH.raw <- SIH.raw %>% mutate(refyear = Year)

    #inconsistent data types
    SIH.raw$hh.id <- as.character(SIH.raw$hh.id)

SIH.raw

}

hh<- map_df(years_HH,sih_importer_hh)



### IU ###

import_sih_iu<- function(Years,...) {

    Yearchar <- as.character(Years)
    Year <- as.numeric(Yearchar)

    # Read in the data
    SIH.raw <-  read_sas(paste0(data_directory,"/",file.names$Filename[file.names$Year==Year]))


    #change column names to a common one.
    Columname <- paste0("SIHIU", Yearchar)
    Varnamesfiltered <- filter(var.names, eval(parse(text=Columname))!="")

    ##check if all the variable names are correct
    ##test<-data.frame(key.vars, key.vars %in% names(SIH.raw))

    key.names <- Varnamesfiltered$r.var.name
    key.vars <- eval(parse(text=paste0("Varnamesfiltered$SIHIU",Yearchar)))
    names(SIH.raw) <- toupper(names(SIH.raw))
    SIH.raw <- SIH.raw[,key.vars]
    colnames(SIH.raw) <- key.names

    SIH.raw <- SIH.raw %>% mutate(refyear = Year)

    #inconsistent data types
    SIH.raw$hh.id <- as.character(SIH.raw$hh.id)

SIH.raw

}#end for loop

print("yay, we mapped the first one")
iu<- map_df(years_IU,import_sih_iu)
print("yay, we mapped the second one")
bind_rows(iu,hh)
}

