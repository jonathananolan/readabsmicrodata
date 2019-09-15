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

import_S<- function(data_directory=getwd()){

    ### Read in data ----

    #var.names<-read.csv(paste0(data_directory,"/HESvariables/HESkeyvariablenames.csv"), stringsAsFactors=FALSE)
    #file.names<-read.csv(paste0(data_directory,"/HESvariables/filenames.csv"), stringsAsFactors=FALSE)

var.names <- read.csv("SIHvariables/SIHkeyvariablenames.csv", stringsAsFactors=FALSE)
file.names<-read.csv("SIHvariables/filenamesSIH.csv", stringsAsFactors=FALSE)

years_HH <- c("1995","2000","2003","2005","2007","2009","2011","2013","2015")
years_IU <- c("1986","1990")

### HH ###
for (Years in years_HH) {

    Yearchar <- as.character(Years)
    Year <- as.numeric(Yearchar)

    # Read in the data
    SIH.raw <-  read_sas(file.names$Filename[file.names$Year==Year])

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

    #don't think we need to have individual year dataframes
    #assign(paste0("SIH.HH.",Year),SIH.raw)

    #bind years
    if (Years == years_HH[1]){
        data.SIH <- SIH.raw
    } else {
        data.SIH <- bind_rows(data.SIH, SIH.raw)
    }

}#end for loop

### IU ###

for (Years in years_IU) {
    Yearchar <- as.character(Years)
    Year <- as.numeric(Yearchar)

    # Read in the data
    SIH.raw <-  read_sas(file.names$Filename[file.names$Year==Year])

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

    #don't think we need to have individual year dataframes
    #assign(paste0("SIH.HH.",Year),SIH.raw)

    #bind years
    data.SIH <- bind_rows(data.SIH, SIH.raw)

}#end for loop

### Export data -----

write.csv(data.SIH, "SIH.raw.csv")
