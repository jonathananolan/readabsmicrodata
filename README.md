# readabsmicrodata

Reduce the pain involved in reading multiple years of ABS microdata


## Installation

## Installation

You can install `grattandata` from Github as follows:

``` r
# If the `remotes` package is not installed, install it
if(!require(remotes)) {
  install.packages("remotes")
}

# Install `grattandata` from GitHub using remotes like this:
remotes::install_github("readabsmicrodata",
                        dependencies = TRUE, 
                        upgrade = "always", 
                        build_vignettes = TRUE)
```

## About the package
The ABS's Survey of Income and Housing and Household Expenditure Survey are great, but looking at multiple years can sometimes be a pain. Sometimes variables can have the same name with different meaning. The ABS also makes subtle changes, like using upper or lower case variable names in different years, or storing values in cents instead of dollars. 

This package aims to overcome this by binding together many years of SIH and HES microdata and creating a common, easy to understand variable name for the most commonly used information. 

## Get started

To run the package you will need to first download the SAS version of any HES or SIH that you are interested in. Unzip them all into a folder, with sub-folders for each year of the dataset. Keep the format files associated with each year in the same folder as the sas7bdat folder. 

Now you can import many years of the dataset. 
``` r
# If the `remotes` package is not installed, install it
library(readabsmicrodata)

data<- read_abs_microdata(survey = "hes","household",grattan=TRUE)

```

This function will bind together every year of your dataset into one big dataframe that contains every year of the HES or the SIH. Easy to remember variable names have been created for the most commonly used variables. 

For less common variables, the original variable name has been kept, with the addition of a suffix for the latest year in which that variable appears in the dataset: e.g. famcomp_2017. variables with the same name from earlier years are only assinged to famcomp_2017 where both the variable name and the label are similar. This reduces the risk of merging two variables with the same name that different meanings across different years. 

When you run read_abs_microdata a data dictionary will be created to help you compare each variable and understand what years contain which variables.  