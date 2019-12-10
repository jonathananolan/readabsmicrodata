# readabsmicrodata

Reduce the pain involved in reading multiple years of ABS microdata


## Installation

You can install `readabsmicrodata` from Github as follows:

``` r
# If the `remotes` package is not installed, install it
if(!require(remotes)) {
  install.packages("remotes")
}

# Install `grattandata` from GitHub using remotes like this:
remotes::install_github("jonathananolan/readabsmicrodata",
                        dependencies = TRUE, 
                        upgrade = "always", 
                        build_vignettes = TRUE)
```

## About the package
The ABS's Survey of Income and Housing and Household Expenditure Survey are great, but merging multiple years can sometimes be a pain.

This package aims to overcome some of these problems by binding together many years of SIH and HES microdata and creating easy to remember variable names for the most commonly used variables. 

## Get started

To run the package you will need to first download the SAS version of the HES or SIH surveys that you are interested in. Unzip them all into a folder, with sub-folders for each year of the survey e.g. data/2015/sih15bh.sas7bdat. Keep the format files associated with each year in the same folder as the sas7bdat folder. 

Now you can import many years of the dataset.

``` r
library(readabsmicrodata)

data<- read_abs_microdata(survey = "hes",
						  file   = "household",
						  data_dir="C:/data")

```

If you work at grattan and have access to the grattan data warehouse, you can instead run: 

``` r
library(readabsmicrodata)

data<- read_abs_microdata(survey  = "hes",
						  file    = "household",
						  grattan = TRUE)

```

This function will bind together every year of your dataset into one big dataframe that contains every year of the HES or the SIH. Easy to remember names have been created for the most commonly used variables. 

For less common variables, the original name has been kept, with the addition of a suffix for the year, e.g. famcomp_2017. Where variables are used in multiple years, and the variable label is very similar in both years, the most recent year is assigned to variables for both years. This reduces the risk of merging two variables with the same name that different meanings across different years. 

When you run read_abs_microdata a data dictionary will be created to help you compare each variable. There's no subsitute for reading the ABS microdata userguide and checking for changes, but this is a good start. 


There's a few other functions that might help you analyse the HES and SIH too: 

``` r
       fix_old_vars() %>%    
```

Will take old surveys with variables in cents instead of dollars, and weights that need dividing by 10,000 - and fix them. 

``` r
inflate_survey(to_year = "2019-20")
```
Will take any item in the data dictionary marked "inflate" and inflate it to the year you specify

``` r
equivalise_survey()
```
Will take any item in the data dictionary marked "inflate" and divide it by the ABS household equivalisation factor. For earlier years we construct an equivalisation factor from the component variables. 

``` r
add_ages()
```

Adds ages in 5, 10 and 20 year groupings. Be careful at the higher age groups in earlier years, because we have not yet included the "75+" groupings as separate categories. 

``` r
add_cohorts()
```

Adds date of birth groupings in 5,10 and 20 year cohorts. These cohorts are slightly imprecise due to a change in the interval of HES surveys in 2009 and 2015. 