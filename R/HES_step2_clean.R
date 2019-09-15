#' Import HES data
#'
#' This function allows you to clean HES data.
#' @param data a vector containing all the HES years you want to import.
#' @keywords HES
#' @export
#' @examples
#' @import dplyr

clean_HES<- function(data) {

### Clean-up -----

## Age groups

# Some ages are grouped, some are single years -- let's split them out into a single age number.

data<- data %>%
  mutate(age.approx = as.numeric(gsub(".*([0-9]{2})\\syears.*",    # retreive single numeric for age
                                                       "\\1",
                                                       age.desc)))
data<- data %>%
  mutate(age.group = case_when(age.approx < 25 ~ "15-24",
                               age.approx < 35 ~ "25-34",
                               age.approx < 45 ~ "35-44",
                               age.approx < 55 ~ "45-54",
                               age.approx < 65 ~ "55-64",
                               age.approx < 75 ~ "65-74",
                               TRUE ~ "75 and over"))


## Get rid of cents and replace them with dollars

data<- data %>%
  mutate(
    exp.housing     = ifelse(is.na(exp.housing), exp.housing.cents/100, exp.housing),
    exp.fuel        = ifelse(is.na(exp.fuel), exp.fuel.cents/100, exp.fuel),
    exp.food        = ifelse(is.na(exp.food), exp.food.cents/100, exp.food),
    exp.alcohol     = ifelse(is.na(exp.alcohol), exp.alcohol.cents/100, exp.alcohol),
    exp.tobacco     = ifelse(is.na(exp.tobacco), exp.tobacco.cents/100, exp.tobacco),
    exp.clothing    = ifelse(is.na(exp.clothing), exp.clothing.cents/100, exp.clothing),
    exp.furnishings = ifelse(is.na(exp.furnishings), exp.furnishings.cents/100, exp.furnishings),
    exp.hhservices  = ifelse(is.na(exp.hhservices), exp.hhservices.cents/100, exp.hhservices),
    exp.medical     = ifelse(is.na(exp.medical), exp.medical.cents/100, exp.medical),
    exp.transport   = ifelse(is.na(exp.transport), exp.transport.cents/100, exp.transport),
    exp.recreation  = ifelse(is.na(exp.recreation), exp.recreation.cents/100, exp.recreation),
    exp.personal    = ifelse(is.na(exp.personal), exp.personal.cents/100, exp.personal),
    exp.misc        = ifelse(is.na(exp.misc), exp.misc.cents/100, exp.misc),
    exp.tax         = ifelse(is.na(exp.tax), exp.tax.cents/100, exp.tax),
    exp.mortgage    = ifelse(is.na(exp.mortgage), exp.mortgage.cents/100, exp.mortgage),
    exp.ochc        = ifelse(is.na(exp.ochc), exp.ochc.cents/100, exp.ochc),
    exp.super       = ifelse(is.na(exp.super), exp.super.cents/100, exp.super),
    exp.total.g.s   = ifelse(is.na(exp.total.g.s), exp.total.g.s.cents/100, exp.total.g.s),
    exp.total       = ifelse(is.na(exp.total), exp.total.cents/100, exp.total),
    total.benefits  = ifelse(is.na(total.benefits), total.benefits.cents/100, total.benefits),
    total.ind.benefits  = ifelse(is.na(total.ind.benefits), total.ind.benefits.cents/100, total.ind.benefits),
    ind.health      = ifelse(is.na(ind.health), ind.health.cents/100, ind.health),
    ind.edu         = ifelse(is.na(ind.edu), ind.edu.cents/100, ind.edu),
    ind.welfare     = ifelse(is.na(ind.welfare), ind.welfare.cents/100, ind.welfare),
    total.taxes     = ifelse(is.na(total.taxes), total.taxes.cents/100, total.taxes))


## Adjust weights

data<- data %>%
  mutate(weight.final = ifelse(is.na(weight), weight.10000/10000, weight))


## Create equivalising factor for earlier years

# check: unique(data$hhsize)

data<- data %>%
  mutate(Persons = case_when(hhsize %in% c(1, "1 person", "One person") ~ 1,
                             hhsize %in% c(2, "2 persons", "Two persons") ~ 2,
                             hhsize %in% c(3, "3 persons", "Three persons") ~ 3,
                             hhsize %in% c(4, "4 persons", "Four persons") ~ 4,
                             hhsize %in% c(5, "5 persons", "Five persons") ~ 5,
                             hhsize %in% c(6, "6 persons", "Six persons") ~ 6,
                             hhsize %in% c(7, "7 or more persons", "Seven persons") ~ 7,
                             hhsize %in% c(8, "Eight or more persons") ~ 8,
                             hhsize == 9 ~ 9,
                             hhsize == 10 ~ 10,
                             is.na(hhsize) ~ NA_real_,
                             TRUE ~ 0))

# check unique(data$persons.15.over)

data<- data %>%
  mutate(Adults = case_when(persons.15.over %in% c(1, "1 person aged 15 years and over") ~ 1,
                            persons.15.over %in% c(2, "2 persons aged 15 years and over") ~ 2,
                            persons.15.over %in% c(3, "3 persons aged 15 years and over") ~ 3,
                            persons.15.over %in% c(4, "4 persons aged 15 years and over") ~ 4,
                            persons.15.over %in% c(5, "5 persons aged 15 years and over") ~ 5,
                            persons.15.over %in% c(6, "6 persons aged 15 years and over") ~ 6,
                            persons.15.over == 7 ~ 7,
                            is.na(persons.15.over) ~ NA_real_,
                            TRUE ~ 0))

# HES 88 & 93 are NA for persons.15.over -- need to add in d1 to d4 variables

data<- data %>%
  mutate(d1age = case_when(d1age %in% c(1,"One person", "1 peson") ~ 1,
                           d1age %in% c(2,"Two person", "2 persons") ~ 2,
                           d1age %in% c(3,"Three persons", "3 persons") ~ 3,
                           is.na(d1age) ~ NA_real_,
                           TRUE ~ 0))

data<- data %>%
  mutate(d2age = case_when(d2age %in% c(1, "One person", "1 person") ~ 1,
                           d2age %in% c(2,"Two person", "2 persons") ~ 2,
                           d2age %in% c(3,"Three persons", "3 persons") ~ 3,
                           is.na(d2age) ~ NA_real_,
                           TRUE ~ 0))

data<- data %>%
  mutate(d3age = case_when(d3age %in% c(1, "One person", "1 person") ~ 1,
                           d3age %in% c(2, "Two person", "2 persons") ~ 2,
                           d3age %in% c(3, "Three persons", "3 persons") ~ 3,
                           d3age %in% c(4, "Four or more persons", "4 or more persons") ~ 4,
                           is.na(d3age) ~ NA_real_,
                           TRUE ~ 0))

data<- data %>%
  mutate(d4age = case_when(d4age %in% c(1, "One person", "1 person") ~ 1,
                           d4age %in% c(2, "Two person", "2 persons") ~ 2,
                           d4age %in% c(3, "Three persons", "3 persons") ~ 3,
                           is.na(d4age) ~ NA_real_,
                           TRUE ~ 0))

# Derive equivalising factor

data<- data %>%
  mutate(persons.under.15 = d1age + d2age + d3age + d4age,
         Adults = ifelse(is.na(Adults), Persons-persons.under.15, Adults),
         hh.equiv.derived = (1 + 0.5 * (Adults - 1) + 0.3 * (Persons-Adults)))



### Extra variables -----

## Create total g+s exp variable for earlier years
data<- data %>%
  mutate(
    exp.total.g.s = ifelse(is.na(exp.total.g.s),
        exp.housing+
        exp.fuel+
        exp.food+
        exp.alcohol+
        exp.tobacco+
        exp.clothing+
        exp.furnishings+
        exp.hhservices+
        exp.medical+
        exp.transport+
        exp.recreation+
        exp.personal+
        exp.misc, exp.total.g.s))

## Create total exp variable for earlier years
data<- data %>%
  mutate(
    exp.total = ifelse(is.na(exp.total),
                             exp.housing+
                             exp.fuel+
                             exp.food+
                             exp.alcohol+
                             exp.tobacco+
                             exp.clothing+
                             exp.furnishings+
                             exp.hhservices+
                             exp.medical+
                             exp.transport+
                             exp.recreation+
                             exp.personal+
                             exp.misc+
                             exp.tax+
                             exp.mortgage+
                             exp.ochc+
                             exp.super, exp.total))

## Create equivalised variables
data<- data %>%
  mutate(
    inc.total.equiv = tot.income / hh.equiv.derived,
    inc.disp.equiv = disp.income / hh.equiv.derived,
    net.wealth.equiv = net.wealth / hh.equiv.derived,
    exp.total.equiv = exp.total / hh.equiv.derived)

## Expenditure less tax
data$exp.less.tax<- data$exp.total - data$exp.tax

# The ABS adjusts all negative disposable income to zero for its calculation of equivalised disposable income
# Source: Explanatory notes, 2015-16 HES, http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6530.0Explanatory%20Notes12015-16?OpenDocument
data<- data %>%
  mutate(
    disp.income.ABS = ifelse(disp.income < 0, 0, disp.income),
    inc.disp.equiv.ABS = (disp.income.ABS / hh.equiv.derived),
    exp.total.ABS = ifelse(exp.total < 0, 0, exp.total),
    exp.total.equiv.ABS = (exp.total.ABS / hh.equiv.derived),
    exp.less.tax.ABS = ifelse(exp.less.tax < 0, 0, exp.less.tax),
    exp.less.tax.equiv.ABS = (exp.less.tax.ABS / hh.equiv.derived),
    exp.total.g.s.ABS = ifelse(exp.total.g.s < 0, 0, exp.total.g.s),
    exp.total.g.s.equiv.ABS = (exp.total.g.s.ABS / hh.equiv.derived))

## Create savings variables
data<- data %>%
  mutate(
    Hh.savings = disp.income - exp.total.g.s, #don't include super, mortgage or other capital housing costs as these are investments; don't include tax
    Hh.savings.equiv = Hh.savings / hh.equiv.derived,
    Hh.savings.ABS = disp.income.ABS - exp.total.g.s.ABS,
    Hh.savings.equiv.ABS = Hh.savings.ABS / hh.equiv.derived,
    Hh.savings.rate.equiv.disp = (Hh.savings / disp.income) / hh.equiv.derived,
    Hh.savings.rate.equiv.disp.ABS = (Hh.savings.ABS / disp.income.ABS) / hh.equiv.derived)

#NA's created by zero divided by zero -> convert these to zero; note some Inf also created, we've kept these in the data but excluded them when calculating averages
data<- data %>%
  mutate(
    Hh.savings.rate.equiv.disp = ifelse(is.na(Hh.savings.rate.equiv.disp), 0, Hh.savings.rate.equiv.disp),
    Hh.savings.rate.equiv.disp.ABS = ifelse(is.na(Hh.savings.rate.equiv.disp.ABS), 0, Hh.savings.rate.equiv.disp.ABS))


## Create tax and transfer variables
data<- data %>%
  mutate(net.benefits = total.benefits - total.taxes,
         net.benefits.equiv = net.benefits / hh.equiv.derived)


### Export -----

data$X<- NULL

return(data) }

