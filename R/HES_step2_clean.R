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
  mutate(age_approx = as.numeric(gsub(".*([0-9]{2})\\syears.*",    # retreive single numeric for age
                                                       "\\1",
                                                       age_desc)))
data<- data %>%
  mutate( age_20 = case_when(age_approx < 25 ~ "15-24",
                            age_approx  < 45 ~ "25-44",
                            age_approx  < 45 ~ "45-64",
                            age_approx  < 55 ~ "64-85",
                                        TRUE ~ "85 and over"),
          age_10 = case_when(age_approx < 25 ~ "15-24",
                               age_approx < 35 ~ "25-34",
                               age_approx < 45 ~ "35-44",
                               age_approx < 55 ~ "45-54",
                               age_approx < 65 ~ "55-64",
                               age_approx < 75 ~ "65-74",
                               age_approx < 85 ~ "75-84",
                               TRUE ~ "85 and over"),
         age_5 = case_when(age_approx < 20 ~ "15-19",
                           age_approx < 25 ~ "20-24",
                           age_approx < 30 ~ "25-29",
                           age_approx < 35 ~ "30-34",
                           age_approx < 40 ~ "35-39",
                           age_approx < 45 ~ "40-44",
                           age_approx < 50 ~ "45-49",
                           age_approx < 55 ~ "50-54",
                           age_approx < 60 ~ "55-59",
                           age_approx < 65 ~ "60-64",
                           age_approx < 70 ~ "65-69",
                           age_approx < 75 ~ "70-74",
                           age_approx < 80 ~ "75-79",
                           age_approx < 85 ~ "80-84",
                           TRUE ~ "85 and over"))



## Get rid of cents and replace them with dollars

data<- data %>%
  mutate(
    exp_housing     = ifelse(is.na(exp_housing), exp_housing_cents/100, exp_housing),
    exp_fuel        = ifelse(is.na(exp_fuel), exp_fuel_cents/100, exp_fuel),
    exp_food        = ifelse(is.na(exp_food), exp_food_cents/100, exp_food),
    exp_alcohol     = ifelse(is.na(exp_alcohol), exp_alcohol_cents/100, exp_alcohol),
    exp_tobacco     = ifelse(is.na(exp_tobacco), exp_tobacco_cents/100, exp_tobacco),
    exp_clothing    = ifelse(is.na(exp_clothing), exp_clothing_cents/100, exp_clothing),
    exp_furnishings = ifelse(is.na(exp_furnishings), exp_furnishings_cents/100, exp_furnishings),
    exp_hhservices  = ifelse(is.na(exp_hhservices), exp_hhservices_cents/100, exp_hhservices),
    exp_medical     = ifelse(is.na(exp_medical), exp_medical_cents/100, exp_medical),
    exp_transport   = ifelse(is.na(exp_transport), exp_transport_cents/100, exp_transport),
    exp_recreation  = ifelse(is.na(exp_recreation), exp_recreation_cents/100, exp_recreation),
    exp_personal    = ifelse(is.na(exp_personal), exp_personal_cents/100, exp_personal),
    exp_misc        = ifelse(is.na(exp_misc), exp_misc_cents/100, exp_misc),
    exp_tax         = ifelse(is.na(exp_tax), exp_tax_cents/100, exp_tax),
    exp_mortgage    = ifelse(is.na(exp_mortgage), exp_mortgage_cents/100, exp_mortgage),
    exp_ochc        = ifelse(is.na(exp_ochc), exp_ochc_cents/100, exp_ochc),
    exp_super       = ifelse(is.na(exp_super), exp_super_cents/100, exp_super),
    exp_total_g_s   = ifelse(is.na(exp_total_g_s), exp_total_g_s_cents/100, exp_total_g_s),
    exp_total       = ifelse(is.na(exp_total), exp_total_cents/100, exp_total),
    total_benefits  = ifelse(is.na(total_benefits), total_benefits_cents/100, total_benefits),
    total_ind_benefits  = ifelse(is.na(total_ind_benefits), total_ind_benefits_cents/100, total_ind_benefits),
    ind_health      = ifelse(is.na(ind_health), ind_health_cents/100, ind_health),
    ind_edu         = ifelse(is.na(ind_edu), ind_edu_cents/100, ind_edu),
    ind_welfare     = ifelse(is.na(ind_welfare), ind_welfare_cents/100, ind_welfare),
    total_taxes     = ifelse(is.na(total_taxes), total_taxes_cents/100, total_taxes))


## Adjust weights

data<- data %>%
  mutate(weight = ifelse(is.na(weight), weight_10000/10000, weight)) %>%
  select(-weight_10000)


## Create equivalising factor for earlier years

# check: unique(data$hhsize)

data<- data %>%
  mutate(persons = case_when(hhsize %in% c(1, "1 person", "One person") ~ 1,
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

# check unique(data$persons_15_over)

data<- data %>%
  mutate(persons_15_over = case_when(persons_15_over %in% c(1, "1 person aged 15 years and over") ~ 1,
                            persons_15_over %in% c(2, "2 persons aged 15 years and over") ~ 2,
                            persons_15_over %in% c(3, "3 persons aged 15 years and over") ~ 3,
                            persons_15_over %in% c(4, "4 persons aged 15 years and over") ~ 4,
                            persons_15_over %in% c(5, "5 persons aged 15 years and over") ~ 5,
                            persons_15_over %in% c(6, "6 persons aged 15 years and over") ~ 6,
                            persons_15_over == 7 ~ 7,
                            is.na(persons_15_over) ~ NA_real_,
                            TRUE ~ 0))

# HES 88 & 93 are NA for persons_15_over -- need to add in d1 to d4 variables

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
         persons_15_over = ifelse(is.na(persons_15_over), persons-persons.under.15, persons_15_over),
         hh_equiv_derived = (1 + 0.5 * (persons_15_over - 1) + 0.3 * (persons-persons_15_over)),
         hh_equiv = ifelse(is.na(hh_equiv_derived),hh_equiv,hh_equiv_derived)) %>%
         select(-hh_equiv_derived,-persons.under.15)



### Extra variables -----

## Create total g+s exp variable for earlier years
data<- data %>%
  mutate(
    exp_total_g_s = ifelse(is.na(exp_total_g_s),
        exp_housing+
        exp_fuel+
        exp_food+
        exp_alcohol+
        exp_tobacco+
        exp_clothing+
        exp_furnishings+
        exp_hhservices+
        exp_medical+
        exp_transport+
        exp_recreation+
        exp_personal+
        exp_misc, exp_total_g_s))

## Create total exp variable for earlier years
data<- data %>%
  mutate(
    exp_total = ifelse(is.na(exp_total),
                             exp_housing+
                             exp_fuel+
                             exp_food+
                             exp_alcohol+
                             exp_tobacco+
                             exp_clothing+
                             exp_furnishings+
                             exp_hhservices+
                             exp_medical+
                             exp_transport+
                             exp_recreation+
                             exp_personal+
                             exp_misc+
                             exp_tax+
                             exp_mortgage+
                             exp_ochc+
                             exp_super, exp_total))


## Expenditure less tax
data$exp_less.tax<- data$exp_total - data$exp_tax


# delete 'cents' variables that are not required

data<- data %>%
              select(-contains("_cents"))
### Export -----

data$X<- NULL

return(data) }

