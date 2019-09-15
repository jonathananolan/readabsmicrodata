
#Program: BPIR
#Report: Wealth of Generations 2.0
#Purpose: To calculate income and wealth across age groups and years from SIH data
#Data sources: SIH microdata

# Install packages
#install.packages("dplyr")

# Load packages
library(dplyr) # for data manipulation
library(grattan) # for decile creation


### Read in data ----
# Set working directory

data <- read.csv("SIH.raw.csv")


### Clean-up -----

## Unique household ID (hh ids repeat in 1995 and 2000)

data$hh.id.unique<- paste(data$refyear, data$hh.id, sep = "-")

#check
#nrow(data) - length(unique(data$hh.id.unique)) #6859 (extra rows due to income units in 1986 and 1990)


## Group Units within a Household
# Earlier SIH (1986 and 1990) are only available as "income units" -> need to group income units within a single household
# Warning: income units within a household are not unique (i.e. there can be more than one IU1 per household)
#          The ABS id system goes Household to Family to Income Unit to Person -> multiple families within a single household means there can be multiple IU1s within a household

# Note on converting income unit weights to household weights:
# Looking at 2011 and 2015 SIH, all IU weights within a HH are identical to each other and to the HH weight
# So we can take the average of IU weights to correctly identify the HH weight

# Exclude income unit type = 0 (no data)
data<- data %>%
  mutate(income.unit.zero = ifelse(income.unit.type == 0, T, F),
         income.unit.zero = ifelse(is.na(income.unit.zero), F, income.unit.zero))

data<- data %>%
  filter(income.unit.zero == F)

#checks
# unique(data$income.unit.type)
# nrow(data) - length(unique(data$hh.id.unique)) #6801


# Separate income unit data (1986 and 1990) from rest
data.hh<- data %>%
  filter(is.na(income.unit.type))

data.iu<- data %>%
  filter(!is.na(income.unit.type))

#checks
# nrow(data.iu) - length(unique(data.iu$hh.id.unique)) #6801
# nrow(data.hh) - length(unique(data.hh$hh.id.unique)) #should be zero

# Summarise income unit data at the household level
data.iu<- data.iu %>%
  group_by(hh.id.unique) %>%
  summarise(refyear = max(refyear),
            age.code.1 = max(age.code.1),   ##double check
            persons = sum(persons), 
            persons.under.15 = sum(persons.under.15),
            persons.15.64 = sum(persons.15.64),
            persons.65.plus = sum(persons.65.plus),
            total.income.0304 = sum(total.income.0304),
            tax.weekly = sum(tax.weekly), 
            tax.annual = sum(tax.annual), 
            weight.10000 = mean(weight.10000),
            income.unit.count = max(income.unit.type))

# Recombine 1986 and 1990 with the rest
data.clean<- merge(data.hh, data.iu, by="hh.id.unique", all.x=T, all.y=T)

#check
#nrow(data) - nrow(data.clean) #6801 (check this matches above - i.e. the only rows lost were the income units within a household)


#clean up column names post-merge
data.clean<- data.clean %>%
  mutate(refyear = ifelse(is.na(refyear.x), refyear.y, refyear.x),
         age.code.1 = ifelse(is.na(age.code.1.y), age.code.1.x, age.code.1.y),
         persons = ifelse(is.na(persons.x), persons.y, persons.x),
         persons.under.15 = ifelse(is.na(persons.under.15.x), persons.under.15.y, persons.under.15.x),
         persons.15.64 = ifelse(is.na(persons.15.64.y), persons.15.64.x, persons.15.64.y),
         persons.65.plus = ifelse(is.na(persons.65.plus.y), persons.65.plus.x, persons.65.plus.y), 
         weight.10000 = ifelse(is.na(weight.10000.x), weight.10000.y, weight.10000.x),
         total.income.0304 = ifelse(is.na(total.income.0304.x), total.income.0304.y, total.income.0304.x),
         tax.weekly = ifelse(is.na(tax.weekly.x), tax.weekly.y, tax.weekly.x),
         tax.annual = ifelse(is.na(tax.annual.x), tax.annual.y, tax.annual.x),
         refyear.x = NULL,
         refyear.y = NULL,
         age.code.1.x = NULL,
         age.code.1.y = NULL,
         persons.x = NULL,
         persons.y = NULL,
         persons.under.15.x = NULL,
         persons.under.15.y = NULL,
         persons.15.64.x = NULL,
         persons.15.64.y = NULL,
         persons.65.plus.x = NULL,
         persons.65.plus.y = NULL,
         total.income.0304.x = NULL,
         total.income.0304.y = NULL,
         tax.weekly.x = NULL,
         tax.weekly.y = NULL,
         tax.annual.x = NULL,
         tax.annual.y = NULL,
         weight.10000.x = NULL,
         weight.10000.y = NULL,
         X = NULL)


## Adjust household weights
# Weights from 1986 to 2000 need to be divided by 10,000
data.clean<- data.clean %>%
  mutate(weight.final = ifelse(is.na(weight), weight.10000/10000, weight))


## Adjust tax for 1986
# 1986 lists tax as yearly instead of weekly
data.clean<- data.clean %>% 
  mutate(tax = ifelse(is.na(tax.weekly), tax.annual/365*7, tax.weekly))


## Age groups

# First check that there are no real NAs for age
# age.test1<- data.clean %>%
#   filter(refyear == c("1986","1990"))
# unique(age.test1$age.code.1) #should be no NAs
# 
# age.test2<- data.clean %>%
#   filter(refyear == c("1995","2000"))
# unique(age.test2$age.code.2) #should be no NAs
# 
# age.test3<- data.clean %>%
#   filter(refyear == c("2005", "2007", "2009", "2011", "2013"))
# unique(age.test3$age.code.3) #should be no NAs
# 
# age.test4<- data.clean %>%
#   filter(refyear == "2015")
# unique(age.test4$age) #should be no NAs


# Convert age codes to a meaningful age description
data.clean<- data.clean %>% 
  mutate(age.desc = case_when(age.code.1 == 1 ~ "15 years",
                              age.code.1 == 2 ~ "16-17 years",
                              age.code.1 == 3 ~ "18-20 years",
                              age.code.1 == 4 ~ "21-24 years",
                              age.code.1 == 5 ~ "25-29 years",
                              age.code.1 == 6 ~ "30-34 years",
                              age.code.1 == 7 ~ "35-39 years",
                              age.code.1 == 8 ~ "40-44 years",
                              age.code.1 == 9 ~ "45-49 years",
                              age.code.1 == 10 ~ "50-54 years",
                              age.code.1 == 11 ~ "55-59 years",
                              age.code.1 == 12 ~ "60-64 years",
                              age.code.1 == 13 ~ "65-69 years",
                              age.code.1 == 14 ~ "70-74 years",
                              age.code.1 == 15 ~ "75 years plus",
    
                              age.code.2 == 1 ~ "15 years",
                              age.code.2 == 2 ~ "16 years",
                              age.code.2 == 3 ~ "17 years",
                              age.code.2 == 4 ~ "18 years",
                              age.code.2 == 5 ~ "19 years",
                              age.code.2 == 6 ~ "20 years",
                              age.code.2 == 7 ~ "21 years",
                              age.code.2 == 8 ~ "22 years",
                              age.code.2 == 9 ~ "23 years",
                              age.code.2 == 10 ~ "24 years",
                              age.code.2 == 11 ~ "25-29 years",
                              age.code.2 == 12 ~ "30-34 years",
                              age.code.2 == 13 ~ "35-39 years",
                              age.code.2 == 14 ~ "40-44 years",
                              age.code.2 == 15 ~ "45-49 years",
                              age.code.2 == 16 ~ "50-54 years",
                              age.code.2 == 17 ~ "55 years",
                              age.code.2 == 18 ~ "56 years",
                              age.code.2 == 19 ~ "57 years",
                              age.code.2 == 20 ~ "58 years",
                              age.code.2 == 21 ~ "59 years",
                              age.code.2 == 22 ~ "60 years",
                              age.code.2 == 23 ~ "61 years",
                              age.code.2 == 24 ~ "62 years",
                              age.code.2 == 25 ~ "63 years",
                              age.code.2 == 26 ~ "64 years",
                              age.code.2 == 27 ~ "65-69 years",
                              age.code.2 == 28 ~ "70-74 years",
                              age.code.2 == 29 ~ "75 years plus",
                          
                              age.code.3 == 1 ~ "15 years",
                              age.code.3 == 2 ~ "16 years",
                              age.code.3 == 3 ~ "17 years",
                              age.code.3 == 4 ~ "18 years",
                              age.code.3 == 5 ~ "19 years",
                              age.code.3 == 6 ~ "20 years",
                              age.code.3 == 7 ~ "21 years",
                              age.code.3 == 8 ~ "22 years",
                              age.code.3 == 9 ~ "23 years",
                              age.code.3 == 10 ~ "24 years",
                              age.code.3 == 11 ~ "25-29 years",
                              age.code.3 == 12 ~ "30-34 years",
                              age.code.3 == 13 ~ "35-39 years",
                              age.code.3 == 14 ~ "40-44 years",
                              age.code.3 == 15 ~ "45-49 years",
                              age.code.3 == 16 ~ "50-54 years",
                              age.code.3 == 17 ~ "55 years",
                              age.code.3 == 18 ~ "56 years",
                              age.code.3 == 19 ~ "57 years",
                              age.code.3 == 20 ~ "58 years",
                              age.code.3 == 21 ~ "59 years",
                              age.code.3 == 22 ~ "60 years",
                              age.code.3 == 23 ~ "61 years",
                              age.code.3 == 24 ~ "62 years",
                              age.code.3 == 25 ~ "63 years",
                              age.code.3 == 26 ~ "64 years",
                              age.code.3 == 27 ~ "65-69 years",
                              age.code.3 == 28 ~ "70-74 years",
                              age.code.3 == 29 ~ "75-79 years",
                              age.code.3 == 30 ~ "80 years plus",

                              TRUE ~ NA_character_))


# Create a single numeric variable for age (can only be approximate)
data.clean<- data.clean %>% 
  mutate(age.approx = ifelse(is.na(age), 
                             as.numeric(gsub(".*([0-9]{2})\\syears.*", "\\1", age.desc)),
                             age))

summary(data.clean$age.approx) 
#NB: this is not necessarily true age, 34 could mean: age 34 OR age group 30-34
#there should no NAs -- fix before proceeding
#there should be no age under 15 or over 85

data.clean<- data.clean %>% 
  mutate(age.group = case_when(age.approx < 25 ~ "15-24",
                               age.approx < 35 ~ "25-34",
                               age.approx < 45 ~ "35-44",
                               age.approx < 55 ~ "45-54",
                               age.approx < 65 ~ "55-64",
                               age.approx < 75 ~ "65-74",
                               TRUE ~ "75 and over"))


## Create equivalising factor for earlier years (1986-2000)

# For 1986 and 1990, Adults = persons.15.64 + persons.65.plus
# For all other years Adults = persons.15.over
# For 1986 to 2005 Persons = Adults + persons.under.15
# For all other years Persons = persons

# Derive equivalising factor
data.clean<- data.clean %>%
  mutate(Adults = ifelse(is.na(persons.15.over), persons.15.64 + persons.65.plus, persons.15.over),
         Persons = ifelse(is.na(persons), Adults + persons.under.15, persons),
         persons.under.15 = ifelse(is.na(persons.under.15), persons - Adults, persons.under.15),
         hh.equiv.derived = (1 + 0.5 * (Adults - 1) + 0.3 * (persons.under.15)))

#checks
# unique(data.clean$Adults) #there should be no NAs
# unique(data.clean$Persons) #there should be no NAs
# unique(data.clean$persons.under.15) #there should be no NAs

# test<- data.clean %>%
#   filter(refyear == c("1986", "2003", "2005", "2007", "2009", "2011", "2013", "2015")) #for whichever years persons was directly imported
# sum(test$persons) - sum(test$Persons) #should be zero


### Extra variables -----

# The best available total income measure for each year
data.clean <- data.clean %>%
  mutate(tot.income.avail = case_when(!is.na(total.income.0708) ~ total.income.0708,
                                       !is.na(total.income.0506) ~ total.income.0506,
                                       !is.na(total.income.0304) ~ total.income.0304,
                                       TRUE ~ NA_real_))


## Calculate a single measure of disposable income for all years 
# The Medicare levy surcharge has been calculated and deducted from gross income in the calculation of disposable income since the 2007-08 cycle of SIH
# This means disposable.income.0708 is a better measure of disposable income, but only available for some years

# The best available disposable income measure for each year
data.clean <- data.clean %>%
  mutate(disp.income.avail = case_when(!is.na(disposable.income.0708) ~ disposable.income.0708,
                                       !is.na(disposable.income.0506) ~ disposable.income.0506,
                                       !is.na(disposable.income.0304) ~ disposable.income.0304,
                                       !is.na(tax) ~ (total.income.0304 - tax),
                                       TRUE ~ NA_real_))

# A scaled approximation of disposable.income.0708 for earlier years
# See Test.income.measures.R

scale<- read.csv("Scaling.for.disp.inc.csv")

scale$X<- NULL
scale$mean.disp.inc.0304<- NULL
scale$mean.disp.inc.0708<- NULL

data.clean<- data.clean %>% 
  mutate(age.for.deciles = case_when(age.approx < 25 ~ "Under 25",
                                     age.approx < 65 ~ "25-64",
                                     TRUE ~ "Over 65"))

data.clean <- data.clean %>% 
  group_by(age.for.deciles) %>%
  mutate(disp.inc.decile.weighted.age3 = weighted_ntile(disposable.income.0304, weights= weight.final, 10))

#merge
data.clean <- merge(data.clean, scale, by = c("age.for.deciles", "disp.inc.decile.weighted.age3"), all.x=T)

#calculate scaled disp income for earlier years
data.clean <- data.clean %>%
  mutate(disp.income.scaled = case_when(!is.na(disposable.income.0708) ~ disposable.income.0708,
                                        !is.na(disposable.income.0304) ~ disposable.income.0304 * scale.disp.inc,
                                        !is.na(tax) ~ (total.income.0304 - tax) * scale.disp.inc,
                                        TRUE ~ NA_real_))

summary(data.clean$disp.income.scaled)
summary(data.clean$disp.income.avail)

## Create equivalised variables

# The ABS adjusts all negative disposable income to zero for its calculation of equivalised disposable income
# Source: Explanatory notes, 2015-16 HES, http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6530.0Explanatory%20Notes12015-16?OpenDocument

data.clean<- data.clean %>%
  mutate(net.wealth.equiv = net.wealth / hh.equiv.derived,
    disp.inc.equiv = disp.income.avail / hh.equiv.derived,
    disp.income.ABS = ifelse(disp.income.avail < 0, 0, disp.income.avail),
    disp.inc.equiv.ABS = (disp.income.ABS / hh.equiv.derived),
    disp.inc.scaled.equiv = disp.income.scaled / hh.equiv.derived,
    disp.income.scaled.ABS = ifelse(disp.income.scaled < 0, 0, disp.income.scaled),
    disp.inc.scaled.equiv.ABS = (disp.income.scaled.ABS / hh.equiv.derived))


## Create our own net wealth variable, check it's the same as ABS
#sum assets (wealth.type=="asset")
#sum liabilities (wealth.type=="liability")

wealth.check <- data.clean %>%
  filter(!is.na(net.wealth)) %>%
  group_by(hh.id.unique) %>%
  mutate(sum.assets = sum(house.value,
                          contents.value,
                          bonds.value,
                          bank.deposit.value,
                          business.value,
                          investments.value,
                          non.res.prop,
                          offset.value,
                          other.value,
                          loans.value,
                          trusts.all,
                          private.trust.value,
                          public.trust.value,
                          oth.house.value,
                          shares.value,
                          partnership.value,
                          super.gov.value,
                          super.non.gov.value,
                          business.unincorp.value,
                          car.value,
                          kids.value,
                          na.rm = T),
         sum.liabilities = sum(credit.card.debt,
                               help.debt,
                               investment.debt,
                               oth.house.debt,
                               loans.debt,
                               oth.all.house.debt,
                               house.debt,
                               student.debt,
                               car.loans,
                               na.rm = T),
         net.wealth.qc = sum.assets - sum.liabilities,
         net.wealth.dif = net.wealth.qc - net.wealth,
         net.wealth.percent.dif = net.wealth.qc/net.wealth*100)

summary(wealth.check$net.wealth.dif)
summary(wealth.check$net.wealth.percent.dif)

wealth.check.nomatch<- wealth.check %>%
  filter(net.wealth.percent.dif <=99 | net.wealth.percent.dif >=101)

nrow(wealth.check.nomatch)/nrow(wealth.check)*100 #only 1.1% don't match (all in 2009 and 2011)
#2009-10 differences may be explained by the "note on correct calculation" xlsx here:
#http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6503.02009-10?OpenDocument
#No similar note for 2011-12


### Export -----

data.clean$X<- NULL

write.csv(data.clean, "SIH.clean.csv")



