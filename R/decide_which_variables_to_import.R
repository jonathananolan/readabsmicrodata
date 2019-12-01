#Create a data dictionary

years_HH_c<- as.character(years_HH)
if(import_all !="none"){
label_dictionary <- create_variable_dictionary()
      }

if(import_all == "all") {
         label_dictionary <- label_dictionary %>%
                             mutate(r_var_name = paste0(var_name,"_",latest_year))
      }

if(import_all == "latest_year") {
  label_dictionary <- label_dictionary %>%
                      ungroup %>%
                      filter(latest_year == max(year)) %>%
                      mutate(r_var_name = paste0(var_name,"_",latest_year))
      }



data<-map_df(years_HH,
             import_one_year)


import_one_year(2015)
