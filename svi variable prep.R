#library(pdftools)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(stringr)

#url <- c("https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2020.html")
#raw_text <- map(url, pdf_text)

svi_var <- read_xlsx("2020svi_dictionary.xlsx") %>% 
  clean_names() #column names starting with numbers not good for wrangling


skim(svi_var)
#look at variable type of columns

pages <- c(8:32) %>% as.character()
 
#get rid of page number, MOE
#keeping rows with na in variable names -some are wrapped content from previous row
svi_var_clean <- svi_var %>% 
  filter(!x2018_table_field_calculation_if_changed %in% pages) %>% 
  mutate(x2020_variable_name = replace_na(x2020_variable_name, "x")) %>% 
  filter(!str_starts(x2020_variable_name,"M")) 

#table with theme, variable, calculation
var_cal <- svi_var_clean %>% 
  select(x2020_variable_name, theme, x2020_table_field_calculation) %>% 
  drop_na(x2020_table_field_calculation) %>% 
  filter(!str_detect(x2020_table_field_calculation,"M\\ \\^")) 
  #"M ^", some variables named x were wrapped info from MOE rows

#inspect, modify calculations that spill over (within E_, EP_)
var_cal$x2020_table_field_calculation[29] <- "(E_POV150 /S1701_C01_001E) * 100"

#extract variables from calculation
var_cal <- var_cal %>% 
  mutate(
    x2020_table_field_calculation = str_replace_all(x2020_table_field_calculation, "\r\n",""),
    #pdf to excel introduce line breaks, not visible in view
    census_var = str_replace_all(x2020_table_field_calculation,
                                      "[^[:alnum:][:blank:]_]",
                                      ""))

#remember this table still has x and NA, 
#but for E_ and EP_,calculation is complete and matching variable
#so you could safely filter out x when needed

#subsetting E_ and EP_ variables
var_prefix <- c("E_", "EP_")

var_cal2 <- 
  var_cal %>%
  filter(str_detect(x2020_variable_name, paste(var_prefix, collapse = "|"))) %>% 
  mutate(theme = case_when(
    x2020_variable_name%in%c("E_TOTPOP","E_HU","E_HH") ~ 0,  #must be same class
    is.na(theme) ~ 5, #adjunct variables
    TRUE ~ theme
  ))

#save a simpler table to load into calculation (name, theme, cal)
var_cal_eep <- var_cal2 %>% 
  select(-census_var)

saveRDS(var_cal_eep, file = "data/variable_e_ep_calculation_2020.rds")


#make a function to pull variables from each theme 
#E_ and EP_ (estimate and percentage of total, which *sometimes pull new variables)
theme_var_df <- function(n){
  var_cal2 %>% 
    filter(theme == n) %>%
    select(x2020_variable_name, census_var) %>% 
    separate_rows(census_var, sep = " ")  %>% 
    filter(!str_starts(census_var, "E_"),
           !census_var%in%c("","100")) %>% 
    pull(census_var)
}



var_list <- map(0:5, theme_var_df) 

#name elements in the list by theme (t0 = total, t5 = adjunct)
names(var_list) <- c("t0","t1","t2","t3","t4","t5")

saveRDS(var_list, "data/census_variables_2020.rds")  



#make a named vector storing calculation
#this has been incorporated into svi_calculation 
#(E_ and EP_ have to be processed seperately, in order)


# construct a xwalk for theme and variable (moved to calculation script)
# xwalk_theme_var <- var_cal2 %>% 
#   select(-x2020_table_field_calculation)

