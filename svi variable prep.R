#library(pdftools)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(stringr)

#url <- c("https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2020.html")
raw_text <- map(url, pdf_text)

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

#inspect, modify some names (that were x)
var_cal$x2020_table_field_calculation[29] <- "(E_POV150 /S1701_C01_001E) * 100"

var_cal <- var_cal %>% 
  mutate(census_var = str_replace_all(x2020_table_field_calculation,
                                      "[^[:alnum:][:blank:]_]",
                                      ""))

#make a function to pull variables from each theme 
#E_ and EP_ (estimate and percentage of total, which *sometimes pull new variables)
theme_var_df <- function(n){
  var_prefix <- c("E_", "EP_")
  
  var_cal %>% 
    filter(theme == n,
           str_detect(x2020_variable_name, paste(var_prefix, collapse = "|"))) %>%
    select(x2020_variable_name, census_var) %>% 
    separate_rows(census_var, sep = " ")  %>% 
    filter(!str_starts(census_var, "E_"),
           !census_var%in%c("","100"))
}

theme1_var <- theme_var_df(1) %>% 
  pull(census_var)  #inspect before pull

theme2_var <- theme_var_df(2) %>% 
  pull(census_var)

theme3_var <- theme_var_df(3) %>% 
  pull(census_var)

theme4_var <- theme_var_df(4) %>% 
  pull(census_var)

theme_xtr <- var_cal %>% 
  
