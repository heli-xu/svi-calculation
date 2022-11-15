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
svi_var_clean <- svi_var %>% 
  filter(!x2018_table_field_calculation_if_changed %in% pages) %>% 
  mutate(x2020_variable_name = replace_na(x2020_variable_name, "x")) %>% 
  filter(!str_starts(x2020_variable_name,"M")) 

#variables that need to be pulled from census
#not including further percentage calculation
var_cal <- svi_var_clean %>% 
  select(x2020_variable_name, theme, x2020_table_field_calculation) %>% 
  drop_na(x2020_table_field_calculation) %>% 
  filter(!str_detect(x2020_table_field_calculation,"M\\ \\^"),
         str_starts(x2020_variable_name, "E_"))
  
  
