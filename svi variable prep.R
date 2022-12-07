#library(pdftools)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(stringr)

#url <- c("https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2020.html")
#raw_text <- map(url, pdf_text)

svi_var <- read_xlsx("download/2014svi_dictionary.xlsx") %>% 
  clean_names() #column names starting with numbers not good for wrangling


skim(svi_var)
#look at variable type and names of columns (names might be different yr to yr)

svi_var <- svi_var %>% 
  rename(x2014_table_field_calculation = table_field_calculation)
#edits from 2016 & 2014 


# Remove MOE -----------------------------------------------------------------
#select the autodetect tables across pages during import table to excel from pdf
#so you don't have to deal with page numbers
#keeping rows with na in variable names -some are wrapped content from previous row

#pages <- c(8:32) %>% as.character()

svi_var_clean <- svi_var %>% 
  #filter(!x2020_table_field_calculation_if_changed %in% pages) %>% 
  mutate(x2014_variable_name = replace_na(x2014_variable_name, "x")) %>% 
  filter(!str_starts(x2014_variable_name,"M")) 

#table with theme, variable, calculation
var_cal <- svi_var_clean %>% 
  select(x2014_variable_name, theme, x2014_table_field_calculation) %>% 
  drop_na(x2014_table_field_calculation) %>% 
  filter(!str_detect(x2014_table_field_calculation,"\\^"))
  #"M ^", some variables named x were wrapped info from MOE rows
#2016 edits: sometimes no space between M and ^
#M or ^ will remove var name with M in it

#inspect, modify calculations that spill over (within E_, EP_)
##2020
var_cal$x2020_table_field_calculation[29] <- "(E_POV150 /S1701_C01_001E) * 100"
##2018 no need
##2016
var_cal$x2016_table_field_calculation[36] <- "(E_LIMENG/HD01_VD01)*100"
var_cal$x2016_table_field_calculation[42] <- "(E_GROUPQ/E_TOTPOP)*100"
##2014 (something like "10 0" will be dealt with later - it's the \r\n)
var_cal$x2014_table_field_calculation[36] <- "(E_LIMENG/HD01_VD01)*100"
var_cal$x2014_table_field_calculation[42] <- "(E_GROUPQ/E_TOTPOP)*100"
 
#extract variables from calculation
var_cal <- var_cal %>% 
  mutate(
    x2014_table_field_calculation = str_replace_all(x2014_table_field_calculation, "\r\n",""),
    #pdf to excel introduce line breaks, not visible in view- use $ index to see 
    census_var = str_replace_all(x2014_table_field_calculation,
                                      "[^[:alnum:][:blank:]_]",
                                      " ")) 
#replace with blank instead of nothing: 100 will be separated by at least one blank with string
#edits from 2018 table

#remember this table still has x and NA, 
#but for E_ and EP_,calculation is complete and matching variable
#so you could safely filter out x when needed

#subsetting E_ and EP_ variables
var_prefix <- c("E_", "EP_")

var_cal2 <- 
  var_cal %>%
  filter(str_detect(x2014_variable_name, paste(var_prefix, collapse = "|"))) %>% 
  mutate(theme = case_when(
    x2014_variable_name%in%c("E_TOTPOP","E_HU","E_HH") ~ 0,  #must be same class
    is.na(theme) ~ 5, #adjunct variables
    TRUE ~ theme
  ))

#save a simpler table to load into calculation (name, theme, cal)
var_cal_eep <- var_cal2 %>% 
  select(-census_var)

saveRDS(var_cal_eep, file = "data/variable_e_ep_calculation_2014.rds")


#make a function to pull variables from each theme 
#E_ and EP_ (estimate and percentage of total, which *sometimes pull new variables)
theme_var_df <- function(n){
  var_cal2 %>% 
    filter(theme == n) %>%
    select(x2014_variable_name, census_var) %>% 
    separate_rows(census_var, sep = " ")  %>% 
    filter(!str_starts(census_var, "E_"),
           !census_var%in%c("","100")) %>% 
    pull(census_var)
}



var_list <- map(0:5, theme_var_df) 

#name elements in the list by theme (t0 = total, t5 = adjunct)
names(var_list) <- c("t0","t1","t2","t3","t4","t5")

saveRDS(var_list, "data/census_variables_2014.rds")  



#make a named vector storing calculation
#this has been incorporated into svi_calculation 
#(E_ and EP_ have to be processed seperately, in order)


# construct a xwalk for theme and variable (moved to calculation script)
# xwalk_theme_var <- var_cal2 %>% 
#   select(-x2020_table_field_calculation)

