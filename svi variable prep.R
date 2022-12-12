#library(pdftools)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(stringr)

#url <- c("https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2020.html")
#raw_text <- map(url, pdf_text)

## EDITS: 2016 and 2014 dictionary use FactFinder to download census data,
## which generates column names that don't match variable names
## therefore census_var cannot be pulled from table_field_calcualtion
## used 2018 table_field_calculation instead (check if actual var and calculation changed)
## added theme info from dictionary though


# import dictionary -------------------------------------------------------

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

#  simply table to theme, variable, calculation --------------------------------------
var_cal <- svi_var_clean %>% 
  select(x2014_variable_name, theme, x2014_table_field_calculation) %>% 
  drop_na(x2014_table_field_calculation) %>% 
  filter(!str_detect(x2014_table_field_calculation,"\\^"))
  #"M ^", some variables named x were wrapped info from MOE rows
#2016 edits: sometimes no space between M and ^
#M or ^ will remove var name with M in it

#inspect, modify calculations that spill over (within E_, EP_)
#(something like "10 0" will be dealt with later - it's the \r\n)
##2020
var_cal$x2020_table_field_calculation[29] <- "(E_POV150 /S1701_C01_001E) * 100"

##2018 no need (scratch that, edits: a weird + at the end)
# var_cal$x2018_table_field_calculation[21] <- "B16005_007E + B16005_008E + B16005_012E 
# + B16005_013E + B16005_017E + B16005_018E + B16005_022E + B16005_023E + B16005_029E 
# + B16005_030E + B16005_034E + B16005_035E + B16005_039E + B16005_040E + B16005_044E 
# + B16005_045E"
##BEWARE OF HITTING that ENTER for line breaks! it actually added \n to strings
#ofc can just paste without line breaking, here use subtracting last x chr
var_cal$x2018_table_field_calculation[21] <- var_cal %>% 
  filter(x2018_variable_name == "E_LIMENG") %>% 
  pull(x2018_table_field_calculation) %>% 
  str_sub(.,1, -2) #sigh, last chr is space, so last 2
  

# ##2016
# var_cal$x2016_table_field_calculation[36] <- "(E_LIMENG/HD01_VD01)*100"
# var_cal$x2016_table_field_calculation[42] <- "(E_GROUPQ/E_TOTPOP)*100"

# ##2014 
# var_cal$x2014_table_field_calculation[36] <- "(E_LIMENG/HD01_VD01)*100"
# var_cal$x2014_table_field_calculation[42] <- "(E_GROUPQ/E_TOTPOP)*100"

 
# For 2020 and 2018, extract variables from calculation -------------------------

#remember this table still has x and NA, 
#but for E_ and EP_,calculation is complete and matching variable
#so you could safely filter out x when needed




var_cal2 <- 
  var_cal %>%
  filter(str_detect(x2014_variable_name, paste(var_prefix, collapse = "|"))) %>% 
  #modify the blank themes
  mutate(theme = case_when(
    x2014_variable_name%in%c("E_TOTPOP","E_HU","E_HH") ~ 0,  #must be same class
    is.na(theme) ~ 5, #adjunct variables
    TRUE ~ theme
  )) %>% 
  mutate(
    #pdf to excel introduce line breaks, not visible in view- use $ index to see
    x2014_table_field_calculation = str_replace_all(x2014_table_field_calculation, "\r\n",""),
    census_var = str_replace_all(x2014_table_field_calculation,
      "[^[:alnum:][:blank:]_]",
      " ")) 
#replace with blank instead of nothing: 100 will be separated by at least one blank with string
#edits from 2018 table


#save a simpler table to load into calculation (name, theme, cal)
var_cal_eep <- var_cal2 %>% 
  select(-census_var)

saveRDS(var_cal_eep, file = "data/variable_e_ep_calculation_2018.rds")

##EDITS: for 2016 and 2014, turns out census_var cannot be pulled from  table_field_cal.
##just use 2018 table and var_list, rename to avoid confusion
## 2016 ##
var_cal_table <- readRDS("data/variable_e_ep_calculation_2018.rds") %>% 
  rename(x2016_variable_name = x2018_variable_name,
    x2016_table_field_calculation = x2018_table_field_calculation)

saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2016.rds")

## 2014 ##
var_cal_table <- var_cal_table %>% 
  rename(x2014_variable_name = x2016_variable_name,
    x2014_table_field_calculation = x2016_table_field_calculation)

saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2014.rds")

#make a function to pull variables from each theme 
#E_ and EP_ (estimate and percentage of total, which *sometimes pull new variables)
theme_var_df <- function(n){
  var_cal2 %>% 
    filter(theme == n) %>%
    select(x2018_variable_name, census_var) %>% 
    separate_rows(census_var, sep = " ")  %>% 
    filter(!str_starts(census_var, "E_"),
           !census_var%in%c("","100")) %>% 
    pull(census_var)
}



var_list <- map(0:5, theme_var_df) 

#name elements in the list by theme (t0 = total, t5 = adjunct)
names(var_list) <- c("t0","t1","t2","t3","t4","t5")

saveRDS(var_list, "data/census_variables_2018.rds")  

## EDITS:
saveRDS(var_list, file = "data/census_variables_2016.rds")
saveRDS(var_list, file = "data/census_variables_2014.rds")


#make a named vector storing calculation
#this has been incorporated into svi_calculation 
#(E_ and EP_ have to be processed seperately, in order)


# construct a xwalk for theme and variable (moved to calculation script)
# xwalk_theme_var <- var_cal2 %>% 
#   select(-x2020_table_field_calculation)

