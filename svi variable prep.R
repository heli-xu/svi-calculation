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
## used 2018 data to modify 2016 and 2014 instead 
## checked if actual var and calculation, theme info changed (no)


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

#subsetting E_ and EP_ variables
var_prefix <- c("E_", "EP_")

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

## EDITS: 2021 ## same as 2020
var_cal_table <- readRDS("data/variable_e_ep_calculation_2020.rds") %>% 
  rename(x2021_variable_name = x2020_variable_name,
    x2021_table_field_calculation = x2020_table_field_calculation)

saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2021.rds")

## EDITS: 2019 ##
var_cal_table <- readRDS("data/variable_e_ep_calculation_2020.rds") %>% 
  rename(x2019_variable_name = x2020_variable_name,
    x2019_table_field_calculation = x2020_table_field_calculation)

saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2019.rds")


##EDITS: for 2016 and 2014, turns out census_var cannot be pulled from  table_field_cal.
##just use 2018 table and var_list, rename to avoid confusion
## 2017 ##
var_cal_table <- readRDS("data/variable_e_ep_calculation_2018.rds") %>% 
  rename(x2017_variable_name = x2018_variable_name,
    x2017_table_field_calculation = x2018_table_field_calculation)

saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2017.rds")

## 2016 ##
var_cal_table <- readRDS("data/variable_e_ep_calculation_2018.rds") %>% 
  rename(x2016_variable_name = x2018_variable_name,
    x2016_table_field_calculation = x2018_table_field_calculation)

#edits: guess what a var needs changing
#E_AGE65, EP_AGE65
#when searching for var, don't just look within the original table
#sometimes count and percentage could be on different table (S0101, S0103etc)
#but some of the tables completion level lower, and not on all geo levels
#For S0103, only available at county level...so not ideal
#here still using percentage*total population
var_cal_table$x2016_table_field_calculation[8] <- "S0101_C01_028E * E_TOTPOP / 100"
var_cal_table$x2016_table_field_calculation[23] <- "S0101_C01_028E"

saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2016.rds")

## 2015 ## same as 2016
var_cal_table <- readRDS("data/variable_e_ep_calculation_2018.rds") %>% 
  rename(x2015_variable_name = x2018_variable_name,
    x2015_table_field_calculation = x2018_table_field_calculation)

var_cal_table$x2015_table_field_calculation[8] <- "S0101_C01_028E"
var_cal_table$x2015_table_field_calculation[23] <- "S0101_C01_028E"

saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2015.rds")

## 2014 ##
var_cal_table <- readRDS("data/variable_e_ep_calculation_2018.rds") %>% 
  rename(x2014_variable_name = x2018_variable_name,
    x2014_table_field_calculation = x2018_table_field_calculation)
#E_CROWD
var_cal_table$x2014_table_field_calculation[16] <- "DP04_0077E +DP04_0078E"
#E_NOVEH
var_cal_table$x2014_table_field_calculation[17] <- "DP04_0057E"
#remember to check the EP_ value too!!! sometimes it's also pulled from census
#EP_CROWD no need to adjust
#EP_NOVEH
var_cal_table$x2014_table_field_calculation[32] <- "DP04_0057PE"
#E_AGE65 and EP_AGE65
#no count data from census, the label "total estimate" is actually a percentage
var_cal_table$x2014_table_field_calculation[8] <- "S0101_C01_028E"
var_cal_table$x2014_table_field_calculation[23] <- "S0101_C01_028E"


saveRDS(var_cal_table, file = "data/variable_e_ep_calculation_2014.rds")


#make a function to pull variables from each theme 
#E_ and EP_ (estimate and percentage of total, which *sometimes pull new variables)
theme_var_df <- function(n){
  var_cal2 %>% 
    filter(theme == n) %>%
    select(x2016_variable_name, census_var) %>% 
    separate_rows(census_var, sep = " ")  %>% 
    filter(!str_starts(census_var, "E_"),
           !census_var%in%c("","100")) %>% 
    pull(census_var)
}



var_list <- map(0:5, theme_var_df) 

#name elements in the list by theme (t0 = total, t5 = adjunct)
names(var_list) <- c("t0","t1","t2","t3","t4","t5")

saveRDS(var_list, "data/census_variables_2018.rds")  

## EDITS: 2021 same as 2020 ----------------
## EDITS: 2019 same as 2020 ----------------

## EDITS: 2017 same as 2018 -----------------

## EDITS: 2016 ------------------------
var_cal_table$x2016_table_field_calculation

var_cal2 <- var_cal_table %>% 
  mutate(census_var = str_replace_all(x2016_table_field_calculation,
    "[^[:alnum:][:blank:]_]",
    " ")) 

# modify theme_var_df function 
var_list <- map(0:5, theme_var_df) 
names(var_list) <- c("t0","t1","t2","t3","t4","t5")

saveRDS(var_list, file = "data/census_variables_2016.rds")

## EDITS: 2015 same as 2016  -----------------

## EDITS: 2014 ----------------------------
var_cal_table$x2014_table_field_calculation 
# just to check no line break nonsense

var_cal2 <- var_cal_table %>% 
  mutate(census_var = str_replace_all(x2014_table_field_calculation,
  "[^[:alnum:][:blank:]_]",
  " ")) 

# modify theme_var_df function 
var_list <- map(0:5, theme_var_df) 
names(var_list) <- c("t0","t1","t2","t3","t4","t5")
saveRDS(var_list, file = "data/census_variables_2014.rds")

#make a named vector storing calculation
#this has been incorporated into svi_calculation 
#(E_ and EP_ have to be processed seperately, in order)


# construct a xwalk for theme and variable (moved to calculation script)
# xwalk_theme_var <- var_cal2 %>% 
#   select(-x2020_table_field_calculation)

