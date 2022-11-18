library(tidycensus)
library(tidyverse)


# load variables needed 
var_list <- readRDS("data/census_variables.rds") %>% 
  unlist() %>% #autofill names for each string in the vector
  unname() #get rid of names --otherwise will rename the column in census pull

var_expr <- readRDS("data/variable_calculation.rds")

# tract level PA all var 2019

pa_co_raw <- get_acs(
  geography = "county",
  state = "PA",
  year = 2020,
  variables = var_list_list,
  output = "wide"
)


var_sub_name <- names(var_sub)

df <- 
  map2_dfc(var_sub_name, var_sub, function(var_sub_name, var_sub){
    pa_co_raw %>%
    transmute(
     !!var_sub_name := eval(str2lang(var_sub))
    )
}) %>% 
  bind_cols(pa_co_raw, .)

pa_co_raw %>% 
  rename('E_TOTPOP'="S0601_C01_001E")

# alternative: eval(parse(text=x)) 
#str2expression seems working too
#lazy_eval() somehow not working
pa_co_var <- pa_co_raw %>%
  mutate(
    E_LIMENG = eval(str2lang(var_expr["E_LIMENG"])),
    .after = NAME
  )
