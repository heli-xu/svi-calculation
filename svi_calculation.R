library(tidycensus)
library(tidyverse)


# Load variables needed ----------------------------------
var_list <- readRDS("data/census_variables.rds") %>% 
  unlist() %>% #autofill names for each string in the vector
  unname() #get rid of names --otherwise will rename the column in census pull

# Load table for calculation and XWALK for theme--------------------------
var_cal_table <- readRDS("data/variable_e_ep_calculation.rds")



# Pull data from census-------------------------------------

pa_co_raw <- get_acs(
  geography = "county",
  state = "PA",
  year = 2020,
  variables = var_list,
  output = "wide"
)

# Extract named vector from val_cal_table (don't run)------------
var_name <- var_cal_table$x2020_variable_name

var_expr <- var_cal_table$x2020_table_field_calculation

names(var_expr) <- var_name


var_expr["EP_ASIAN"]  



# Test calculation E_LIMENG with eval a string (don't run)---------------------------
# alternative: eval(parse(text=x)) 
#str2expression seems working too
#lazy_eval() somehow not working

pa_co_test <- pa_co_raw %>%
  mutate(
    E_LIMENG = eval(str2lang(var_expr["E_LIMENG"])),  #use one that actually has plus
    .after = NAME
  )

pa_co_test2 <- pa_co_raw %>%
  mutate(
    E_LIMENG = B16005_007E +B16005_008E +B16005_012E +B16005_013E +B16005_017E 
    +B16005_018E +B16005_022E +B16005_023E +B16005_029E +B16005_030E +B16005_034E 
    +B16005_035E +B16005_039E +B16005_040E +B16005_044E +B16005_045E,
    .after = NAME)

pa_co_test$E_LIMENG == pa_co_test2$E_LIMENG

# Test map iteration with transmute (don't run)------------------------------------------
var_sub <- var_expr[c("E_TOTPOP","E_HU","E_HH")]
var_sub_name <- names(var_sub)

df <- 
  map2_dfc(var_sub_name, var_sub, function(var_sub_name, var_sub){
    pa_co_raw %>%
      transmute(
        !!all_of(var_sub_name) := eval(str2lang(var_sub))
      )
  }) %>% 
  bind_cols(pa_co_raw, .) #to avoid duplicating rows

# Create svi variables with iteration (theme0-4)--------------------------------
## set up E_ vector
E_var <- 
  var_cal_table %>% 
  filter(theme%in%c(0:4),
    str_detect(x2020_variable_name, "E_")) 

E_var_name <- E_var$x2020_variable_name
E_var_expr <- E_var$x2020_table_field_calculation
names(E_var_expr) <- E_var_name

## set up EP_ vector
EP_var <-
  var_cal_table %>% 
  filter(theme%in%c(0:4),
  str_detect(x2020_variable_name, "EP_"))

EP_var_name <- EP_var$x2020_variable_name
EP_var_expr <- EP_var$x2020_table_field_calculation
names(EP_var_expr) <- EP_var_name

## iterate with E_ vector and THEN EP_ vector
pa_co_var <- 
  map2_dfc(E_var_name, E_var_expr, function(E_var_name, E_var_expr){
    pa_co_raw %>% 
      transmute(
        !!all_of(E_var_name) := eval(str2lang(E_var_expr))
      )
  }) %>% 
  bind_cols(pa_co_raw, .) 

pa_co_var2 <- 
  map2(EP_var_name, EP_var_expr, function(EP_var_name, EP_var_expr){
    pa_co_var %>% 
      transmute(
        !!all_of(EP_var_name) := eval(str2lang(EP_var_expr))
      )
  }) %>% 
      bind_cols(pa_co_var, .) %>% 
  #keep the new columns, GEOID, CO+STATE
  select(GEOID, NAME, E_var_name, EP_var_name) 

#as a check to cdc published data in pa at county level
#PA_2020_svi_co <- read_csv("2020svi_pa_co_cdc.csv")

# PA_2020_svi_co$EP_LIMENG == pa_co_var2$EP_LIMENG
# not exactly identical, because rounded numbers after digit point

# calculate rankings with tidy form data (on EP_)-----------------------------
options(scipen=999) #disable scientific notation

pa_co_var3 <- pa_co_var2 %>% 
  select(-all_of(E_var_name)) %>%   #tidyselect, column or external vector
  pivot_longer(!c(GEOID,NAME),   #all but GEOID and NAME- no need to know total columns
    names_to = "svi_var",
    values_to = "value") %>% 
  separate(NAME, into = c("county", "state"), sep = ",") 

# Calculate pct_rank of each variable (EP_)------------------------------------
## note for 2020, PCI(income) changed into housing cost burden--no need to reverse 
pa_co_pct1 <- pa_co_var3 %>%
  group_by(svi_var) %>%
  mutate(rank =  rank(-value, ties.method = "min")) %>% 
  #check out count() "wt" arg, if NULL, count rows
  add_count(svi_var) %>%  
  mutate(EPL_var = (rank-1)/(n-1)) %>% 
  ungroup()

# Calculate pct_rank of each domain/theme--------------------------------------
## set up xwalk from EP_var or originally, var_cal_table
xwalk_theme_var <- EP_var %>% 
  select(-x2020_table_field_calculation) %>% 
  rename(svi_var = x2020_variable_name)

pa_co_pct2 <- pa_co_pct1 %>% 
  left_join(xwalk_theme_var, by = "svi_var") %>% 
  group_by
  


PA_2020_svi_co %>% 
  filter(COUNTY == "Adams County") %>% 
  select(EP_POV150, EPL_POV150)

pa_co_pct1 %>% 
  filter(county=="Adams County")
  