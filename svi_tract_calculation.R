library(tidycensus)
library(tidyverse)


# Load variables needed ----------------------------------
var_list <- readRDS("data/census_variables_2014.rds") %>% 
  unlist() %>% #autofill names for each string in the vector
  unname() #get rid of names --otherwise will rename the column in census pull

# Load table for calculation and XWALK for theme--------------------------
var_cal_table <- readRDS("data/variable_e_ep_calculation_2014.rds")


# Pull data from census-------------------------------------

ri_ct_raw <- get_acs(
  geography = "tract",
  state = "RI",
  year = 2014,
  variables = var_list,
  output = "wide"
)


save(ri_ct_raw, file = "data/ri_tract_2014_raw.rds")

# Create svi variables with iteration (theme0-4)--------------------------------
## set up theme 0 vector, because sometimes other E_var calculation refer to them
var_0 <- var_cal_table %>% 
  filter(theme == 0)

var_0_name <- var_0$x2014_variable_name
var_0_expr <- var_0$x2014_table_field_calculation
names(var_0_expr) <- var_0_name

## set up E_ vector
E_var <- 
  var_cal_table %>% 
  filter(theme%in%c(1:4),
    str_detect(x2014_variable_name, "E_")) 

E_var_name <- E_var$x2014_variable_name
E_var_expr <- E_var$x2014_table_field_calculation
names(E_var_expr) <- E_var_name

## set up EP_ vector
EP_var <-
  var_cal_table %>% 
  filter(theme%in%c(1:4),
    str_detect(x2014_variable_name, "EP_"))

EP_var_name <- EP_var$x2014_variable_name
EP_var_expr <- EP_var$x2014_table_field_calculation
names(EP_var_expr) <- EP_var_name

## iterate with E_ vector and THEN EP_ vector
ri_ct_0 <- 
  map2_dfc(var_0_name, var_0_expr, function(var_0_name, var_0_expr){
    ri_ct_raw %>% 
      transmute(
        !!all_of(var_0_name) := eval(str2lang(var_0_expr))
      )
  }) %>% 
  bind_cols(ri_ct_raw, .) 

ri_ct_e <- 
  map2_dfc(E_var_name, E_var_expr, function(E_var_name, E_var_expr){
    ri_ct_0 %>% 
      transmute(
        !!all_of(E_var_name) := eval(str2lang(E_var_expr))
      )
  }) %>% 
  bind_cols(ri_ct_0, .) 

ri_ct_e_ep <- 
  map2(EP_var_name, EP_var_expr, function(EP_var_name, EP_var_expr){
    ri_ct_e %>% 
      transmute(
        !!all_of(EP_var_name) := eval(str2lang(EP_var_expr))
      )
  }) %>% 
  bind_cols(ri_ct_e, .) %>% 
  #keep the new columns, GEOID, NAME
  select(GEOID, NAME, all_of(E_var_name), all_of(EP_var_name))   
 # drop_na() #only keeps complete rows (rows with any NAs get dropped)
#removed because not making much difference at epl level (na gets dropped in long form)

#sanity check (optional)
RI_2014_ct <- read_csv("download/RI_tract_2014.csv") %>% 
  rename(GEOID = FIPS)
 
test <- ri_ct_e_ep %>% 
  mutate(EP_LIMENG = round(EP_LIMENG, 1)) %>% 
  select(GEOID, E_LIMENG, EP_LIMENG, B16005_001E) %>% 
  #census var B--- only available before the select step above
  filter(is.na(EP_LIMENG)) 

geo <- pull(test, GEOID)

cdc <- RI_2014_ct %>% 
  select(GEOID, E_LIMENG, EP_LIMENG) %>% 
  filter(GEOID %in% geo)
## NAs sometimes are 0 early on, and then -999 later in EPL_ in cdc data
## in this case, cdc EP_LIMENG has 247 rows, 
## but one row with 0 (E_LIMENG also 0)shows up -999 later on
## test data has 250 rows, with 4 NAs

test2 <- ri_ct_e_ep %>% 
  mutate(EP_LIMENG = round(EP_LIMENG, 1)) %>%
  select(GEOID, EP_LIMENG) %>% 
  drop_na(EP_LIMENG) 

cdc2 <- RI_2014_ct %>% 
  select(GEOID, EP_LIMENG)


# calculate pct_rank of each variable (EPL_))--------------------------------
## using long/tidy form data
options(scipen=999) #disable scientific notation
## for year >= 2020
ri_ct_epl <- ri_ct_e_ep %>% 
  select(GEOID, NAME, all_of(EP_var_name)) %>%   #tidyselect, column or external vector
  pivot_longer(!c(GEOID,NAME),   #all but GEOID and NAME - no need to know total columns
    names_to = "svi_var",
    values_to = "value") %>% 
  drop_na(value) %>%  # in case there's *some* variables missing in some tracts
  group_by(svi_var) %>%
  mutate(rank =  rank(value, ties.method = "min")) %>% 
  #check out count() "wt" arg, if NULL, count rows
  add_count(svi_var) %>%  
  mutate(EPL_var = (rank-1)/(n-1),
    EPL_var = round(EPL_var, 4)) %>%
  ungroup()

## for year < 2020
ri_ct_epl <- ri_ct_e_ep %>% 
  select(GEOID, NAME, all_of(EP_var_name)) %>%   #tidyselect, column or external vector
  pivot_longer(!c(GEOID,NAME),   #all but GEOID and NAME - no need to know total columns
    names_to = "svi_var",
    values_to = "value") %>% 
  drop_na(value) %>%  # in case there's *some* variables missing in some tracts
  group_by(svi_var) %>%
  mutate(rank =  rank(value, ties.method = "min")) %>% 
  #check out count() "wt" arg, if NULL, count rows
  add_count(svi_var) %>%  
  mutate(EPL_var = ifelse(svi_var == "EP_PCI",
    1 - ((rank - 1) / (n - 1)),
    (rank - 1) / (n - 1)), 
    EPL_var = round(EPL_var, 4)) %>%
  ungroup()

#check
test <- ri_ct_epl %>% 
  filter(svi_var == "EP_PCI") %>% 
  select(GEOID, svi_var, value, EPL_var)

cdc2 <- RI_2014_ct %>% 
  select(GEOID, EPL_PCI)


# Calculate sum of pct_rank in each domain/theme (SPL_x)--------------------------------------
## set up xwalk from EP_var or originally, var_cal_table
xwalk_theme_var <- EP_var %>% 
  select(-x2014_table_field_calculation) %>% 
  rename(svi_var = x2014_variable_name)

ri_ct_spl <- ri_ct_epl %>% 
  left_join(xwalk_theme_var, by = "svi_var") %>% 
  group_by(theme, GEOID, NAME) %>%  #GEOID and NAME just there to keep the column
  summarise(SPL_theme = sum(EPL_var)) %>% 
  ungroup()

##check
RI_2014_ct %>% 
  select(GEOID, SPL_THEME1, SPL_THEME2, SPL_THEME3, SPL_THEME4) 

ri_ct_spl %>% 
  group_by(theme) %>% 
  group_modify(~head(.x, 2))


# Calculate pct_rank of each domain/theme (RPL_x)--------------------------------------
## summarised table, no longer contain individual svi_var info
ri_ct_rpl <- ri_ct_spl  %>% 
  group_by(theme) %>% 
  mutate(rank_theme = rank(SPL_theme, ties.method = "min")) %>% 
  add_count(theme) %>%  #rows per group, count the group_by param
  mutate(RPL_theme = (rank_theme-1)/(n-1),
    RPL_theme = round(RPL_theme, 4)) %>% 
  ungroup()


# Calculate sum of SPL_ of all domains/themes ----------------------------------
ri_ct_rpl_thms <- ri_ct_rpl %>% 
  group_by(GEOID, NAME) %>% 
  summarise(SPL_themes = sum(SPL_theme),
    .groups = "drop") %>% 
  # ungroup() %>% 
  add_count() %>% 
  mutate(rank_themes = rank(SPL_themes, ties.method = "min"),
    RPL_themes = (rank_themes-1)/(n-1),
    RPL_themes = round(RPL_themes, 4))

##check
RI_2014_ct %>% 
  select(GEOID, SPL_THEMES, RPL_THEMES) 

ri_ct_rpl_thms %>% 
  select(GEOID, SPL_themes, RPL_themes)


# Construct a wide form complied data -----------------------------------------
##ri_ct_e_ep = E_ data and EP_ data 


EPL_var <- 
  ri_ct_epl %>% 
  mutate(EPL_var_name = paste0("EPL_", str_remove(svi_var, "EP_")),
    .before = EPL_var) %>% 
  select(-c(svi_var, value, rank, n)) %>% 
  pivot_wider(names_from = EPL_var_name,
    values_from = EPL_var)

SPL_theme <- ri_ct_spl %>% 
  pivot_wider(names_from = theme,
    names_prefix = "SPL_theme",
    values_from = SPL_theme)

RPL_theme <- ri_ct_rpl %>% 
  select(-c(SPL_theme, rank_theme, n)) %>% 
  pivot_wider(names_from = theme,
    names_prefix = "RPL_theme",
    values_from = RPL_theme)

SPL_RPL_themes <- ri_ct_rpl_thms %>% 
  select(-c(n, rank_themes)) 

svi_complete <- list(ri_ct_e_ep, EPL_var, SPL_theme, RPL_theme, SPL_RPL_themes) %>% 
  reduce(left_join, by = c("GEOID", "NAME")) 
#GEOID suffice, adding NAME to avoid non-joining duplicate columns 
#keeping the NAs in e_ep_
