library(tidycensus)
library(tidyverse)


# Load variables needed ----------------------------------
var_list <- readRDS("data/census_variables_2016.rds") %>% 
  unlist() %>% #autofill names for each string in the vector
  unname() #get rid of names --otherwise will rename the column in census pull

# Load table for calculation and XWALK for theme--------------------------
var_cal_table <- readRDS("data/variable_e_ep_calculation_2016.rds")



# Pull data from census-------------------------------------

pa_co_raw <- get_acs(
  geography = "county",
  state = "PA",
  year = 2016,
  variables = var_list,
  output = "wide"
)

saveRDS(pa_co_raw, file = "data/pa_co_raw_2016.rds")

#pa_co_raw <- readRDS("data/pa_co_raw_2016.rds")

# Extract named vector from val_cal_table (don't run)------------
var_name <- var_cal_table$x2016_variable_name

var_expr <- var_cal_table$x2016_table_field_calculation

names(var_expr) <- var_name


var_expr["EP_MINRTY"]  



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

# Create svi variables with iteration (theme0-4)-----------------------------------
## set up theme 0 vector, because sometimes other E_var calculation refer to them
var_0 <- var_cal_table %>% 
  filter(theme == 0)

var_0_name <- var_0$x2016_variable_name
var_0_expr <- var_0$x2016_table_field_calculation
names(var_0_expr) <- var_0_name

## set up E_ vector
E_var <- 
  var_cal_table %>% 
  filter(theme%in%c(1:4),
    str_detect(x2016_variable_name, "E_")) 

E_var_name <- E_var$x2016_variable_name
E_var_expr <- E_var$x2016_table_field_calculation
names(E_var_expr) <- E_var_name

## set up EP_ vector
EP_var <-
  var_cal_table %>% 
  filter(theme%in%c(1:4),
  str_detect(x2016_variable_name, "EP_"))

EP_var_name <- EP_var$x2016_variable_name
EP_var_expr <- EP_var$x2016_table_field_calculation
names(EP_var_expr) <- EP_var_name

## iterate with E_ vector and THEN EP_ vector 
pa_co_var0 <- 
  map2_dfc(var_0_name, var_0_expr, function(var_0_name, var_0_expr){
    pa_co_raw %>% 
      transmute(
        !!all_of(var_0_name) := eval(str2lang(var_0_expr))
      )
  }) %>% 
  bind_cols(pa_co_raw, .) 
  
pa_co_var <- 
  map2_dfc(E_var_name, E_var_expr, function(E_var_name, E_var_expr){
    pa_co_var0 %>% 
      transmute(
        !!all_of(E_var_name) := eval(str2lang(E_var_expr))
      )
  }) %>% 
  bind_cols(pa_co_var0, .) 
#cannot select columns, because they may be needed for later

pa_co_var2 <- 
  map2(EP_var_name, EP_var_expr, function(EP_var_name, EP_var_expr){
    pa_co_var %>% 
      transmute(
        !!all_of(EP_var_name) := eval(str2lang(EP_var_expr))
      )
  }) %>% 
      bind_cols(pa_co_var, .) %>% 
  #keep the new columns, GEOID, CO+STATE
  select(GEOID, NAME, all_of(E_var_name), all_of(EP_var_name))
# separate(NAME, into = c("county", "state"), sep = ",")
# easier without separating


#as a check to cdc published data in pa at county level
PA_2016_svi_co <- read_csv("download/2016svi_pa_co_cdc.csv")

PA_2016_svi_co %>% 
  filter(COUNTY == "Adams") %>% 
  select(COUNTY, EP_POV)

pa_co_var2 %>% 
  filter(GEOID == "42001") %>%  #geoid is character
  select(GEOID, EP_POV)
# might *NOT* be exactly identical, because rounded numbers after digit point
#CDC only keeps one digit after decimal


# calculate rankings with tidy form data (on EP_)-----------------------------
options(scipen=999) #disable scientific notation

pa_co_var3 <- pa_co_var2 %>% 
  select(GEOID, NAME, all_of(EP_var_name)) %>%   #tidyselect, column or external vector
  pivot_longer(!c(GEOID,NAME),   #all but GEOID and co/st- no need to know total columns
    names_to = "svi_var",
    values_to = "value") 
  #separate(NAME, into = c("county", "state"), sep = ",") 

# Calculate pct_rank of each variable (EPL_)------------------------------------
## if year=2020, PCI(income) changed into housing cost burden--no need to reverse 
pa_co_pct1 <- pa_co_var3 %>%
  group_by(svi_var) %>%
  mutate(rank =  rank(value, ties.method = "min")) %>% 
  #check out count() "wt" arg, if NULL, count rows
  add_count(svi_var) %>%  
  mutate(EPL_var = (rank-1)/(n-1),
    EPL_var = round(EPL_var, 4)) %>%
  ungroup()

## if year < 2020, PCI needs to be reversed (1-rank)
pa_co_pct1 <- pa_co_var3 %>%
  group_by(svi_var) %>%
  mutate(rank =  rank(value, ties.method = "min")) %>% 
  #check out count() "wt" arg, if NULL, count rows
  add_count(svi_var) %>%  
  mutate(EPL_var = ifelse(svi_var == "EP_PCI",
    1 - ((rank - 1) / (n - 1)),
    (rank - 1) / (n - 1)), 
    EPL_var = round(EPL_var, 4)) %>%
  ungroup()



##check EPL with cdc data
PA_2016_svi_co %>% 
  filter(COUNTY == "Adams") %>% 
  select(COUNTY, EP_PCI, EPL_PCI)

pa_co_pct1 %>% 
  filter(GEOID == "42001",
    svi_var == "EP_PCI")



# Calculate sum of pct_rank in each domain/theme (SPL_x)--------------------------------------
## set up xwalk from EP_var or originally, var_cal_table
xwalk_theme_var <- EP_var %>% 
  select(-x2016_table_field_calculation) %>% 
  rename(svi_var = x2016_variable_name)


pa_co_pct2 <- pa_co_pct1 %>% 
  left_join(xwalk_theme_var, by = "svi_var") %>% 
  group_by(theme, GEOID, NAME) %>%  #GEOID and NAME just there to keep the column
  summarise(SPL_theme = sum(EPL_var)) %>% 
  ungroup()


##check with cdc data
PA_2016_svi_co %>% 
  filter(COUNTY == "Forest") %>% 
  #select(COUNTY, SPL_THEME1, SPL_THEME2, SPL_THEME3, SPL_THEME4) 
  select(COUNTY, E_AGE65, EP_AGE65, EP_AGE17, EP_DISABL, EP_SNGPNT)


pa_co_pct2 %>% 
  filter(GEOID == "42053")

##not match (especially in theme4)
##go back to EPL, and then EP--the one digit after decimal problem from above

PA_2016_svi_co %>% 
  filter(COUNTY == "Adams") %>% 
  select(COUNTY, EP_MUNIT,
    EP_MOBILE,
    EP_CROWD,
    EP_NOVEH,
    EP_GROUPQ) 

pa_co_pct1 %>% 
  filter(GEOID == "42001",
    svi_var%in%c("EP_MUNIT",
    "EP_MOBILE",
    "EP_CROWD",
    "EP_NOVEH",
    "EP_GROUPQ")) 
#WTF?
##later: realized using 2016 var is wrong for 2016--corrected

pa_co_var2 %>% 
  filter(GEOID == "42001") %>% 
  select(
      E_CROWD,
      E_NOVEH)

PA_2016_svi_co %>% 
  filter(COUNTY == "Adams") %>% 
  select(COUNTY, 
   
    E_CROWD,
    E_NOVEH) 

# Calculate pct_rank of each domain/theme (RPL_x)--------------------------------------
## summarised table, no longer contain individual svi_var info
pa_co_pct3 <- pa_co_pct2  %>% 
  group_by(theme) %>% 
  mutate(rank_theme = rank(SPL_theme, ties.method = "min")) %>% 
  add_count(theme) %>%  #rows per group, count the group_by param
  mutate(RPL_theme = (rank_theme-1)/(n-1),
    RPL_theme = round(RPL_theme, 4)) %>% 
  ungroup()
  
##sanity check
x<- PA_2016_svi_co %>% 
  filter(COUNTY == "Forest") %>% 
  select(COUNTY, RPL_THEME1, RPL_THEME2,
    RPL_THEME3, RPL_THEME4)

pa_co_pct3 %>% 
  filter(GEOID == "42053")

# Calculate sum of SPL_ of all domains/themes ----------------------------------
pa_co_pct4 <- pa_co_pct3 %>% 
  group_by(GEOID, NAME) %>% 
  summarise(SPL_themes = sum(SPL_theme),
    .groups = "drop") %>% 
  # ungroup() %>% 
  add_count() %>% 
  mutate(rank_themes = rank(SPL_themes, ties.method = "min"),
   RPL_themes = (rank_themes-1)/(n-1),
      RPL_themes = round(RPL_themes, 4))

## sanity check    
PA_2016_svi_co %>% 
  filter(COUNTY == "Adams") %>% 
  select(COUNTY, SPL_THEMES, RPL_THEMES)
 
pa_co_pct4 %>% 
  filter(GEOID == "42001")
   
# Construct a wide form complied data -----------------------------------------
##pa_co_var2 = E_ data and EP_ data

EPL_var <- 
  pa_co_pct1 %>% 
  mutate(EPL_var_name = paste0("EPL_", str_remove(svi_var, "EP_")),
    .before = EPL_var) %>% 
  select(-c(svi_var, value, rank, n)) %>% 
  pivot_wider(names_from = EPL_var_name,
    values_from = EPL_var)

SPL_theme <- pa_co_pct2 %>% 
  pivot_wider(names_from = theme,
    names_prefix = "SPL_theme",
    values_from = SPL_theme)

RPL_theme <- pa_co_pct3 %>% 
  select(-c(SPL_theme, rank_theme, n)) %>% 
  pivot_wider(names_from = theme,
    names_prefix = "RPL_theme",
    values_from = RPL_theme)
    
SPL_RPL_themes <- pa_co_pct4 %>% 
  select(-c(n, rank_themes)) 

svi_complete <- list(pa_co_var2, EPL_var, SPL_theme, RPL_theme, SPL_RPL_themes) %>% 
  reduce(left_join, by = c("GEOID", "NAME")) 
#GEOID suffice, adding co/st to avoid non-joining duplicate columns 
    
    
    
    