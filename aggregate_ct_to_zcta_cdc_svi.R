
# 0. Set up (.rds saved in data/)---------------------------------------------------------------
{
#import downloaded relationship file from census (change every 10 yrs)
  ##2020
ct_zcta <- read.table("cdc_us_svi/tab20_zcta520_tract20_natl.txt", 
  sep = "|", header = TRUE)
#don't panic with all the empty rows, just arranged weirdly-
#there're numbers that look like zip code down there!

ct_zcta_xwalk2020 <- ct_zcta %>% 
  select(ZCTA = GEOID_ZCTA5_20, 
    GEOID = GEOID_TRACT_20) %>% 
  mutate(ZCTA = as.character(ZCTA))

#load population in each zcta from result of that year
#(or pull tidycensus: var="S0601_C01_001E")
pop2020_zcta <- result2020 %>%  
  select(GEOID, E_TOTPOP)

ct_zcta_xwalk2020 <- ct_zcta_xwalk2020 %>% 
  left_join( pop2020_zcta %>% rename(ZCTA = GEOID), 
    by = "ZCTA") %>% 
  rename(ZCTA_POP = E_TOTPOP)

saveRDS(ct_zcta_xwalk2020, "data/ct_zcta_xwalk2020.rds")
#include population on zcta level
}

{
  ##2010
ct_zcta10 <- read.table("cdc_us_svi/zcta_tract_rel_10.txt", sep = ",", header = T)
#different separator 
#ZPOP is population for 2010
  
# pop2018_zcta <- result2018 %>% 
#   select(GEOID, E_TOTPOP)
#opt to leave pop out of xwalk so that it's more versitile (decade specific)

ct_zcta_xwalk2010 <- ct_zcta10 %>% 
  select(ZCTA = ZCTA5, GEOID) %>% 
  mutate(ZCTA = as.character(ZCTA)) 

saveRDS(ct_zcta_xwalk2010, "data/ct_zcta_xwalk2010.rds")
}


#load svi from cdc (ct level in PA)
svi_pa_2020 <- read_csv("cdc_us_svi/cdc_svi_2020_pa_ct.csv") %>% 
  rename(GEOID = FIPS)

svi_pa_2018 <- read_csv("cdc_us_svi/cdc_svi_2018_pa_ct.csv") %>% 
  rename(GEOID = FIPS)

#load variable table for aggregation 
var_table <- readRDS("data/variable_e_ep_calculation_2018.rds")



# 1. Join ct_zcta xwalk to cdc svi ----------------------------------------

zsvi_pa <- svi_pa_2018 %>% 
  left_join(ct_zcta_xwalk2010, by = "GEOID") %>% 
  relocate(ZCTA, .after = GEOID)

zsvi_pa %>% filter(is.na(ZCTA))


# 2. Aggregate E/EP variables in CDC SVI data (ct to zcta level) --------------

#Subsetting those with ct:zcta<=2 to check correlation
#when one census tract includes multiple zcta code, aggregated data usually quite off 
#since you're doing sum for var, and you're adding bigger area to rep a small area
#on the other hand, some tract has multiple zcta, leaving out whole tract will 
#lead to underestimated value for some zcta 
#tested ratio 1-4, 2 has best cor for var, 1 has best cor for rpl
ct_zcta_ratio2 <- zsvi_pa %>% group_by(GEOID) %>% 
  count() %>% 
  arrange(n) %>% 
  filter(n<=2) %>% 
  pull(GEOID)

ct_zcta_ratio1 <- zsvi_pa_2020 %>% group_by(GEOID) %>%
  count() %>%
  arrange(n) %>%
  filter(n==1) %>%
  pull(GEOID)

#Split E_var and EP_var for aggregating differently 
var_e <- var_table %>% 
  filter(theme%in%c(0:4),
    str_detect(.[[1]], "E_")) %>% 
  pull(1)

var_ep <- var_table %>% 
  filter(theme%in%c(0:4),
    str_detect(.[[1]], "EP_")) %>% 
  pull(1)

cdc <- zsvi_pa %>% 
  select(GEOID, ZCTA, all_of(var_e), all_of(var_ep)) %>%
  filter(GEOID%in%all_of(ct_zcta_ratio2)) %>% 
  pivot_longer(-c(GEOID,ZCTA), names_to = "var_name", values_to = "value") %>% 
  filter(value >= 0) %>% 
  group_by(ZCTA, var_name) %>% 
  # summarise(var_zcta = sum(value)) %>% 
  summarise(sum = sum(value),
    mean = mean(value)) %>%
  mutate(var_zcta = case_when(
    str_starts(var_name, "E_") ~ sum,
    str_starts(var_name, "EP_") ~ mean
  )) %>%
  ungroup() %>% 
  select(-sum, -mean)


# 3. Join aggregated CDC data to calculated result for correlation------------

result <- result2018 %>% 
  select(ZCTA = GEOID, all_of(var_e), all_of(var_ep)) %>% 
  pivot_longer(-ZCTA, names_to = "var_name", values_to = "value_hx") %>% 
  filter(value_hx >= 0)

cdc_result <- cdc %>% 
  left_join(result, by= c("ZCTA", "var_name")) %>% 
  drop_na() %>% 
  group_by(var_name) %>% 
  mutate(cor = cor(var_zcta, value_hx)) %>% 
  ungroup()

#for E_PCI should be average not sum, but EP_PCI is the same value, and taken average
#so just ignore that cor for E_PCI


# 4. Aggregate RPLs in CDC SVI and check coorelation ----------------------

##averaging ct RPLs to zcta levels

cdc2 <- zsvi_pa %>% 
  drop_na(ZCTA) %>% 
  filter_all(all_vars(.>=0)) %>% 
  filter(GEOID%in%all_of(ct_zcta_ratio1)) %>% 
  group_by(ZCTA) %>% 
  summarise(
    cdc_RPL_themes = mean(RPL_THEMES), 
    cdc_RPL_theme1 = mean(RPL_THEME1),
    cdc_RPL_theme2 = mean(RPL_THEME2),
    cdc_RPL_theme3 = mean(RPL_THEME3),
    cdc_RPL_theme4 = mean(RPL_THEME4)) %>% 
  mutate(ZCTA = paste(ZCTA)) %>% 
  ungroup()


cdc2_result2 <- cdc2 %>% 
  left_join(
     result2020 %>% 
      select(
        ZCTA = GEOID, 
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3, 
        RPL_theme4
      )) %>%
  drop_na() %>% 
  mutate(cor_all = cor(cdc_RPL_themes, RPL_themes),
    cor1 = cor(cdc_RPL_theme1, RPL_theme1),
    cor2 = cor(cdc_RPL_theme2, RPL_theme2),
    cor3 = cor(cdc_RPL_theme3, RPL_theme3),
    cor4 = cor(cdc_RPL_theme4, RPL_theme4))


cdc2_result2 %>% 
  ggplot(aes(x = cdc_RPL_themes, y = RPL_themes)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = 'red')


