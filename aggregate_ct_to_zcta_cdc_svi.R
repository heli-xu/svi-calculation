
ct_zcta <- read.table("cdc_us_svi/tab20_zcta520_tract20_natl.txt", 
  sep = "|", header = TRUE)
#don't panic with all the empty rows, just arranged weirdly-
#there're numbers that look like zip code down there!

ct_zcta_xwalk2020 <- ct_zcta %>% 
  select(ZCTA = GEOID_ZCTA5_20, 
    GEOID = GEOID_TRACT_20) %>% 
  mutate(ZCTA = as.character(ZCTA))

#load population in each zcta (or pull again S0601_C01_001E)
#using data argument from the svi function
pop2020_zcta <- data2020_pa %>%  
  select(GEOID, "S0601_C01_001E")

ct_zcta_xwalk2020 <- ct_zcta_xwalk2020 %>% 
  left_join( pop2020_zcta %>% rename(ZCTA = GEOID), 
    by = "ZCTA") %>% 
  rename(ZCTA_POP = S0601_C01_001E)


saveRDS(ct_zcta_xwalk2020, "data/ct_zcta_xwalk2020.rds")

#load svi from cdc (ct level in PA)
svi_pa_2020 <- read_csv("cdc_us_svi/cdc_svi_2020_pa_ct.csv") %>% 
  rename(GEOID = FIPS)

zsvi_pa_2020 <- svi_pa_2020 %>% 
  left_join(ct_zcta_xwalk2020, by = "GEOID") %>% 
  relocate(ZCTA, .after = GEOID)

var_name2020 <- readRDS("data/variable_e_ep_calculation_2020.rds") %>% 
  filter(theme%in%c(0:4)) %>% 
  pull(1)

cdc <- zsvi_pa_2020 %>% 
  select(GEOID, ZCTA, all_of(var_name2020)) %>%
  pivot_longer(-c(GEOID,ZCTA), names_to = "var_name", values_to = "value") %>% 
  group_by(ZCTA, var_name) %>% 
  summarise(var_zcta = sum(value))

result <- result2020_2 %>% 
  select(ZCTA = GEOID, all_of(var_name2020)) %>% 
  pivot_longer(-ZCTA, names_to = "var_name", values_to = "value_hx") 

cdc_result <- cdc %>% 
  left_join(result, by= c("ZCTA", "var_name")) %>% 
  drop_na() %>% 
  group_by(var_name) %>% 
  summarise(cor = cor(var_zcta, value)) %>% 
  ungroup()

skim(zsvi_pa_2020)
# check ZCTA NAs, worked BUT too many columns, pretty crazy report
# better to just filter:
zsvi_pa_2020 %>% filter(is.na(ZCTA))

zsvi_pa_2020 %>% 
  select(GEOID, ZCTA, E_TOTPOP, S0601_C01_001E)

zzsvi_pa_2020 <- zsvi_pa_2020 %>% 
  drop_na(ZCTA) %>% 
  filter_all(all_vars(.>=0)) %>% 
  group_by(ZCTA) %>% 
  summarise(
    cdc_RPL_themes = weighted.mean(RPL_THEMES, w = AREA_SQMI), 
    cdc_RPL_theme1 = weighted.mean(RPL_THEME1, w = AREA_SQMI),
    cdc_RPL_theme2 = weighted.mean(RPL_THEME2, w = AREA_SQMI),
    cdc_RPL_theme3 = weighted.mean(RPL_THEME3, w = AREA_SQMI),
    cdc_RPL_theme4 = weighted.mean(RPL_THEME4, w = AREA_SQMI)) %>% 
  rename(GEOID = ZCTA) %>% 
  mutate(GEOID = paste(GEOID)) %>% 
  ungroup()

dfa <- zzsvi_pa_2020 %>% 
  left_join(
   x <- result2020 %>% 
      select(
        GEOID, 
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3, 
        RPL_theme4
      ))

skim(dfa)
