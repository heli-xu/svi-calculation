
ct_zcta <- read.table("cdc_us_svi/tab20_zcta520_tract20_natl.txt", 
  sep = "|", header = TRUE)
#don't panic with all the empty rows, just arranged weirdly-
#there're numbers that look like zip code down there!

ct_zcta_xwalk2020 <- ct_zcta %>% 
  select(ZCTA = GEOID_ZCTA5_20, 
    GEOID = GEOID_TRACT_20) 

ct_zcta %>% group_by(GEOID_ZCTA5_20) %>% 
  drop_na(GEOID_ZCTA5_20) %>% 
  count()

skim(ct_zcta)  

svi_pa_2020 <- read_csv("cdc_us_svi/cdc_svi_2020_pa_ct.csv") %>% 
  rename(GEOID = FIPS)

zsvi_pa_2020 <- svi_pa_2020 %>% 
  left_join(ct_zcta_xwalk2020, by = "GEOID") 

skim(zsvi_pa_2020)
# check ZCTA NAs, worked BUT too many columns, pretty crazy report
# better to just filter:
zsvi_pa_2020 %>% filter(is.na(ZCTA))

zzsvi_pa_2020 <- zsvi_pa_2020 %>% 
  drop_na(ZCTA) %>% 
  filter_all(all_vars(.>=0)) %>% 
  group_by(ZCTA) %>% 
  summarise(
    cdc_RPL_themes = mean(RPL_THEMES), 
    cdc_RPL_theme1 = mean(RPL_THEME1),
    cdc_RPL_theme2 = mean(RPL_THEME2),
    cdc_RPL_theme3 = mean(RPL_THEME3),
    cdc_RPL_theme4 = mean(RPL_THEME4)) %>% 
  rename(GEOID = ZCTA) %>% 
  mutate(GEOID = paste(GEOID)) %>% 
  ungroup()

dfa <- zzsvi_pa_2020 %>% 
  left_join(
    result2020 %>% 
      select(
        GEOID, 
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3, 
        RPL_theme4
      ))

skim(dfa)
