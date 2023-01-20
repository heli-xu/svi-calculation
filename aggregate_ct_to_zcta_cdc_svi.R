
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

zsvi_pa_2020 %>% filter(is.na(ZCTA))

#when one census tract includes multiple zcta code, aggregated data usually quite off 
#since you're doing sum for var, and you're adding bigger area to rep a small area

ct_zcta_1_1 <- zsvi_pa_2020 %>% group_by(GEOID) %>% 
  count() %>% 
  arrange(n) %>% 
  filter(n==1) %>% 
  pull(GEOID)

var_table <- readRDS("data/variable_e_ep_calculation_2020.rds")

var_e <- var_table %>% 
  filter(theme%in%c(0:4),
    str_detect(.[[1]], "E_")) %>% 
  pull(1)

var_ep <- var_table %>% 
  filter(theme%in%c(0:4),
    str_detect(.[[1]], "EP_")) %>% 
  pull(1)

cdc <- zsvi_pa_2020 %>% 
  select(GEOID, ZCTA, all_of(var_e), all_of(var_ep)) %>%
  filter(GEOID%in%all_of(ct_zcta_1_1)) %>% 
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


result <- result2020_2 %>% 
  select(ZCTA = GEOID, all_of(var_e), all_of(var_ep)) %>% 
  pivot_longer(-ZCTA, names_to = "var_name", values_to = "value_hx") %>% 
  filter(value_hx >= 0)

cdc_result <- cdc %>% 
  left_join(result, by= c("ZCTA", "var_name")) %>% 
  drop_na() %>% 
  group_by(var_name) %>% 
  mutate(cor = cor(var_zcta, value_hx)) %>% 
  ungroup()


##averaging ct RPLs to zcta levels

cdc2 <- zsvi_pa_2020 %>% 
  drop_na(ZCTA) %>% 
  filter_all(all_vars(.>=0)) %>% 
  filter(GEOID%in%all_of(ct_zcta_1_1)) %>% 
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
  mutate(cor_all = cor(cdc_RPL_themes, RPL_themes),
    cor1 = cor(cdc_RPL_theme1, RPL_theme1),
    cor2 = cor(cdc_RPL_theme2, RPL_theme2),
    cor3 = cor(cdc_RPL_theme3, RPL_theme3),
    cor4 = cor(cdc_RPL_theme4, RPL_theme4))

