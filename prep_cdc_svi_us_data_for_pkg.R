
cdc_data <- read_csv("SVI2020_US.csv")

US2020svi_ct <- cdc_data %>% 
  select(ST, 
    STATE,
    ST_ABBR,
    STCNTY,
    COUNTY,
    FIPS,
    LOCATION,
    RPL_THEME1,
    RPL_THEME2,
    RPL_THEME3,
    RPL_THEME4,
    RPL_THEMES) %>% 
  rename(GEOID = FIPS)

save(US2020svi_ct, file = "data/US2020svi_ct.rda")


cdc_data <- read_csv("cdc_us_svi/SVI2014_US.csv")

US2014svi_ct <- cdc_data %>% 
  select(ST, 
    STATE,
    ST_ABBR,
    STCNTY,
    COUNTY,
    FIPS,
    LOCATION,
    RPL_THEME1,
    RPL_THEME2,
    RPL_THEME3,
    RPL_THEME4,
    RPL_THEMES) %>% 
  rename(GEOID = FIPS)







