library(tidyverse)

#crosswalk of all ZCTAs in PA
pa_zcta2020 <- read_csv("cdc_us_svi/ACS5Y2020_zcta_PA_S1901.csv") %>% 
  select(1:2)

pa_zcta2020 <- pa_zcta2020 %>% slice(-1) %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1)) %>% 
  pull(GEO_ID)

st_zcta <- function(year){
  read_csv(paste0("cdc_us_svi/ACS5Y", year, "_zcta_PA_S1901.csv")) %>% 
    select(1:2) %>% 
    slice(-1) %>% 
    mutate(GEO_ID = str_sub(GEO_ID, -5, -1)) %>% 
    pull(GEO_ID)
    
}

pa_zcta2021 <- st_zcta(2021)

#2021 has more ZCTA than 2020 (same as 2019) 
pa2020 <- read_csv("cdc_us_svi/ACS5Y2020_zcta_PA_S1901.csv") %>% 
  select(1:3) %>% slice(-1) %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1))
pa2021 <- read_csv("cdc_us_svi/ACS5Y2021_zcta_PA_S1901.csv") %>% 
  select(1:3) %>% slice(-1) %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1))

pa <- pa2021 %>% left_join(pa2020, by = "GEO_ID") %>% 
  filter(is.na(NAME.y))
