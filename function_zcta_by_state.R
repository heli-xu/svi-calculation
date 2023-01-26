library(tidyverse)

#list of all ZCTAs in PA
##this table is downloaded from data.census.gov selecting all zcta in PA
pa_zcta2020 <- read_csv("cdc_us_svi/ACS5Y2020_zcta_PA_S1901.csv") %>% 
  select(1:2)

pa_zcta2020 <- pa_zcta2020 %>% slice(-1) %>% 
  mutate(GEO_ID = str_sub(GEO_ID, -5, -1)) %>% 
  pull(GEO_ID)


##another approach to get the list:
##uses cdc svi2020 ct level joined with ct_zcta_xwalk2020 (relationship file from census)
##then pull all zcta (2 tracts does not corespond to any zcta)
##zsvi_pa_2020: joined table, refer to aggregate_Ct_to_zcta_cdc_svi.R
##data2020: all zcta data in US pulled from tidycensus

pa_zcta2020_2 <- zsvi_pa_2020 %>% 
  select(GEOID, ZCTA) %>% 
  group_by(ZCTA) %>% 
  count() %>% 
  ungroup() %>% 
  pull(ZCTA)
#it has 1834 zctas, but filter these from data2020 only gives me 1780
#first method above gives me 1798 (which generate result2020)
#now we are checking extra zcta that's not in result2020

zcta_diff <- zsvi_pa_2020 %>% 
  select(GEOID, ZCTA) %>% 
  group_by(ZCTA) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(
    result2020 %>% 
      select(ZCTA=GEOID,
        E_TOTPOP)
  ) 

missing_zcta <- pa_zcta2020_2 %>% filter(is.na(E_TOTPOP)) %>% 
  pull(ZCTA)

data2020 %>% filter(GEOID%in%all_of(missing_zcta))
##turns out these zcta are not included in raw data


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
