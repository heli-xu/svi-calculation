library(skimr)
library(readr)
library(tidyverse)

## load cdc data
# keep in mind in 2019 changed from PCI to HBURD


PA_2016_svi_co <- read_csv("download/2016svi_pa_co_cdc.csv") %>% 
  rename(GEOID = FIPS) 

PA_2020_ct <- read_csv("cdc_us_svi/cdc_svi_2020_pa_ct.csv") %>% 
  rename(GEOID = FIPS) 

RI_2020_svi_ct <- read_csv("download/RI_tract_2020.csv") %>% 
  rename(GEOID = FIPS)

## joining data with our result
dfa <- PA_2020_ct %>% 
  # filter(ST_ABBR == "RI") %>%  # if checking against US data
  select(
    GEOID, 
    cdc_RPL_themes = RPL_THEMES, 
    cdc_RPL_theme1 = RPL_THEME1,
    cdc_RPL_theme2 = RPL_THEME2,
    cdc_RPL_theme3 = RPL_THEME3,
    cdc_RPL_theme4 = RPL_THEME4
    ) %>%
  mutate(GEOID = paste(GEOID)) %>% 
  left_join(
    result %>% 
      select(
        GEOID, 
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3, 
        RPL_theme4
        )
  ) 



#check NA (sometimes CDC has -999 for NA)
skim(dfa)

# check missing 
dfa %>% 
  filter(is.na(RPL_theme1)) %>% view()

dfa <- dfa %>% 
  drop_na() %>%   ## remove NA rows
  filter_all(all_vars(.>=0))  ##sometimes no NA in DIY, but -999 in cdc data

## check correlation between cdc result and ours
cor(dfa$cdc_RPL_themes, dfa$RPL_themes)
cor(dfa$cdc_RPL_theme1, dfa$RPL_theme1)
cor(dfa$cdc_RPL_theme2, dfa$RPL_theme2)
cor(dfa$cdc_RPL_theme3, dfa$RPL_theme3)
cor(dfa$cdc_RPL_theme4, dfa$RPL_theme4)


dfa %>% 
  ggplot(aes(x = cdc_RPL_themes, y = RPL_themes)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = 'red')
