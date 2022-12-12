## load cdc data
RI_2014_ct <- read_csv("download/RI_tract_2014.csv") %>% 
  rename(GEOID = FIPS) 

## joining data with our result
dfa = RI_2014_ct %>% 
  select(
    GEOID, res_cdc = RPL_THEMES) %>% 
  mutate(GEOID = paste(GEOID)) %>% 
  left_join(
    svi_complete %>% 
      select(GEOID, res_heli = RPL_themes)
  ) 

#check NA (sometimes CDC has -999 for NA)
skim(dfa)

# check missing 
dfa %>% 
  filter(is.na(res_heli))

dfa <- dfa %>% 
  drop_na(res_heli)

## check correlation between cdc result and ours
cor(dfa$res_cdc, dfa$res_heli)

dfa %>% 
  ggplot(aes(x = res_cdc, y = res_heli)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = 'red')
