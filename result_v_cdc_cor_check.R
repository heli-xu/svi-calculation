## load cdc data
PA_2016_svi_co <- read_csv("download/2016svi_pa_co_cdc.csv") %>% 
  rename(GEOID = FIPS) 


dfa = PA_2016_svi_co %>% 
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
