## cdc
dfa = RI_2020_ct %>% 
  select(
    GEOID, res_cdc = RPL_THEMES) %>% 
  mutate(GEOID = paste(GEOID)) %>% 
  inner_join(
    ri_ct_rpl_thms %>% 
      select(GEOID, res_heli = RPL_themes)
  ) 

skim(dfa)

# check missing
RI_2020_ct %>% 
  select(
    GEOID, res_cdc = RPL_THEMES) %>% 
  mutate(GEOID = paste(GEOID)) %>% 
  left_join(
    ri_ct_rpl_thms %>% 
      select(GEOID, res_heli = RPL_themes)
  ) %>% 
  filter(is.na(res_heli))

## home
cor(dfa$res_cdc, dfa$res_heli)

dfa %>% 
  ggplot(aes(x = res_cdc, y = res_heli)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = 'red')
