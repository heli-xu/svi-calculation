
# get_census_data() -------------------------------------------------------


get_census_data <- function(year, geography, state, ...){
  
  var_list <- readRDS(paste0("data/census_variables_",year,".rds")) %>%
    unlist() %>%
    unname()

  
  get_acs(
    geography = geography,
    state = state,
    year = year,
    variables = var_list,
    output = "wide"
  )
}


get_census_data(2014, "tract", "RI")


# svi_e_ep() --------------------------------------------------------------

svi_e_ep <- function(year, data, theme){
  
  var_cal_table <- readRDS(paste0("data/variable_e_ep_calculation_", year, ".rds"))
  
 ## set up theme 0 vector, because sometimes other E_var calculation refer to them
  var_0 <- var_cal_table %>% 
    filter(theme == 0)
  
  var_0_name <- var_0[[1]]
  var_0_expr <- var_0[[3]]
  names(var_0_expr) <- var_0_name
  
  ## set up E_ vector
  E_var <- 
    var_cal_table %>% 
    filter(theme%in%c(1:4),
      str_detect(.[[1]], "E_")) 
  
  E_var_name <- E_var[[1]]
  E_var_expr <- E_var[[3]]
  names(E_var_expr) <- E_var_name
  
  ## set up EP_ vector
  EP_var <-
    var_cal_table %>% 
    filter(theme%in%c(1:4),
      str_detect(.[[1]], "EP_"))
  
  EP_var_name <- EP_var[[1]]
  EP_var_expr <- EP_var[[3]]
  names(EP_var_expr) <- EP_var_name
  
  ## iterate with E_ vector and THEN EP_ vector
  svi0 <- 
    map2_dfc(var_0_name, var_0_expr, function(var_0_name, var_0_expr){
      data %>% 
        transmute(
          !!all_of(var_0_name) := eval(str2lang(var_0_expr))
        )
    }) %>% 
    bind_cols(data, .) 
  
  svi_e <- 
    map2_dfc(E_var_name, E_var_expr, function(E_var_name, E_var_expr){
      svi0 %>% 
        transmute(
          !!all_of(E_var_name) := eval(str2lang(E_var_expr))
        )
    }) %>% 
    bind_cols(svi0, .) 
  
  
    map2(EP_var_name, EP_var_expr, function(EP_var_name, EP_var_expr){
      svi_e %>% 
        transmute(
          !!all_of(EP_var_name) := eval(str2lang(EP_var_expr))
        )
    }) %>% 
    bind_cols(svi_e, .) %>% 
    #keep the new columns, GEOID, NAME
    select(GEOID, NAME, all_of(E_var_name), all_of(EP_var_name)) 
  
}

svi_e_ep(2014, data, c(1:4))
