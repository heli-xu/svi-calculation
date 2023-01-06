check_variable <- function(year1, year2, dataset, var_list){
  var1 <- load_variable(year1, dataset)
  var2 <- load_variables(year2, dataset)
  var_to_check <- readRDS(paste0("data/census_variables_",year1,".rds")) %>% 
    unlist() %>% 
    unname() %>% 
    str_sub(1L, -2L)
  
  var1_2 <- var1 %>% 
    filter(name%in%all_of(var_to_check)) %>% 
    select(-concept) %>% 
    left_join(
      var2 %>% 
        filter(name%in%all_of(var_to_check)),
      by = "name"
    )
  
  return(var1_2)
}


var1_2$label.x == var1_2$label.y
