library(tidyverse)
library(mice)
## The goal here is to isolate only the people who earn wages in each household.
## We're trying to figure out who would be impacted by an increased minimum wage
## so we need to determine everyone's wage. 

## This will be a function that takes a data.frame in and returns it cleaned

acs_cleaning = function(acs){
  ## We begin by checking if the required columns are here. If something is missing, 
  ## we should return an error and say what's not here.
  required_vars =c('incwage', 'uhrswork', 'wkswork1', 'ownershp' , 'rentgrs', 
                   'owncost', 'empstat', 'puma')
  
  n = length(required_vars)
  test_index = 1
 
  ## Checking required variables
  for(i in 1:n){
    if(required_vars[test_index] %in% names(acs)) {
      required_vars = required_vars[-test_index]
    } else {
      test_index = test_index + 1
    }
  }
  
  if(length(required_vars) != 0 ){
    print('Missing Requried Variables')
    return(required_vars)
  }
  
  ## Attaching HoH demo info to everyone
  acs = acs %>% 
    group_by(serial) %>% 
    mutate(hoh_age = first(age),
           hoh_race = first(race),
           hoh_sex = first(sex),
           hoh_hisp = first(hispan)) %>% 
    ungroup()
  
  ## Adding some additional info here. Any kids in household and 1vs2 parent
  acs = acs %>% 
    within({anykid = ave(age, serial, FUN = min)}) %>% 
    mutate(anykid = ifelse(anykid < 18, 1, 0),
           adult = ifelse(age >= 18, 1, 0),
           worker = ifelse(empstat == 1, 1,0)) %>%
    within({nadult = ave(adult, serial, FUN = sum)}) %>% 
    within({hhsize = ave(pernum, serial, FUN = max)}) %>% 
    within({nworkers = ave(worker, serial, FUN = sum)}) %>% 
    mutate(nchild = hhsize - nadult)
    
  
  ## First step is going to be to remove everyone who isn't employed, people who
  ## don't earn traditional wages, and people living in group quarters
  ## We also make a single housing cost variable
  acs = acs %>% filter(empstat == 1,
                       gq != 4,
                       incwage !=0) %>% 
    mutate(housing = case_when(
      ownershp ==  1 ~ owncost,
      ownershp ==  2 ~ rentgrs
    ))
  
  ## We need to deal with people who did not respond to the weeks worked question
  ## We're going to use the MICE algorithm
  
  if(min(acs$wkswork1)==0){  ## Order the data so we can stitch it back together later
    acs = acs[order(acs$wkswork1),]
    
    ## We start by selecting variables to include in our model
    ## We also will define the missing values in wkswork1
    input_data  = acs %>% select(age, puma, ownershp, housing, incwage, uhrswork, wkswork1) %>% 
      mutate(wkswork1 = ifelse(wkswork1 == 0, NA, wkswork1))
    
    ## Running MICE
    set.seed(123)
    impute = mice(input_data, maxit = 20)
    
    ## Figuring out which imputation is closest to our mean value
    avgs = rep(NA,5)
    for(i in 1:5){
      avgs[i] = mean(impute$imp$wkswork1[,i])
    } 
    
    avgs = (avgs - mean(input_data$wkswork1, na.rm = T))^2
    winner = which(avgs == min(avgs))
    
    ## Stitching
    observed_data = input_data$wkswork1[!is.na(input_data$wkswork1)]
    imputed_col = c(impute$imp$wkswork1[,winner], observed_data)
    
    acs$imp_wks = imputed_col
    
    
    ## Now we get an implied hourly rate
    ## We assume that people who's implied hourly rate is less than $7.25 are 
    ## earning their money from non-wage labor (e.g self-employed people)
    acs = acs %>% mutate(wage = incwage / uhrswork / imp_wks) %>% 
      filter(wage >= 7.25)
    
    ## Putting back in original order
    acs = acs[order(acs$serial),]
  } else {
      acs = acs %>% mutate(imp_wks = wkswork1,
                           wage = incwage / uhrswork / imp_wks) %>% 
        filter(wage >= 7.25)
    }
  
  ## IDing the first person in each household
  acs = acs %>% within({first_pers = !duplicated(serial)})
  
  ## Adding new min wage and calculating the new family income ----
  acs = acs %>% mutate(new_wage = ifelse(wage <= 15, 15, wage),
                       new_sal = new_wage * uhrswork * imp_wks,
                       newinc = ftotinc - incwage + new_sal) %>% 
    within({tot_old_inc = ave(incwage, serial, FUN = sum)}) %>% 
    within({tot_new_inc = ave(new_sal, serial, FUN = sum)})
  
  return(acs)
}

