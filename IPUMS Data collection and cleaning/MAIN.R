
  library(tidyverse)
  
  ## Getting ACS data ----
  samp = 'us2023c' # Currently 2022 5-year data. Remember to update when 2023 drops
  
  source('D:/Scioto/OK Min Wage/Minimum Wage Model/modeldata.R') #ACS data request function
  
  ## https://usa.ipums.org/usa-action/variables/group
  vars = list( 
    var_spec('STATEFIP', case_selections = ('40')), #Just OK data
    'AGE', 
    'PUMA',
    'RACE',
    'HISPAN',
    'SEX',
    'UHRSWORK',
    'WKSWORK1',
    'INCWAGE',
    'OWNERSHP',
    'RENTGRS',
    'OWNCOST',
    'NUMPREC',
    'EMPSTAT',
    'FTOTINC',
    'FAMUNIT',
    'SPLOC',
    'MOMLOC',
    'POPLOC',
    'METRO',
    'OCC',
    'IND',
    'POVERTY'
  )
  
  acs_pull(samp = samp, vars = vars, description = 'OK Min Wage 2023 3_3')
  
  ## Reading/cleaning data ----
  source('D:/Scioto/OK Min Wage/Minimum Wage Model/ipums cleanup.R') #Cleaning function
  dat = read.csv('OK Min Wage 2023 3_3.csv')
  dat = acs_cleaning(dat)
  
  ## Employment Impact----
  # This takes SIGNIFCANTLY longer if same_els is FALSE
  reps = 1e3
  same_els = TRUE # this determines whether we use the same elasticity for everyone
  source('D:/Scioto/OK Min Wage/Minimum Wage Model/employment impact.R')
