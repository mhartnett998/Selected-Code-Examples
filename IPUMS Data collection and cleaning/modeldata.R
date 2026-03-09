
## I'm trying something different by keeping parts of the code in separate scripts
## My hope is that this makes it easier to read and troubleshoot in the future as
## this project grows.

library(tidyverse)
library(ipumsr)


## Defining this as a function so I can call it in "Model.R"

acs_pull = function(samp, vars, description){
  acs_ext = define_extract_usa(description,
                               samples = samp,
                               variables = vars)
  
  submitted_acs = submit_extract(acs_ext)
  extract_wait = wait_for_extract(submitted_acs)
  acs_request = download_extract(extract_wait)
  acs = read_ipums_micro(acs_request)
  
  names(acs) = tolower(names(acs))
  
  ## Things to make writing this unique
    name = paste(description, 
                 '.csv',
                 sep = '')
  
  write.csv(acs, name, row.names = F)
}

