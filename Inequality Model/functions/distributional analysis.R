
## Input a function name, a column to make subgroups with, and everything needed
## for the requested function.

distributional_analysis = function(func, subgroups, ...){
  require(tidyverse)
  
  ## Function input must be a string
  if(!is.character(func)) {
    warning('Input function name as a string')
    return()
  }
  
  ## Warning
  ## There might be a way for us to work around this, but this seems simpler
  warning("ALWAYS DOUBLE CHECK INPUTS AND RESULTS. INPUTS MUST BE IN THE CORRECT ORDER FOR RESULTS TO BE VALID")
  
  ## Figure out how many groups
  groups = unique(subgroups)
  n = length(unique(subgroups))
  
  ## New formatting for storing results. This requires functions to fit our [n,100] matrix structure
  results = matrix(nrow = 1, ncol = 100)
  results[1,1] = 'Distributional Analysis'
  
  ## Getting pass through data
  args = list(...)
  n_args = length(args)
  arg_names = letters[1:n_args]
  
  ## Putting things into a dataframe for filtering
  df = data.frame(subgroups,
                  args)
  names(df) = c('subgroups', arg_names)  
  
  ## Running the functions
  for(i in 1:n){
    ## Filter
    current_df = df[df$subgroups == groups[i], ]
    clean_args = unname(as.list(current_df[, -1, drop = FALSE]))
    
    current_dim = nrow(results)
    
    ## Execute
    results = rbind(results, do.call(func, clean_args))
    results[current_dim+1, 1] = groups[i]
  }
  
  ## Sending it all back
  return(results)
}
