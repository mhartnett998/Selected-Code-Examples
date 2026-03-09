
own_home = function(incomes, weights = NULL, tenure_code = 1){
  ## Simple function. Require package - run function, return results
  require(tidyverse)
  
  ## Allowing for a simple calculation if we don't input weights
  n = length(incomes)
  if(is.null(weights)){weights = rep(1, n)}
  
  own = ifelse(incomes == tenure_code, 1,0)
  
  results = matrix(nrow = 1, ncol = 100)
  results[1,1] = 'Homeownership rate'
  results[1,2] = sum(weights * own) / sum(weights)
  return(results)
}