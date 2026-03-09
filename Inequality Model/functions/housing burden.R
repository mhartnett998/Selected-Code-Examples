
housing_burden = function(incomes, costs, weights = NA){
  ## Simple function. Require package - run function, return results
  require(tidyverse)
  
  ## Allowing for a simple calculation if we don't input weights
  n = length(incomes)
  if(is.na(weights)){weights = rep(1, n)}
  
  burden = ifelse(costs / incomes >= .3, 1,0)
  
  results = matrix(nrow = 1, ncol = 100)
  results[1,1] = 'Housing burden rate'
  results[1,2] = sum(weights * burden) / sum(weights)
  return(results)
}