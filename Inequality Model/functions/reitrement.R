
retirement = function(reitirement, weights = NULL, tenure_code = 3){
  ## Simple function. Require package - run function, return results
  require(tidyverse)
  
  results = matrix(nrow=1, ncol=100)
  ## Allowing for a simple calculation if we don't input weights
  n = length(reitirement)
  if(is.null(weights)){weights = rep(1, n)}
  
  own = ifelse(reitirement == tenure_code, 1,0)
  results[1,2] = 'Percent w/ retirement savings'
  results[1,3] = sum(weights * own) / sum(weights)
  
  return(results)
}