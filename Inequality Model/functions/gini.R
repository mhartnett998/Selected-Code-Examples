## Note, the DescTools package has a function to calculate this directly. 
## I'm writing this in a seperate file so we can use it in other projects potentially

gini = function(incomes, weights = NULL){
  ## Simple function. Require package - run function, return results
  require(DescTools)
  gini = Gini(incomes, weights = weights)
  gini_unweighted = Gini(incomes)
  
  results = matrix(nrow = 2, ncol = 100)
  results[1,2] = 'Weighted Gini Coefficient'
  results[1,3] = 'Unweighted Gini Coefficient'
  
  results[2,2] = gini
  results[2,3] = gini_unweighted
  
  return(results)
}