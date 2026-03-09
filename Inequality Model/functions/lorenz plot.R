lorenz_plot = function(incomes){
  require(DescTools)
  ## Lorenz Curve stuff 
  ## I should make this a function, or maybe it should be an option in the gini file
  lorenz = Lc(incomes, plot = T)
  lorenz = data.frame(p = lorenz$p, L = lorenz$L)
  
  p = lorenz %>% ggplot(mapping = aes(x = p, y = L)) +
    geom_area(aes(x = p, y = p), fill = 'lightgrey') + geom_area(fill = 'darkgrey') + 
    geom_line(aes(x = p, y = p)) +  geom_line() +
    scale_y_continuous(name = 'Percent of Total Cumulative Income', labels = scales::percent) +
    scale_x_continuous(name = 'Percent of Population', labels = scales::percent) +
    theme_few()
  
  return(p)
}