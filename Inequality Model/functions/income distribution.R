## Plotting income percentiles

inc_dist = function(incomes){
  require(tidyverse) # Technically don't need the entire tidyverse but wth, I'm not a programmer
  require(ggthemes)
  ## Making sure this is flexible with different amounts of data
  n = length(incomes)
  
  if(n > 2000){  
    points = min(round(n/200), 2000) # I don't think using more than 2000 would be needed
  } else {
    points = n / 2 # I'm including this for completeness, but I know there is a better way to do this
    print('Low sample size')
  }
  ## Calculating income percentiles
  probs = seq(0,1, length.out = points)
  percentiles = quantile(incomes, probs)
  
  ## Putting things in a dataframe for plotting 
  plot_dat = data.frame(Income = percentiles, #can change to "wealth"
                        Percentile = probs)
  
  ## Plotting
 plot = plot_dat %>% ggplot(mapping = aes(x=Percentile, y=Income)) +
    geom_area(fill = 'lightgrey') +
    geom_line() + theme_hc()  +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::dollar)
 
 return(plot)
}
