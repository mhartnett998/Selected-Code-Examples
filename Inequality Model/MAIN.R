# Setup ----
## Add any new packages here ----
packages = c('tidyverse', 'DescTools', 'ggthemes', 'stringr', 'rlang', 'sf')

## Installing and loading libraries
for (package_name in packages) {
  # Check if the package is already installed, and install if not
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  library(package_name, character.only = TRUE)
}

## Reading base data ----
cps_dat = read.csv('input data/cpsdata.csv')
fp_dat = read.csv('input data/fpdata.csv')

## Read all ACS data into one data frame
files = list.files(path = 'input data/pastacs', full.names = TRUE)
temp = lapply(files, read.csv)
acs_dat = do.call(rbind, temp)
rm(files)
rm(temp)

## Geography
geo = read_sf('input data/geos.shp')
acs_dat = acs_dat %>% mutate(geo = case_when(
  COUNTYFIP != 0 ~ 39*1000 + COUNTYFIP,
  COUNTYFIP == 0 ~ 39*100000 + PUMA
), inc_lowhigh = case_when(
  FTOTINC < 16225.76  ~ 'Low',
  FTOTINC > 181209.62  ~ 'High',
  .default = 'mid'
))

acs_dat = acs_dat %>% mutate(RACE = case_when(
  RACE == 1 & HISPAN == 0 ~ 'White',
  RACE == 2 & HISPAN == 0 ~ 'Black',
  RACE == 3 & HISPAN == 0 ~ 'Native American',
  RACE < 7 & HISPAN == 0 ~ 'Asian',
  HISPAN != 0 ~ 'Hispanic',
  .default = 'Other'
),
AGE = case_when(
  AGE > 55 ~ '55+',
  AGE > 30 ~ '30 - 54',
  AGE >= 25 ~ '< 30'
)
)
## Filter to current year for main results
acs_dat22 = acs_dat %>% filter(acs_dat$YEAR == 2022)
fp_dat23 = fp_dat %>% filter(fp_dat$time.period == '2023')

## Defining our outputs ----
results_matrix = matrix(nrow = 1, ncol = 100)
results_matrix[1,1] = "Results"
results_matrix = rbind(results_matrix, "", "")

# Main Calculations ----
source('functions/time series.R')
source('functions/distributional analysis.R')

## Gini coefficient ----
source('functions/gini.R')

map_dat = distributional_analysis('gini', acs_dat22$geo, acs_dat22$FTOTINC, weights = acs_dat22$HHWT)
map = data.frame(fips = unique(acs_dat22$geo))
map$gini = NA
row = 3
for(i in 1:nrow(map)){
  map$gini[i] = as.numeric(map_dat[row, 2])
  row = row+2
}

geo = geo %>% left_join(map, by = 'fips')
geo %>% ggplot(mapping = aes(fill = gini)) +
  geom_sf()

library(tidycensus)
test = get_acs(geography = 'public use microdata area', 
               variables = 'S0101_C01_001',
               state = 'OH',
               geometry = T) %>% 
  mutate(puma = as.numeric(GEOID))

acs_dat22$puma = 100000 *39 + as.numeric(acs_dat22$PUMA)


dist_test = distributional_analysis('gini', acs_dat22$puma, acs_dat22$FTOTINC, weights = acs_dat22$HHWT)
map = data.frame(puma = unique(acs_dat22$puma))
map$gini = NA
row = 3
for(i in 1:nrow(map)){
  map$gini[i] = as.numeric(dist_test[row, 2])
  row = row+2
}
test = test %>% left_join(map, by = 'puma')
test %>% ggplot(mapping = aes(fill = gini)) +
  geom_sf()

## Storing results
results_matrix[1,1] = 'State Gini Coefficient'
results_matrix[2,1] = 'Unweighted State Gini Coefficient'
coefs = gini(acs_dat$FTOTINC, weights = acs_dat$HHWT)

results_matrix[1,2] = coefs[1]
results_matrix[2,2] = coefs[2]

test = distributional_analysis('gini', subgroups = acs_dat$RACE, i1 = acs_dat$FTOTINC, acs_dat$HHWT)
## Past years 
## I'm gonna write this here but we should talk about how to handle cases like this

for(i in 2014:2022){
  ## Setup
  if(i == 2014) row = 3
  
  ## Define file and read
  file = paste('input data/pastacs/acsdata', i, '.csv', sep='')
  a = read.csv(file)
  
  ## Calculate it
  coefs = gini(a$FTOTINC, weights = a$HHWT)
  
  ## Store it
  results_matrix[row,1] = paste('State Gini Coefficient', i)
  results_matrix[(row + 1),1] = paste('Unweighted State Gini Coefficient', i)
  
  results_matrix[row,2] = coefs[1]
  results_matrix[(row + 1),2] = coefs[2]
  
  row = row + 2
}
rm(a)

## Plot it
## Get Gini coefs from results_matrix
gini_year = rep(NA, 10)
for(i in 1:10){
  row = 1
  gini_year[i] = results_matrix[i,2]
  row = row+2
}
gini_year = as.numeric(gini_year) * 100
year = seq(2014, 2023, by=1)

## Graph 
gini_plot = data.frame(year = year, gini = gini_year) %>% 
  ggplot(mapping = aes(x = year, y = gini_year)) +
  geom_line() + 
  scale_y_continuous(name = 'Gini Coefficient') +
  scale_x_continuous(name = 'Year') +
  theme_hc()

## Lorenz Curve ----
source('functions/lorenz plot.R')
lorenz = lorenz_plot(acs_dat$FTOTINC)

ggsave('Lorenz Curve.png', plot = lorenz, path = 'outputs/figures/')


## Income Distribution ---- 
source('functions/income distribution.R')

p1 = inc_dist(acs_dat$FTOTINC)
ggsave('Income Distribution.png', plot = p1, path = 'outputs/figures/')

temp = time_series(inc_dist, acs_dat$YEAR, acs_dat$FTOTINC)


## Income Percentages ----
source('functions/income percentages.R')
inc_perc_results = matrix(nrow = 2, ncol = 100)
inc_perc_results[1,1] = "Income Percentage Results"

## Main Results
### Set up main matrix
results = matrix(nrow = 1, ncol = 2)
results[1,1] = "Main Results"
results = rbind(results, c('Value', 'Category'))

### Get Results
matrix = as.matrix(inc_perc(acs_dat23$FTOTINC, acs_dat23$HHWT))
matrix = cbind(matrix, c('Bottom 50%', 'Bottom 90%', 'Top 10%', 'Top 1%'))

### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
inc_perc_results = rbind(inc_perc_results, results, "")

## Distributional Analysis
### Race
#### Set up race matrix
results = matrix(nrow = 1, ncol = 5)
results[1,1] = "Distributional Analysis, Race"
results = rbind(results, c('Bottom 50%', 'Bottom 90%', 'Top 10%', 'Top 1%', 'Race'))

#### Get results
matrix = do.call(rbind, distributional_analysis("inc_perc", acs_dat23$RACE, acs_dat23$FTOTINC, acs_dat23$HHWT))
matrix = cbind(matrix, rownames(matrix))

#### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
inc_perc_results = rbind(inc_perc_results, results, "")

### Geography
#### Set up geography matrix
results = matrix(nrow = 1, ncol = 5)
results[1,1] = "Distributional Analysis, Geography"
results = rbind(results, c('Bottom 50%', 'Bottom 90%', 'Top 10%', 'Top 1%', 'PUMA'))

#### Get results
matrix = do.call(rbind, distributional_analysis("inc_perc", acs_dat23$PUMA, acs_dat23$FTOTINC, acs_dat23$HHWT))
matrix = cbind(matrix, rownames(matrix))

#### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
inc_perc_results = rbind(inc_perc_results, results, "")

### Age
#### Set up geography matrix
results = matrix(nrow = 1, ncol = 5)
results[1,1] = "Distributional Analysis, Age"
results = rbind(results, c('Bottom 50%', 'Bottom 90%', 'Top 10%', 'Top 1%', 'Age'))

#### Get results
matrix = do.call(rbind, distributional_analysis("inc_perc", acs_dat23$AGE, acs_dat23$FTOTINC, acs_dat23$HHWT))
matrix = cbind(matrix, rownames(matrix))

#### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
inc_perc_results = rbind(inc_perc_results, results, "")

## Time Series
### Set up time series matrix
results = matrix(nrow = 1, ncol = 5)
results[1,1] = "Time Series"
results = rbind(results, c('Bottom 50%', 'Bottom 90%', 'Top 10%', 'Top 1%', 'Year'))

### Get results
matrix = as.matrix(time_series(inc_perc, acs_dat$MULTYEAR, acs_dat$FTOTINC, acs_dat$HHWT))
matrix = cbind(matrix, rownames(matrix))

### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
inc_perc_results = rbind(inc_perc_results, results)

## Bind to total results
rownames(inc_perc_results) = NULL
results_matrix = rbind(results_matrix, inc_perc_results, "", "")
rm(inc_perc_results)

## Emergency Savings ----
source('functions/income cutoffs.R')
source('functions/emergency savings.R')
emerg_save_results = matrix(nrow = 2, ncol = 100)
emerg_save_results[1,1] = "Emergency Saving Results"

## Establish income cutoffs
income_cutoff_results = time_series(inc_cut, acs_dat$MULTYEAR, acs_dat$FTOTINC, acs_dat$HHWT)
colnames(income_cutoff_results) = c('Bottom 10%', 'Bottom 50%', 'Top 10%')
write.csv(income_cutoff_results, 'input data/income_cutoffs.csv')
rm(income_cutoff_results)

## Main Results
###  Prepare for function
qcol_index = which(colnames(fp_dat23) == "hhincome") + 1 # find first column where questions begin 
question_ids = colnames(fp_dat23)[qcol_index:ncol(fp_dat23)] # create list with question ids
write.csv(question_ids, 'input data/question_ids.csv')
question_data = fp_dat23[question_ids] # subset dataset based on question ids 

### Set up main matrix
results = matrix(nrow = 1, ncol = 7)
results[1,1] = "Main Results"
results = rbind(results, c('Answer', 'Count', 'ID', 'Proportion', 'Question Name', 'Answer Name', 'Bracket'))

### Get Results
matrix = as.matrix(inject(emerg_save(fp_dat23$time.period, fp_dat23$hhincome, !!!question_data)))

### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
emerg_save_results = rbind(emerg_save_results, results, "")

## Distributional Analysis
### Race
#### Set up race matrix
results = matrix(nrow = 1, ncol = 8)
results[1,1] = "Distributional Analysis, Race"
results = rbind(results, c('Answer', 'Count', 'ID', 'Proportion', 'Question Name', 'Answer Name', 'Bracket', 'Race'))

#### Get Results
matrix = inject(distributional_analysis("emerg_save", fp_dat23$race, fp_dat23$time.period, fp_dat23$hhincome, !!!question_data))
list_with_names = mapply(function(mat, name) {
  cbind(mat, NameColumn = rep(name, nrow(mat)))
}, matrix, names(matrix), SIMPLIFY = FALSE)
matrix = do.call(rbind, list_with_names)

#### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
emerg_save_results = rbind(emerg_save_results, results, "")

### Age
#### Set up age matrix
results = matrix(nrow = 1, ncol = 8)
results[1,1] = "Distributional Analysis, Age"
results = rbind(results, c('Answer', 'Count', 'ID', 'Proportion', 'Question Name', 'Answer Name', 'Bracket', 'Age'))

#### Get Results
matrix = inject(distributional_analysis("emerg_save", fp_dat23$age.range, fp_dat23$time.period, fp_dat23$hhincome, !!!question_data))
list_with_names = mapply(function(mat, name) {
  cbind(mat, NameColumn = rep(name, nrow(mat)))
}, matrix, names(matrix), SIMPLIFY = FALSE)
matrix = do.call(rbind, list_with_names)

#### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
emerg_save_results = rbind(emerg_save_results, results, "")

## Time Series
###  Prepare for function
qcol_index = which(colnames(fp_dat) == "hhincome") + 1 # find first column where questions begin 
question_ids = colnames(fp_dat)[qcol_index:ncol(fp_dat)] # create list with question ids
write.csv(question_ids, 'input data/question_ids.csv')
question_data = fp_dat[question_ids] # subset dataset based on question ids 

### Set up main matrix
results = matrix(nrow = 1, ncol = 8)
results[1,1] = "Time Series"
results = rbind(results, c('Answer', 'Count', 'ID', 'Proportion', 'Question Name', 'Answer Name', 'Bracket', 'Year'))

### Get Results
matrix = inject(time_series(emerg_save, fp_dat$time.period, fp_dat$time.period, fp_dat$hhincome, !!!question_data))
matrix = cbind(matrix, rownames(matrix))
matrix = matrix %>%
  separate_wider_delim( 
    cols = "rownames(matrix)",
    delim = ".",
    names = c("time.period", "remove")
  )
matrix$remove = NULL
matrix = as.matrix(matrix)

### Bind to results matrix
results = rbind(results, matrix)
padding = matrix("", nrow = nrow(results), ncol = 100 - ncol(results))
results = cbind(results, padding)
emerg_save_results = rbind(emerg_save_results, results, "")

## Retirement ----
source('functions/reitrement.R')

cps_dat = cps_dat %>% mutate(RACE = case_when(
  RACE == 100 & HISPAN == 0 ~ 'White',
  RACE == 200 & HISPAN == 0 ~ 'Black',
  RACE == 300 & HISPAN == 0 ~ 'Native American',
  RACE < 700 & HISPAN == 0 ~ 'Asian',
  HISPAN != 0 ~ 'Hispanic',
  .default = 'Other'
  ),
  AGE = case_when(
    AGE > 45 ~ '45+',
    AGE > 34 ~ '35 - 44',
    AGE >= 25 ~ '25 - 34'
  )
)

results_matrix = rbind(results_matrix, retirement(cps_dat$PENSION, cps_dat$ASECWT))
results_matrix = rbind(results_matrix, distributional_analysis('retirement', subgroups = cps_dat$RACE, cps_dat$PENSION, cps_dat$ASECWT))
results_matrix = rbind(results_matrix, distributional_analysis('retirement', subgroups = cps_dat$AGE, cps_dat$PENSION, cps_dat$ASECWT))

probs = c(.1, .5, .9)

## Home ownership ----
source('functions/home ownership.R')

res = own_home(acs_dat22$OWNERSHP, acs_dat22$HHWT)
dist = distributional_analysis('own_home', acs_dat22$inc_lowhigh, acs_dat22$OWNERSHP, acs_dat22$HHWT)
dist = distributional_analysis('own_home', acs_dat$YEAR, acs_dat$OWNERSHP, acs_dat$HHWT)


## Income taX ----
## Post-tax incomes calculated in pre-processing
source('functions/gini.R')

res = gini(acs_dat22$posttax_fam0, acs_dat22$HHWT)
state = gini(acs_dat22$FTOTINC - acs_dat22$posttax_state0, acs_dat22$HHWT)
fed = gini(acs_dat22$FTOTINC - acs_dat22$posttax_fed0, acs_dat22$HHWT)
res = rbind(res, state, fed)


main = gini(acs_dat22$posttax_fam1, acs_dat22$HHWT)
state = gini(acs_dat22$FTOTINC - acs_dat22$posttax_state1, acs_dat22$HHWT)
fed = gini(acs_dat22$FTOTINC - acs_dat22$posttax_fed1, acs_dat22$HHWT)
res = rbind(res, main, state, fed)

main = gini(acs_dat22$posttax_fam2, acs_dat22$HHWT)
state = gini(acs_dat22$FTOTINC - acs_dat22$posttax_state2, acs_dat22$HHWT)
fed = gini(acs_dat22$FTOTINC - acs_dat22$posttax_fed2, acs_dat22$HHWT)
res = rbind(res, main, state, fed)

main = gini(acs_dat22$posttax_fam3, acs_dat22$HHWT)
state = gini(acs_dat22$FTOTINC - acs_dat22$posttax_state3, acs_dat22$HHWT)
fed = gini(acs_dat22$FTOTINC - acs_dat22$posttax_fed3, acs_dat22$HHWT)
res = rbind(res, main, state, fed)

sum(acs_dat22$posttax_state0* acs_dat22$HHWT)
sum(acs_dat22$posttax_state1* acs_dat22$HHWT)
sum(acs_dat22$posttax_state2* acs_dat22$HHWT)
sum(acs_dat22$posttax_state3* acs_dat22$HHWT)
