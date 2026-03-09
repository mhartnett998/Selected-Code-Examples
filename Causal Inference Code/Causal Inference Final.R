## Load many useful packages 
library(tableone)
library(stargazer)
library(knitr)
library(MASS)
library(randomForest)
library(expss)
library(kableExtra)
library(boot)
library(forestplot)
library(survival)
library(cmprsk)
library(cmprskcoxmsm)
library(ipw)
library(ggplot2)
library(dplyr)
library(reshape2)
library(survey)

## Read in the data
## Data are from the NLMS 
## https://www.census.gov/topics/research/nlms.Project_Overview.html

full = read.csv('11_new.csv')

## For this project we only consider subjects between 18 and 51 years
## old. These are the 25th and 75th percentiles of the data. Also,
## For the sake of this question it makes sense to only consider adults
## who are independent and not yet old enough to qualify for medicare.

## Furthermore we reduce the rows to only include the variables of interest
dat = full[full$age>=18 & full$age<= 51,]
dat.small = dat[,c(1,2,3,4,7,19,21,22,23,29,36)]

## We have enough data to just drop individuals who are missing their
## health insurance status
dat = dat[!is.na(dat$histatus),]

## Separating medical from non-medical deaths. The final variable is coded as 
## 0 = no death, 1 = medical death, 2 = non-medical death
## This is a subjective decision based on the listed causes of death
bad.codes = c(44,45,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113)
dat.small$med = factor(ifelse(!(dat$cause113 %in% bad.codes) & dat$inddea==1, 1, 0))
dat.small$nmed = as.numeric(dat$med)-1

dat.small$nonmed = factor(ifelse(dat$cause113 %in% bad.codes & dat$inddea==1, 1, 0))

dat.small$deathtype = 0
dat.small$deathtype[dat$med == 1] = 1
dat.small$deathtype[dat$nonmed == 1] = 2
dat.small$deathtype = factor(dat$deathtype)

## More basic data cleaning stuff 
dat$inddea = factor(dat$inddea)
dat$histatus = factor(dat$histatus)
dat.small$nstat = as.numeric(dat$histatus) - 1
dat$urban = factor(dat$urban)
dat$sex = factor(dat$sex)
dat = dat[!is.na(dat$race),]
dat$race = factor(dat$race)
dat = dat[!is.na(dat$health),]
dat$health = factor(dat$health)
dat = dat[!is.na(dat$adjinc),]
dat$adjinc = factor(dat$adjinc)

## The cmprskcoxmsm package can not handle too much data. Here we 
## take a random subset of 15,000 to do the analysis
set.seed(7485)
n=1.5e4
dat.sample = dat.small[sample(1:n, n, replace = TRUE),]


## Table 1 and preparing IPW figure
vars=c('inddea', 'med', 'nonmed', 'age', 'sex', 'urban', 'race', 'health', 'adjinc')
convars=c('age', 'sex', 'urban', 'race', 'health', 'adjinc')

dat = apply_labels(dat, histatus = 'Health Insurnace Status',
                   age = 'Age',
                   sex = 'Sex',
                   urban = 'Urban',
                   race = 'Race',
                   health = 'General Assement of Health',
                   adjinc = 'Adjusted Income')

tabUnmatched = CreateTableOne(vars = vars, strata = "histatus",
                               data = dat.sample, test = FALSE)
t1 = print(tabUnmatched, smd = TRUE, showAllLevels = TRUE, varLabels = TRUE)

## LaTeX table output
kable(t1, format = 'latex', booktabs = T)%>%
  kable_styling(full_width = F, latex_options = c("striped", 'scale_down'))

## Cumulative incidence analysis
dat.lab = dat.sample
levels(dat.lab$histatus) = c('Uninsured /', 'Insured /')
levels(dat.lab$deathtype) = c('Alive', 'Medical cause of death', 'Non-medical cause of death')


test = cuminc(ftime = dat.lab$follow, fstatus = dat.lab$deathtype, group = dat.lab$histatus,
              cencode = 'Alive')

plot(test, ylim = c(0,.04), xlab = 'Days', color = c('red', 'blue', 'brown', 'darkgreen'))

## Here, X is the matrix of covariates
X = cbind(dat$histatus, dat$age, dat$sex, dat$urban, dat$race, dat$health, dat$adjinc)
colnames(X) = c('histatus', 'age', 'sex', 'urban', 'race', 'health', 'adjinc')

fit = crr(ftime = dat$follow, fstatus = dat$deathtype, cov1 = X, failcode = 1, cencode =0)

## Now we are
w1 = ipwpoint(exposure = histatus, family = 'binomial', link = 'logit', 
              numerator = ~ 1, denominator = ~age+sex+urban+race+health+adjinc, data = dat.sample)
wfull = ipwpoint(exposure = histatus, family = 'binomial', link = 'logit', 
              numerator = ~ 1, denominator = ~age+sex+urban+race+health+adjinc, data = dat)


ipw.log.rank(times = dat$follow, failures = dat$nmed, variable = dat$nstat, weights = w1$ipw.weights)

## Fitting with cmprskcoxmsm
inv.wts.sample = doPS(data = dat.sample, 
                      Trt = 'nstat', 
                      Trt.name = '1', VARS. = c('age', 'sex', 'urban', 'race', 'health', 'adjinc'))

inv.wt.dat = inv.wts.sample[['Data']]


dat.sample$inc = ifelse(dat.sample$adjinc == 13| dat.sample$adjinc == 14,
                        1, 0)
dat.sample$inc = as.factor(dat.sample$inc)

inc.dops = doPS(data = dat.sample, 
                Trt = 'inc', 
                Trt.name = '1', VARS. = c('age', 'sex', 'urban', 'race', 'health', 'histatus'))

inc.dat = inc.dops[['Data']]

## 
wt.cox = weight_cause_cox(data = inv.wt.dat, time = 'follow', 
                          Event.var = 'deathtype', Event = 1, 
                          weight.type = 'Stabilized', ties = NULL)

wt.inc = weight_cause_cox(data = inc.dat, time = 'follow', 
                          Event.var = 'deathtype', Event = 1, 
                          weight.type = 'Stabilized', ties = NULL)

cif = cif_est(data = inv.wt.dat, time = 'follow', 
              Event.var = 'deathtype', Events = c(1,2), 
              cif.event = 1, weight.type = 'Stabilized',
              ties = NULL)

plot_est_cif(cif$cif_data, color = c('red' ,'green'))

## Using the ipw package
ipwplot(weights = w1$ipw.weights, logscale = FALSE,
        main = "Stabilized weights")

## Pretty much everything here directly follows the lecture notes
## The one exception is that I use ipwpoint() to get my weights
dat.sample$weight = w1$ipw.weights

datSvy = svydesign(ids = ~ 1, data = dat.sample, weights = ~ weight)

tabWeighted = svyCreateTableOne(vars = vars, strata = "histatus",
                                 data = datSvy, test = FALSE)


dataPlot = data.frame(variable  = rownames(ExtractSmd(tabUnmatched)),
                       Unweighted = as.numeric(ExtractSmd(tabUnmatched)),
                       ipwpoint = as.numeric(ExtractSmd(tabWeighted)))
dataPlot = dplyr::filter(dataPlot, 
                          variable != "inddea")

dataPlotMelt = melt(data          = dataPlot,
                     id.vars       = c("variable"),
                     variable.name = "Method",
                     value.name    = "SMD")

varNames = as.character(dataPlot$variable)[order(dataPlot$Unweighted)]
dataPlotMelt$variable = factor(dataPlotMelt$variable,
                                levels = varNames)

ggplot(data = dataPlotMelt,
       mapping = aes(x = variable, y = SMD, group = Method, color = Method)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() + theme(legend.key = element_blank())


## Trying to fit a weighted cox model
surv.dat = Surv(time = dat.sample$follow, event = dat.sample$nmed)

fit.cox = coxph(surv.dat ~ histatus+age+sex+urban+race+health+adjinc, data = dat.sample)

fit.cox.wt = coxph(surv.dat ~ histatus+age+sex+urban+race+health+adjinc, data = dat.sample, 
                weights = w1$ipw.weights)

## Logit model for comparison
bad = glm(nmed ~ histatus+age+sex+urban+race+health+adjinc, data = dat.sample, 
          family = 'binomial')





