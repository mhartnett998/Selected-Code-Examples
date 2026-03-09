library(MASS)
library(ggplot2)
library(stargazer)
library(mice)
library(tableone)
library(knitr)
library(kableExtra)
library(dplyr)
library(ipw)

dat = read.csv('WINCS_2.8.2022 stats class dataset 447 (edited var names).csv')
dat = dat[-c(1,2,3,4,5,6,8,9,12,13,16,17,18,19),]
dat = dat[dat$genderidentity == 'Man'|dat$genderidentity == 'Woman',]

## MBI Categorization 
## The responses are the same for each question
## This means the only thing that needs to change for each category is the
## index of the columns. The new variables will categorical from 0 - 3
## Low - High burnout and 3 = missing

dat$pa.b = dat$d.b = dat$ee.b = 0

## This function calculates the burnout score for a given subject and given
## burnout questions 1-3. It returns the category 0-2 that the subject is in
score = function(sub, burn.q){
  b.mat = rbind(c(1,6,9), c(2,5,8), c(3,4,7))
  burn.ind = b.mat[burn.q,]
  cat = 2
  
  ## Take the burnout question index and reference it to the correct column
  burn.ind = burn.ind + 118
  n = length(burn.ind)
  
  burn.score = numeric(n)
  for(i in 1:n){
    if(dat[sub, burn.ind[i]] == 'A few times per year')
      burn.score[i] = 1
    if(dat[sub, burn.ind[i]] == 'Once per month or less')
      burn.score[i] = 2
    if(dat[sub, burn.ind[i]] == 'A few times per month')
      burn.score[i] = 3
    if(dat[sub, burn.ind[i]] == 'Once per week')
      burn.score[i] = 4
    if(dat[sub, burn.ind[i]] == 'A few times per week')
      burn.score[i] = 5
    if(dat[sub, burn.ind[i]] == 'Every day')
      burn.score[i] = 6
  }
  if(burn.q == 1){
    tot = sum(burn.score)
    if (tot >= 15)
      cat = 0
    if(tot==13 | tot==14)
      cat = 1
  }
  
  if(burn.q == 2){
    tot = sum(burn.score)
    if (tot <= 3)
      cat = 0
    if(tot==4 | tot==6)
      cat = 1
  }
  
  if(burn.q == 3){
    tot = sum(burn.score)
    if (tot <= 6)
      cat = 0
    if(tot >= 7 & tot <= 10)
      cat = 1
  }
  
  return(cat)
}

## Here we actually categorize the subjects 
p = nrow(dat) 

for(i in 1:p){
  dat$pa.b[i] = score(i, 1)
}

for(i in 1:p){
  dat$d.b[i] = score(i, 2)
}

for(i in 1:p){
  dat$ee.b[i] = score(i, 3)
}

dat[dat==''] = NA

dat$pa.b[dat$burnoutopt == 'No'] = 3
dat$d.b[dat$burnoutopt == 'No']  = 3
dat$ee.b[dat$burnoutopt == 'No'] = 3

## Data cleaning 
dat$pa.b = factor(dat$pa.b, levels = c(0,1,2,3), 
                  labels = c('Low', 'Medium', 'High', 'Missing'))
dat$d.b = factor(dat$d.b, levels = c(0,1,2,3), 
                  labels = c('Low', 'Medium', 'High', 'Missing'))
dat$ee.b = factor(dat$ee.b, levels = c(0,1,2,3), 
                  labels = c('Low', 'Medium', 'High', 'Missing'))

dat$care = ifelse(dat$dep == 'None', 0, 1)
dat$care = factor(dat$care, levels = c(0,1), 
                  labels = c('Not Caregiver', 'Caregiver'))

dat$genderidentity = factor(dat$genderidentity)
dat$raceethnic = factor(dat$raceethnic)
dat$marital = factor(dat$marital)
dat$degree = factor(dat$degree)
dat$specialty = factor(dat$specialty)
dat$track = factor(dat$track)
dat$rank = factor(dat$rank)
dat$leadership = factor(dat$leadership)
dat$compmodel = factor(dat$compmodel)

dat$race = ifelse(dat$raceethnic == 'White' | dat$race == 'Asian', 0, 2)
dat$race[dat$raceethnic == 'Asian'] = 1 
dat$race[dat$raceethnic == 'Prefer not to say' | dat$raceethnic == 'Not reported'| 
           dat$raceethnic == 'Unknown'] = 3
dat$race = factor(dat$race, levels = c(0,1,2,3), 
                  labels = c('White', 'Asian', 'Underrepresented', 'Missing'))

dat$award[392] = NA


vars = c('genderidentity', 'race', 'marital', 'years','rank', 
         'yearsinrank', 'leadership', 'leadernumber.entry', 'pub', 
         'author', 'fund', 'award', 'guest','compmodel')

tabUnmatched = CreateTableOne(vars = vars, strata = 'care', data = dat, test = FALSE,
                    smd = TRUE)
t1 = print(tabUnmatched, smd = TRUE, showAllLevels = TRUE, varLabels = TRUE)
kable(t1, format = 'latex', booktabs = T)%>%
  kable_styling(full_width = F, latex_options = c("striped", 'scale_down'))


dat$famworkconflict1 = factor(dat$famworkconflict1, levels = c('Very Strongly Disagree (1)',
                                                               'Strongly Disagree (2)',
                                                               'Disagree (3)',
                                                               'Neither Agree or Disagree (4)',
                                                               'N/A',
                                                               'Agree (5)',
                                                               'Strongly Agree (6)',
                                                               'Very Strongly Agree (7)'),
                              ordered = TRUE)

dat$famworkconflict2 = factor(dat$famworkconflict2, levels = c('Very Strongly Disagree (1)',
                                                               'Strongly Disagree (2)',
                                                               'Disagree (3)',
                                                               'Neither Agree or Disagree (4)',
                                                               'N/A',
                                                               'Agree (5)',
                                                               'Strongly Agree (6)',
                                                               'Very Strongly Agree (7)'),
                              ordered = TRUE)

dat$famworkconflict3 = factor(dat$famworkconflict3, levels = c('Very Strongly Disagree (1)',
                                                               'Strongly Disagree (2)',
                                                               'Disagree (3)',
                                                               'Neither Agree or Disagree (4)',
                                                               'N/A',
                                                               'Agree (5)',
                                                               'Strongly Agree (6)',
                                                               'Very Strongly Agree (7)'),
                              ordered = TRUE)

dat$famworkconflict4 = factor(dat$famworkconflict4, levels = c('Very Strongly Disagree (1)',
                                                               'Strongly Disagree (2)',
                                                               'Disagree (3)',
                                                               'Neither Agree or Disagree (4)',
                                                               'N/A',
                                                               'Agree (5)',
                                                               'Strongly Agree (6)',
                                                               'Very Strongly Agree (7)'),
                              ordered = TRUE)

dat$famworkconflict5 = factor(dat$famworkconflict5, levels = c('Very Strongly Disagree (1)',
                                                               'Strongly Disagree (2)',
                                                               'Disagree (3)',
                                                               'Neither Agree or Disagree (4)',
                                                               'N/A',
                                                               'Agree (5)',
                                                               'Strongly Agree (6)',
                                                               'Very Strongly Agree (7)'),
                              ordered = TRUE)

dat$famworkconflict6 = factor(dat$famworkconflict6, levels = c('Very Strongly Disagree (1)',
                                                               'Strongly Disagree (2)',
                                                               'Disagree (3)',
                                                               'Neither Agree or Disagree (4)',
                                                               'N/A',
                                                               'Agree (5)',
                                                               'Strongly Agree (6)',
                                                               'Very Strongly Agree (7)'),
                              ordered = TRUE)


## Here is a function that should calculate the FWC score for a given subject 
## in a data set
fwc.score = function(sub, dat){
  fwc.ind = c(102:107)
  n = length(fwc.ind)
  
  ## Just adding up scores
  fwc.score = numeric(n)
  for(i in 1:n){
    if(dat[sub, fwc.ind[i]] == 'Very Strongly Disagree (1)')
      fwc.score[i] = 1
    if(dat[sub, fwc.ind[i]] == 'Strongly Disagree (2)')
      fwc.score[i] = 2
    if(dat[sub, fwc.ind[i]] == 'Disagree (3)')
      fwc.score[i] = 3
    if(dat[sub, fwc.ind[i]] == 'Neither Agree or Disagree (4)')
      fwc.score[i] = 4
    if(dat[sub, fwc.ind[i]] == 'Agree (5)')
      fwc.score[i] = 5
    if(dat[sub, fwc.ind[i]] == 'Strongly Agree (6)')
      fwc.score[i] = 6
    if(dat[sub, fwc.ind[i]] == 'Very Strongly Agree (7)')
      fwc.score[i] = 7
    if(dat[sub, fwc.ind[i]] == 'N/A')
      fwc.score[i] = 500
  }
  
  tot = sum(fwc.score)
  if(tot > 500)
    tot = NA
  
  return(tot)
}

dat$famworkconflict1[is.na(dat$famworkconflict1)] = 'N/A'
dat$famworkconflict2[is.na(dat$famworkconflict2)] = 'N/A'
dat$famworkconflict3[is.na(dat$famworkconflict3)] = 'N/A'
dat$famworkconflict4[is.na(dat$famworkconflict4)] = 'N/A'
dat$famworkconflict5[is.na(dat$famworkconflict5)] = 'N/A'
dat$famworkconflict6[is.na(dat$famworkconflict6)] = 'N/A'

dat$fwc = 0
for(i in 1:p){
  dat$fwc[i] = fwc.score(i, dat)
}

## Pretty Plots :)
int.p = dat[!is.na(dat$fwc),]
interaction.plot(x.factor = int.p$genderidentity,
                 trace.factor = int.p$care,
                 response = int.p$fwc,
                 ylab = 'Family-Work Conflict Score (0-42)',
                 xlab = 'Gender',
                 ylim = c(0,30),
                 col = c("pink", "blue"),
                 lty = 1,
                 lwd = 2, 
                 trace.label = 'Caregiver Status')
a1 = aov(fwc ~ genderidentity*care, data = int.p)
summary(a1)


int.p = dat[!is.na(dat$leadernumber.entry),]
interaction.plot(x.factor = int.p$genderidentity,
                 trace.factor = int.p$care,
                 response = int.p$leadernumber.entry,
                 ylab = 'Number of Leadership Positions',
                 xlab = 'Gender',
                 ylim = c(0,4.5),
                 col = c("pink", "blue"),
                 lty = 1,
                 lwd = 2, 
                 trace.label = 'Caregiver Status')
a1 = aov(leadernumber.entry ~ genderidentity*care, data = int.p)
summary(a1)

int.p = dat[!is.na(dat$pub),]
interaction.plot(x.factor = int.p$genderidentity,
                 trace.factor = int.p$care,
                 response = int.p$pub,
                 ylab = 'Publications in the Last 5 Years',
                 xlab = 'Gender',
                 ylim = c(0,9),
                 col = c("pink", "blue"),
                 lty = 1,
                 lwd = 2, 
                 trace.label = 'Caregiver Status')
a1 = aov(pub ~ genderidentity*care, data = int.p)
summary(a1)

int.p = dat[!is.na(dat$author),]
interaction.plot(x.factor = int.p$genderidentity,
                 trace.factor = int.p$care,
                 response = int.p$author,
                 ylab = 'Publications as First/Last Author',
                 xlab = 'Gender',
                 ylim = c(0,9),
                 col = c("pink", "blue"),
                 lty = 1,
                 lwd = 2, 
                 trace.label = 'Caregiver Status')
a1 = aov(author ~ genderidentity*care, data = int.p)
summary(a1)

int.p = dat[!is.na(dat$fund),]
interaction.plot(x.factor = int.p$genderidentity,
                 trace.factor = int.p$care,
                 response = int.p$fund,
                 ylab = 'Funded Projects',
                 xlab = 'Gender',
                 ylim = c(0,16.5),
                 col = c("pink", "blue"),
                 lty = 1,
                 lwd = 2, 
                 trace.label = 'Caregiver Status')
a1 = aov(fund ~ genderidentity*care, data = int.p)
summary(a1)

int.p = dat[!is.na(dat$award),]
interaction.plot(x.factor = int.p$genderidentity,
                 trace.factor = int.p$care,
                 response = int.p$award,
                 ylab = 'Number of Awards',
                 xlab = 'Gender',
                 ylim = c(0,6.5),
                 col = c("pink", "blue"),
                 lty = 1,
                 lwd = 2, 
                 trace.label = 'Caregiver Status')
a1 = aov(award ~ genderidentity*care, data = int.p)
summary(a1)

int.p = dat[!is.na(dat$guest),]
interaction.plot(x.factor = int.p$genderidentity,
                 trace.factor = int.p$care,
                 response = int.p$guest,
                 ylab = 'Number of Guest Speakerships',
                 xlab = 'Gender',
                 ylim = c(0,50),
                 col = c("pink", "blue"),
                 lty = 1,
                 lwd = 2, 
                 trace.label = 'Caregiver Status')
a1 = aov(guest ~ genderidentity*care, data = int.p)
summary(a1)


## Test caregiver men vs caregiver women/noncaregiver men vs noncaregiver women
## Simple anova, not adjusted for other potential confounders
fwc.care = int.p[int.p$care == 'Caregiver',]
fwc.non = int.p[int.p$care == 'Not Caregiver',]




