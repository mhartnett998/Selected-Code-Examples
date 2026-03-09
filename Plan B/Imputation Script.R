#########################
## Load libraries from 'Plan B.R'

## Creating the needed variables
dat$relationship = ifelse(dat$marital == 'Married' | dat$marital == 'Domestic Partnership' | 
                            dat$marital == 'Long-term committed relationship', 1, 0)

dat$institution[is.na(dat$institution)] = 0
dat$institution = factor(dat$institution)

dat$research = ifelse(dat$researchactual >= 80, 0 , 1)
dat$research[dat$researchactual == 0] = 2
dat$research = factor(dat$research, levels = c(0,1,2), 
                      labels = c('Full Time', 'Part Time', 'No Research'))

dat$rank3 = ifelse(dat$rank == 'Associate Professor', 1, 2)
dat$rank3[dat$rank == 'Professor'] = 0
dat$rank3 = factor(dat$rank3, levels = c(0,1,2), 
                   labels = c('Professor', 'Associate Professor', 'Other'))

dat$tenure = ifelse(dat$track == 'Tenure Track', 1, 0)
dat$tenure = factor(dat$tenure, levels = c(0,1),
                    labels = c('Not Tenure Track', 'Tenure Track'))

dat$inpt1 = ifelse(dat$inpt > 0, 1, 0)
dat$inpt1 = factor(dat$inpt1, levels = c(0,1),
                   labels = c('No Inpatient Service', 'Some Inpatient Service'))

dat$out.g = 0 
dat$out.g[dat$out == '1-5 hours per week'] = 1
dat$out.g[dat$out == '6-10 hours per week'] = 2
dat$out.g[dat$out == '11-16 hours per week'] = 3
dat$out.g[dat$out == '17-21 hours per week'] = 4
dat$out.g[dat$out == '>22 hours per week'] = 5
dat$out.g=factor(dat$out.g, labels = c('0 hours per week',
                                       '1-5 hours per week',
                                       '6-10 hours per week',
                                       '11-16 hours per week',
                                       '17-21 hours per week',
                                       '>22 hours per week'))

dat$hour = 0 
dat$hour[dat$hours == '21-40 hours/week'] = 1
dat$hour[dat$hours == '41-60 hours/week'] = 2
dat$hour[dat$hours == '61-80 hours/week'] = 3
dat$hour[dat$hours == '>80 hours/week'] = 4
dat$hour=factor(dat$hour, labels = c('< 20 hours/week',
                                       '21-40 hours/week',
                                       '41-60 hours/week',
                                       '61-80 hours/week',
                                       '>80 hours/week'))

dat$pemploy = 0 
dat$pemploy[dat$partneremploy == 'Part-time'] = 1
dat$pemploy[dat$partneremploy == 'Not employed'] = 2
dat$pemploy[dat$partneremploy == 'Does not apply/prefer not to say'] = 3
dat$pemploy=factor(dat$pemploy, labels = c('Full-time',
                                     'Part-time',
                                     'Not employed',
                                     'Does not apply/prefer not to say'))

dat$genderidentity = factor(dat$genderidentity)
dat$raceethnic = factor(dat$raceethnic)
dat$marital = factor(dat$marital)
dat$degree = factor(dat$degree)
dat$specialty = factor(dat$specialty)
dat$track = factor(dat$track)
dat$rank = factor(dat$rank)
dat$leadership = factor(dat$leadership)
dat$compmodel = factor(dat$compmodel)
dat$wage = factor(dat$wage)

dat$race = ifelse(dat$raceethnic == 'White' | dat$race == 'Asian', 0, 2)
dat$race[dat$raceethnic == 'Asian'] = 1 
dat$race[dat$raceethnic == 'Prefer not to say' | dat$raceethnic == 'Not reported'| 
           dat$raceethnic == 'Unknown'] = 3
dat$race = factor(dat$race, levels = c(0,1,2,3), 
                  labels = c('White', 'Asian', 'Underrepresented', 'Missing'))

dat$care = ifelse(dat$dep == 'None', 0, 1)
dat$care = factor(dat$care, levels = c(0,1), 
                  labels = c('Not Caregiver', 'Caregiver'))

dat$award[392] = NA



## MICE Stuff
vars = c('genderidentity', 'race', 'marital', 'years', 'rank', 'yearsinrank', 
         'leadership', 'leadernumber.entry', 'pub', 'author', 'guest', 'compmodel')

mice = dat %>% select(vars)

## IPW Stuff first
w1 = ipwpoint(exposure = care, family = 'binomial', link = 'logit', 
              numerator = ~ 1, denominator = ~ genderidentity + race + marital +
                years + rank + yearsinrank + leadership + leadernumber.entry + 
                pub + author + guest + compmodel, data = dat)
