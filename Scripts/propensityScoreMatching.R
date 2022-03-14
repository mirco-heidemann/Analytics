## -------------------------------------------------------------------------
## Exercise to "Propensity Score Matching"
##
## The R-script follows the paper "A Step-by-Step Guide to 
## Propensity Score Matching in R"
##
## Dataset can be downloaded from http://justusrandolph.net/psm/newyork.csv
## (10/2015)
##
## Mirco Heidemann, 10/2015
## -------------------------------------------------------------------------

## The PC-Matching requires the "MatchIt" package:
require (MatchIt)

## Set workdirectory (to change on other computers..)
setwd('I:/R/Propensity Score/')

# ## read data as csv-file
# dat <- read.csv('pcm.exercise.newyork.csv')
## ... or directly from the web:
dat <- read.csv('http://justusrandolph.net/psm/newyork.csv', sep=',', header=T)

## Check data
head(dat)
summary(dat)

## Dataset:
## -------------
## Data from an observational study are used to illustrate propensity
## score matching. In the study, publicly available school-level data from
## several states are used to investigate whether an intervention (here,
## being designated a "Schools to Watch school") was a predictor of success
## in reading and mathematics achievement, when controlling for school size
## (tot), percentage of minority students (min), and percentage of students
## receiving free and reduced lunch (dis). For the example here, only data
## from the state of New York are used. In the New York data set, there were
## 25 stw schools and 560 non-stw schools. As matching variables, school
## size, percentage of minority students, and percentage of students
## receiving free and reduced lunch are chosen. The rationale for choosing
## those matching variables was that previous research had shown that they
## tended to covary with academic achievement. By matching on those
## variables, the goal was to reduce selection bias between "treated"
## (stw) and "control" (non-stw) schools.

## Grouping variable (treatment or control group):
## stw:   It specifies whether a particular case (a school) has been  
##        designated as a "Schools to Watch school" (1) or not (0). 

## Matching variables:
## tot:   school size
## min:   percentage of minority students in the school
## dis:   percentage of students receiving free and reduced lunch
## -------------

## Eliminate any missing data...
ind <- which(is.na(data))
if (length(ind) > 0) {
  dat <- dat[-ind,]}

## Perform the ps-Matching.
## In these case, the "nearest neighbor methode" was chosen. In general,
## the methode that results in the lowest mean differences between the
## groups (matched control and treatment) is the most appropriate.
m.nearest = matchit(stw ~ tot + min + dis,
                data = dat, method = "nearest",
                ratio = 1)
summary(m.nearest)

## Distribution of Propensity Scores:
plot(m.nearest, type = "jitter")

## Histograms before (left) and after matching (right).
## Look the treated and control groud similar after matching?
plot(m.nearest, type = "hist")

## Matched data for follow up statistical analyses containing only
## the matched control and treatment cases.
dat.nearest <- match.data(m.nearest)

# ## Export data as a CSV File for use in Excel:
# write.csv2(dat.nearest, 'psm.nearest.newyork.csv')
