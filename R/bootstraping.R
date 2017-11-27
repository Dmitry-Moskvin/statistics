#### Bootstrapping a Single Statistic (k=1) ####
# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=mtcars, statistic=rsq, 
                R=1000, formula=mpg~wt+disp)

# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")

#### Bootstrapping several Statistics (k>1) ####
# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=mtcars, statistic=bs, 
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 

# get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # wt 
boot.ci(results, type="bca", index=3) # disp

#### Bootstrapping examples with custom functions ####
data(Lock5Data::CommuteAtlanta)
time.mean = with(CommuteAtlanta, mean(Time))
time.mean
B = 1000 # count of bootstraps
n = nrow(CommuteAtlanta)
boot.samples = matrix(sample(CommuteAtlanta$Time, size = B * n, replace = TRUE), B, n)
boot.statistics = apply(boot.samples, 1, mean)

require(ggplot2)
ggplot(data.frame(meanTime = boot.statistics),aes(x=meanTime)) +
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red")

time.se = sd(boot.statistics)
me = ceiling(10 * 2 * time.se)/10
round(time.mean, 1) + c(-1, 1) * me

boot.mean = function(x,B,binwidth=NULL) {
  n = length(x)
  boot.samples = matrix( sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  require(ggplot2)
  if ( is.null(binwidth) )
    binwidth = diff(range(boot.statistics))/30
  p = ggplot(data.frame(x=boot.statistics),aes(x=x)) +
    geom_histogram(aes(y=..density..),binwidth=binwidth) + geom_density(color="red")
  plot(p)
  interval = mean(x) + c(-1,1)*2*se
  print( interval )
  return( list(boot.statistics = boot.statistics, interval=interval, se=se, plot=p) )
}

out = with(CommuteAtlanta, boot.mean(Time, B = 10000))

## Confident level of proportion "1" element ##
boot.mean(c(rep(1, 100), rep(0, 900)), 1000,binwidth = 0.005)

## Non-parametric diff in means ##
data(StudentSurvey)
with(StudentSurvey, summary(Exercise))
with(StudentSurvey, summary(Gender))
with(StudentSurvey, by(Exercise, Gender, mean, na.rm = TRUE))

newStudent = with(StudentSurvey, StudentSurvey[!is.na(Exercise), ])
ggplot(newStudent, aes(x=Gender,y=Exercise)) +
  geom_boxplot(color="red",outlier.colour="red") +
  geom_point(position = position_jitter(h=0,w=0.3)) +
  ylab('# of hours exercise per week') +
  coord_flip()

n = with(newStudent, by(Exercise, Gender, length))
B = 1000
female.samples = with(newStudent, matrix(sample(Exercise[Gender == "F"], size = n[1] *
                                                  B, replace = TRUE), B, n[1]))
male.samples = with(newStudent, matrix(sample(Exercise[Gender == "M"], size = n[2] *
                                                B, replace = TRUE), B, n[2]))
female.means = apply(female.samples, 1, mean)
male.means = apply(male.samples, 1, mean)
boot.stat = male.means - female.means
ggplot(data.frame(x = boot.stat), aes(x = x)) + geom_density()

xbars = with(newStudent, by(Exercise, Gender, mean))
me = 2 * sd(boot.stat)
(xbars[2] - xbars[1]) + c(-1, 1) * me


## boot package ##
library(boot)
data(CommuteAtlanta)
my.mean = function(x, indices) {
  return( mean( x[indices] ) )
}
time.boot = boot(CommuteAtlanta$Time, my.mean, 10000)
boot.ci(time.boot)