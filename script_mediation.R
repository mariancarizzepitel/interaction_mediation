library(tidyverse)
library(apaTables)

mediation.data <- analytic.data %>% select (Exam, Preparation, Anxiety) # order of Y, X, Mediator
psych::mediate(y = 1, x = 2, m = 3, data = mediation.data) #1=first column, 2=second, 3=third
# 'ab' effect estimates - 0 refers to correlation 
# ab = c1 - c OR a*b 
# non signficant mediation b/c CI includes 0 value
# but it snugs up close to 0 .. so really, is it valuable? 
# true way to look at mediation REQUIRES lab manipulation 

