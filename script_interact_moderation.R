library(tidyverse)
library(apaTables)
my_data <- read.csv(file="lectureData6060.csv",header=TRUE,sep=",",na.strings=c("NA"))
glimpse(my_data) # determine that there are 3 variables 
analytic.data <- my_data

## 

analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(Anxiety,center=T,scale=F)) )
#Scale the anxiety column, center it (T=yes), scale=F means don't change SD   
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(Preparation,center=T,scale=F)) )
#Scale the Prep column - center it and do not change SDs 

interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered),
                              data=analytic.data, na.action=na.exclude) # only works on two lines 
##na.action = na.exclude - means if any row has any missing value, drop the row !! because it messes up product terms 

apa.reg.table(interaction.regression)

####
#Another approach - how most faculty is trained
####

block1 <- lm(Exam~x.centered + z.centered,data=analytic.data, na.action=na.exclude)
block2 <- lm(Exam~x.centered + z.centered + I(x.centered*z.centered), data=analytic.data, na.action=na.exclude)

#as long as there is a 1-variable difference between block 1 and 2, the delta R2 will be exactly the same as sr2 of interaction

##Getting the line on the surface at +1SD
sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data.plus1SD <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z) # the last part (- sd.z would be + sd.z if you centered 0 at -1SD)
# z centered at plus 1SD = 
## reposition z scores so that 0 is at 1SD by taking all scores and subtracting 1SD 
simple.slope.plus.1SD <- lm(Exam~x.centered+z.centered.at.plus.1SD + I(x.centered*z.centered.at.plus.1SD), data=analytic.data.plus1SD, na.action=na.exclude) 
summary(simple.slope.plus.1SD) # DIFFERENT LINE 
apa.reg.table(simple.slope.plus.1SD)
# because of centering you can IGNORE the loweest 2 lines/rows bc can plug 0 into these 
# just look at (interept) and x.centered (b) values 
# 

#Getting line on surface at -1SD
analytic.data.minusSD <- analytic.data %>% mutate(z.centered.at.minus.1SD = z.centered + sd.z) 
simple.slope.minus.1SD <- lm(Exam~x.centered+z.centered.at.minus.1SD + I(x.centered*z.centered.at.minus.1SD), data=analytic.data.minusSD, na.action=na.exclude) 
summary(simple.slope.minus.1SD) # DIFFERENT LINE 
apa.reg.table(simple.slope.minus.1SD)
# IGNORE THE 2 LOWEST LINES (the Z LINES)
