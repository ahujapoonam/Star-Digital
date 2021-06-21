#Star Digital Assignment
#Poonam Ahuja, Sammi Chang, Isha Chaudhari, Sujaya Darke, Song Han

setwd("/Users/isha_/Desktop")

star <- read.csv("star_digital.csv",stringsAsFactors=FALSE)

#separate into test and control 
star_test = subset(star, star$test == 1) 
star_control = subset(star, star$test ==0) 

#conduct t-test
t_test = t.test(star_test$purchase,star_control$purchase) 
t_test 

#two sample t-test to test for means, Null Hyp is means are equal. since p-value 
#is .06, this is not significant under an alpha level of 0.05. So, we fail 
#to reject the null hypothesis, and we say that the means are equal. 

#Question 1
#logistic regression to see if there is a relationship between test and purchase 
regression = glm(purchase ~ test, family = binomial(), data = star) 
summary(regression) 
exp(coef(regression)

#Question2
#calculate total impressions 
star$Timp = rowSums(star[,4:9]) 

#make a column that only does rowSums if it is test/treatment group 
star$Timp_t = star$test*star$Timp 

#run regression for frequency effect
regression2 = glm(purchase ~ Timp_t + Timp + test , family = binomial(), data = star) 
summary(regression2) 

exp(coef(regression2))

#Question 3
#calculate total impressions for sites 1-5 
star$Imp15 = rowSums(star[,4:8])

regression3 = glm(purchase ~ Imp15 + imp_6 + Imp15*test + imp_6*test , family = binomial(), data = star) 
summary(regression3)

exp(coef(regression3))

#q4
star$offset <- 6.48
glm(purchase ~ imp_6 , offset = offset, data = star, family = 'binomial') 

glm(purchase ~ Imp15 , offset = offset, data = star, family = 'binomial') 


