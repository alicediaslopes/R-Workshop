####################################################################################
##                              4.Data Analysis                                  ##
####################################################################################

# R Basics ---------------------------------------------------------------------
snp <- read.csv("data/snp.csv")
names(snp) # names variables in the data frame.
search() # shows what is in the current R environment.
attach(snp) # attach the dataset to the R environment. once you have attached the data in the environment, you can just use variables
# within your data frame like any other R objects - just calling them by their name. Before attaching the data, you needed 
# to type snp$name. However, if you are working with several data frames with the same variables names, attach may cause 
# you a lot of problems.
search() # you will see that the data frame will be in your R environment.      
str(snp) # overview of the data frame
summary(snp) # for factors R presents the number of apparences for each level.
# for numerics R presents the minimum, 1st quantile, median, mean, 3rd quartile and maximum. 
head(snp) # top six rows of the data.
tail(snp) # bottom six rows of the data.
detach(snp) # detach the data frame.
# rm(list = ls()) # remove all objects from environment.

# Stats Basics ---------------------------------------------------------------------
### mean
sum(snp$likes_count_fb)/length(snp$likes_count_fb) # caculate the sum of the variable and dived it by the length.
mean(snp$likes_count_fb)
### median
length(snp$likes_count_fb)/2 # calculate the length of the variable and dived by two.
sort(snp$likes_count_fb)[866] # sort the variable for likes count on Facebook and returns the 866 value.
median(snp$likes_count_fb)
### mode
mode(snp$likes_count_fb) # this command will not give you the mode!
# returns a description of the "type" of a certain object.
# estimating the mode for a continuos variable is not impossible, but quite technical. there is a quite an easy way to
# get approximate mode. let's draw a histogram for the variable number of likes on Facebook. we will use the base R 
# code to draw the graph, but you can easily do it using ggplot2.
# there is a really cool discussion online about when to use R base code to draw graphs:
# Why I don't use ggplot2, Professor Jeff Leek: https://simplystatistics.org/2016/02/11/why-i-dont-use-ggplot2/
# Why I use ggplot2, David Robinson (Data Camp): http://varianceexplained.org/r/why-I-use-ggplot2/
hist(snp$likes_count_fb) # histogram for variable.
par(mfrow = c(1,3)) # presents the graphs 1 row and 3 columns.
hist(snp$likes_count_fb, breaks = 10) # the argument breaks sets the number of bins.
hist(snp$likes_count_fb, breaks = 60)
hist(snp$likes_count_fb, breaks = 80)
par(mfrow = c(1,1)) # present the graph in 1 row and 1 columns (restoring the default)
# to calculate the mode, you can download the package "modeest"
install.packages("devtools")
devtools::install_github("paulponcet/modeest")
library(modeest) # this package is tricky to use because there are many ways of estimating the mode.
library(help = "modeest") # open the documentation for the package. we want to calculate the most likely value 'mlv'
mlv(snp$likes_count_fb)
# if the package modeest doesn't work, you can create a function to calculate the mode.
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(snp$likes_count_fb)
### variance and standard deviation
var(snp$likes_count_fb)
sd(snp$likes_count_fb)
### quantile
quantile(snp$likes_count_fb)
### summary function
summary(snp$likes_count_fb)
### z tranformation 
zstand_likes_count_fb <- scale(snp$likes_count_fb)
head(zstand_likes_count_fb)
snp$zstand_likes_count_fb <- scale(snp$likes_count_fb) # include the standardised values in your data frame
hist(snp$likes_count_fb)
### tables
table1 <- table(snp$type, snp$sentiment) # type will be rows, sentiment will be columns 
table1 # print table 
# proportion
prop.table(table1) # cell percentages
prop.table(table1, 1) # row percentages 
prop.table(table1, 2) # column percentages
# percentage
100*prop.table(table1,1)
100*prop.table(table1,2)
# rounding by 2 decimal places
round(100*prop.table(table1,1),2)
round(100*prop.table(table1,2),2)

# LMs ---------------------------------------------------------------------

# T-TEST: 
# the function for both one- and two-sample t test is t.test.
### one-sample t-test: we just need to specify the variable of interest and then we need to specify the baseline for comparison.
# let's pretend that 150 is the mean number of likes per post on Facebook for all the political parties in the UK. 
# we want to test if the mean number of likes per post on Facebook on the SNP page is the different for the mean for 
# all political parities in the UK.
# H0: SNP has the same mean number of likes per post on Facebook as the population mean (other political parties).
# H1: SNP has differnt mean number of likes per post on Facebook.
t.test(snp$likes_count_fb, mu = 150) # p < 0.05: we can reject the null hypothesis.

### two-sample t-test: we want to compare the difference in the mean of two different groups.
# let's compare the mean number of likes per post in posts that contain a link vs posts that contain photo and video
# first, we will need to recode the variable for type of post into a dummy for link (link = 1, video & photo = 0)
summary(snp$type)
library(plyr)
snp$link <- revalue(snp$type, c("link"= 1, "video"=0, "photo"=0))
summary(snp$link)
# t-test
ttest <- t.test(snp$likes_count_fb ~ snp$link)
ttest

# ANOVA
# used to compare the difference between three or more groups. It tells if there is a significant difference, but we are not sure 
# which specific groups are different from each other. We can see if there is a different in the number of likes by type of post using
# anova:
anova <- aov(snp$likes_count_fb ~ snp$type)
summary(anova)
# ANOVA = LM, for that reason, people don't use ANOVA anymore.

# REGRESSION:
# let's perform a regression of number of likes on Facebook post by comments and sentiment. 

# DEPENDENT VARIABLE
# first, let's check the distribution of the dependent variable (you can also try to write the ggplot command):
hist(snp$likes_count_fb) # the variable is not normally distributed.
summary(snp$likes_count_fb)
# logarithm transformation is often used for right skewed data (mean > median).
# you can save the transformed variable in your data frame:
snplikes_count_fb2 <- log10(snp$likes_count_fb)             
# or you can simply include the log10 transformation when you are analysis the variable, for example:
library(ggplot2)
ggplot(snp, aes(x=log10(likes_count_fb)))+
  geom_histogram()+
  theme_bw()

# INDEPENDENT VARIABLES
# (1) number of comments
summary(snp_lm$comments_count_fb)
# we are also going to use logarithm transformation on this variable.
# because our variable includes 0, I will exclude those cases to avoid problems (there are more complex 
# and better ways to deal with this problem, but this is beyond the purpose of this workshop.)
snp_lm <- snp[which(snp$comments_count_fb !=0),] # creating a new data frame with no 0 values for the dependent variable.
                                                 # we will use this data frame to run the regression analysis.
                                                 # sample size drops from 1732 to 1724.
hist(snp_lm$comments_count_fb) # checking the distribution for the independent variable
hist(log10(snp_lm$comments_count_fb)) # checking the transformation for the independent variable
# (2) sentiment
summary(snp_lm$sentiment) # because the variable is a factor, R will automatically create dummies when you run the regression.
# I usually create dummies (it is easier to change reference group, combine categories, etc.) using the 
# plyr package.
snp_lm$negative <- revalue(snp_lm$sentiment, c("negative"= 1, "neutral"=0, "positive"=0))
snp_lm$neutral <- revalue(snp_lm$sentiment, c("negative"= 0, "neutral"=1, "positive"=0))
snp_lm$positive <- revalue(snp_lm$sentiment, c("negative"= 0, "neutral"=0, "positive"=1))
table(snp_lm$negative,snp_lm$sentiment) # checking the recoding is correct
table(snp_lm$neutral,snp_lm$sentiment)
table(snp_lm$positive,snp_lm$sentiment)

#### MODEL 1:
# scatter plot with the regression line using ggplot2
library(ggplot2)
ggplot(snp_lm, aes(x=log10(comments_count_fb), y=log10(likes_count_fb)))+
  geom_point()+
  geom_smooth(method='lm', se = FALSE) +
  theme_bw()
# regression model
lm1 <- lm(log10(likes_count_fb) ~ log10(comments_count_fb), data = snp_lm)
summary(lm1)
# checking the residuals
plot(lm1)
# if you are interest in using R to perform robust regression, you can check the step by step on the UCLA website:
# https://stats.idre.ucla.edu/r/dae/robust-regression/
# you can also use ggplot2 to visualise your residuals: https://www.r-bloggers.com/visualising-residuals/

#### MODEL 2:
lm2 <- lm(log10(likes_count_fb) ~ log10(comments_count_fb) + neutral + positive, data = snp_lm)
summary(lm2)

# presenting the two models.
install.packages("stargazer")
library(stargazer)
stargazer(lm1, lm2,
          title = "Regression on number of likes on Facebook by comments and sentiment",
          dep.var.labels=c("Log 10 of number of likes on Facebook"),
          covariate.labels=c("log 10 of number of comments","neutral", "positive", "constant"),
          align = TRUE, type = "text", out = "ols_likes_fb.txt")

# GLMs ---------------------------------------------------------------------

# X-SQUARE TEST: 
# difference between the observed cell values and the expected cell values.
table1 <- table(snp$type, snp$sentiment) # table between the two categorical variables in the data frame.
table1
x_square <- chisq.test(table1)
x_square
# you can use the package "gmodels" to calculate the expected values if there was no relatioship between the variables.
install.packages("gmodels")
library(gmodels)
# check the documentation for command CrossTable
?CrossTable
CrossTable(table1, expected = TRUE, prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE)

# LOGISTIC REGRESSION
# let's perfom a logistic regression on negative posts by type of post and number of likes.
# we will use the glm function: glm(formula, family=familytype(link=linkfunction), data=)

# Family	Default / Link Function
# binomial /	(link = "logit")
# # binomial /	(link = "probit")
# gaussian	/ (link = "identity")
# Gamma / (link = "inverse")
# inverse.gaussian	/ (link = "1/mu^2")
# poisson /	(link = "log")
# quasibinomial /	(link = "logit")
# quasi /	(link = "identity", variance = "constant")
# quasipoisson	/ (link = "log")

# DEPENDENT VARIABLE
# first, we need to create a binary variable for negative comments.
snp$negative <- revalue(snp$sentiment, c("negative"= 1, "neutral"=0, "positive"=0))

# INDEPENDENT VARIABLE
# (1) type of post
summary(snp$type)
# (2) number of likes
summary(snp$likes_count_fb)
hist(snp$likes_count_fb)
hist(log10(snp$likes_count_fb))
snp$likes_count_fb2 <- log10(snp$likes_count_fb) # we will need to create a new variable for the 
                                                 # log 10 of # of likes to calculate the predicted
                                                 # probability. (you will understand why below)

#### MODEL1: using link as the reference group:
glm1 <- glm(negative ~ type, family=binomial(link='logit'), data=snp) 
summary(glm1) 
exp(coef(glm1))

#### MODEL2:
glm2 <- glm(negative ~ type + likes_count_fb2, family=binomial(link='logit'), data=snp) 
summary(glm2) 
exp(coef(glm2))

# predicted probabiltiies: 
# the codes below are taken from: https://stats.idre.ucla.edu/r/dae/logit-regression/
# we recomend checking the website if you want to calculate predicted probabilities using your data. 
# 
# (1) we will start by calculating the predicted probability of having more than average comments on 
# the post for each type of post holding likes on the post at the mean.
# first, we create the data frame to calculate the predicted probability. the names of the variables in the new
# data frame must have the same names as the variables in your logistic regression above 
# (e.g. in this example the mean for the log10 "likes_count_fb2" must be named "likes_count_fb2": that is why you
# need to create a new variable for the log10).
data.pred <- with(snp, data.frame(likes_count_fb2 = mean(likes_count_fb2), type = c("video", "link","photo"))) 
data.pred  
# now that we have the data frame we want to use to calculate the predicted probabilities, we can tell R to 
# create the predicted probabilities. 
data.pred$pred <- predict(glm2, newdata = data.pred , type = "response")
# (a) the predict_glm2$pred tells R that we want to create a new variable in the dataset (data frame) predict_glm2 called "pred"
# (b) the rest of the command tells R that the values of "pred" should be predictions made using the predict( ) function
# (c) the options within the parentheses tell R that the predictions should be based on the analysis glm2 with values of the predictor 
# variables coming from predict_glm2
data.pred 
# the predicted probability of having more than average comments on the post is 0.52 for posts that contains a video.

# plotting the predicted probability:
# we are going to plot the predicted probability of having negative comments by number of likes and
# type of post, so we will create 50 values for the likes between 0.6021 and 4.4158, at each value of type (3).
# you can play with those values when plotting your own data.
data.pred2 <- with(snp, data.frame(likes_count_fb2 = rep(seq(from = 0.6021, to = 4.4158, length.out = 50), 3), type = c("video", "link","photo")))
data.pred2
data.pred2$pred2 <- predict(glm2, newdata = data.pred2 , type = "response")
head(data.pred2)
# code for the graph
library(ggplot2)
ggplot(data.pred2, aes(x = likes_count_fb2, y = pred2)) + 
  geom_line(aes(colour = type)) +
  theme_bw()
