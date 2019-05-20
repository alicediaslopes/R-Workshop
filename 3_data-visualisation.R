# ---
# title: Data Visualisation using ggplot2
# author: Justin Ho
# last updated: 19/05/2019
# ---

####################################################################################
## Loading dataset into R and basic exploration                                   ##
####################################################################################

# The following code will load a dataset into R's memory and stored an object named 'snp'.
snp <- read.csv("data/snp.csv", stringsAsFactors = FALSE) # Load the file 'snp.csv', put it as an object called 'snp'

# Let's conduct some basic exploration
# The 'head()' function will return the first sixth element of an object:
head(snp)

# find out the number of rows and number of columns
dim(snp) 

# structure of the object and information about the class, length and content of each column
str(snp) 
# summary statistics for each column
summary(snp)

# change the variables to the right format
snp$date <- as.Date(snp$date)
snp$type <- as.factor(snp$type)
snp$sentiment <- as.factor(snp$sentiment)

# try again
summary(snp)

# We could use functions to gain information from the vector
# For example, 'mean()' will gives us the mean, 'max()' will give us the maximun value
max(snp$likes_count_fb)
mean(snp$likes_count_fb)

# We could use 'table()' to get frequency table
table(snp$type)
prop.table(table(snp$type)) # To show in precentage instead of count.

# For more advanced usage of functions, we could use 'which.max()' 
# to identify the element that contains the maximum value
which.max(snp$likes_count_fb)

# The result is 675, meaning that the 675th post of the dataset has the most likes.
# We can then use the slicing operator ('[' and ']') to find the post:
snp[675, ]

# We can combine them into one single line:
snp[which.max(snp$likes_count_fb), ]

########## Exercise ########## 
# Find out which post has most comments and shares

# Tricky Questions: Could you find out how to just extract the link?
# Tips: start with extracting all the links ('snp$post_link'),
# then use the index to select the one we want
##############################

####################################################################################
## Data visualisation with ggplot2                                                ##
####################################################################################
# We start by loading the required package.
# install.packages("ggplot2") # You only have to run it once
library(ggplot2)

# Building plots with **`ggplot2`** is typically an iterative process. 
# We start by defining the dataset we'll use:
ggplot(data = snp)

# lay out the axis(es), for example we can set up the axis for the Comments count ('comments_count_fb'):
ggplot(data = snp, aes(x = comments_count_fb)) 

# and finally choose a geom
ggplot(data = snp, aes(x = comments_count_fb)) +
  geom_histogram()

# We could change the binwidth by adding arguments
ggplot(data = snp, aes(x = comments_count_fb)) +
  geom_histogram(binwidth = 100)

# We could add a line showing the mean value by adding a geom
# First we calculate the mean values:
mean_like <- mean(snp$likes_count_fb)
mean_comment <- mean(snp$comments_count_fb)
mean_share <- mean(snp$shares_count_fb)

ggplot(data = snp, aes(x = comments_count_fb)) +
  geom_histogram(binwidth = 100) +
  geom_vline(xintercept=mean_comment, color = "red", linetype = "dashed")

# We could change color by adding arguments (fill means color of the filling in ggplot2)
ggplot(data = snp, aes(x = comments_count_fb)) +
  geom_histogram(binwidth = 100, fill = "red")

# How about making it transparent?
ggplot(data = snp, aes(x = comments_count_fb)) +
  geom_histogram(binwidth = 100, fill = "red", alpha = 0.5)

# Instead of colouring everything with the same colour, we could colour them by type 
# since we are using a variable in the dataframe, we have to put it inside aes()
ggplot(data = snp, aes(x = comments_count_fb, fill = type)) +
  geom_histogram(binwidth = 100, alpha = 0.5)

# Remember aesthetic mappings can also be set in individual layers
# In this case the 'fill' will only apply to geom_histogram (even if there are more geoms)
ggplot(data = snp, aes(x = comments_count_fb)) +
  geom_histogram(aes(fill = type), binwidth = 100, alpha = 0.5)

########## Exercise ########## 
# Using the codes above, create a histogram for Likes count ('likes_count_fb')
##############################


####################################################################################
## Visualising categorical variables                                              ##
####################################################################################
# Barplots are also useful for visualizing categorical data. By default,
# `geom_bar` accepts a variable for x, and plots the number of instances each
# value of x (in this case, post type) appears in the dataset.

# We could create a bar plot:
ggplot(snp, aes(x = type)) + 
  geom_bar()

# We can use the `fill` aesthetic for the `geom_bar()` geom to color bars by
# the portion of sentiment of each post type.
ggplot(snp, aes(x = type, fill = sentiment)) + 
  geom_bar()

# We can change how these bars are placed, 
# for example 'position = "dodge"' will place them side by side
ggplot(snp, aes(x = type, fill = sentiment)) + 
  geom_bar(position = "dodge")

# If we use 'position = "fill"', all bar will strech out to fill the whole y axis
# The y axis then become proportion
ggplot(snp, aes(x = type, fill = sentiment)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion")

####################################################################################
## Visualising continuous and categorical variable                                ##
####################################################################################
# We can use boxplots to visualize the comment count for each post type:
# Remember the continous variable always go to y
ggplot(snp, aes(x = type, y = comments_count_fb)) +
  geom_boxplot()

# We could also add points to a boxplot to have a better idea of the number of
# measurements and of their distribution:
ggplot(snp, aes(x = type, y = comments_count_fb)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

########## Exercise ########## 
# 1. Create a boxplot for `valence` for each post type. Overlay the boxplot
# layer on a jitter layer to show actual measurements (the ordering matters!).
# - Color the data points on your jitter layer by sentiment (`sentiment`).
# Remember you can add aes() inside a geom_*
# 2. Boxplots are useful summaries, but hide the *shape* of the distribution. For
# example, if the distribution is bimodal, we would not see it in a
# boxplot. An alternative to the boxplot is the violin plot, where the shape
# (of the density of points) is drawn.
# - Replace the box plot with a violin plot; see `geom_violin()`.
############################## 


####################################################################################
## Visualising two continuous variables                                           ##
####################################################################################

# Creating a plot for two continuous variables (comments and likes):
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb))

# We could use geom_point() for scatter plot
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb)) +
  geom_point()

# Again, you can add the mean values 
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb)) +
  geom_point() +
  geom_vline(xintercept = mean_comment) +
  geom_hline(yintercept = mean_like)

# You can even add the same geom twice, with different values on the x and y axes
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb)) +
  geom_point() +
  geom_point(aes(x = mean_comment, y = mean_like), color = "red", size = 6)

# Use the argument 'color = "red"' for red dots
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb)) +
  geom_point(color = "red")

# Or coloring them by post type, we have to put it inside aes() since we are invoking a variable
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb, color = type)) +
  geom_point() 

# There are many things you can do by adding layers into the ggplot
# You could log them.
# PS: You will see a warning message, don't worry, it is just because our data contain zeros
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb, color = type)) +
  geom_point(alpha = 0.5) + # I made the points transparent for visiblity
  scale_x_log10() +
  scale_y_log10()

# Or fit a line
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb, color = type)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE)

# And add labels and legend
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb, color = type)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Comments Count", y = "Likes Count",
     title = "Comments and Likes",
     subtitle = "One post per dot")

# Change the theme by adding theme_bw()
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb, color = type)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Comments Count", y = "Likes Count",
       title = "Comments and Likes",
       subtitle = "One post per dot") + 
  theme_bw()

# To save your graph, you could first define the graph as an object then use ggsave:
myplot <- ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb, color = type)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Comments Count", y = "Likes Count",
       title = "Comments and Likes",
       subtitle = "One post per dot")
ggsave(myplot, filename = "my_plot.png")

# Facet to make small multiples
ggplot(data = snp, aes(x = comments_count_fb, y = likes_count_fb)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  facet_wrap(~ type)


########## Exercise ########## 
# Make a scatter plot of shares by comments count, log both axes,
# color them by post type, change shape by post type (adding 'shape = type ' in aes())
##############################


####################################################################################
## It's about time                                                                ##
####################################################################################
# We need this package
# install.packages("scales")
# install.packages("lubridate")
library(scales)
library(lubridate)

# To plot a time series, the first thing you have to do is to transform your data into time/date format
# There are many formats for time and date, but the simpliest way would probably be:

# Simply plot the same scatter point, using 'date' as the x axis
ggplot(data = snp, aes(x = date, y = likes_count_fb)) +
  geom_point()

# Changing the x scale using scale_x_date()
ggplot(data = snp, aes(x = date, y = likes_count_fb)) +
  geom_point() +
  scale_x_date(labels = date_format("%m/%y"), date_breaks = "1 month")

# We could use line
ggplot(data = snp, aes(x = date, y = likes_count_fb, color = type)) +
  geom_line() +
  scale_x_date(labels = date_format("%m/%y"), date_breaks = "1 month")

# Or use geom_smooth to fit a local regression line
ggplot(data = snp, aes(x = date, y = likes_count_fb, color = type)) +
  geom_smooth(method = 'loess') +
  scale_x_date(labels = date_format("%m/%y"), date_breaks = "1 month")

# You could add two geoms on top of each other
ggplot(data = snp, aes(x = date, y = likes_count_fb, color = type)) +
  geom_smooth(method = 'loess') +
  geom_point(alpha = 0.3) +
  scale_x_date(labels = date_format("%m/%y"), date_breaks = "1 month")

# You could also truncate the y axis (use with caution!)
ggplot(data = snp, aes(x = date, y = likes_count_fb, color = type)) +
  geom_smooth(method = 'loess') +
  geom_point(alpha = 0.3) +
  scale_x_date(labels = date_format("%m/%y"), date_breaks = "1 month") +
  ylim(c(0, 2000))


####################################################################################
## Data Wrangling with dplyr                                                      ##
####################################################################################
library(dplyr)
library(lubridate)

# For some plots, it might be easier if you transform the data in advance
# We need the following functions:
# group_by() is a function to split the data into groups
# summarise() is a function to make calculation and put the result into a new variable

plot_data <- snp %>% # Take 'snp', put into a pipe
  group_by(type, date=floor_date(date, "month")) %>% # split the data by post type and by month
  summarise(total_likes = sum(likes_count_fb)) # calculate total likes by taking the sum of likes count

# Have a look at the transformed data
ggplot(data = plot_data, aes(x = date, y = total_likes, color = type)) +
  geom_line()

# We could use a new geom, geom_area for area plots
# There are three positions, "stack" means stacking one on top of the other
ggplot(data = plot_data, aes(x = date, y = total_likes, fill = type)) +
  geom_area(position = "stack")

# "identity would overlap. You might want to make it transparent
ggplot(data = plot_data, aes(x = date, y = total_likes, fill = type)) +
  geom_area(position = "identity", alpha = 0.8)

# "fill" would calculate the proportion of the post type each day and fill the whole plot
ggplot(data = plot_data, aes(x = date, y = total_likes, fill = type)) +
  geom_area(position = "fill")

########## Exercise ########## 
# 1. Using the above codes, aggregate comment counts by week (look up ?floor_date()).
# Plot an line graph showing the comments count per post type
# TIPS: use something like this
#  plot_data <- snp %>% 
#     group_by(type, date=floor_date(date, ######)) %>%
#     summarise(###### = sum(#######))
#
# 2. Use geom_vline() to add the date of the EU referendum (23 June 2016).
# TIPS: You have to use as.Date() to change the human date to Date format,
# and use it to specify the 'xintercept = ' (lookup ?geom_vline()).
#
##############################

####################################################################################
## What's next?                                                                   ##
####################################################################################

# You won't be able to learn everything about R in three hours,
# but I hope this workshop would give you a head start in your progarmming journey.
# If you would like to learn more, there are plenty (free) resources online:
# https://www.tidyverse.org/
# http://www.cookbook-r.com/
# https://socviz.co/
