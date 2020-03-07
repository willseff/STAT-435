library(ggplot2)
library(ggthemes)

##################### initial baseline study ##########################

# initial data 
initial <- read.delim("baseline_initial.csv")
initial2 <- read.delim("initial2.csv")

# intitial data engineering
initial <- rbind(initial, initial2)
initial$hour <- as.integer(initial$partnum/60)

# model with hour
model <- lm(y300 ~ as.factor(hour) + as.factor(shift), initial)
aov <- summary(aov(model))
aov

# pooled standard deviation
sqrt(aov[1][[1]][[3]][2])
# mean
mean(initial$y300)

#groupby mean
aggregate(initial[, 4], list(initial$hour), mean)

# boxplot of initial data by hour
ggplot(initial, aes(x = hour, y = y300, group = hour)) +
  geom_boxplot()

# plot of initial data by hour
ggplot(initial, aes(x = hour, y = y300)) + geom_point()

# plot of initial data by partnum
ggplot(initial, aes(x = partnum, y = y300)) + geom_point()

############## actual baseline study data ######################

# clear environment
remove(list = ls())

#import all baseline data 
baseline <- read.delim("baseline.csv")
initial <- read.delim("baseline_initial.csv")
initial2 <- read.delim("initial2.csv")

# add initial study data to baseline df
baseline <- rbind(initial,initial2,baseline)

# data engineering
baseline$hour <- as.integer(baseline$partnum/60)
baseline$shift.day <- baseline$shift + (baseline$daycount-1)*3
baseline$minute <- baseline$partnum%%60

# theme for ggplots
gg.base <- ggplot(baseline) + theme_bw() + scale_shape_cleveland()

# histogram of all data
gg.base + geom_histogram(aes(x=y300),bins=40) +
  ggtitle('Histogram of y300 Values')

# plot of all points
ggplot(baseline, aes(x = partnum, y = y300)) + 
  geom_point() + 
  theme_bw() + 
  scale_shape_cleveland() + 
  ggtitle('Plot of All data')

# mean median and standard deviation
summary(baseline$y300)
sd(baseline$y300)
max(baseline$y300)
min(baseline$y300)

# number of observationf outside of spec
outside.spec <- baseline[baseline$y300>10 | baseline$y300< -10,]
length(outside.spec)

# box plot by shift
ggplot(baseline, aes(x = shift.day, y = y300, group = shift.day)) +
  theme_bw() + 
  scale_shape_cleveland() +
  geom_boxplot() + ggtitle('Boxplot by Shift and Day')

#box plot by day
ggplot(baseline, aes(x = daycount, y = y300, group = daycount)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_shape_cleveland() + 
  ggtitle('Boxplot by Day')

# dotplot by hour
ggplot(baseline, aes(x = hour, y = y300, group = hour)) + 
  geom_dotplot(binaxis = "y", dotsize = 1, stackdir = "center", binwidth=0.25) + 
  ggtitle("Dotplot by Hour") +
  theme_bw() + 
  scale_shape_cleveland()

# violin plot by day
ggplot(baseline, aes(x = daycount, y = y300, group = daycount)) + geom_violin() + 
  theme_bw() + 
  scale_shape_cleveland() +
  ggtitle('Violin plot by Day')

# violin plot by shift
ggplot(baseline, aes(x=shift.day, y=y300, group = shift.day)) + 
  geom_violin()

# violin plot by hour
ggplot(baseline, aes(x=hour, y=y300, group=hour)) +
  geom_violin()

max(baseline$y300)
min(baseline$y300)

#nested anova model
nested.model <- lm(y300 ~ as.factor(daycount)/as.factor(shift.day)/as.factor(hour), baseline)
summary(aov(nested.model))

# minute analysis
ggplot(baseline, aes(x=minute)) + 
  geom_histogram(bins=40) +
  theme_bw() + 
  scale_shape_cleveland() + 
  ggtitle('Histogram of Minutes')

ggplot(baseline, aes(x=minute,y=y300)) + geom_point() +
  theme_bw() + 
  scale_shape_cleveland() + 
  ggtitle('Part by Minute')

model.minute <- lm(y300 ~ minute, baseline)
summary(model.minute)
