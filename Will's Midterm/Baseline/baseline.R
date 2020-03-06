library(ggplot2)
library(ggthemes)

initial <- read.delim("~/Desktop/stat_435_take_home_midterm/baseline_initial.csv")
initial2 <- read.delim("~/Desktop/stat_435_take_home_midterm/initial2.csv")

initial$hour <- as.integer(initial$partnum/60)
model <- lm(y300 ~ as.factor(hour), baseline.initial)

aov <- summary(aov(model))

ggplot(baseline.initial, aes(x = hour, y = y300, group = hour)) +
  geom_boxplot()

ggplot(baseline.initial, aes(x = hour, y = y300)) + geom_point()

# pooled standard deviation
sqrt(aov[1][[1]][[3]][2])
# mean
mean(baseline.initial$y300)

#groupby mean
aggregate(baseline.initial[, 4], list(baseline.initial$hour), mean)

# actual baseline study data
baseline <- read.delim("~/Desktop/stat_435_take_home_midterm/baseline.csv")
# add initial study data to baseline df
baseline <- rbind(initial,initial2,baseline)

# data engineering
baseline$hour <- as.integer(baseline$partnum/60)
baseline$shift.day <- baseline$shift + (baseline$daycount-1)*3

summary(baseline)

#histogram of all data
ggplot(baseline, aes(x=y300)) + geom_histogram(bins=40) +
  ggtitle('Histogram of y300 Values') + 
  theme_economist_white(gray_bg=FALSE) + 
  scale_colour_economist()

# plot of all points
ggplot(baseline, aes(x = partnum, y = y300)) + 
  geom_point() + 
  theme_bw() + 
  scale_shape_cleveland() + 
  ggtitle('Plot of all data')

# box plot by shift
ggplot(baseline, aes(x = shift.day, y = y300, group = shift.day)) +
  theme_bw() + 
  scale_shape_cleveland() +
  geom_boxplot() + ggtitle('Boxplot by Shift and Day')

#box plot by day
ggplot(baseline, aes(x = daycount, y = y300, group = daycount)) +
  geom_boxplot() + theme_bw() + scale_shape_cleveland() + ggtitle('Boxplot by Day')

# dotplot by hour
ggplot(baseline, aes(x = hour, y = y300, group = hour)) + 
  geom_dotplot(binaxis = "y", dotsize = 1, stackdir = "center", binwidth=0.25) + 
  ggtitle("Dotplot by Hour")

# violinplot by day
ggplot(baseline, aes(x = daycount, y = y300, group = daycount)) + geom_violin() + 
  theme_bw() + 
  scale_shape_cleveland() +
  ggtitle('Violin plot by Day')

# violin plot by shift
ggplot(baseline, aes(x=shift.day, y=y300, group = shift.day)) + 
  geom_violin()

max(baseline$y300)
min(baseline$y300)

#nested anova model
nested.model <- lm(y300 ~ as.factor(daycount)/as.factor(shift.day)/as.factor(hour), baseline)
summary(aov(nested.model))

sd(baseline$y300)

