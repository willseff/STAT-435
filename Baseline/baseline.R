# STAT 435 Team 2
# Establishing the Baseline - Analysis code

library(ggplot2)

data <- read.table("Baseline.csv", sep="\t", header=TRUE)
data$day <- data$daycount
data$hour <- data$partnum %/% 60
data$shift.day <- data$shift + (data$daycount-1)*3

head(data)

y300.mean <- mean(data$y300); y300.mean
y300.sd <- sd(data$y300); y300.sd

ci.halfwidth <- qnorm(0.975) * y300.sd / sqrt(length(data$y300))
y300.mean - ci.halfwidth; y300.mean + ci.halfwidth

y300.min <- min(data$y300); y300.min
y300.max <- max(data$y300); y300.max

num.outof.spec <- sum((data$y300 > 10) | (data$y300 < -10)); num.outof.spec
num.outof.spec/ length(data$y300)

# Histogram showing distribution of the data
ggplot(data=data, mapping=aes(x=y300)) +
  geom_histogram(color="#CCCCCC", fill="#686868") +
  geom_vline(xintercept=-10, color="#747474", linetype="dashed") +
  geom_vline(xintercept=10, color="#747474", linetype="dashed") +
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12)) +
  ggtitle("Distribution of observed values\nwith target specification limits") +
  xlab("Observed y300") +
  ylab("Frequency")

# A base theme so all plots look consistent
plots.base <- ggplot(data=data) + 
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12)) +
  geom_hline(yintercept=y300.min, color="#747474") +
  geom_hline(yintercept=y300.max, color="#747474")

# Prep for the multivari plot
hours.df <- data.frame(hour=unique(data$hour))
hours.df$y300.avg <- sapply(hours.df$hour, 
                            function(x){
                              return(mean(subset(data, hour==x)$y300))
                            })

# Multivari plot by hour
plots.base + geom_point(aes(y=y300, x=partnum), color="blue", alpha=0.3, size=1, show.legend=TRUE) +
  geom_line(data=hours.df, 
            mapping=aes(y=y300.avg, x=as.integer(hour)*60),
            colour="#EE4422", size=1, alpha=0.7, show.legend=FALSE) +
  geom_point(data=hours.df, 
             mapping=aes(y=y300.avg, x=as.integer(hour)*60),
             colour="red", size=2, alpha=0.7, show.legend=TRUE) +
  xlab("Part number") +
  ggtitle("Time series of output variable y300\nover 5 days of observation") +
  geom_hline(yintercept=-10, color="#747474", linetype="dashed") +
  geom_hline(yintercept=10, color="#747474", linetype="dashed")

# Boxplot by day
plots.base + geom_boxplot(aes(y=y300, x=as.factor(day))) +
  xlab("Day") + ylab("y300") +
  ggtitle("Distribution of output variable y300 by day")

# Boxplot by shift
plots.base + geom_boxplot(aes(y=y300, x=as.factor(shift))) +
  xlab("Shift") + ylab("y300") +
  ggtitle("Distribution of output variable y300 by shift")

# Boxplot by hour
plots.base + geom_boxplot(aes(y=y300, x=as.factor(hour))) +
  xlab("Hour") + ylab("y300") +
  ggtitle("Distribution of output variable y300 by hour")

# Boxplot by shift by day
plots.base + geom_boxplot(aes(y=y300, x=as.factor(shift.day))) +
  xlab("Shift") + ylab("y300") +
  ggtitle("Distribution of output variable y300 by shift (by day)")


# There was a lot of hourly variability from the multivari chart. 
# We can check this with a linear model. 
# We get an R-squared of 65%: 2/3 of variability can be explained by hour-to-hour 
# (or on the scale of 4 hours) variation
hour.model <- lm(y300 ~ as.factor(hour), data)
summary(hour.model)
1 - summary(hour.model)$r.squared

plots.base + geom_point(aes(y=hour.model$residuals, x=partnum), color="blue", alpha=0.3, size=1) +
  xlab("Part number") +
  ylab("Hour-adjusted y300") +
  ggtitle("Residuals of linear model of output y300 on hour\nover 5 days of observation") +
  geom_hline(yintercept=-10, color="#747474", linetype="dashed") +
  geom_hline(yintercept=10, color="#747474", linetype="dashed")

shift.model <- lm(y300 ~ as.factor(shift), data)
summary(shift.model)

day.model <- lm(y300 ~ as.factor(day), data)
summary(day.model)

shift.day.model <- lm(y300 ~ as.factor(shift.day), data)
summary(shift.day.model)
data$shift.day

summary(shift.day.model)$r.squared - summary(day.model)$r.squared
summary(hour.model)$r.squared - summary(shift.day.model)$r.squared
