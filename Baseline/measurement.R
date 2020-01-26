# STAT 435 Team 2
# Measurement Assessment (W2) - Analysis code

library(ggplot2)

baseline.data <- read.table("Baseline.csv", header=TRUE)

# In class, we called a "good" measurement error as less than 1/3 of the total process
# error.
baseline.sd <- sd(baseline.data$y300); baseline.sd
# A 95% confidence interval for the standard deviation
baseline.df <- length(baseline.data$y300) - 1
baseline.ci.lower <- sqrt(baseline.df*baseline.sd^2/qchisq(0.975, baseline.df)); baseline.ci.lower
baseline.ci.upper <- sqrt(baseline.df*baseline.sd^2/qchisq(0.025, baseline.df)); baseline.ci.upper
# So our 95% confidence interval for the standard deviation is [4.27, 4.85]
# We consider a measurement standard error greater than 1/2 of the total to be the dominant cause,
# and a standard error greater than 1/3 of the total to be a problem that needs to be addressed
# So we can rule out measurement if measurement variation is less than 1/3 of total variation
meas.max <- baseline.sd/3; meas.max
# So the target value we want to discriminate is 1/3 of the total, or 1.515

# Recall, our baseline data was normal-ish, with a few values lying outside spec
ggplot(data=baseline.data, mapping=aes(x=y300)) +
  geom_histogram(color="#CCCCCC", fill="#686868") +
  geom_vline(xintercept=-10, color="#747474", linetype="dashed") +
  geom_vline(xintercept=10, color="#747474", linetype="dashed") +
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12)) +
  ggtitle("Distribution of observed values\nwith target specification limits") +
  xlab("Observed y300") +
  ylab("Frequency")


y300.med <- median(data$y300); y300.med

min.part <- data$partnum[which.min(data$y300)]; min.part
max.part <- data$partnum[which.max(data$y300)]; max.part
med.part <- data$partnum[data$y300 == median(data$y300)][1]; med.part

# Our first go of measuring the data, just to assess whether we need a bigger sample size
# Regardless, we'll want to measure more over multiple hours.
# We looked at the 3 parts, just taking 8 consecutive measurements of each (that's 24 in total)
measurement.initial <- read.table("measurement_1.csv", header=TRUE)
head(measurement.initial)

ggplot(data=measurement.initial, mapping=aes(y=y300, group=partnum)) +
         geom_boxplot()

ggplot(data=measurement.initial, mapping=aes(y=y300, x=partnum)) + geom_point()

model <- lm(y300 ~ as.factor(partnum), data=measurement.initial)
summary(model)


### Determining desired sample size
# Our measurement standard deviation is 1.632
# We can construct a confidence interval:
meas.df <- model$df.residual
meas.sd <- summary(model)$sigma
meas.ci.lower <- sqrt(meas.df*meas.sd^2/qchisq(0.975, meas.df)); meas.ci.lower
meas.ci.upper <- sqrt(meas.df*meas.sd^2/qchisq(0.025, meas.df)); meas.ci.upper
# The average for our measurement is above our 1/3 critical value of 1.515;
# however, the 95% CI for measurement error based on our existing sample includes regions
# lower than the 1/3 value, and above the 1/2 value that we would assign measurement eror
# the dominant cause.
# We need to tighten up our CI (by increasing sample size).
# A reasonable metric would be that we want our CI to be less than the 3/4 distance between
# the 1/3 and 1/2 values; then we could at least say if we're confident if it's on the more
# dangerous, or less dangerous side.
target.width <- (baseline.sd/2 - baseline.sd/3)*3/4; target.width
for (prop.df in meas.df:200)
{
  prop.ci.width <- sqrt(prop.df*meas.sd^2/qchisq(0.025, prop.df)) - 
                   sqrt(prop.df*meas.sd^2/qchisq(0.975, prop.df))
  if (prop.ci.width < target.width) break
}
prop.df
prop.ci.width
