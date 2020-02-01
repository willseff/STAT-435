data <- read.delim("Baseline.csv")
data$shift.day <- data$shift + (data$daycount-1)*3

model <- lm(y300 ~ as.factor(shift.day) + as.factor(daycount) + as.factor(shift), data)
summary(aov(model))
