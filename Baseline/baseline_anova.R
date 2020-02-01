data <- read.delim("Baseline.csv")
data$shift.day <- data$shift + (data$daycount-1)*3
data$hour <- data$hour <- data$partnum %/% 60

model <- lm(y300 ~ as.factor(shift.day) + 
              as.factor(hour), 
            data)
summary(aov(model))
