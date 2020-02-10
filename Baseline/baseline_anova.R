data <- read.delim("Baseline.csv")
data$shift.day <- data$shift + (data$daycount-1)*3
data$hour <- data$hour <- data$partnum %/% 60

View(data)

model <- lm(y300 ~ as.factor(shift.day) + as.factor(hour), data)
anova(model)

# nested model
# same results wtf
model.nested <- lm(y300 ~ as.factor(shift.day) + as.factor(shift.day)/as.factor(hour), data)
anova(model.nested)

# just hour model
model.hour <- lm(y300 ~ as.factor(hour), data)
anova(model.hour)

# just shift.day model 
model.shift <- lm(y300 ~ as.factor(shift.day), data)
anova(model.shift)
