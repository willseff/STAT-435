baseline.y100 <- read.delim('y100baseline.csv')

# data engineering
baseline.y100$hour <- as.integer(baseline.y100$partnum/60)
baseline.y100$shift.day <- baseline.y100$shift + (baseline.y100$daycount-1)*3

ggplot(baseline.y100, aes(x=partnum, y=y100)) + geom_point()
ggplot(baseline.y100, aes(x=shift.day, y=y100)) + geom_point()

nested.model <- lm(y100 ~ as.factor(daycount)/as.factor(shift.day)/as.factor(hour), baseline.y100)
summary(aov(nested.model))

