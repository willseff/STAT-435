baseline.y100 <- read.delim('y100baseline.csv')
transmit<- read.delim('transmit.csv')

transmit$y200 <- NULL
transmit$y300 <- NULL

baseline.y100 <- rbind(baseline.y100, transmit)
# data engineering
baseline.y100$hour <- as.integer(baseline.y100$partnum/60)
baseline.y100$shift.day <- baseline.y100$shift + (baseline.y100$daycount-1)*3

gg.base <- ggplot(baseline.y100)

ggplot(baseline.y100, aes(x=partnum, y=y100)) + geom_point()
ggplot(baseline.y100, aes(x=shift.day, y=y100)) + geom_point()

gg.base + geom_boxplot(aes(x=daycount, y=y100, group=(daycount)))

ggplot(baseline.y100, aes(x=shift.day, y=y100, group = shift.day)) + geom_boxplot()

nested.model <- lm(y100 ~ as.factor(daycount)/as.factor(shift.day), baseline.y100)
summary(aov(nested.model))


