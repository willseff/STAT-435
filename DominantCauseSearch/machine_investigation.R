library(ggplot2)

#machine.data1 <- read.delim("machines1.csv")
#machine.data2 <- read.delim("machines2.csv")
#
#machine.data <- rbind(machine.data1, machine.data2)

machine.data <- read.delim("machines_final.csv")
nrow(machine.data)

machine.data$machine <- as.factor(machine.data$x31)
machine.data$hour <- machine.data$partnum %/% 60

summary(machine.data)

ggplot(machine.data, aes(y=y200, x=machine)) + geom_boxplot()

ggplot(machine.data, aes(y=y200, x=hour, color=machine, shape=machine)) + 
  geom_point(position=position_jitter(width=0.3)) +
  labs(title="Time Series of Y200 observations\nby step 200 machine used",
       x="Hour") +
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12)) +
  scale_color_discrete(name="Machine #") +
  scale_shape_discrete(name="Machine #") +
  scale_x_continuous(breaks=c(200, 202, 204, 206, 208, 210))

machine.model <- lm(y200 ~ as.factor(hour) *machine, data=machine.data)
summary(machine.model)

anova(machine.model)

hour.model <- lm(y200 ~ as.factor(hour), data=machine.data)

ggplot(machine.data, aes(y=hour.model$residuals, x=hour, color=machine, shape=machine)) + 
  geom_point(position=position_jitter(width=0.4)) +
  labs(title="Time Series of hourly mean-adjusted y200\nobservations by step 200 machine used",
       x="Hour",
       y="y200 hour-adjusted residuals") +
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12)) +
  scale_color_discrete(name="Machine #") +
  scale_shape_discrete(name="Machine #") +
  scale_x_continuous(breaks=c(200, 202, 204, 206, 208, 210))
