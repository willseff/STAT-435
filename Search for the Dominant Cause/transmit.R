# import data
transmit <- read.delim('transmit.csv')
summary(transmit)

# plots of data
ggplot(transmit, aes(x=y200,y=y300)) + geom_point()
summary(transmit$y300)

model1 <- lm(y300 ~ y200, transmit)
summary(model1)
model1

model2 <- lm(y200 ~ y100, transmit)
summary(model2)
model2

sd(transmit$y100)
sd(transmit$y200)
sd(transmit$y300)

ggplot(baseline, aes(x=y200)) + 
  geom_violin()

intermediate <- read.delim('intermediate_transmit.csv')
transmit <- transmit[, !(colnames(transmit) %in% c("y300"))]
intermediate <- rbind(intermediate, transmit)

ggplot(intermediate, aes(x=y100,y=y200)) + geom_point()

sd(intermediate$y200)
sd(intermediate$y100)

model3 <- lm(y200 ~ y100, intermediate)
summary(model3)
model3

