transmit <- read.delim('transmit.csv')
summary(transmit)

plot(transmit$y100, transmit$y200)
plot(transmit$y200, transmit$y300)
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

transmit 
