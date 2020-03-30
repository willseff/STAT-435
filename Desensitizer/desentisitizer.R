desen <- read.delim('desensitizer.csv')
library(ggplot2)
library(gridExtra)

summary(desen)

attach(desen)

p1 <- ggplot(data=desen, aes(x=z11, y=y300)) + geom_point()
p2 <- ggplot(data=desen, aes(x=z12,y=y300)) + geom_point()

par(mfrow=c(1,2))

interaction.plot(z11,x29,y300)
interaction.plot(z12,x29,y300)

grid.arrange(p1, p2,nrow = 1)

m <- lm(y300~z11+z12+x29)
summary(aov(m))

################ Second Investigation #####################

desen2 <- read.delim('desen2.csv')
summary(desen2)
attach(desen2)

p1 <- ggplot(data=desen, aes(x=z7, y=y300)) + geom_point()
p2 <- ggplot(data=desen, aes(x=z8, y=y300)) + geom_point()
p3 <- ggplot(data=desen, aes(x=z9, y=y300)) + geom_point()
p4 <- ggplot(data=desen, aes(x=z10, y=y300)) + geom_point()

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

par(mfrow=c(1,1))
interaction.plot(z8,x29,y300)
interaction.plot(z9,x29,y300)
interaction.plot(z7,x29,y300)

m <- lm(y300 ~ z7 + z8 + z9 + z10 + x29 + z8*x29 + z9*x29 + z8*z9)
summary(aov(m))

m <- lm(y300 ~ z7 + z8 + z9 + z10 + x29 + z9*x29 + z)
summary(aov(m))

