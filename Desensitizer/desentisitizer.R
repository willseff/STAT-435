library(ggplot2)
library(gridExtra)

desen <- read.delim('desensitizer.csv')

p1 <- ggplot(data=desen, aes(x=z11, y=y300)) + geom_point() +
  ggtitle('Plots of y300 vs Categorical Inputs')
p2 <- ggplot(data=desen, aes(x=z12,y=y300)) + geom_point() +
  ggtitle(' ')

grid.arrange(p1, p2,nrow = 1)

par(mfrow=c(1,2))
z11 <- desen$z11
z12 <- desen$z12
x29 <- desen$x29
y300 <- desen$y300

interaction.plot(z11,x29,y300)
interaction.plot(z12,x29,y300)
title("Interaction Plots of Categorical Inputs and x29", line = -2.5, outer = TRUE)

m <- lm(y300~z11+z12+x29 + z11*x29 + z12*x29)
DisplayAnovaSummary(summary(aov(m)), title='')

################ Second Investigation #####################

desen2 <- read.delim('desen2.csv')

p1 <- ggplot(data=desen2, aes(x=z7, y=y300)) + geom_point()
p2 <- ggplot(data=desen2, aes(x=z8, y=y300)) + geom_point()
p3 <- ggplot(data=desen2, aes(x=z9, y=y300)) + geom_point()
p4 <- ggplot(data=desen2, aes(x=z10, y=y300)) + geom_point()
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

z7 <- desen2$z7
z8 <- desen2$z8
z9 <- desen2$z9
z10 <- desen2$z10
x29 <- desen2$x29
y300 <- desen2$y300

par(mfrow=c(1,2))
interaction.plot(z7, x29, y300)
interaction.plot(z8, x29, y300)
title("Interaction Plots of Continuous Inputs and x29", line = -2.5, outer = TRUE)

par(mfrow=c(1,2))
interaction.plot(z9, x29, y300)
interaction.plot(z10, x29, y300)

m <- lm(y300 ~ z7 + z8 + z9 + z10 + x29 +
          z7*x29 + z8*x29 + z9*x29 + z10*x29, data=desen2)
DisplayAnovaSummary(summary(aov(m)),title='')


