library(ggplot2)

fact.df <- read.delim("half_factorial1.tsv")

fact.df

fact.model <- aov(y200 ~ as.factor(z7) + as.factor(z8) + as.factor(z9) + as.factor(z10), 
    data=fact.df)
summary(fact.model)

fact.model <- aov(y200 ~ as.factor(z7)*as.factor(z10) + as.factor(z8) * as.factor(z9), 
                  data=fact.df)
summary(fact.model)


ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200))
ggplot(data=fact.df) + geom_point(aes(x=z8, y=y200))
ggplot(data=fact.df) + geom_point(aes(x=z9, y=y200))
ggplot(data=fact.df) + geom_point(aes(x=z10, y=y200))
ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z8)))
ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z9)))
ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z8)))
ggplot(data=fact.df) + geom_point(aes(x=z10, y=y200, color=as.factor(z7)))
ggplot(data=fact.df) + geom_point(aes(x=z8, y=y200, color=as.factor(z9)))

par(mfrow=c(2,3))

interaction.plot(fact.df$z7, fact.df$z8, fact.df$y200)
interaction.plot(fact.df$z7, fact.df$z9, fact.df$y200)
interaction.plot(fact.df$z7, fact.df$z10, fact.df$y200)
interaction.plot(fact.df$z8, fact.df$z9, fact.df$y200)
interaction.plot(fact.df$z8, fact.df$z10, fact.df$y200)
interaction.plot(fact.df$z9, fact.df$z10, fact.df$y200)

##################
## Expeirment 2:
## Only using z7 and z10, but using 3 levels for each 
## to estimate potential quadratic effects

experim.2 <- read.delim("z7-z10.tsv")
summary(experim.2)

ggplot(data=experim.2) + geom_point(aes(x=z7, y=y300, color=as.factor(z10)))

poly.model <- lm(y300 ~ poly(z7, z10, degree=2), 
                  data=experim.2)
summary(poly.model)

interact.model <- lm(y300 ~ z7*z10, data=experim.2)
summary(interact.model)
