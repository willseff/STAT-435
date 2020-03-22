library(ggplot2)

fact.df <- read.delim("half_factorial1.tsv")

summary(fact.df)

fact.model <- aov(y200 ~ as.factor(z7) + as.factor(z8) + as.factor(z9) + as.factor(z10), 
    data=fact.df)
summary(fact.model)

plot(fact.df$z7, fact.df$y200)
ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z8)))

ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z9)))

ggplot(data=fact.df) + geom_point(aes(x=z8, y=y200, color=as.factor(z9)))

ggplot(data=fact.df) + geom_point(aes(x=z10, y=y200, color=as.factor(z8)))


attach(fact.df)

par(mfrow=c(2,3))

interaction.plot(z7, z8, y200)
interaction.plot(z7, z9, y200)
interaction.plot(z7, z10, y200)
interaction.plot(z8, z9, y200)
interaction.plot(z8, z10, y200)
interaction.plot(z9, z10, y200)

fact.model <- aov(y200 ~ as.factor(z7) + as.factor(z8) + as.factor(z9) + as.factor(z10) + 
                    as.factor(z10)*as.factor(z7) + as.factor(z8)*as.factor(z9) , 
                  data=fact.df)
summary(fact.model)

