library(ggplot2)

fact.df <- read.delim("half_factorial1.tsv")

summary(fact.df)

fact.model <- aov(y200 ~ as.factor(z7) + as.factor(z8) + as.factor(z9) + as.factor(z10), 
    data=fact.df)
summary(fact.model)

plot(fact.df$z7, fact.df$y200)
ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z8)))

ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z9)))

ggplot(data=fact.df) + geom_point(aes(x=z7, y=y200, color=as.factor(z10)))

ggplot(data=fact.df) + geom_point(aes(x=z10, y=y200, color=as.factor(z8)))
