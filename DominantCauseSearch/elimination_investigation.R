linked.df1 <- read.delim("elimination_1.csv")
linked.df2 <- read.delim("elimination_2.csv")

linked.df <- rbind(linked.df1, linked.df2)

summary(linked.df)


y200.model <- lm(y300~y200, data=linked.df)
