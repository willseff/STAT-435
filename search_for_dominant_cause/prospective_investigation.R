# read data
data <- read.delim("~/Documents/GitHub/STAT-435/search_for_dominant_cause/elimination.csv")

plots.base <- ggplot(data=data) + 
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12))

# lm model for y300 vs y200
model <- lm(y300 ~ y200, data)

# lm equation in string format
eq <- paste('x =',
            format(coef(model)[1], digits=3),
            '+',
            format(coef(model)[2], digits=3),
            '* x')
# plot
plots.base + geom_point(aes(x=y200, y=y300), color="blue", alpha=0.7, size=1) +
  ggtitle("y300 vs y200") + 
  geom_smooth(method='lm', aes(x=y200, y=y300), se=FALSE) +
  geom_text(x = -4, y = 5, label = eq, parse = TRUE) +
  geom_hline(yintercept=-10, color="#747474", linetype="dashed") +
  geom_hline(yintercept=10, color="#747474", linetype="dashed")


model <- lm(y200 ~ y100, data)
# lm equation in string format
eq <- paste('x =',
            format(coef(model)[1], digits=3),
            '+',
            format(coef(model)[2], digits=3),
            '* x')

# plot
plots.base + geom_point(aes(x=y100, y=y200), color="blue", alpha=0.7, size=1) +
  ggtitle("y200 vs y100") + 
  geom_smooth(method='lm', aes(x=y200, y=y100), se=FALSE) +
  geom_text(x = -4, y = 5, label = eq, parse = TRUE)

model <- lm(y300 ~ partnum, data)
summary(aov(model))
