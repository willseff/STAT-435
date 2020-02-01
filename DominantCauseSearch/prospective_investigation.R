# read data
data <- read.delim("elimination_1.csv")

plots.base <- ggplot(data=data) + 
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12))

#boxplot
#melt data
data.m <- melt(data,id.vars='partnum', measure.vars=c('y100','y200','y300'))
#plot
ggplot(data=data.m) + geom_boxplot(aes(x=variable, y=value))

# lm model for y300 vs y200
model <- lm(y300 ~ y200, data)

# lm equation in string format
eq <- paste('y =',
            format(coef(model)[1], digits=3),
            '+',
            format(coef(model)[2], digits=3),
            '* x')

# plot
plots.base + geom_point(aes(x=y200, y=y300), color="blue", alpha=0.7, size=1) +
  ggtitle("y300 vs y200") + 
  geom_smooth(method='lm', aes(x=y200, y=y300), se=FALSE) +
  annotate('text', label=eq, x=-4, y=5) +
  geom_hline(yintercept=-10, color="#747474", linetype="dashed") +
  geom_hline(yintercept=10, color="#747474", linetype="dashed")

# lm mode for y200 vs y100
model <- lm(y200 ~ y100, data)
# lm equation in string format
eq <- paste('y =',
            format(coef(model)[1], digits=3),
            '+',
            format(coef(model)[2], digits=3),
            '* x')

# plot
plots.base + geom_point(aes(x=y100, y=y200), color="blue", alpha=0.7, size=1) +
  ggtitle("y200 vs y100") +
  annotate('text', label=eq, x=-3, y=6) +
  geom_smooth(method='lm', aes(x=y200, y=y100), se=FALSE) + 
  xlim(-5,5)

#anova analysis of y300 by hour
data$hour <- data$partnum %/% 60
model <- lm(y300 ~ as.factor(hour), data)
summary(aov(model))

# plot by hour of y300
plots.base + geom_point(aes(x=hour, y=y300))

# timeseries plot

# anova on y200


