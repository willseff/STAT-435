# read data
data1 <- read.delim("elimination_1.csv")
data2 <- read.delim("elimination_2.csv")

data <- rbind(data1,data2)

plots.base <- ggplot(data=data)

#boxplot by output (y100,y200,y300)
#melt data
data.m <- melt(data,id.vars='partnum', measure.vars=c('y100','y200','y300'))
#plot
ggplot(data=data.m) + geom_boxplot(aes(x=variable, y=value)) +
  ggtitle('Boxplot of Data by Output') + ylab('Value') + xlab('Output')

#boxplot by hour
data$hour = data$partnum %% 60
data.m <- melt(data,id.vars='hour', measure.vars=c('y100','y200','y300'))
ggplot(data=data.m) + geom_boxplot(aes(x=as.factor(hour), y=value)) + 
  xlab('hour') + ggtitle('Boxpot of Output by Hour')

# lm model for y300 vs y200
model <- lm(y300 ~ y200, data)

# lm equation in string format
eq <- paste('y =',
            format(coef(model)[1], digits=3),
            '+',
            format(coef(model)[2], digits=3),
            '* x')
#r squared value
r2.value = summary(model)$r.squared
r2.value
# plot
ggplot(data=data) + geom_point(aes(x=y200, y=y300), color="blue", alpha=0.7, size=1) +
  ggtitle("y300 Output vs y200 Output") + 
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

# r squared value
r2.value = summary(model)$r.squared
r2.value

# plot
ggplot(data=data) + geom_point(aes(x=y100, y=y200), color="blue", alpha=0.7, size=1) +
  ggtitle("y200 Output vs y100 Output") +
  annotate('text', label=eq, x=-3.5, y=6) +
  geom_smooth(method='lm', aes(x=y200, y=y100), se=FALSE) + 
  xlim(-5,5)

#anova analysis of y300 by hour
data$hour <- data$partnum %/% 60
model <- lm(y300 ~ as.factor(hour), data)
summary(aov(model))

#anova analysis of y200 by hour
model <- lm(y200 ~ as.factor(hour), data)
summary(aov(model))

#anova analyais of y100 by hour
model <- lm(y100 ~ as.factor(hour), data)
summary(aov(model))


# Prep for the multivariate plot
# new df
hours.df <- data.frame(hour=unique(data$hour))
# columns for the average value of each output for each hour
hours.df$y300.avg <- sapply(hours.df$hour, 
                            function(x){
                              return(mean(subset(data, hour==x)$y300))
                            })
hours.df$y200.avg <- sapply(hours.df$hour, 
                            function(x){
                              return(mean(subset(data, hour==x)$y200))
                            })

hours.df$y100.avg <- sapply(hours.df$hour, 
                            function(x){
                              return(mean(subset(data, hour==x)$y100))
                            })

# plot by hour of y300
# unable to produce legend
ggplot() + 
  geom_point(data=data, aes(x=hour, y=y300),color="blue", alpha=0.7, size=1) + 
  geom_line(data=hours.df, aes(x=hour, y=y300.avg), color="blue") + 
  geom_point(data=data, aes(x=hour, y=y200),color="pink", alpha=1, size=1) +
  geom_line(data=hours.df, aes(x=hour, y=y200.avg), color="pink") +
  geom_point(data=data, aes(x=hour, y=y100),color="green", alpha=1, size=1) +
  geom_line(data=hours.df, aes(x=hour, y=y100.avg), color="green") +
  ylab('Value') + xlab('Hour') + ggtitle('Multivariate Plot of Output')






