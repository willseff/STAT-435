library(ggplot2)

# import data
initial <- read.delim("baseline_initial.csv")
initial2 <- read.delim("initial2.csv")
baseline <- read.delim("baseline.csv")

#combine dfs
baseline <- rbind(initial,initial2,baseline)

# data engineering
baseline$hour <- as.integer(baseline$partnum/60)
baseline$shift.day <- baseline$shift + (baseline$daycount-1)*3

# order baseline data
attach(baseline)
baseline.ordered <- baseline[order(y300),]

ggplot(data=head(baseline.ordered, n=20), aes(x=y300)) + geom_dotplot(bins=40)

tail(baseline.ordered, n=10)
head(baseline.ordered, n=10)

ggplot(data=tail(baseline.ordered, n=20), aes(x=y300)) + geom_dotplot() 
  #geom_text(aes(label=partnum),hjust=0, vjust=0)

# parts chosen are 1303(y=-11.1) and 214( y300 = 12.4) 2020 (y300= 0.3)
selected <- baseline[baseline$partnum %in% c(1303,4664,2020),]
selected$study <- 'baseline'
selected$minute = selected$partnum
measure <- read.delim('measure.csv')
measure$study <- 'measurement'
measure <- rbind(measure,selected)
measure$day.shift <- measure$shift + (measure$daycount-1)*3
summary(measure)

ggplot(data=measure, aes(x=as.factor(partnum), y=y300, color=study)) + geom_point()

model <- lm(y300 ~ as.factor(partnum) + as.factor(day.shift), measure)
summary(aov(model))

model <- lm(y300 ~ as.factor(partnum), measure)
aov <- summary(aov(model))
sqrt(aov[1][[1]][[3]][2])
