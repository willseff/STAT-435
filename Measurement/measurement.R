library(ggplot2)

# import data
initial <- read.delim("baseline_initial.csv")
initial2 <- read.delim("initial2.csv")
baseline <- read.delim("baseline.csv")

#combine dfs
baseline <- rbind(initial,initial2,baseline)

# data engineering
#baseline$hour <- as.integer(baseline$partnum/60)
#baseline$shift.day <- baseline$shift + (baseline$daycount-1)*3

# order baseline data
attach(baseline)
baseline.ordered <- baseline[order(y300),]

# visualization of tail ends of data
ggplot(data=head(baseline.ordered, n=20), aes(x=y300)) + geom_dotplot(bins=40)

ggplot(data=tail(baseline.ordered, n=20), aes(x=y300)) + geom_dotplot() 
  #geom_text(aes(label=partnum),hjust=0, vjust=0)

# largest and smallest values
tail(baseline.ordered, n=10)
head(baseline.ordered, n=10)

# parts chosen are 1303(y=-11.1) and 214( y300 = 12.4) 2020 (y300= 0.3)
# subset just selected parts
selected <- baseline[baseline$partnum %in% c(214,1303,4664,2020),]

#data engineering
selected$study <- 'baseline'
selected$minute = selected$partnum

# import data from measurement study
measure <- read.delim('measure.csv')
measure$study <- 'measurement'
measure <- measure[measure$partnum %in% c(214,1303,2020),]

# combine data from measurement study and the data of the selected parts from the baseline
measure <- rbind(measure,selected)
measure$day.shift <- measure$shift + (measure$daycount-1)*3

# plot of measurements by partnum
ggplot(data=measure, aes(x=as.factor(partnum), y=y300, color=study)) + 
  geom_point() +
  ggtitle('Measurements by Partnum')

# plot of measurements by minute
ggplot(data = measure, aes(x=minute, y=y300, color = as.factor(partnum)))+ 
  geom_point()

# anova model with shift/ study blocked out. The shift is different for every study
model <- lm(y300 ~ as.factor(partnum) + as.factor(day.shift), measure)
summary(aov(model))

# anova model with just partnum blocked out
model <- lm(y300 ~ as.factor(partnum), measure)
aov <- summary(aov(model))

# use this standard deviation for the discrimination ratio
sqrt(aov[1][[1]][[3]][2])
aov

# anova model with study blocked out/ same results as the one with shift blocked out
# this one is prolly worse than the one above
model <- lm(y300 ~ as.factor(partnum) + as.factor(study), measure)
summary(aov(model))
