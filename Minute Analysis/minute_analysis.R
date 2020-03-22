initial <- read.delim("baseline_initial.csv")
initial2 <- read.delim("initial2.csv")
baseline <- read.delim("baseline.csv")
transmit <- read.delim('transmit.csv')

transmit <- transmit[, !(colnames(transmit) %in% c("y100","y200"))]

data <- rbind(initial, initial2, baseline, transmit)

data$shift.min <- data$partnum %% 480

ggplot(data, aes(x=shift.min, y = y300)) + geom_point()
# no minute to minute effect

var(data$y300)
