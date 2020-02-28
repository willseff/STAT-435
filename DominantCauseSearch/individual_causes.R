library(ggplot2)

individ.data <- read.delim("individual_causes.csv")
head(individ.data, 16)
summary(individ.data)

individ.data$hour <- individ.data$partnum %/% 60
hours <- unique(individ.data$hour)

unique(individ.data$hour)

plots.base <- ggplot(data=individ.data) + 
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12))

# Dominant cause! - x29
plots.base + geom_point(aes(x=x29, y=y200)) +
  labs(title="Plot of input x29 (power level)\nvs intermediate output y200") +
  geom_smooth(mapping=aes(x=x29, y=y200), method=lm)

#  lims(x=c(-10, 10), y=c(-10, 10))

plots.base + geom_point(aes(x=partnum/60, y=x29)) +
  labs(title="Plot of input x29 (power level)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL)

plots.base + geom_point(aes(x=x30, y=y200)) +
  labs(title="Plot of input x30 (feed rate)\nvs intermediate output y200")

plots.base + geom_point(aes(x=partnum/60, y=x30)) +
  labs(title="Plot of input x30 (feed rate)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL)


plots.base + geom_point(aes(x=x32, y=y200)) +
  labs(title="Plot of input x32 (squeeze time)\nvs intermediate output y200")

plots.base + geom_point(aes(x=partnum/60, y=x32)) +
  labs(title="Plot of input x32 (squeeze time)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL)


plots.base + geom_point(aes(x=x35, y=y200)) +
  labs(title="Plot of input x35 (contamination)\nvs intermediate output y200") +
  geom_smooth(mapping=aes(x=x35, y=y200), method=lm)

plots.base + geom_point(aes(x=partnum/60, y=x35)) +
  labs(title="Plot of input x35 (contamination)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL)


plots.base + geom_point(aes(x=x37, y=y200)) +
  labs(title="Plot of input x37 (humidity)\nvs intermediate output y200")

plots.base + geom_point(aes(x=partnum/60, y=x37)) +
  labs(title="Plot of input x37 (humidity)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL)


plots.base + geom_point(aes(x=x42, y=y200)) +
  labs(title="Plot of input x42 (component placement)\nvs intermediate output y200")

plots.base + geom_point(aes(x=partnum/60, y=x42)) +
  labs(title="Plot of input x42 (component placement)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL)


plots.base + geom_point(aes(x=x43, y=y200)) +
  labs(title="Plot of input x43 (spacing parameter)\nvs intermediate output y200")

plots.base + geom_point(aes(x=partnum/60, y=x43)) +
  labs(title="Plot of input x43 (spacing parameter)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL)


plots.base + geom_point(aes(x=x45, y=y200)) +
  labs(title="Plot of input x45 (fixture #)\nvs intermediate output y200") +
  scale_x_continuous(breaks=unique(individ.data$x45), minor_breaks=NULL)

plots.base + geom_point(aes(x=partnum/60, y=x45)) +
  labs(title="Plot of input x45 (fixture #)\nmeasurements",
       x="Hour") +
  scale_x_continuous(breaks=hours, minor_breaks=NULL) +
  scale_y_continuous(breaks=unique(individ.data$x45), minor_breaks=NULL)


full.model <- lm(y200 ~ x29 + x30 + x32 + x35 + x37 + x42 + x43 + as.factor(x45), 
                 data=individ.data)
summary(full.model)

partial.model.x45 <- lm (y200 ~ x29 + x30 + x32 + x35 + x37 + x42 + x43, 
                         data=individ.data)
summary(partial.model.x45)
anova(full.model, partial.model.x43)

partial.model.x35 <- lm(y200 ~ x29 + x30 + x32 + x37 + x42 + x43 + as.factor(x45), 
                       data=individ.data)
summary(partial.model.x35)

partial.model.x29 <- lm(y200 ~ x30 + x32 + x35 + x37 + x42 + x43 + as.factor(x45), 
                        data=individ.data)
summary(partial.model.x29)
anova(partial.model.x29, full.model)
summary(full.model)$r.squared - summary(partial.model.x29)$r.squared

x29.model <- lm(y200 ~ x29, data=individ.data)
summary(x29.model)
individ.data$daycount
