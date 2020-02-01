data <- read.delim("~/Documents/GitHub/STAT-435/Baseline/Baseline.csv")
data$shift.day <- data$shift + (data$daycount-1)*3

model <- lm(y300 ~ as.factor(shift.day) + as.factor(daycount) + as.factor(shift), data)
summary(aov(model))

data <- read.delim("~/Documents/GitHub/STAT-435/search_for_dominant_cause/elimination.csv")

plot(y200~y300, data)
plot(y100~y200, data)
plot(y100~y300, data)

# equation to get equation of linear regression line
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}



plots.base <- ggplot(data=data) + 
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12))

model <- lm(y300 ~ y200, data)
eq <- paste('y =',
            format(coef(model)[1], digits=3),
            '+',
            format(coef(model)[2], digits=3),
            '* x')
plots.base + geom_point(aes(x=y200, y=y300), color="blue", alpha=0.7, size=1) +
  ggtitle("y300 vs y200") + 
  geom_smooth(method='lm', aes(x=y200, y=y300), se=FALSE) +
  geom_text(x = -4, y = 5, label = as.character(as.expression(eq)), parse = TRUE)
