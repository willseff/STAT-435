# import data
transmit <- read.delim('transmit.csv')
summary(transmit)

# plots of data
gg.base <- ggplot(transmit) + theme_bw() + scale_shape_cleveland() 

# function for getting linear regression equation
lm_eqn <- function(m){
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

# plot y300 vs y200
gg.base + geom_point(aes(x=y200,y=y300)) +
  ggtitle('Plot of y300 vs y200') +
  geom_smooth(aes(x=y200,y=y300),method='lm', formula= y~x) +
  geom_text(x = 1.5, y = 9, label = lm_eqn(lm(y300~y200, transmit)), parse = TRUE
            , size = 5)

# plot of y200 vs y100
gg.base + geom_point(aes(x=y100,y=y200)) +
  ggtitle('Plot of y200 vs y100') +
  geom_smooth(aes(x=y100,y=y200),method='lm', formula= y~x) +
  geom_text(x = 7, y = 0, label = lm_eqn(lm(y200~y100, transmit)), parse = TRUE
            , size = 5)


model1 <- lm(y300 ~ y200, transmit)
summary(model1)
model1

model2 <- lm(y200 ~ y100, transmit)
summary(model2)
model2

sd(transmit$y100)
sd(transmit$y200)
sd(transmit$y300)

ggplot(baseline, aes(x=y200)) + 
  geom_violin()

intermediate <- read.delim('intermediate_transmit.csv')
transmit <- transmit[, !(colnames(transmit) %in% c("y300"))]
intermediate <- rbind(intermediate, transmit)

ggplot(intermediate, aes(x=y100,y=y200)) + geom_point()

sd(intermediate$y200)
sd(intermediate$y100)

model3 <- lm(y200 ~ y100, intermediate)
summary(model3)
model3

