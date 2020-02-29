library(ggplot2)

# The second longer observation of just the x29 input
investigation.df <- read.delim("x29_prospective2.csv")

# The experimental data
verification.df <- read.delim("x29_verification.csv")

# The data from the individual causes investigation
individual.df <- read.delim("individual_causes.csv")

individual.x29.df <- subset(individual.df, select = c("y200", "x29"))
individual.x29.df$type <- c(rep("Prospective", nrow(individual.df)))

plots.base <- ggplot() + 
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12)) 

# plot of data from experimental investigation
plots.base + geom_point(aes(x=x29, y=y300), verification.df) + 
  ggtitle('y300 vs x29 (Experimental Investigation)') +
  xlim(-7, 10) + 
  geom_hline(aes(yintercept=13.3), linetype='dashed') +
  geom_hline(aes(yintercept=-13.95), linetype='dashed')

both.df <- rbind(investigation.df[c("y200", "x29")], verification.df[c("y200", "x29")])
both.df$type <- c(rep("Prospective", nrow(investigation.df)), 
                  rep("Experimental", nrow(verification.df)))

plots.base + geom_point(aes(x=x29, y=y200, color=type), both.df)

# df of all data with x29 and y200
all.df <- rbind(both.df,individual.x29.df)
plots.base + geom_point(aes(x=x29, y=y200, color=type), all.df) +
  geom_smooth(aes(x=x29, y=y200), method='lm', formula= y~x, all.df) + 
  ggtitle('y200 vs x29(Power Level)')



