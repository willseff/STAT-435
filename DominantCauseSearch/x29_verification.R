library(ggplot2)

# The second longer observation of just the x29 input
investigation.df <- read.delim("x29_prospective2.csv")

# The experimental data
verification.df <- read.delim("x29_verification.csv")

plots.base <- ggplot() + 
  theme(plot.title=element_text(hjust=0.5, face="bold"), 
        axis.title=element_text(size=12))


plots.base + geom_point(aes(x=x29, y=y300), verification.df)

both.df <- rbind(investigation.df[c("y200", "x29")], verification.df[c("y200", "x29")])
both.df$type <- c(rep("Prospective", nrow(investigation.df)), 
                  rep("Experimental", nrow(verification.df)))

plots.base + geom_point(aes(x=x29, y=y200, color=type), both.df)
