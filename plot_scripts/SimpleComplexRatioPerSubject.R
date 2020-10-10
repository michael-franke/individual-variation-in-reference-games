require('ggplot2')

source("Monster-data.R")
source("iqr-helpers.r")

# obsPlotL = ggplot(data = get.bsData(comp), aes(x = simple, y = complex)) + 
#   geom_point(shape=19, position=position_jitter(width=0.015,height=.015)) +
#   xlim(c(0,1.015)) + ylim(c(0,1.015)) + geom_abline(intercept = 0, slope = 1, color = "gray")

dd = get.bsData(comp)
dd$CS = paste(dd$complex,dd$simple)
t = as.data.frame(table(dd$CS,dd$simple,dd$complex))
t = droplevels(subset(t, Freq != 0))
colnames(t) = c("CS","simple","complex","Frequency")
obsPlotL = ggplot(t, aes(x = as.numeric(as.character(simple)), y = as.numeric(as.character(complex)), size=Frequency)) + 
  geom_abline(intercept = 0, slope = 1, color = "gray") +  
  geom_point(shape=19) +
  scale_size_continuous(name="Number of\nparticipants",breaks=c(1,2,3)) +
  scale_x_continuous(name="Proportion of target choices (simple)",limits=c(0,1.015)) +
  scale_y_continuous(name="Proportion of target choices (complex)",limits=c(0,1.015)) + 
  theme(plot.margin=unit(c(0,-0.5,0,0),units="cm"))
obsPlotL

ggsave(filename = "../text/monsterpaper/pics/SimpleComplexRatios-Listener.pdf", obsPlotL, width = 5, height = 3.2)


# obsPlotS = ggplot(data = get.bsData(prod), aes(x = simple, y = complex)) + 
#   geom_point(shape=19, position=position_jitter(width=0.015,height=.015)) +
#   xlim(c(0,1.015)) + ylim(c(0,1.015)) + geom_abline(intercept = 0, slope = 1, color = "gray")

dd = get.bsData(prod)
dd$CS = paste(dd$complex,dd$simple)
t = as.data.frame(table(dd$CS,dd$simple,dd$complex))
t = droplevels(subset(t, Freq != 0))
colnames(t) = c("CS","simple","complex","Frequency")

obsPlotS = ggplot(t, aes(x = as.numeric(as.character(simple)), y = as.numeric(as.character(complex)), size=Frequency)) + 
  geom_abline(intercept = 0, slope = 1, color = "gray") +  
  geom_point(shape=19) +
  scale_size_continuous(name="Number of\nparticipants",breaks=c(1,3,5)) +
  scale_x_continuous(name="Proportion of target choices (simple)",limits=c(0,1.015)) +
  scale_y_continuous(name="Proportion of target choices (complex)",limits=c(0,1.015)) + 
  theme(plot.margin=unit(c(0,-0.5,0,0),units="cm"))
obsPlotS

ggsave(filename = "../text/monsterpaper/pics/SimpleComplexRatios-Speaker.pdf", obsPlotS, width = 5, height = 3.2)
