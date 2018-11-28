### reproducing wright 1940 fig. 1: var in allele frequencies at different pop sizes

# load libraries
library(ggplot2)

# calculate sd for distributions
calc.sd <- function(q,Nm){
  var <- q*(1-q)/(4*Nm+1)
  sd <- sqrt(var)*2
  print(sd)
}

# run function
calc.sd(0.5,50) #0.07
calc.sd(0.5,5) #0.22
calc.sd(0.5,0.5) #0.58
calc.sd(0.5,0.05) #0.91

# set up special characters
temp1 <- expression("Nm=50"~sigma[q]==0.07~sqrt(pq))
temp2 <- expression("Nm=5         "~sigma[q]==0.22~sqrt(pq))
temp3 <- expression("Nm=.5       "~sigma[q]==0.58~sqrt(pq))
temp4 <- expression("Nm=.05      "~sigma[q]==0.91~sqrt(pq))

# plot
p1 <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  theme_classic()+
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 0.035)) + 
  annotate("text", x = 0.5, y = 12, label = temp1) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 0.11)) + 
  annotate("text", x = 0.58, y = 4, label = temp2) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 0.29)) + 
  annotate("text", x = 0.58, y = 1.5, label = temp3) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 0.455)) + 
  annotate("text", x = 0.58, y = 0.5, label = temp4) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ylab("")

# look!
p1

