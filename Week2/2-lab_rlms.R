# lab 2
library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")
library("rlms")
library("devtools")

h <- read.rlms("21wave.sav")
saveRDS(h, "21wave.RDS")
h2 <- select(h, qm1, qm2, qh6,qh5)
describe(h2)
h3 <- rename(h2, ves=qm1, rost=qm2, sex=qh5, b_year=qh6)
h3 <- mutate(h3, vozrast = 2019-b_year)
describe(h3)

glimpse(h3$sex)

h4 <- filter(h3, sex==1) # Мужской
qplot(data = h4, rost, ves)
qplot(data = h4, ves)
