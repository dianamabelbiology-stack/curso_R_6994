

install.packages("xaringanExtra")
library(dplyr)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
library(readxl)
install.packages("ggpmisc")
library(ggpmisc)
install.packages("ggbeeswarm")
library(ggbeeswarm)
library(broom)
install.packages("ggstatsplot")
library(ggstatsplot)
theme_set(theme_bw(16))

#load data
my.formula = 

msleep <- read.csv("msleep.csv")

ggplot(msleep,
       aes(x = log(bodywt),
           y=sleep_total, 
           color=vore)) + 
  geom_point() +
  geom_smooth(method = "lm", )
