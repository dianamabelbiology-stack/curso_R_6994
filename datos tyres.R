
library(ggplot2)

tyre <- read.csv("data/tyre.csv")
head(tyre)


p <- ggplot(tyre, aes(x=Brands, y=Mileage)) +
  geom_boxplot(aes(fill=Brands)) +

#labels
 labs(title = "Gomas en Puerto Rico", 
     x="Gomas", y="kilometers",
     fill="Cars Brands") + 
  
  #themes
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "red")) +
  theme(axis.title.y = element_text(size = 18,
                                    face = "bold",
                                    color = "blue"),
        axis.text.y = element_text(size = 14,
                                   face = "plain",
                                   color = "black")) + 
  theme(axis.title.x = element_text(size = 18,
                                    face = "bold",
                                    color = "blue"),
        axis.text.x = element_text(size = 14,
                                   face = "plain",
                                   color = "black")) +
  #theme legend
  
theme(legend.title =element_text(size = 18,
                                 face = "plain",
                                 color = "red")) + 
  

  ylim(10,100)

# ANOVA-------------------------------------------------------
mod <- aov(Mileage ~ Brands, data = tyre)
summary(mod)

resid_anova <- resid(mod)
shapiro.test(resid_anova)

install.packages("car")
library(car)
car::leveneTest(Mileage ~ Brands, data=tyre)

TukeyHSD(mod)

# safe plot--------------------------------------------------------------}
p
ggsave(filename = "plot.png",
       plot = p,
       width = 8,
       height = 6,
       dpi = 300)
